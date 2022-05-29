module Chimera.Compiler.Generate

open System.Collections.Generic
open LLVMSharp
open Chimera.Compiler.Syntax
open Chimera.Compiler.Common
open Chimera.Compiler.Kernel

// Apparently LLVMSharp is missing some definitions.
module LLVM' =
    open System.Runtime.InteropServices

    [<DllImport("libLLVM", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LLVMBuildMemCpy")>]
    extern LLVMValueRef BuildMemCpy(LLVMBuilderRef B, LLVMValueRef Dst, uint DstAlign, LLVMValueRef Src, uint SrcAlign, LLVMValueRef Size)

let expect msg err = if err then failwith msg

// Initialize the LLVM Native target.
// This should be done in the main program according to the API.
// do !> LLVM.InitializeNativeTarget()
// |> expect "failed to initialize the native target."

// Translate a Kernel type into an LLVM type.
let rec toLLType: Type -> LLVMTypeRef =
    function
    | Unit -> LLVM.VoidType()
    | Int
    | Word -> LLVM.Int64Type()
    | Char -> LLVM.Int8Type()
    | Bool -> LLVM.Int1Type()
    | Arrow (input, output) ->
        match output with
        | Array _ ->
            // When the return type is an array, we want to put it in the parameters
            // as sret instead, and return void.
            let paramTypes =
                List.map toLLParamType (output :: input)
                |> List.toArray

            LLVM.FunctionType(LLVM.VoidType(), paramTypes, false)
        | _ ->
            let paramTypes = List.map toLLParamType input |> List.toArray
            LLVM.FunctionType(toLLType output, paramTypes, false)

    | Array (_, type') -> failwith "ICE: not supposed to call this right now." // toLLArray (type', uint32 (size))

// Create the LLVM representation of a Chimera array.
and toLLArray (type': Type) =
    LLVM.StructType(
        [| toLLType Word
           LLVM.PointerType(toLLType type', 0u) |],
        false
    )

// Translate a Kernel type into an LLVM function parameter type.
// This exists because functions have to be passed in as pointers.
and toLLParamType: Type -> LLVMTypeRef =
    function
    | Arrow (_, _) as type' -> LLVM.PointerType(toLLType type', 0u)
    | Array (_, type') -> toLLArray type'
    | other -> toLLType other

// Create the LLVM representation of a Chimera literal.
let toLLConst: Literal -> LLVMValueRef =
    function
    // FIXME: this is a really silly way of doing this,
    // but it should be the case that we never try to evaluate this.
    // How to check though?
    | Empty -> LLVMValueRef(0)
    | Integer i -> LLVM.ConstInt(toLLType Int, uint64 i, true)
    | Boolean b ->
        let llBool = if b then 1uL else 0uL
        LLVM.ConstInt(toLLType Bool, llBool, true)
    | Character c ->
        let llChar = uint64 c
        LLVM.ConstInt(toLLType Char, llChar, true)

type Emitter(ctx: Context, module': LLVMModuleRef) =
    // Keep track of the LLVM values corresponding to a given symbol.
    let llValues = Dictionary<Symbol, LLVMValueRef>(42)
    let builder = LLVM.CreateBuilder()

    // What is the LLVM value for this symbol?
    member this.ValueOf(symbol: Symbol) : LLVMValueRef =
        if not (llValues.ContainsKey(symbol)) then
            failwithf "ICE: rogue symbol `%i`" symbol
        else
            llValues.GetValueOrDefault(symbol)

    // Map a symbol to an LLVM value.
    member this.SetValue(symbol: Symbol, value: LLVMValueRef) =
        // if not (llValues.TryAdd(symbol, value)) then
        //     failwithf "ICE: re-mapping of symbol %i while generating LLVM" symbol
        llValues[symbol] <- value

    // Convert a Kernel value into an LLVM value.
    member this.Convert(value: Value) : LLVMValueRef =
        match value with
        | Literal literal -> toLLConst literal
        | Symbol symbol -> this.ValueOf(symbol)

    // Emit code for evaluating a function call.
    member this.EvalCall(fn: LLVMValueRef, callee: Symbol, args: list<Symbol>) : LLVMValueRef =
        let name = ctx.GenerationName(callee)

        let llArgs =
            match ctx.Typer.TypeOf(callee) with
            // If we're trying to call a function that's supposed to return an Array in Kernel,
            // then we have to retrieve the return value through a stack allocation instead.
            // FIXME: this is all very noisy and could be better optimized.
            | Arrow (_, Array (Const size, type')) ->

                let len = toLLConst (Integer(int64 size))
                let ptr = LLVM.BuildArrayAlloca(builder, toLLType type', len, "")

                let mutable array = LLVM.BuildAlloca(builder, toLLArray (type'), "")
                array <- LLVM.BuildLoad(builder, array, "")
                array <- LLVM.BuildInsertValue(builder, array, len, 0u, "")
                array <- LLVM.BuildInsertValue(builder, array, ptr, 1u, "")

                array :: List.map this.ValueOf args
            | _ ->
                // No need to do anything special with the return type.
                List.map this.ValueOf args

        LLVM.BuildCall(builder, fn, llArgs |> List.toArray, "")

    // Emit code for evaluating a Kernel Expression inside a function.
    member this.Eval(expr: Expr) : LLVMValueRef =
        match expr with
        | Value value -> this.Convert(value)
        | Add (lhs, rhs) -> LLVM.BuildAdd(builder, this.ValueOf(lhs), this.ValueOf(rhs), "")
        | Eq (lhs, rhs) -> LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, this.ValueOf(lhs), this.ValueOf(rhs), "")
        | Call (tail, symbol, args) ->
            let fn = this.ValueOf(symbol)
            let call = this.EvalCall(fn, symbol, args)
            LLVM.SetTailCall(call, tail)
            call
        | List symbols ->
            let type' = ctx.Typer.TypeOf(symbols[0])
            let size = Integer(List.length symbols) |> toLLConst
            let ptr = LLVM.BuildArrayAlloca(builder, type' |> toLLType, size, "")

            for (i, symbol) in List.indexed symbols do
                let gep = LLVM.BuildGEP(builder, ptr, [| Integer(i) |> toLLConst |], "")

                LLVM.BuildStore(builder, this.ValueOf(symbol), gep)
                |> ignore

            let mutable localArray = LLVM.BuildAlloca(builder, toLLArray type', "")
            localArray <- LLVM.BuildLoad(builder, localArray, "")
            localArray <- LLVM.BuildInsertValue(builder, localArray, size, 0u, "")
            LLVM.BuildInsertValue(builder, localArray, ptr, 1u, "")
        | Index (array, index) ->
            let ptr = LLVM.BuildExtractValue(builder, this.ValueOf(array), 1u, "")
            let gep = LLVM.BuildGEP(builder, ptr, [| this.ValueOf(index) |], "")
            LLVM.BuildLoad(builder, gep, "")
        | Len array -> LLVM.BuildExtractValue(builder, this.ValueOf(array), 0u, "")

    // Translate a Kernel Return Term.
    member this.TranslateReturn(fn: Symbol, value: Value) =

        match ctx.Typer.TypeOf(fn) with
        // If this function is supposed to return an Array in Kernel,
        // then at this point we're sure that we've been passed a pointer
        // to an array as the first argument. Thus we can simply memcpy
        // the obtained value to the caller's stack.
        | Arrow (_, Array _) ->
            let localArray = this.Convert(value)
            let param = LLVM.GetParam(this.ValueOf(fn), 0u)
            let size = LLVM.BuildExtractValue(builder, param, 0u, "")
            let array = LLVM.BuildExtractValue(builder, param, 1u, "")

            LLVM'.BuildMemCpy(builder, localArray, 0u, array, 0u, size)
            |> ignore

            LLVM.BuildRetVoid(builder)
        | Arrow (_, Unit) ->
            // It's important not to call Convert here: it will ICE!
            LLVM.BuildRetVoid(builder)
        | _ ->
            // Convert the given value to LLVM and return it.
            LLVM.BuildRet(builder, this.Convert(value))

    // Translate Kernel Term's into LLVM instructions inside a function.
    member this.Translate(fn: Symbol, term: Term) =
        match term with
        | Return value -> this.TranslateReturn(fn, value) |> ignore

        | Cond (cond, then', else') ->
            // Initilize all basic blocks.
            let thenBB = LLVM.AppendBasicBlock(this.ValueOf(fn), "then")
            let elseBB = LLVM.AppendBasicBlock(this.ValueOf(fn), "else")

            // Generate code for the condition.
            let llCond = this.Eval(cond)

            LLVM.BuildCondBr(builder, llCond, thenBB, elseBB)
            |> ignore

            // Generate code for the then branch.
            LLVM.PositionBuilderAtEnd(builder, thenBB)
            this.Translate(fn, then')

            // Generate code for the else branch.
            LLVM.PositionBuilderAtEnd(builder, elseBB)
            this.Translate(fn, else')

        | Bind (symbol, expr, term) ->
            let llValue = this.Eval(expr)
            this.SetValue(symbol, llValue)
            this.Translate(fn, term)

    // Map the function parameter symbols to LLVM values for later use.
    member this.RegisterParams(symbol: Symbol, params': list<Symbol>) =
        let fn = this.ValueOf(symbol)

        let aux i param =
            let llParam = LLVM.GetParam(fn, uint i)

            let llValue =
                match ctx.Typer.TypeOf(param) with
                | Array (_, type') ->
                    let size = LLVM.BuildExtractValue(builder, llParam, 0u, "")
                    let ptr = LLVM.BuildExtractValue(builder, llParam, 1u, "")
                    let localPtr = LLVM.BuildArrayAlloca(builder, toLLType type', size, "")

                    LLVM'.BuildMemCpy(builder, localPtr, 0u, ptr, 0u, size)
                    |> ignore

                    let mutable localArray = LLVM.BuildAlloca(builder, toLLArray (type'), "")
                    localArray <- LLVM.BuildLoad(builder, localArray, "")
                    localArray <- LLVM.BuildInsertValue(builder, localArray, size, 0u, "")
                    LLVM.BuildInsertValue(builder, localArray, ptr, 1u, "")

                | _ -> llParam

            this.SetValue(param, llValue)

        List.iteri aux params'

    member this.AddInliningInfo(symbol: Symbol) =
        let fn = this.ValueOf(symbol)
        let lcx = LLVM.GetModuleContext(module')

        let aux name =
            let kind = LLVM.GetEnumAttributeKindForName(name, String.length name)
            // FIXME: I'm not sure what the last parameter does.
            let attr = LLVM.CreateEnumAttribute(lcx, kind, 0uL)
            LLVM.AddAttributeAtIndex(fn, LLVMAttributeIndex.LLVMAttributeFunctionIndex, attr)

        if ctx.Marker.Has (Inline Always) symbol then
            aux "alwaysinline"

        if ctx.Marker.Has (Inline Never) symbol then
            aux "noinline"

        if ctx.Marker.Has (Inline Hint) symbol then
            aux "inlinehint"

    // Emit a Kernel Def inside the LLVM module.
    member this.Emit: Def -> unit =
        function
        | Function (symbol, params', body) ->
            let type' = ctx.Typer.TypeOf(symbol) |> toLLType
            let name = ctx.GenerationName(symbol)
            let fn = LLVM.AddFunction(module', name, type')
            this.SetValue(symbol, fn)

            this.AddInliningInfo(symbol)

            let entry = LLVM.AppendBasicBlock(fn, "entry")
            LLVM.PositionBuilderAtEnd(builder, entry)
            this.RegisterParams(symbol, params')

            this.Translate(symbol, body)

        | Signature symbol ->
            // FIXME: refactor this part.
            let type' = ctx.Typer.TypeOf(symbol) |> toLLType
            let name = ctx.GenerationName(symbol)
            let fn = LLVM.AddFunction(module', name, type')
            LLVM.SetLinkage(fn, LLVMLinkage.LLVMExternalLinkage)
            this.SetValue(symbol, fn)

let execute (ctx, kir) =
    let module' = LLVM.ModuleCreateWithName("scratchpad.chi")
    let emitter = Emitter(ctx, module')

    System.IO.File.WriteAllTextAsync("scratchpad.k", kir.ToString())
    |> Async.AwaitTask
    |> ignore

    for def in kir.defs do
        emitter.Emit(def)

    let mutable error = null

    !> LLVM.VerifyModule(module', LLVMVerifierFailureAction.LLVMAbortProcessAction, &error)
    |> expect error

    !> LLVM.PrintModuleToFile(module', "scratchpad.ll", &error)
    |> expect error

    async {
        let clang =
            System.Diagnostics.Process.Start(
                "clang",
                "-x ir \
                    scratchpad.ll \
                    -o scratchpad \
                    -Wno-override-module \
                    -O0"
            )

        do! clang.WaitForExitAsync() |> Async.AwaitTask
        let program = System.Diagnostics.Process.Start("./scratchpad")
        do! program.WaitForExitAsync() |> Async.AwaitTask
        return program.ExitCode
    }
    |> Async.RunSynchronously
