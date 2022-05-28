module Chimera.Compiler.Generate

open System.Collections.Generic
open LLVMSharp
open Chimera.Compiler.Syntax
open Chimera.Compiler.Common
open Chimera.Compiler.Kernel

let expect msg err = if err then failwith msg

// Initialize the LLVM Native target.
// This should be done in the main program according to the API.
// do !> LLVM.InitializeNativeTarget()
// |> expect "failed to initialize the native target."

// Translate a Kernel type into an LLVM type.
let rec toLLType: Type -> LLVMTypeRef =
    function
    | Int
    | Word -> LLVM.Int64Type()
    | Char -> LLVM.Int8Type()
    | Bool -> LLVM.Int1Type()
    | Arrow (input, output) ->
        let paramTypes = List.map toLLParamType input |> List.toArray
        LLVM.FunctionType(toLLType output, paramTypes, false)

// Create the LLVM representation of a Chimera array.
and toLLSlice (type': Type, len: uint32) =
    LLVM.StructType(
        [| toLLType Word
           LLVM.ArrayType(toLLType type', len) |],
        false
    )

// Translate a Kernel type into an LLVM function parameter type.
// This exists because functions have to be passed in as pointers.
and toLLParamType: Type -> LLVMTypeRef =
    function
    | Arrow (_, _) as type' -> LLVM.PointerType(toLLType type', 0u)
    | other -> toLLType other

// Create the LLVM representation of a Chimera literal.
let toLLConst: Literal -> LLVMValueRef =
    function
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

    // Emit code for evaluating a Kernel Expression inside a function.
    member this.Eval(expr: Expr) : LLVMValueRef =
        match expr with
        | Value value -> this.Convert(value)
        | Add (lhs, rhs) -> LLVM.BuildAdd(builder, this.ValueOf(lhs), this.ValueOf(rhs), "")
        | Eq (lhs, rhs) -> LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, this.ValueOf(lhs), this.ValueOf(rhs), "")
        | Len slice -> LLVM.BuildExtractValue(builder, this.ValueOf(slice), 0u, "")
        | Call (tail, symbol, args) ->
            let name = ctx.GenerationName(symbol)
            let fn = this.ValueOf(symbol)
            let llArgs = List.map this.ValueOf args |> List.toArray
            let call = LLVM.BuildCall(builder, fn, llArgs, name)
            LLVM.SetTailCall(call, tail)
            call
        | _ -> failwith "not yet implemented!"

    // Translate Kernel Term's into LLVM instructions inside a function.
    member this.Translate(fn: LLVMValueRef, term: Term) =
        match term with
        | Return value ->
            LLVM.BuildRet(builder, this.Convert(value))
            |> ignore // I have no idea what these methods return at all!

        | Cond (cond, then', else') ->
            // Initilize all basic blocks.
            let thenBB = LLVM.AppendBasicBlock(fn, "then")
            let elseBB = LLVM.AppendBasicBlock(fn, "else")

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
            this.SetValue(param, llParam)

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

            this.RegisterParams(symbol, params')
            this.AddInliningInfo(symbol)

            let entry = LLVM.AppendBasicBlock(fn, "entry")
            LLVM.PositionBuilderAtEnd(builder, entry)
            this.Translate(fn, body)

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
