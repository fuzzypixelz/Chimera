module Chimera.Compiler

module Common =
    open System.Collections.Generic

    // The type of variable and function names.
    type Name = string
    // All AST variables are renamed into numbers.
    type Symbol = int
    // Enumeration of Chimera literals.
    type Literal =
        | Integer of int64
        | Boolean of bool

    // Small utility class for renaming variables inside functions.
    // This is a very thin wrapper around a Dictionnary and doesn't do much.
    // It mainly serves as documenation for myself; to keep my sanity.
    type Renamer() =
        let symbols = Dictionary<Name, Symbol>(42)

        // We need a fresh symbol each time we perform a bind.
        // This is because Name's can be reused so we cannot rely on symbols.Count.
        let mutable counter = 0

        // Generate a new "anonymous" symbol, without an underlying name.
        // This is mainly used to introduce let-bindings in Kernel.
        member this.Fresh() : Symbol =
            let symbol = counter
            counter <- counter + 1
            symbol

        // Generates a new symbol for the given name and registers the mapping.
        // Any subsequent calls with the same name will overrite the old name.
        // Used primarily for introducing new names with let-expression.
        // The counter starts from 0
        member this.Bind(name: Name) : Symbol =
            let symbol = this.Fresh()
            symbols[name] <- symbol
            symbol

        // Retrieves the last bound of the given name.
        // For example, if one binds `x` twice in `let x = 1 in let x = 2 in x`,
        // the first call to Bind may return 0, the second 1. Hence when the `x`
        // variable is needed it would refer to the second one, with value = 2.
        member this.SymbolOf(name: Name) : Symbol = symbols.GetValueOrDefault(name)

    // Fancy operator for doing C# implicit type coversion.
    // NOTE: relying on the compiler to do it for you fails in some cases,
    // an alterntive is to sprinkle `: type` around; I'm not doing that.
    // Credit: kvb, StackOverflow.
    let inline (!>) (x: ^a) : ^b =
        ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)

module AST =
    open Common

    // Associativity of an Infix operator.
    type Associativity =
        | None
        | Left
        | Right

    // Precedence of an operator.
    type Precedence = int

    // Export type.
    type Extern =
        | Import
        | Export

    // Enumeration of Chimera attributes.
    // The syntax is a free form list of CamelCase names seperated by spaces,
    // and enclosed in `![ ... ]`. Attribute "arguments" are represented in
    // the same order they appear in the program.
    type Attr =
        | Whatever // For testing purposes :3
        | Inline
        | Entry
        | Extern of Extern * Name
        | Intrinsic of list<Name>
        | Infix of Associativity * Precedence
        | Prefix of Precedence
        | Postfix of Precedence

    // Type annotation.
    // Currently all function parameters need to be annotated, until we
    // achieve a full-fledged HM type system.
    type Ann =
        | Int
        | Word
        | Bool
        | Arrow of list<Ann> * Ann

    // Is this a tail call?
    type Tail = bool

    // Syntactic expression.
    type Expr =
        | Literal of Literal
        | Name of Name
        | Bind of Name * Expr * Expr
        | Cond of Expr * Expr * Expr
        | Call of Tail * Name * list<Expr>

    // Enumeration of Item kinds.
    type ItemKind = Function of Name * list<Name * Ann> * Ann * Expr

    // Top-level Item.
    type Item =
        { attr: option<Attr>
          kind: ItemKind }
        static member make attr kind = { attr = attr; kind = kind }

    // The Abstract Syntax Tree.
    type IR =
        { items: list<Item> }
        static member make items = { items = items }

module Parser =
    open FParsec
    open Common
    open AST

    // NOTE: at the moment the parser is explicitly ASCII-only,
    // this will help avoid UTF-8 issues for now and limit the
    // surface area we have to test and support.
    // Yes, no emojis for now :(

    // Type synonym for less typing :^)
    type Parser<'a> = Parser<'a, unit>

    // Defines Chimera's Horizontal space characters.
    let isHSpace = isAnyOf [| ' '; '\t' |]

    // Horizontal space consumer, i.e no newlines.
    let hsc p = p .>> skipManySatisfy isHSpace

    // General space consumer, includes LF, CR and CRLF.
    let sc p = p .>> spaces

    // Parse a literal string, use this for keywords and operators.
    // This consumes horizontal spaces.
    let symbol s = pstring s |> sc

    // Variant of `symbol` that doesn't parser a newline at the end.
    let hsymbol s = pstring s |> hsc

    // TODO: get rid of all these definitions.
    // Run a parser between a pair of parentheses.
    let parens p = between (symbol "(") (symbol ")") p

    // Used to seperate variable names from their type annotations.
    let colon: Parser<Name> = symbol ":"

    // Used to define function and value items.
    let equal: Parser<Name> = symbol "="

    // User to denote function types and match arms.
    let arrow: Parser<Name> = symbol "->"

    // Used as a seperator inside brackets and parentheses.
    let comma: Parser<Name> = symbol ","

    // Keyword parser constructor.
    // NOTE: Why? because we can't be bothered to type `symbol "keyword"` every time that's why!
    // I don't think it's better to represent keywords as data in the AST because they're
    // just punctuation; not really part of the grammar as I see it.
    let (!@) (keyword: string) : Parser<unit> = symbol keyword |>> ignore

    // Variant of `!@` that doesn't consume a newline at the end.
    let (!@^) (keyword: string) : Parser<unit> = hsymbol keyword |>> ignore

    // Parse a comma-seperated list of elements surrounded by parantheses.
    let tupled p = parens (sepBy (hsc p) comma)

    // Variant of `tupled` that doesn't consume a newline at the end.
    let htupled p =
        between (!@ "(") (!@^ ")") (sepBy (hsc p) comma)

    // Parse a Chimera variable name.
    // The first character can't be an upper case letter.
    // Digits are only allowed starting from the second character.
    let name: Parser<Name> =
        let isAsciiIdStart c = isAsciiLower c
        let isAsciiIdContinue c = isAsciiLetter c || isDigit c

        IdentifierOptions(
            isAsciiIdStart = isAsciiIdStart,
            isAsciiIdContinue = isAsciiIdContinue,
            invalidCharMessage = "Chimera variable names can only contain letters, digits and underscores."
        )
        |> identifier
        |> hsc

    // Parse a Chimera type name.
    // Must start with an upper case letter.
    // The rest an be any alphanumeric character.
    let typename: Parser<Name> =
        let isAsciiIdStart c = isAsciiUpper c
        let isAsciiIdContinue c = isAsciiLetter c || isDigit c

        IdentifierOptions(
            isAsciiIdStart = isAsciiIdStart,
            isAsciiIdContinue = isAsciiIdContinue,
            invalidCharMessage = "Chimera type names can only contain letters, digits and underscores."
        )
        |> identifier
        |> hsc

    // Parse a type annotation.
    let ann: Parser<Ann> =
        let ann, annRef = createParserForwardedToRef ()

        let wordAnn = symbol "Word" >>% Word

        let intAnn = symbol "Int" >>% Int

        let boolAnn = symbol "Bool" >>% Bool

        let arrowAnn =
            parens (sepBy1 ann comma) .>> arrow .>>. ann
            |>> Arrow

        annRef.Value <-
            choice [ wordAnn
                     intAnn
                     boolAnn
                     arrowAnn ]

        ann

    // Parse Chimera literals.
    let literal: Parser<Literal> =
        choice [ pint64 |>> Integer
                 !@ "true" >>% Boolean true
                 !@ "false" >>% Boolean false ]

    // Parse a Chimera expression, a rule to keep in mind is to never consume newlines here.
    let expr: Parser<Expr> =
        let expr, exprRef = createParserForwardedToRef ()

        // NOTE: newlines and expressions have a weird interaction.
        // Because we want to be able to write `{ expr1\n expr2 }` we
        // need to treat newlines after expressions in a special way.
        // This is why we have `sc` and `hsc`.
        // Only use `sc` when you're parsing "nested" expressions,
        // since that wouldn't change the effect of the newline at the edge.
        // Of course, all this would be alleviated if we just used semicolons;
        // but who does that? So C.

        let nameExpr = name |>> Name

        let literalExpr = literal |>> Literal

        let bindExpr =
            tuple3 (!@ "let" >>. name) (equal >>. sc expr) (!@ "in" >>. expr)
            |>> Bind

        let callExpr =
            let become = opt (!@ "become") |>> Option.isSome
            tuple3 become name (htupled <| sc expr) |>> Call

        let condExpr =
            tuple3 (!@ "if" >>. sc expr) (!@ "then" >>. sc expr) (!@ "else" >>. expr)
            |>> Cond

        exprRef.Value <-
            choice [ literalExpr
                     bindExpr
                     condExpr
                     attempt callExpr
                     nameExpr ]
            |> hsc

        expr

    // Parse a function parameter.
    let param: Parser<Name * Ann> = tuple2 (name .>> colon) ann

    // Parse a function item.
    let itemKind: Parser<ItemKind> =
        tuple4 name (tupled param) (colon >>. ann) (equal >>. expr)
        |>> Function
        |> sc

    // Parse a Chimera attribute, this is usually added on top of functions and types.
    let attr: Parser<Attr> = symbol "![Whatever]" >>% Whatever |> sc

    // Transform an itemKind parser into one that supports optional attributes.
    let item: Parser<Item> = pipe2 (opt attr) itemKind Item.make

    // Parse the full syntax tree.
    let ast: Parser<IR> = many item .>> eof |>> IR.make

    // Helper function for running the parser
    let parse s =
        let result = runParserOnStream ast () "" s (System.Text.ASCIIEncoding())

        match result with
        | Success (tree, _, _) -> tree
        | Failure (error, _, _) -> failwith error

module Kernel =
    open System.Collections.Generic
    open Common

    // Enumeration of Chimera's types.
    type Type =
        | Int
        | Word
        | Bool
        | Arrow of list<Type> * Type

    // The Typing Context.
    // This is seperated out into two parts since we choose to keep
    // the names of functions for "nicer" object file symbols.
    type Context() =
        let variables = Dictionary<Symbol, Type>(420)
        let functions = Dictionary<Name, Type>(69)

        // Get the type of a variable.
        member this.TypeOf(variable: Symbol) : Type = variables.GetValueOrDefault(variable)

        // Get the type of a function.
        member this.TypeOf(function': Name) : Type = functions.GetValueOrDefault(function')

        // Set the type of a vatiable.
        member this.SetType(variable: Symbol, type': Type) = variables.Add(variable, type')

        // Set the type of a vatiable.
        // Throws an exception when `type_` is not an Arrow.
        member this.SetType(function': Name, type': Type) =
            match type' with
            | Arrow _ -> functions.Add(function', type')
            // Defensive Programming Go!
            | _ -> failwithf "ICE: %A is not an Arrow type." type'

    // First level of the ANF hiearchy; allowed in Expr and the Return Term.
    type Value =
        | Literal of Literal
        | Symbol of Symbol

    // Is this a tail call?
    type Tail = bool

    // Second level of the ANF hiearchy; not allowed in Value but used in a Binding's RHS.
    type Expr =
        | Value of Value
        | Call of Tail * Name * list<Symbol>

    // Third level of the ANF hiearchy; the highest recursive representation.
    type Term =
        | Return of Value
        | Bind of Symbol * Expr * Term
        | Cond of Expr * Term * Term

    // Top-level definitions.
    type Def = Function of Name * list<Symbol> * Term

    // The Kernel IR.
    type IR =
        { defs: list<Def>
          ctx: Context }
        static member make defs ctx = { defs = defs; ctx = ctx }

module Elaborator =
    open Common
    open AST
    open Kernel

    // Convert an AST annotation into a Kernel IR type.
    // NOTE: At the moment, this is unremarkable administrative work,
    // Is the seperation between Type and Ann really that important?
    let rec toType ann =
        match ann with
        | AST.Int -> Int
        | AST.Word -> Word
        | AST.Bool -> Bool
        | AST.Arrow (input, output) -> Arrow(List.map toType input, toType output)

    type Translator() =
        // Generates unique symbols for variables.
        // Re-using the same structure and changing it place *should* lead
        // to better performance as we're avoiding a number of allocations.
        // However, I can't say without some benchmarks.
        let renamer = Renamer()

        // Holds the types of all functions and variables within the current "module".
        let ctx = Context()

        // The resulting context is very useful for the code generation phase.
        member this.Ctx = ctx

        // Transform an AST Expr into a Term.
        // Beware, this function's logic is quite involved; we are essentially transforming a
        // tree of expessions into assembly language; ANF is suprisingly a very hierarchical IR.
        // The first argument `expr` is the AST Expr we wish to "flatten" in a Term,
        // the `result` is the Kernel Symbol that should point to the value of the expression,
        // lastly, cont (for "continuation") is the rest of the computation (CPS is lurking here);
        // i.e. code that should be placed after evaluating `expr`, which can be for example a Return.
        member this.Flatten(expr: AST.Expr, result: Symbol, cont: Term) : Term =

            // Helper function that captures a pattern here (it's just less typing).
            let bind expr = Bind(result, expr, cont)

            match expr with
            | AST.Literal literal -> Value(Literal literal) |> bind
            | AST.Name name -> Value(Symbol(renamer.SymbolOf(name))) |> bind
            | AST.Bind (name, expr, body) ->
                let boundResult = renamer.Bind(name)
                let bodyTerm = this.Flatten(body, result, cont)
                this.Flatten(expr, boundResult, bodyTerm)
            | AST.Call (tail, name, args) ->
                let results = [ for _ in 1 .. List.length args -> renamer.Fresh() ]
                let aux state (expr, result) = this.Flatten(expr, result, state)
                List.fold aux (bind (Call(tail, name, results))) (List.zip args results)
            | AST.Cond (cond, then', else') ->
                let condResult = renamer.Fresh()
                let thenTerm = this.Flatten(then', result, cont)
                let elseTerm = this.Flatten(else', result, cont)
                let nextTerm = Cond(Value(Symbol(condResult)), thenTerm, elseTerm)
                this.Flatten(cond, condResult, nextTerm)

        // Transform an Item into a Def.
        member this.Definition(item: Item) =
            match item.kind with
            | AST.Function (name, params', return', body) ->

                // Rename function parameter symbols and register their types.
                let aux (param, ann) =
                    let symbol = renamer.Bind(param)
                    let type' = toType ann
                    ctx.SetType(symbol, type')
                    (symbol, type')

                let (symbols, types) = List.unzip <| List.map aux params'

                // FIXME: currently function return types (and values) are hard-coded :Y
                ctx.SetType(name, Arrow(types, toType return'))

                let result = renamer.Fresh()
                Function(name, symbols, this.Flatten(body, result, (Return(Symbol(result)))))

    // Transform an AST into Kernel IR.
    let kernel ast =
        let translator = Translator()

        { defs = List.map translator.Definition ast.items
          ctx = translator.Ctx }

module Generator =
    open System.Collections.Generic
    open LLVMSharp
    open Common
    open Kernel

    let expect msg err = if err then failwith msg

    // Initialize the LLVM Native target.
    // This should be done in the main program according to the API.
    // do !> LLVM.InitializeNativeTarget()
    // |> expect "failed to initialize the native target."

    // Translate a Kernel type into an LLVM type.
    let rec toLLType type' =
        match type' with
        | Int
        | Word -> LLVM.Int64Type()
        | Bool -> LLVM.Int1Type()
        | Arrow (input, output) ->
            let paramTypes = List.map toLLType input |> List.toArray
            LLVM.FunctionType(toLLType output, paramTypes, false)

    let toLLConst literal =
        match literal with
        | Integer i -> LLVM.ConstInt(toLLType Int, uint64 i, true)
        | Boolean b ->
            let llBool = if b then 1uL else 0uL
            LLVM.ConstInt(toLLType Bool, llBool, true)

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
            | Call (tail, name, args) ->
                let fn = LLVM.GetNamedFunction(module', name)
                let llArgs = List.map this.ValueOf args |> List.toArray
                let call = LLVM.BuildCall(builder, fn, llArgs, name)
                LLVM.SetTailCall(call, tail)
                call

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
        member this.RegisterParams(fn: LLVMValueRef, params': list<Symbol>) =
            let aux i param =
                let llParam = LLVM.GetParam(fn, uint i)
                this.SetValue(param, llParam)

            List.iteri aux params'

        // Emit a Kernel Def inside the LLVM module.
        member this.Emit(def: Def) =
            match def with
            | Function (name, params', body) ->
                let type' = ctx.TypeOf(name) |> toLLType
                printfn "%s : %A" name type'
                let fn = LLVM.AddFunction(module', name, type')
                this.RegisterParams(fn, params')
                let entry = LLVM.AppendBasicBlock(fn, "entry")
                LLVM.PositionBuilderAtEnd(builder, entry)
                this.Translate(fn, body)

    // Convert Kernel IR to LLVM IR.
    let compile (program: IR) : LLVMModuleRef =
        let module' = LLVM.ModuleCreateWithName("chimera")
        let emitter = Emitter(program.ctx, module')

        printfn "%A" program

        for def in program.defs do
            emitter.Emit(def)

        module'

    let execute program =
        let module' = compile program
        LLVM.DumpModule(module')

        let mutable error = null

        !> LLVM.VerifyModule(module', LLVMVerifierFailureAction.LLVMAbortProcessAction, &error)
        |> expect "invalid module."

        let mutable engine = LLVMExecutionEngineRef(0)

        !> LLVM.CreateExecutionEngineForModule(&engine, module', &error)
        |> expect error

        let main = LLVM.GetNamedFunction(module', "main")
        let ret = LLVM.RunFunction(engine, main, [||])

        LLVM.GenericValueToInt(ret, false)

open System.IO

[<EntryPoint>]
let main _ =
    let stream = File.Open("sample.chi", FileMode.Open)

    Parser.parse stream
    |> Elaborator.kernel
    |> Generator.execute
    |> printfn "program returned: %A"

    0
