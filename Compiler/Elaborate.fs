module Chimera.Compiler.Elaborate

open Chimera.Compiler.Syntax
open Chimera.Compiler.Common
open Chimera.Compiler.Kernel

// Convert an AST annotation into a Kernel IR type.
// NOTE: At the moment, this is unremarkable administrative work,
// Is the seperation between Type and Ann really that important?
let rec toType: Ann -> Type =
    function
    | Syntax.Int -> Int
    | Syntax.Word -> Word
    | Syntax.Bool -> Bool
    | Syntax.Char -> Char
    | Syntax.Arrow (input, output) -> Arrow(List.map toType input, toType output)

type Translator(ctx: Context) =

    // Transform an AST Expr into a Term.
    // Beware, this function's logic is quite involved; we are essentially transforming a
    // tree of expessions into assembly language; ANF is suprisingly a very hierarchical IR.
    // The first argument `expr` is the AST Expr we wish to "flatten" in a Term,
    // the `result` is the Kernel Symbol that should point to the value of the expression,
    // lastly, cont (for "continuation") is the rest of the computation (CPS is lurking here);
    // i.e. code that should be placed after evaluating `expr`, which can be for example a Return.
    member this.Flatten(expr: Syntax.Expr, result: Symbol, cont: Term) : Term =

        // Helper function that captures a pattern here (it's just less typing).
        let bind expr = Bind(result, expr, cont)

        match expr with
        | Syntax.Literal literal -> Value(Literal literal) |> bind
        | Syntax.Name name -> Value(Symbol(ctx.Renamer.SymbolOf(name))) |> bind
        | Syntax.Bind (name, expr, body) ->
            let boundResult = ctx.Renamer.Bind(name)
            let bodyTerm = this.Flatten(body, result, cont)
            this.Flatten(expr, boundResult, bodyTerm)
        | Syntax.Call (become, fn, args) ->
            let results = [ for _ in args -> ctx.Renamer.Fresh() ]
            let aux state (expr, result) = this.Flatten(expr, result, state)
            let fnResult = ctx.Renamer.Fresh()

            let call =
                List.fold aux (bind (Call(become, fnResult, results))) (List.zip args results)

            this.Flatten(fn, fnResult, call)
        | Syntax.Cond (cond, then', else') ->
            let condResult = ctx.Renamer.Fresh()
            let thenTerm = this.Flatten(then', result, cont)
            let elseTerm = this.Flatten(else', result, cont)
            let nextTerm = Cond(Value(Symbol(condResult)), thenTerm, elseTerm)
            this.Flatten(cond, condResult, nextTerm)
        | Syntax.Array body ->
            // TODO: refactor this part.
            let results = [ for _ in body -> ctx.Renamer.Fresh() ]
            let len = Integer(List.length results)
            let aux state (expr, result) = this.Flatten(expr, result, state)
            List.fold aux (bind (Array(results, len))) (List.zip body results)
        | Syntax.Index (array, index) ->
            let arrayResult = ctx.Renamer.Fresh()
            let indexResult = ctx.Renamer.Fresh()
            let aux state (expr, result) = this.Flatten(expr, result, state)

            List.fold
                aux
                (bind (Index(arrayResult, indexResult)))
                [ (array, arrayResult)
                  (index, indexResult) ]

    // Transform an Item into a Def.
    member this.Definition: Item -> Def =
        function
        | Syntax.Function (name, params', return', body) ->

            // Rename function parameter symbols and register their types.
            let aux (param, ann) =
                let symbol = ctx.Renamer.Bind(param)
                let type' = toType ann
                ctx.Typer.SetType(symbol, type')
                (symbol, type')

            let (symbols, types) = List.unzip <| List.map aux params'

            let symbol = ctx.Renamer.Bind(name)
            ctx.Typer.SetType(symbol, Arrow(types, toType return'))

            let result = ctx.Renamer.Fresh()
            let flatBody = this.Flatten(body, result, (Return(Symbol(result))))
            Function(symbol, symbols, flatBody)
        | Syntax.Signature (name, ann) ->
            let symbol = ctx.Renamer.Bind(name)
            ctx.Typer.SetType(symbol, toType ann)
            Signature symbol

// Transform an AST into Kernel IR.
let kernel (ctx, ast) =
    let translator = Translator(ctx)

    let kir =
        List.map translator.Definition ast.items
        |> IR.make

    (ctx, kir)
