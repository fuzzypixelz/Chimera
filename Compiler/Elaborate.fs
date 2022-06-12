module Chimera.Compiler.Elaborate

open Chimera.Compiler.Syntax
open Chimera.Compiler.Common
open Chimera.Compiler.Kernel
open System.Collections.Generic

// Convert an AST annotation into a Kernel IR type.
// NOTE: At the moment, this is unremarkable administrative work,
// Is the seperation between Type and Ann really that important?
let rec toType: Ann -> Type =
    function
    | Syntax.Unit -> Unit
    | Syntax.Int -> Int
    | Syntax.Word -> Word
    | Syntax.Bool -> Bool
    | Syntax.Char -> Char
    | Syntax.Arrow (input, output) -> Arrow(List.map toType input, toType output)
    // The size of the array should be inferred upon construction.
    | Syntax.Array (size, ann) -> Array(size, toType ann)

type Translator(ctx: Context) =

    // Not very functional, shh don't tell.
    // This exists because Bind expressions are flattened into terms, but can
    // also contain items. So in order to "lift" the items up into Defs, we need
    // Normalize to have return type (Term, option<Item>) and thread this through the
    // already compilcated recursive calls. Not very convenient.
    // We instead opt into simply dumping the definitions here in the order we find them.
    let defs = List<Def>()

    // Transform an AST Expr into a Term.
    // Beware, this function's logic is quite involved; we are essentially transforming a
    // tree of expessions into assembly language; ANF is suprisingly a very hierarchical IR.
    // The first argument `expr` is the AST Expr we wish to "flatten" in a Term,
    // the `result` is the Kernel Symbol that should point to the value of the expression,
    // lastly, cont (for "continuation") is the rest of the computation (CPS is lurking here);
    // i.e. code that should be placed after evaluating `expr`, which can be for example a Return.
    member this.Normalize(expr: Syntax.Expr, result: Symbol, cont: Term) : Term =

        // Helper function that captures a pattern here (it's just less typing).
        let bind expr = Bind(result, expr, cont)

        match expr with
        | Syntax.Literal literal -> Value(Literal literal) |> bind

        | Syntax.Name name -> Value(Symbol(ctx.Renamer.SymbolOf(name))) |> bind

        | Syntax.Bind (items, body) ->
            // Start by binding all names and filtering for variables.
            let bindings =
                items
                |> List.choose (function
                    | Syntax.Function _
                    | Syntax.Signature _ as i ->
                        this.ToDef(i)
                        None
                    | Syntax.Variable (name, expr) ->
                        let result = ctx.Renamer.Bind(name)
                        Some(expr, result))

            let bodyTerm = this.Normalize(body, result, cont)
            let aux (expr, result) state = this.Normalize(expr, result, state)

            List.foldBack aux bindings bodyTerm

        | Syntax.Call (become, fn, args) ->
            let results = [ for _ in args -> ctx.Renamer.Fresh() ]
            let aux state (expr, result) = this.Normalize(expr, result, state)
            let fnResult = ctx.Renamer.Fresh()

            let call =
                List.fold aux (bind (Call(become, fnResult, results))) (List.zip args results)

            this.Normalize(fn, fnResult, call)

        | Syntax.Cond (cond, then', else') ->
            let condResult = ctx.Renamer.Fresh()
            let thenTerm = this.Normalize(then', result, cont)
            let elseTerm = this.Normalize(else', result, cont)
            let nextTerm = Cond(Value(Symbol(condResult)), thenTerm, elseTerm)
            this.Normalize(cond, condResult, nextTerm)

        | Syntax.List body ->
            // TODO: refactor this part.
            let results = [ for _ in body -> ctx.Renamer.Fresh() ]
            let aux state (expr, result) = this.Normalize(expr, result, state)
            List.fold aux (bind (Kernel.List results)) (List.zip body results)

        | Syntax.Index (array, index) ->
            let arrayResult = ctx.Renamer.Fresh()
            let indexResult = ctx.Renamer.Fresh()
            let aux state (expr, result) = this.Normalize(expr, result, state)

            List.fold
                aux
                (bind (Index(arrayResult, indexResult)))
                [ (array, arrayResult)
                  (index, indexResult) ]

    // Transform an Item into a Def.
    member this.ToDef: Item -> Unit =
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
            let flatBody = this.Normalize(body, result, (Return(Symbol(result))))
            defs.Add(Function(symbol, symbols, flatBody))

        | Syntax.Signature (name, ann) ->
            let symbol = ctx.Renamer.Bind(name)
            ctx.Typer.SetType(symbol, toType ann)
            defs.Add(Signature symbol)

        | Syntax.Variable (name, expr) ->
            let result = ctx.Renamer.Bind(name)
            let term = this.Normalize(expr, result, Nothing)
            defs.Add(Variable(result, term))

    // Transform the
    member this.ToKernel(items: list<Item>) : Kernel.IR =
        for item in items do
            this.ToDef(item)

        defs |> List.ofSeq |> IR.make

// Transform an AST into Kernel IR.
let kernel (ctx, ast) =
    (ctx, Translator(ctx).ToKernel(ast.items))
