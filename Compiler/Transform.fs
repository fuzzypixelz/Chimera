module Chimera.Compiler.Transform

open Chimera.Compiler.Syntax
open Chimera.Compiler.Common
open Chimera.Compiler.Kernel

type Passes(ctx: Context) =
    member this.ExpandBuiltins(kir: IR) : IR =
        let aux =
            function
            | Signature symbol when ctx.Marker.Has (Builtin Syntax.Add) symbol ->
                let (input, output) =
                    match ctx.Typer.TypeOf(symbol) with
                    | Arrow (x, y) -> (x, y)
                    | _ -> failwith "Error: the `Add` builtin is a function value."

                if not (List.length input = 2) then
                    failwith "Error: the `Add` builtin takes exactly two parameters."

                let params' =
                    [ for type' in input ->
                          let param = ctx.Renamer.Fresh()
                          ctx.Typer.SetType(param, type')
                          param ]

                let (lhs, rhs) = (List.head params', List.last params')
                let result = ctx.Renamer.Fresh()

                let body = Bind(result, Kernel.Add(lhs, rhs), Return(Symbol(result)))
                Function(symbol, params', body)

            | Signature symbol when ctx.Marker.Has (Builtin Syntax.Eq) symbol ->
                // TODO: refactor this part.
                let (input, output) =
                    match ctx.Typer.TypeOf(symbol) with
                    | Arrow (x, y) -> (x, y)
                    | _ -> failwith "Error: the `Eq` builtin is a function value."

                if not (List.length input = 2) then
                    failwith "Error: the `Eq` builtin takes exactly two parameters."

                let params' =
                    [ for type' in input ->
                          let param = ctx.Renamer.Fresh()
                          ctx.Typer.SetType(param, type')
                          param ]

                let (lhs, rhs) = (List.head params', List.last params')
                let result = ctx.Renamer.Fresh()

                let body = Bind(result, Eq(lhs, rhs), Return(Symbol(result)))
                Function(symbol, params', body)

            | Signature symbol when ctx.Marker.Has (Builtin Syntax.Len) symbol ->
                // TODO: refactor this part.
                let (input, output) =
                    match ctx.Typer.TypeOf(symbol) with
                    | Arrow (x, y) -> (x, y)
                    | _ -> failwith "Error: the `Eq` builtin is a function value."

                if not (List.length input = 1) then
                    failwith "Error: the `Eq` builtin takes exactly one parameter."

                let params' =
                    [ for type' in input ->
                          let param = ctx.Renamer.Fresh()
                          ctx.Typer.SetType(param, type')
                          param ]

                let slice = List.head params'
                let result = ctx.Renamer.Fresh()

                let body = Bind(result, Kernel.Len slice, Return(Symbol(result)))
                Function(symbol, params', body)

            | other -> other

        List.map aux kir.defs |> IR.make

    // Infer the types of all symbols in the IR.
    // TODO: perform type-checking.
    // TODO: move all type inference to the typer.
    member this.InferTypes(kir: IR) : IR =
        for def in kir.defs do
            ctx.Typer.InferType(def) |> ignore

        kir

    // Sequence of Identity passes.
    member this.All = this.InferTypes >> this.ExpandBuiltins

// Run of the Identity passes.
let passes (ctx, kir) = (ctx, Passes(ctx).All(kir))
