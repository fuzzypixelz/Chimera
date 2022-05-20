module Chimera.Tests

open NUnit.Framework
open FParsec
open Compiler.Parser
open Compiler.AST
open Compiler.Elaborator
open Compiler.Generator

[<Test>]
let SimpleFunctionItem () =
    let input = "first (x: Int, y: Int): Int = x\n"

    let expected =
        IR.make [ Item.make Option.None (Function("first", [ ("x", Int); ("y", Int) ], Int, (Name "x"))) ]

    match run ast input with
    | Success (actual, _, _) -> Assert.AreEqual(expected, actual)
    | Failure (error, _, _) ->
        printfn "%s" error
        Assert.Fail()

// Helper function to JIT compile and execute a program from a string.
// The program is assumed to be syntactically correct.
let run program =
    match runParserOnString ast () "Test" program with
    | Success (tree, _, _) -> tree |> kernel |> execute
    | Failure (error, _, _) ->
        printfn "%s" error
        failwith "failed to parse a testing program; not so good eh?"

[<Test>]
let FunctionCalls () =
    let input = "fst(x: Int, y: Int): Int = x\nmain(): Int = fst(42, 69)\n"
    Assert.AreEqual(42, run input)

[<Test>]
let NestedBindings () =
    let input = "main(): Int = let x = 13 in let y = 69 in let x = 42 in x\n"
    Assert.AreEqual(42, run input)

[<Test>]
let NestedConditional () =
    let input =
        "not(x: Bool): Bool = if x then false else true\n\
        main(): Int = if not(false) then if not(true) then 13 else 42 else 69\n"

    Assert.AreEqual(42, run input)

[<Test>] // TODO.
let TailCallElimination () = Assert.Pass()
