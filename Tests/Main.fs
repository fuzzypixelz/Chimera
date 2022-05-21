module Chimera.Tests

open NUnit.Framework
open FParsec
open Compiler.Common
open Compiler.Parser
open Compiler.AST
open Compiler.Elaborator
open Compiler.Generator

[<Test>]
let SimpleFunctionItem () =
    let input = "first (x: Int, y: Int): Int = x\n"

    let expected =
        IR.make [ Function("first", [ ("x", Int); ("y", Int) ], Int, (Name "x")) ]

    match runParserOnString ast (Context()) "" input with
    | Success (actual, _, _) -> Assert.AreEqual(expected, actual)
    | Failure (error, _, _) ->
        printfn "%s" error
        Assert.Fail()

// Helper function to JIT compile and execute a program from a string.
// The program is assumed to be syntactically correct.
let run program =
    let ctx = Context()

    match runParserOnString ast ctx "Test" program with
    | Success (tree, _, _) -> (ctx, tree) |> kernel |> execute
    | Failure (error, _, _) ->
        printfn "%s" error
        failwith "failed to parse a testing program; not so good eh?"

[<Test>]
let FunctionCalls () =
    let input =
        "fst(x: Int, y: Int): Int = x\n\
        ![Entry]\n\
        main(): Int = fst(42, 69)\n"

    Assert.AreEqual(42, run input)

[<Test>]
let NestedBindings () =
    let input =
        "![Entry]\n\
        main(): Int = let x = 13 in let y = 69 in let x = 42 in x\n"

    Assert.AreEqual(42, run input)

[<Test>]
let NestedConditional () =
    let input =
        "not(x: Bool): Bool = if x then false else true\n\
        ![Entry]\n\
        main(): Int = if not(false) then if not(true) then 13 else 42 else 69\n"

    Assert.AreEqual(42, run input)

[<Test>]
let PassingFunctionToFunction () =
    let input =
        "fortyTwo(): Int = 42\n\
        zero(): Int = 0\n\
        call(number: () -> Int): Int = number()\n\
        ![Entry]\n\
        main(): Int = if true then call(fortyTwo) else call(zero)\n"

    Assert.AreEqual(42, run input)

[<Test>] // TODO.
let TailCallElimination () = Assert.Pass()

[<Test>]
let ShadowedDefinitionsWithAttrs () =
    let input =
        // "![Entry]
        "main(): Int = 13\n\
        ![Entry]\n\
        main(): Int = 42\n"

    Assert.AreEqual(42, run input)
