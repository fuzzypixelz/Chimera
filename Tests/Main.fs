module Chimera.Tests

open NUnit.Framework
open FParsec
open Chimera.Compiler.Common
open Chimera.Compiler.Syntax
open Chimera.Compiler.Parse
open Chimera.Compiler.Elaborate
open Chimera.Compiler.Transform
open Chimera.Compiler.Generate

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
    | Success (tree, _, _) -> (ctx, tree) |> kernel |> passes |> execute
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

[<Test>]
let ShadowedDefinitionsWithAttrs () =
    let input =
        "main(): Int = 13\n\
        ![Entry]\n\
        main(): Int = 42\n"

    Assert.AreEqual(42, run input)

[<Test>]
let BuiltinAdd () =
    let input =
        "![Builtin Add]\n\
        add: (Int, Int) -> Int\n\
        ![Entry]\n\
        main(): Int = add(21, 21)\n"

    Assert.AreEqual(42, run input)

[<Test>]
let CallExternPutchar () =
    let input =
        "![Extern Import \"putchar\"]\n\
        putchar: (Int) -> Int\n\
        ![Entry]\n\
        main(): Int = putchar(42)\n"

    Assert.AreEqual(42, run input)

[<Test>]
let CallExternAbs () =
    let input =
        "![Extern Import \"abs\"]\n\
        abs: (Int) -> Int\n\
        ![Entry]\n\
        main(): Int = abs(-42)\n"

    Assert.AreEqual(42, run input)

[<Test>]
let TailCallEliminationMatters () =
    let input =
        "![Builtin Eq]\n\
        eq: (Int, Int) -> Bool\n\
        ![Builtin Add]\n\
        add: (Int, Int) -> Int\n\
        count(start: Int, end: Int): Int =\n\
            if eq(start, end)\n\
            then 42\n\
            else count(add(start, 1), end)\n\
        ![Entry]\n\
        main(): Int =\n\
            count(0, 1000000000)"

    // 139 is the process signal for a SEGFAULT.
    // On UNIX at least? This "Test" is so very not-portable.
    Assert.AreEqual(139, run input)

[<Test>]
let TailCallElimination () =
    let input =
        "![Builtin Eq]\n\
        eq: (Int, Int) -> Bool\n\
        ![Builtin Add]\n\
        add: (Int, Int) -> Int\n\
        count(start: Int, end: Int): Int =\n\
            if eq(start, end)\n\
            then 42\n\
            else become count(add(start, 1), end)\n\
        ![Entry]\n\
        main(): Int =\n\
            count(0, 1000000000)"

    Assert.AreEqual(42, run input)

[<Test>]
let ArrayIndex () =
    let input =
        "![Entry]\n\
        main(): Int = let array = [13, 42, 69] in array[1]"

    Assert.AreEqual(42, run input)

[<Test>]
let ArrayLen () =
    let input =
        "![Builtin Len]\n\
        len: ([Int]) -> Int\n\
        ![Builtin Add]\n\
        add: (Int, Int) -> Int\n\
        ![Entry]\n\
        main(): Int = let array = [0] in add(len(array), 41)"

    Assert.AreEqual(42, run input)
