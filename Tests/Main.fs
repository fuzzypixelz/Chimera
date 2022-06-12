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
        "fst(x: Int, y: Int): Int = x
        ![Entry]
        main(): Int = fst(42, 69)"

    Assert.AreEqual(42, run input)

[<Test>]
let NestedBindings () =
    let input =
        "![Entry]
        main(): Int = let x = 13 in let y = 69 in let x = 42 in x"

    Assert.AreEqual(42, run input)

[<Test>]
let NestedConditional () =
    let input =
        "not(x: Bool): Bool = if x then false else true
        ![Entry]
        main(): Int = if not(false) then if not(true) then 13 else 42 else 69"

    Assert.AreEqual(42, run input)

[<Test>]
let PassingFunctionToFunction () =
    let input =
        "fortyTwo(): Int = 42
        zero(): Int = 0
        call(number: () -> Int): Int = number()
        ![Entry]
        main(): Int = if true then call(fortyTwo) else call(zero)"

    Assert.AreEqual(42, run input)

[<Test>]
let ShadowedDefinitionsWithAttrs () =
    let input =
        "main(): Int = 13
        ![Entry]
        main(): Int = 42"

    Assert.AreEqual(42, run input)

[<Test>]
let BuiltinAdd () =
    let input =
        "![Builtin Add]
        add: (Int, Int) -> Int
        ![Entry]
        main(): Int = add(21, 21)"

    Assert.AreEqual(42, run input)

[<Test>]
let CallExternPutchar () =
    let input =
        "![Extern Import \"putchar\"]
        putchar: (Int) -> Int
        ![Entry]
        main(): Int = putchar(42)"

    Assert.AreEqual(42, run input)

[<Test>]
let CallExternAbs () =
    let input =
        "![Extern Import \"abs\"]
        abs: (Int) -> Int
        ![Entry]
        main(): Int = abs(-42)"

    Assert.AreEqual(42, run input)

[<Test>]
let TailCallEliminationMatters () =
    let input =
        "![Builtin Eq]
        eq: (Int, Int) -> Bool
        ![Builtin Add]
        add: (Int, Int) -> Int
        count(start: Int, end: Int): Int =
            if eq(start, end)
            then 42
            else count(add(start, 1), end)
        ![Entry]
        main(): Int =
            count(0, 1000000000)"

    // 139 is the process signal for a SEGFAULT.
    // On UNIX at least? This "Test" is so very not-portable.
    Assert.AreEqual(139, run input)

[<Test>]
let TailCallElimination () =
    let input =
        "![Builtin Eq]
        eq: (Int, Int) -> Bool
        ![Builtin Add]
        add: (Int, Int) -> Int
        count(start: Int, end: Int): Int =
            if eq(start, end)
            then 42
            else become count(add(start, 1), end)
        ![Entry]
        main(): Int =
            count(0, 1000000000)"

    Assert.AreEqual(42, run input)

[<Test>]
let ArrayIndex () =
    let input =
        "![Entry]
        main(): Int = let array = [13, 42, 69] in array[1]"

    Assert.AreEqual(42, run input)

[<Test>]
let ArrayLen () =
    let input =
        "![Builtin Len]
        len: ([]Int) -> Int
        ![Builtin Add]
        add: (Int, Int) -> Int
        ![Entry]
        main(): Int = let array = [0] in add(len(array), 41)"

    Assert.AreEqual(42, run input)

[<Test>]
let UnitType () =
    let input =
        "nothing(): Unit = ()
        ![Entry]
        main(): Int = let unit = nothing() in 42"

    Assert.AreEqual(42, run input)

[<Test>]
let LocalFunction () =
    let input =
        "![Entry]
        main(): Int = let answer(): Int = 42 in answer()"

    Assert.AreEqual(42, run input)

[<Test>]
let GlobalVariable () =
    let input =
        "answer = 42
        ![Entry]
        main(): Int = answer"

    Assert.AreEqual(42, run input)

[<Test>]
let GlobalsAndLocals () =
    let input =
        "answer() : Int = 13
        faux = false
        number = answer()
        ![Entry]
        main() : Int = let 
            leet = 1337
            number = 42
            zero() : Int = 0
        in 
            if faux then zero() else number"

    Assert.AreEqual(42, run input)

[<Test>]
let Comments () =
    let input =
        "{--}
        answer = 42
        --}
        ![Entry]
        -- Toggled comments!
        main () : Int = answer"

    Assert.AreEqual(42, run input)
