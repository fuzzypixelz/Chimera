module Chimera.Compiler.Main

open System.IO
open Chimera.Compiler

[<EntryPoint>]
let main _ =
    // TODO: Implement a proper Driver module.
    let stream = File.Open("scratchpad.chi", FileMode.Open)

    Parse.parse stream
    |> Elaborate.kernel
    |> Transform.passes
    |> Generate.execute
    |> printfn "program returned: %i"

    0
