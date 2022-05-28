module Chimera.Compiler.Common

open System.Collections.Generic
open FParsec
open Chimera.Compiler.Syntax
open Chimera.Compiler.Kernel

// The type of variable and function names.
// Enumeration of Chimera's types.
type Type =
    | Int
    | Word
    | Bool
    | Char
    | Arrow of list<Type> * Type

// The Typing Context.
// This is seperated out into two parts since we choose to keep
// the names of functions for "nicer" object file symbols.
type Typer() =
    let variables = Dictionary<Symbol, Type>(420)

    // Get the type of a variable.
    member this.TypeOf(variable: Symbol) : Type = variables.GetValueOrDefault(variable)

    // Set the type of a vatiable.
    member this.SetType(variable: Symbol, type': Type) = variables.Add(variable, type')

    // Infer the type of a Value.
    member this.InferType(value: Value) : Type =
        match value with
        | Literal literal ->
            match literal with
            | Integer _ -> Int
            | Boolean _ -> Bool
            | Character _ -> Char
        | Symbol symbol -> this.TypeOf(symbol)

    // Infer the type of a Term.
    member this.InferType(expr: Expr) : Type =
        match expr with
        | Value value -> this.InferType(value)
        | Add (symbol, _) -> this.TypeOf(symbol)
        | Eq _ -> Bool
        | Len _ -> Word
        | Call (_, symbol, _) ->
            match this.TypeOf(symbol) with
            | Arrow (_, output) -> output
            | _ -> failwith "Error: attempt to call non-function value."
        | Array (symbols, _) ->
            if List.isEmpty symbols then
                failwith "Error: cannot infer type of empty slice."
            else
                this.TypeOf(List.head symbols)
        | Index (symbol, _) -> this.TypeOf(symbol)

    // Infer the type of a Term.
    member this.InferType(term: Term) : Type =
        match term with
        | Return value -> this.InferType(value)
        | Bind (symbol, expr, term) ->
            this.SetType(symbol, this.InferType(expr))
            this.InferType(term)
        | Cond (_, _, term) -> this.InferType(term)

    member this.InferType(def: Def) : Type =
        match def with
        | Function (symbol, params', term) ->
            let type' = Arrow(List.map this.TypeOf params', this.InferType(term))
            this.SetType(symbol, type')
            type'
        // NOTE: function signatures' types are set during Elaboration,
        // this is because we don't want to propagate the annotation
        // all the way down here. The same goes for function parameters above.
        | Signature symbol -> this.TypeOf(symbol)

// Small utility class for renaming variables inside functions.
// This is a very thin wrapper around a Dictionnary and doesn't do much.
// It mainly serves as documenation for myself; to keep my sanity.
type Renamer() =
    // What is the current symbol for a give name?
    let symbols = Dictionary<Name, Symbol>(42)

    // An additional "inverse" map to remember the original name of each unique Symbol.
    // This is necessary for (efficiently) implementing the `NameOf` method.
    let names = Dictionary<Symbol, Name>(42)

    // Sigh.
    // This the Queue (FIFO) of forwardly-bound symbols.
    // All the symbols here will be deqeued during the "actual" binding.
    let forwardedSymbols = Queue<Name * Symbol>()

    // We need a fresh symbol each time we perform a bind.
    // This is because Name's can be reused so we cannot rely on symbols.Count.
    let mutable counter = 0

    // Generate a new "anonymous" symbol, without an underlying name.
    // This is mainly used to introduce let-bindings in Kernel.
    member this.Fresh() : Symbol =
        let symbol = counter
        names[symbol] <- "anonymous"
        counter <- counter + 1
        symbol

    // Generates a new symbol for the given name and registers the mapping.
    // Any subsequent calls with the same name will overrite the old name.
    // Used primarily for introducing new names with let-expression.
    // The counter starts from 0.
    member this.Bind(name: Name) : Symbol =
        let symbol =
            this.ForwardSymbolOf(name)
            |> Option.defaultValue (this.Fresh())

        symbols[name] <- symbol
        names[symbol] <- name
        symbol

    // Imagine we're dealing with the following "bad" code:
    //     ![Builtin Add]
    //     add : (Int, Int) -> Int
    //     ![Builtin Add]
    //     add : (Word, Word) -> Word
    // This is why we need to mark items with attributes by symbol and not name.
    // Marking however is done during parsing because of `Infix` and such shenanigans.
    // This means that we need to assign symbols to some names way before Elaboration,
    // which is where all names are eliminated.
    // The requirement is thus the ability to say:
    // "I want to reserve a symbol for this name until you effectively bind it."
    // After the eventual call to `Bind`, it should be as if we never did this operation.
    // Think of it as asking for the future symbol of a name.
    member this.ForwardBind(name: Name) : Symbol =
        let symbol = this.Fresh()
        forwardedSymbols.Enqueue((name, symbol))
        symbol

    // Was this name forward-bound? If so return the symbol.
    member this.ForwardSymbolOf(name: Name) : option<Symbol> =
        if forwardedSymbols.Count > 0 then
            let (firstName, symbol) = forwardedSymbols.Peek()

            if firstName = name then
                forwardedSymbols.Dequeue() |> ignore
                Some(symbol)
            else
                None
        else
            None

    // Retrieves the last bound of the given name.
    // For example, if one binds `x` twice in `let x = 1 in let x = 2 in x`,
    // the first call to Bind may return 0, the second 1. Hence when the `x`
    // variable is needed it would refer to the second one, with value = 2.
    member this.SymbolOf(name: Name) : Symbol =
        if not (symbols.ContainsKey(name)) then
            failwithf "ICE: attempt to substitute unknown name `%s`" name
        else
            symbols[name]

    // Retrives the Name behind Symbol.
    // While we are mostly using unique Symbol's in Kernel,
    // We'd like to include the original names in the generated code for easier debugging.
    // As a general rule, the Compiler should preserve as much information as possible.
    // Though this will mostly be used for functions and not variables.
    // If the name doesn't we default to the special name "anonymous".
    // Remember, we can do this only because Symbols are unique.
    member this.OriginalNameOf(symbol: Symbol) : Name =
        if not (names.ContainsKey(symbol)) then
            failwithf "ICE: attempt to substitute rogue symbol `%i`" symbol
        else
            names[symbol]

    // "Better" version of `OriginalNameOf` that concatenates the Name and Symbol.
    // The result should be unique since the symbols themselves are unique.
    // PROOF: if f is injective and g is any function, then x |-> (f(x), g(x))
    // is also injective. QED(?)
    member this.UniqueNameOf(symbol: Symbol) : Name =
        this.OriginalNameOf(symbol) + string symbol

// The Attribute logic handler.
// Attributes cannot be applied to just about any item, and they cannot be
// arbitrarily composed. For example ![Inline] and ![Entry] don't sense
// together; even though it's valid syntax.
// This class thus allows its users to query for specific attributes on a
// symbols. Moreover, this is naturally where the Symbol -> Attr mapping is
// stored, so we should defend against invalid combinations of attributes.
type Marker() =
    let directory = Dictionary<Symbol, HashSet<Attr>>(42)

    // Associate the given symbol with an attribute.
    // This will raise an exception in case of logical errors.
    member this.Mark(symbol: Symbol, attr: Attr) =
        if not (directory.ContainsKey(symbol)) then
            directory.Add(symbol, HashSet())

        match attr with
        | Entry ->
            if not (this.FindAll(Entry) |> Seq.isEmpty) then
                failwith "Error: can only apply the entry attribute once!"
            else
                directory[ symbol ].Add(attr) |> ignore
        // TODO: checks.
        | _ -> directory[ symbol ].Add(attr) |> ignore
    // | _ -> failwithf "ICE: attribute `%A` is not yet supported" attr

    // Does this symbol have the following attribute?
    member this.Has (attr: Attr) (symbol: Symbol) : bool =
        if directory.ContainsKey(symbol) then
            directory[ symbol ].Contains(attr)
        else
            false

    // Sequence of all symbols.
    member this.Symbols() : seq<Symbol> =
        seq {
            for symbol in directory.Keys do
                yield symbol
        }

    // Sequence of attributes of a symbol.
    member this.Attrs(symbol: Symbol) : seq<Attr> =
        if directory.ContainsKey(symbol) then
            seq {
                for attr in directory[symbol] do
                    yield attr
            }
        else
            Seq.empty

    // Give me the last element that's marked with the provided attribute.
    member this.FindLastOne(attr: Attr) : option<Symbol> =
        this.Symbols() |> Seq.tryFindBack (this.Has(attr))

    // A sequence over all symbols with the given attribute.
    member this.FindAll(attr: Attr) : seq<Symbol> =
        this.Symbols() |> Seq.filter (this.Has(attr))

    // Given a symbol, find an attribute satisfying the predicate =
    member this.Search(symbol: Symbol, predicate: Attr -> bool) : option<Attr> =
        this.Attrs(symbol) |> Seq.tryFind predicate

// The Compiler Context.
// This class is a container for compiler queries; methods that can be called
// from several points down the pipeline. This works by composing together the
// Renamer, Typer and Marker classes to handle the logical interactions between.
// For example, an attribute like ![NoMangle] might override the name we will use
// during code generation; something like ![Infix ...] will change the way we parse
// the source code; and ![Builtin Add Int] would make the elaborator generate a
// specific term for a function's body.
type Context() =

    // The composing objects of Context.
    member val OperatorParser = OperatorPrecedenceParser<Expr, unit, Context>()
    member val Marker = Marker()
    // Generates unique symbols for variables.
    // Re-using the same structure and changing it place *should* lead
    // to better performance as we're avoiding a number of allocations.
    member val Renamer = Renamer()
    // Holds the types of all functions and variables within the current "module".
    // The resulting information is very useful for the code generation phase.
    member val Typer = Typer()

    // Should we generate code for the give symbol?
    // This can be influenced by DCE, ![Cfg ...] for example.
    member this.ShouldGenerate(symbol: Symbol) : bool = failwith "todo"

    // What name should be used for this symbol during code generation?
    member this.GenerationName(symbol: Symbol) : string =
        // FIXME: this implemetation doesn't consider attributes ...
        let externResult =
            this.Marker.Search(
                symbol,
                // Create those terrible isVariant functions?
                function
                | Extern _ -> true
                | _ -> false
            )

        let entryResult = this.Marker.Has (Entry) (symbol)

        match externResult with
        | Some (Extern (_, name)) -> name
        | _ when entryResult -> "main"
        | _ -> this.Renamer.UniqueNameOf(symbol)

    // Maybe find the entry point of the program for code generation.
    // The entry point doesn't exist if the user hasn't supplied one.
    member this.FindEntry() : option<Name> =
        this.Marker.FindLastOne(Entry)
        |> Option.map this.GenerationName

// Fancy operator for doing C# implicit type coversion.
// NOTE: relying on the compiler to do it for you fails in some cases,
// an alterntive is to sprinkle `: type` around; I'm not doing that.
// Credit: kvb, StackOverflow.
let inline (!>) (x: ^a) : ^b =
    ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)
