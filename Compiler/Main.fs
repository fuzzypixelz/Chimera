module Chimera.Compiler

module Common =
    open System.Collections.Generic
    open FParsec

    // The type of variable and function names.
    type Name = string

    // All AST variables are renamed into numbers.
    type Symbol = int

    // Enumeration of Chimera literals.
    type Literal =
        | Integer of int64
        | Boolean of bool

    type Associativity =
        | Nothing
        | Left
        | Right

    // Precedence of an operator.
    type Precedence = int

    // Inlining information.
    type Inline =
        | Always
        | Never
        | Hint

    // Export type.
    type Extern =
        | Import
        | Export

    // Compiler builtins.
    type Builtin = | Add

    // Enumeration of Chimera attributes.
    // The syntax is a free form list of CamelCase names seperated by spaces,
    // and enclosed in `![ ... ]`. Attribute "arguments" are represented in
    // the same order they appear in the program.
    type Attr =
        | Whatever // For testing purposes :3
        | Entry
        | Inline of Inline
        | Extern of Extern * Name
        | Builtin of Builtin
        | Infix of Associativity * Precedence
        | Prefix of Precedence
        | Postfix of Precedence

    // Enumeration of Chimera's types.
    type Type =
        | Int
        | Word
        | Bool
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
            | _ -> directory[ symbol ].Add(attr) |> ignore
        // | _ -> failwithf "ICE: attribute `%A` is not yet supported" attr

        // Does this symbol have the following attribute?
        member this.Has (attr: Attr) (symbol: Symbol) : bool =
            if directory.ContainsKey(symbol) then
                directory[ symbol ].Contains(attr)
            else
                false

        // Sequence of all symbols.
        member this.SymbolSeq() : seq<Symbol> =
            seq {
                for symbol in directory.Keys do
                    yield symbol
            }

        // Sequence of attributes of a symbol.
        member this.AttrSeq(symbol: Symbol) : seq<Attr> =
            seq {
                for attr in directory[symbol] do
                    yield attr
            }

        // Give me the last element that's marked with the provided attribute.
        member this.FindLastOne(attr: Attr) : option<Symbol> =
            this.SymbolSeq()
            |> Seq.tryFindBack (this.Has(attr))

        // A sequence over all symbols with the given attribute.
        member this.FindAll(attr: Attr) : seq<Symbol> =
            this.SymbolSeq() |> Seq.filter (this.Has(attr))

        // Given a symbol, find an attribute satisfying the predicate =
        member this.Search(symbol: Symbol, predicate: Attr -> bool) : option<Attr> =
            this.AttrSeq(symbol) |> Seq.tryFind predicate

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
        member val OperatorParser = OperatorPrecedenceParser()
        member val Marker = Marker()
        member val Renamer = Renamer()
        member val Typer = Typer()

        // Should we generate code for the give symbol?
        // This can be influenced by DCE, ![Cfg ...] for example.
        member this.ShouldGenerate(symbol: Symbol) : bool = failwith "todo"

        // What name should be used for this symbol during code generation?
        member this.GenerationName(symbol: Symbol) : string =
            // FIXME: this implemetation doesn't consider attributes ...
            let result =
                this.Marker.Search(
                    symbol,
                    // Create those terrible isVariant functions?
                    fun a ->
                        match a with
                        | Extern _ -> true
                        | _ -> false
                )

            match result with
            | Some (Extern (_, name)) -> name
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

module AST =
    open Common

    // Associativity of an Infix operator.
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

    // Enumeration of Chimera Items.
    type Item =
        | Function of Name * list<Name * Ann> * Ann * Expr
        | Signature of Name * Ann

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
    type Parser<'a> = Parser<'a, Context>

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
            parens (sepBy ann comma) .>> arrow .>>. ann
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

    // Parse an infix, prefex or postfix expression.
    let operator: Parser<Expr> =
        getUserState
        >>= fun ctx -> ctx.OperatorParser.ExpressionParser

    // Parse a function parameter.
    let param: Parser<Name * Ann> = tuple2 (name .>> colon) ann

    // Parse a function item.
    let item: Parser<Item> =
        let functionItem =
            tuple4 name (tupled param) (colon >>. ann) (equal >>. expr)
            |>> Function

        let signatureItem = tuple2 name (colon >>. ann) |>> Signature

        choice [ attempt functionItem
                 signatureItem ]
        |> sc

    // Parse a Chimera attribute, this is usually added on top of functions and types.
    let attr: Parser<Attr> =
        let inlineAttr =
            choice [ !@ "Always" >>% Always
                     !@ "Never" >>% Never
                     !@ "Hint" >>% Hint ]
            |> opt
            // Is this a sane default?
            |>> Option.defaultValue Hint
            |>> Inline

        let builtinAttr = choice [ !@ "Add" >>% Add ] |>> Builtin

        let externAttr =
            tuple2
                (choice [ !@ "Import" >>% Import
                          !@ "Export" >>% Export ])
                (!@ "\"" >>. charsTillString "\"" true 1337)
            |>> Extern

        let inner =
            choice [ !@ "Whatever" >>% Whatever
                     !@ "Entry" >>% Entry
                     !@ "Inline" >>. inlineAttr
                     !@ "Builtin" >>. builtinAttr
                     !@ "Extern" >>. externAttr ]

        !@ "![" >>. inner .>> !@ "]"

    // Ask the Compiler Context to mark this item with its attribute.
    // This is done right after parsing the item itself.
    // FIXME: this doesn't work with recursive operators.
    let registerAttr (attr: option<Attr>, item: Item) : Parser<Item> =
        let aux (name: Name) (ctx: Context) =
            let symbol = ctx.Renamer.ForwardBind(name)

            Option.map (fun attr -> ctx.Marker.Mark(symbol, attr)) attr
            |> ignore

            preturn item

        getUserState
        >>= aux (
            match item with
            | Function (name, _, _, _) -> name
            | Signature (name, _) -> name
        )

    // Transform an itemKind parser into one that supports optional attributes.
    let attrThenItem: Parser<Item> = tuple2 (opt attr) item >>= registerAttr

    // Parse the full syntax tree.
    let ast: Parser<IR> = many attrThenItem .>> eof |>> IR.make

    // Helper function for running the parser
    // Weirdly enough, this is where the Compiler context is created.
    let parse s =
        let ctx = Context()
        ctx.OperatorParser.TermParser <- expr

        let result = runParserOnStream ast ctx "" s (System.Text.ASCIIEncoding())

        match result with
        | Success (tree, _, _) -> (ctx, tree)
        | Failure (error, _, _) -> failwith error

module Kernel =
    open Common

    // First level of the ANF hiearchy; allowed in Expr and the Return Term.
    type Value =
        | Literal of Literal
        | Symbol of Symbol

    // Is this a tail call?
    type Tail = bool

    // Second level of the ANF hiearchy; not allowed in Value but used in a Binding's RHS.
    type Expr =
        | Value of Value
        | Add of Symbol * Symbol
        | Call of Tail * Symbol * list<Symbol>

    // Third level of the ANF hiearchy; the highest recursive representation.
    type Term =
        | Return of Value
        | Bind of Symbol * Expr * Term
        | Cond of Expr * Term * Term

    // Top-level definitions.
    type Def =
        | Function of Symbol * list<Symbol> * Term
        | Signature of Symbol

    // The Kernel IR.
    type IR =
        { defs: list<Def> }
        static member make defs = { defs = defs }

module Elaborator =
    open AST
    open Common
    open Kernel

    // Convert an AST annotation into a Kernel IR type.
    // NOTE: At the moment, this is unremarkable administrative work,
    // Is the seperation between Type and Ann really that important?
    let rec toType (ann: Ann) : Type =
        match ann with
        | AST.Int -> Int
        | AST.Word -> Word
        | AST.Bool -> Bool
        | AST.Arrow (input, output) -> Arrow(List.map toType input, toType output)

    // ???
    type Translator(ctx: Context) =
        // Holds the types of all functions and variables within the current "module".
        // The resulting context is very useful for the code generation phase.
        // member val Typer = Typer()

        // Generates unique symbols for variables.
        // Re-using the same structure and changing it place *should* lead
        // to better performance as we're avoiding a number of allocations.
        // However, I can't say without some benchmarks.
        // Sometimes we wish to refer to the original names of symbols.
        // member val Renamer = Renamer()

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
            | AST.Name name -> Value(Symbol(ctx.Renamer.SymbolOf(name))) |> bind
            | AST.Bind (name, expr, body) ->
                let boundResult = ctx.Renamer.Bind(name)
                let bodyTerm = this.Flatten(body, result, cont)
                this.Flatten(expr, boundResult, bodyTerm)
            | AST.Call (tail, name, args) ->
                let results = [ for _ in 1 .. List.length args -> ctx.Renamer.Fresh() ]
                let aux state (expr, result) = this.Flatten(expr, result, state)
                let fnSymbol = ctx.Renamer.SymbolOf(name)
                List.fold aux (bind (Call(tail, fnSymbol, results))) (List.zip args results)
            | AST.Cond (cond, then', else') ->
                let condResult = ctx.Renamer.Fresh()
                let thenTerm = this.Flatten(then', result, cont)
                let elseTerm = this.Flatten(else', result, cont)
                let nextTerm = Cond(Value(Symbol(condResult)), thenTerm, elseTerm)
                this.Flatten(cond, condResult, nextTerm)

        // Transform an Item into a Def.
        member this.Definition(item: Item) =
            match item with
            | AST.Function (name, params', return', body) ->

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
            | AST.Signature (name, ann) ->
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

// Collection of Kernel -> Kernel transformations.
module Identity =
    open Common
    open Kernel

    type Passes(ctx: Context) =
        member this.ExpandBuiltins(kir: IR) : IR =
            let aux def =
                match def with
                | Signature symbol when ctx.Marker.Has (Builtin Common.Add) symbol ->
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

                    let body = Bind(result, Add(lhs, rhs), Return(Symbol(result)))
                    Function(symbol, params', body)

                | other -> other

            List.map aux kir.defs |> IR.make

        member this.All = this.ExpandBuiltins

    // Run of the Identity passes.
    let passes (ctx, kir) = (ctx, Passes(ctx).All(kir))

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
            let paramTypes = List.map toLLParamType input |> List.toArray
            LLVM.FunctionType(toLLType output, paramTypes, false)

    // Translate a Kernel type into an LLVM function parameter type.
    // This exists because functions have to be passed in as pointers.
    and toLLParamType type' =
        let llType = toLLType type'

        match type' with
        | Arrow (_, _) -> LLVM.PointerType(llType, 0u)
        | _ -> llType

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
            | Add (lhs, rhs) -> LLVM.BuildAdd(builder, this.ValueOf(lhs), this.ValueOf(rhs), "")
            | Call (tail, symbol, args) ->
                let name = ctx.GenerationName(symbol)
                let fn = this.ValueOf(symbol)
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
        member this.RegisterParams(symbol: Symbol, params': list<Symbol>) =
            let fn = this.ValueOf(symbol)

            let aux i param =
                let llParam = LLVM.GetParam(fn, uint i)
                this.SetValue(param, llParam)

            List.iteri aux params'

        member this.AddInliningInfo(symbol: Symbol) =
            let fn = this.ValueOf(symbol)
            let lcx = LLVM.GetModuleContext(module')

            let aux name =
                let kind = LLVM.GetEnumAttributeKindForName(name, String.length name)
                // FIXME: I'm not sure what the last parameter does.
                let attr = LLVM.CreateEnumAttribute(lcx, kind, 0uL)
                LLVM.AddAttributeAtIndex(fn, LLVMAttributeIndex.LLVMAttributeFunctionIndex, attr)

            if ctx.Marker.Has (Inline Always) symbol then
                aux "alwaysinline"

            if ctx.Marker.Has (Inline Never) symbol then
                aux "noinline"

            if ctx.Marker.Has (Inline Hint) symbol then
                aux "inlinehint"

        // Emit a Kernel Def inside the LLVM module.
        member this.Emit(def: Def) =
            match def with
            | Function (symbol, params', body) ->
                let type' = ctx.Typer.TypeOf(symbol) |> toLLType
                let name = ctx.GenerationName(symbol)
                let fn = LLVM.AddFunction(module', name, type')
                this.SetValue(symbol, fn)

                this.RegisterParams(symbol, params')
                this.AddInliningInfo(symbol)

                let entry = LLVM.AppendBasicBlock(fn, "entry")
                LLVM.PositionBuilderAtEnd(builder, entry)
                this.Translate(fn, body)
            | Signature symbol ->
                // FIXME: refactor this part.
                let type' = ctx.Typer.TypeOf(symbol) |> toLLType
                let name = ctx.GenerationName(symbol)
                let fn = LLVM.AddFunction(module', name, type')
                this.SetValue(symbol, fn)

    let execute (ctx, kir) =
        let module' = LLVM.ModuleCreateWithName("chimera")
        let emitter = Emitter(ctx, module')

        printfn "%A" kir

        for def in kir.defs do
            emitter.Emit(def)

        LLVM.DumpModule(module')

        let mutable error = null

        !> LLVM.VerifyModule(module', LLVMVerifierFailureAction.LLVMAbortProcessAction, &error)
        |> expect error

        let mutable engine = LLVMExecutionEngineRef(0)

        !> LLVM.CreateExecutionEngineForModule(&engine, module', &error)
        |> expect error

        let aux entry =
            let main = LLVM.GetNamedFunction(module', entry)
            let ret = LLVM.RunFunction(engine, main, [||])

            LLVM.GenericValueToInt(ret, false)

        let maybeEntry = ctx.FindEntry()

        Option.map aux maybeEntry
        |> Option.defaultValue 0uL

open System.IO

[<EntryPoint>]
let main _ =
    // TODO: Implement a proper Driver module.
    let stream = File.Open("scratchpad.chi", FileMode.Open)

    Parser.parse stream
    |> Elaborator.kernel
    |> Identity.passes
    |> Generator.execute
    |> printfn "program returned: %A"

    0
