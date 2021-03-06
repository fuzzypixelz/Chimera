module Chimera.Compiler.Parse

open FParsec
open Chimera.Compiler.Common
open Chimera.Compiler.Syntax

// By applying this combinator to a parser, you can set a breakpoint here,
// and use the debugger to your heart's content!
let dbg (p: Parser<_, _>) stream = p stream

// Apply this to a parser alongside a label; it allows tracing its execution.
let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

// Reversed version of `|>>`.
let (<<|) (f: 'a -> 'b) (p: Parser<'a, 'u>) : Parser<'b, 'u> = p |>> f

// NOTE: at the moment the parser is explicitly ASCII-only,
// this will help avoid UTF-8 issues for now and limit the
// surface area we have to test and support.
// Yes, no emojis for now :(

// Type synonym for less typing :^)
type Parser<'a> = Parser<'a, Context>

// Line seperator; used to distinguish Items in a sequence.
let endline =
    choice [ skipMany1 newline
             skipChar ';'
             eof ]

// Line comment.
let lineComment: Parser<unit> = pstring "--" >>. skipManyTill anyChar endline

// Block comment.
let blockComment: Parser<unit> =
    let blockComment, blockCommentRef = createParserForwardedToRef ()

    let body = blockComment <|> skipAnyChar

    blockCommentRef.Value <- pstring "{-" >>. skipManyTill body (pstring "-}")

    blockComment

// Defines Chimera's Horizontal space characters.
let isHSpace = isAnyOf [| ' '; '\t' |]

// Horizontal space consumer, i.e no newlines.
let hsc p = p .>> skipManySatisfy isHSpace

// General space consumer, includes LF, CR and CRLF.
let sc p =
    p
    .>> many (spaces1 <|> blockComment <|> lineComment)

// Parse a literal string, use this for keywords and operators.
// This consumes horizontal spaces.
let symbol s = pstring s |> sc

// Variant of `symbol` that doesn't parser a newline at the end.
let hsymbol s = pstring s |> hsc

// Keyword parser constructor.
// NOTE: Why? because we can't be bothered to type `symbol "keyword"` every time that's why!
// I don't think it's better to represent keywords as data in the AST because they're
// just punctuation; not really part of the grammar as I see it.
let (!@) (keyword: string) : Parser<unit> = symbol keyword |>> ignore

// Variant of `!@` that doesn't consume a newline at the end.
let (!@^) (keyword: string) : Parser<unit> = hsymbol keyword |>> ignore

// Parse a comma-seperated list of elements surrounded by parantheses.
let tupled p =
    between (!@ "(") (!@ ")") (sepBy (sc p) (!@ ","))

// Variant of `tupled` that doesn't consume a newline at the end.
let htupled p =
    between (!@ "(") (!@^ ")") (sepBy (sc p) (!@ ","))

// Enumeration of Chimera's keywords.
// NOTE: Bad things will happen if you don't keep this up-to-date!
let keywords =
    [ "false"
      "true"
      "let"
      "in"
      "if"
      "then"
      "else"
      "become" ]

// Parse a Chimera variable name.
// The first character can't be an upper case letter.
// Digits are only allowed starting from the second character.
let name: Parser<Name> =
    let isAsciiIdStart c = isAsciiLower c
    let isAsciiIdContinue c = isAsciiLetter c || isDigit c

    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        invalidCharMessage = "Error: Chimera variable names can only contain letters, digits and underscores."
    )
    |> identifier
    >>= fun s ->
            if List.contains s keywords then
                fail "Error: found keyword in identifier."
            else
                preturn s
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
        invalidCharMessage = "Error: Chimera type names can only contain letters, digits and underscores."
    )
    |> identifier
    |> hsc

// Parse a type annotation.
let ann: Parser<Ann> =
    let ann, annRef = createParserForwardedToRef ()

    let unitAnn = !@^ "Unit" >>% Unit

    let wordAnn = !@^ "Word" >>% Word

    let intAnn = !@^ "Int" >>% Int

    let boolAnn = !@^ "Bool" >>% Bool

    let charAnn = !@^ "Char" >>% Char

    let arrowAnn = tupled ann .>> !@ "->" .>>. ann |>> Arrow

    let constAnn = puint64 |>> Const <|>% Unknown

    let arrayAnn =
        tuple2
        <| between (!@ "[") (!@ "]") constAnn
        <| ann
        |>> Array

    annRef.Value <-
        choice [ unitAnn
                 wordAnn
                 intAnn
                 boolAnn
                 charAnn
                 arrowAnn
                 arrayAnn ]
        |> hsc

    ann

// Parse Chimera literals.
let literal: Parser<Literal> =
    let boolLiteral =
        choice [ !@^ "true" >>% Boolean true
                 !@^ "false" >>% Boolean false ]

    let intLiteral = pint64 |>> Integer

    let charLiteral =
        between (pchar '\'') (pchar '\'') anyChar
        |>> Character

    choice [ boolLiteral
             intLiteral
             charLiteral ]

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

    let builtinAttr =
        choice [ !@ "Add" >>% Add
                 !@ "Eq" >>% Eq
                 !@ "Len" >>% Len ]
        |>> Builtin

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

    between (!@ "![") (!@ "]") inner

// NOTE: we have to do this terribleness because F# doesn't have a notion
// of forward declaration. The important consequence is that expr is define
// in the line that asigns to `expr.Value`.
let expr, exprRef = createParserForwardedToRef ()

// Parse a Chimera Item, without consuming an endline at the end.
let hitem: Parser<Item> =
    // Parse an attribute followed by a name.
    // The attribute doesn't appear in the Syntax Tree but is rather saved in Context.
    // This is done to make Symbol -> Attr lookups easier later on.
    // It's also necessary to support ![Infix ..]-style attributes.
    let attrAndName: Parser<Name> =

        let aux (ctx: Context, attr: option<Attr>, name: Name) =
            let symbol = ctx.Renamer.ForwardBind(name)

            Option.map (fun attr -> ctx.Marker.Mark(symbol, attr)) attr
            |> ignore

            preturn name

        tuple3 getUserState (opt attr) (attempt name)
        >>= aux

    // Parse a function parameter.
    let param: Parser<Name * Ann> = tuple2 (name .>> !@ ":") ann

    let items name =
        let functionItem =
            tuple4
            <| preturn name
            <| tupled param
            <| (!@ ":" >>. ann)
            <| (!@ "=" >>. expr)
            |>> Function

        let variableItem =
            tuple2 (preturn name) (!@ "=" >>. expr)
            |>> Variable

        let signatureItem =
            tuple2 (preturn name) (!@ ":" >>. ann)
            |>> Signature

        choice [ functionItem
                 variableItem
                 signatureItem ]

    attrAndName >>= items |> hsc

// Parse a Chimera Item.
let item: Parser<Item> = hitem .>> endline |> sc

// Parse a Chimera expression, a rule to keep in mind is to never consume newlines here.
exprRef.Value <-
    // NOTE: newlines and expressions have a weird interaction.
    // Because we want to be able to write `{ expr1\n expr2 }` we
    // need to treat newlines after expressions in a special way.
    // This is why we have `sc` and `hsc`.
    // Only use `sc` when you're parsing "nested" expressions,
    // since that wouldn't change the effect of the newline at the edge.
    // Of course, all this would be alleviated if we just used semicolons;
    // but who does that? So C.

    let stringExpr =
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

        let escapedChar =
            pstring "\\"
            >>. (anyOf "\\\"nrt"
                 |>> function
                     | 'n' -> "\n"
                     | 'r' -> "\r"
                     | 't' -> "\t"
                     | c -> string c)

        between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedChar)
        |>> Seq.toList
        |>> List.map (Character >> Literal)

    // Become is seperated from Call because it's not Left-Recursive.
    let becomeExpr =
        !@ "become" >>. expr
        >>= function
            | Call (true, _, _) -> failFatally "Error: there is no use applying `become` twice!"
            | Call (false, callee, args) -> preturn (true, callee, args)
            | _ -> failFatally "Error: `become` can only be applied to function calls."

    let bindExpr =
        let locals =
            attempt (many1 item)
            <|> (hitem |>> List.singleton)

        tuple2 (!@ "let" >>. locals) (!@ "in" >>. expr)

    let condExpr =
        tuple3 (!@ "if" >>. sc expr) (!@ "then" >>. sc expr) (!@ "else" >>. expr)

    let arrayExpr = between (!@ "[") (!@^ "]") (sepBy expr (!@ ","))

    // Parse an infix, prefex or postfix expression.
    let operatorExpr =
        getUserState
        >>= fun ctx -> ctx.OperatorParser.ExpressionParser: Parser<Expr>


    // These are considered primary because they can be identified by the first token.
    let primaryExpr =
        choice [ attempt name |>> Name
                 literal |>> Literal
                 bindExpr |>> Bind
                 condExpr |>> Cond
                 arrayExpr |>> List
                 stringExpr |>> List
                 becomeExpr |>> Call
                 between (!@ "(") (!@^ ")") (expr <|>% Literal Empty) ]

    // Parser Combinators are bad with Left-Recursive Grammars.
    // This function represents the second part of a postfix expression,
    // i.e. the expression that comes after the first primary expression.
    let rec postfixExprEnd pexpr =

        let callExpr =
            tuple3
            <| preturn false // No `become` is parsed here.
            <| preturn pexpr
            <| htupled expr // between (!@ "(") (!@^ ")" <!> ")") ((sepBy expr (!@ ",")) <!> "?")

        let indexExpr =
            tuple2
            <| preturn pexpr
            <| between (!@ "[") (!@^ "]") expr

        choice [ callExpr |>> Call
                 indexExpr |>> Index ]
        >>= postfixExprEnd
        <|> preturn pexpr

    // Parse a primary expression and *then* start composing
    // the full posfix part; this is in order to avoid infinite loops
    // where the parser keeps applying itself to the leftmost sequence.
    let postfixExpr = primaryExpr >>= postfixExprEnd

    postfixExpr |> hsc

// Parse the full syntax tree.
let ast: Parser<IR> =
    (preturn () |> sc) >>. many item .>> eof
    |>> IR.make

// Helper function for running the parser
// Weirdly enough, this is where the Compiler context is created.
let parse s =
    let ctx = Context()
    ctx.OperatorParser.TermParser <- expr

    let result = runParserOnStream ast ctx "" s (System.Text.ASCIIEncoding())

    match result with
    | Success (tree, _, _) -> (ctx, tree)
    | Failure (error, _, _) -> failwith error
