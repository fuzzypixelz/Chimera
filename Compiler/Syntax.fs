module Chimera.Compiler.Syntax

type Name = string

// All AST variables are renamed into numbers.
type Symbol = int

type Const =
    | Unknown
    | Const of uint64

// Enumeration of Chimera literals.
type Literal =
    | Empty
    | Integer of int64
    | Boolean of bool
    | Character of char

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
type Builtin =
    | Add
    | Eq
    | Len

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

// Is this a tail call?
type Become = bool

// Associativity of an Infix operator.
// Type annotation.
// Currently all function parameters need to be annotated, until we
// achieve a full-fledged HM type system.
type Ann =
    | Unit
    | Int
    | Word
    | Bool
    | Char
    | Arrow of list<Ann> * Ann
    | Array of Const * Ann

// Syntactic expression.
type Expr =
    | Literal of Literal
    | Name of Name
    // FIXME: this should have a list<Item>
    | Bind of list<Item> * Expr
    | Cond of Expr * Expr * Expr
    | Call of Become * Expr * list<Expr>
    | List of list<Expr>
    | Index of Expr * Expr

// Enumeration of Chimera Items.
and Item =
    | Function of Name * list<Name * Ann> * Ann * Expr
    | Variable of Name * Expr
    | Signature of Name * Ann

// The Abstract Syntax Tree.
type IR =
    { items: list<Item> }
    static member make items = { items = items }
