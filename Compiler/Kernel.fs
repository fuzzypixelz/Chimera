module Chimera.Compiler.Kernel

open Chimera.Compiler.Syntax

// First level of the ANF hiearchy; allowed in Expr and the Return Term.
type Value =
    | Literal of Literal
    | Symbol of Symbol

// Second level of the ANF hiearchy; not allowed in Value but used in a Binding's RHS.
type Expr =
    | Value of Value
    | Add of Symbol * Symbol
    | Eq of Symbol * Symbol
    | Len of Symbol
    | Call of Become * Symbol * list<Symbol>
    // FIXME: should the size really be called a "Literal"?
    | Array of list<Symbol> * Literal
    | Index of Symbol * Symbol

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
