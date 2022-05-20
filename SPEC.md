# Chimera Language Specification

## Comments

We document our code by writing technical details, motivating decisions and leaving snarky remarks. All of which adds a pleasant experience to whoever ends up reading the code. I encourage you to put as much of your thought process as possible into **comments**.

```hs
-- Seemingly obvious things can be cryptic if left unexplained.

{- I presume you understood that the above is a line comment?
   This one, however, is a block comment,
   It can span many lines.
   By the way, Chimera files are UTF-8 encoded! 🦁 🐐 🐍 -}
```

There is a neat trick you can use to quickly "toggle" a commented block:

```hs
{--
answerToLifeTheUniverseAndEverything = 42
--}
```

Just add or remove the `}` on the first line!

```hs
{--}
answerToLifeTheUniverseAndEverything = 42
--}
```

## Literals

```hs
-- Booleans
good = true
bad = false

-- Integers
number = 13

-- Floats
pi = 3.14

-- Characters
a = 'a'

-- Strings
hello = "Hello, World!"
```

## Functions

A **function** is a first-class value, it can be passed into *and* returned from other functions.
This includes the ability to "capture" variables from the surrounding scope.

Currying is however not encounraged through syntax sugar; there is no tuple type in Chimera as we choose to support multi-parameter functions directly.

A function belongs to the class of `Item`, which is the toplevel things within a `Module` (more on that later). No keyword is used before defining a function. This syntax is reminiscent of Haskell-like languages.

```hs
add (x, y) = z -> x + y + z
six = add(1, 2)(3)
```

Notice here that the expression `add(1, 2)` evaluates to a *function* which is called immediatly afterwards. Moreover, this returned function remembers that `x == 1` and `x == 2` in all subsequent calls.

Another `Item` related to functions is the **signature**, it allows you to specify the type of any value without actually saying what it is. Remember that functions are values too!

```hs
add : (Int, Int) -> (Int -> Int)
six : Int
```

One last element is that you can *optionally* specify type annotations while implementing a function, as opposed to declaring its signature.

```hs
add (x: Word, y: Word): Word = (z: Word): Word -> x + y + z
one = add(0, 0)(1)
```

In Chimera, `Word` is the type of unsigned integers. Here we forced the function type to take a very particular form, and as a result, out number literals will be seen as *unsigned* integers instead of the default *signed* integers.

> NOTE: both `Word` and `Int` have the same size as a pointer on the target platform. This is equivalent to Rust's `usize/isize` and C/C++'s `uintptr_t/intptr_t`.

Lastly, Chimera supports the innovative dot notation syntax, meaning that instead of writing `add(0, 0)(1)` you can do `1.add(0, 0)`. In general, `f(x, y, z, ...)` is the same thing as `x.f(y, z, ...)`.

## Expressions

## Types

## Attributes

## Memory Management

## Effects

## Modules

## Style

## Documentation

## Packaging
