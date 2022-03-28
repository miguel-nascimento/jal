# How Eval should work?

## Core Idea:

Search a main fn in the AST

Eliminate all functions that isn't used by this main function

Eval something like this:

```
main : App (other function) (...others functions)
```

#### Pattern Matching

Match cases could be a lot of lambdas going from a type constructor -> expr

```haskell
let double n =
    match n with
      | Z -> Z;
      | (S x) -> S (S (double x));
```

# Type System

## Why not System F

It was my goal if the language idea was to be 100% explicit typed, but I want to support implicit
and System F is also know to be [undeciable] in type inference

[undeciable]: https://www.quora.com/Why-is-type-inference-in-System-F-undecidable

## HM

See [wikipedia page]

Cons:

1. No Rank-N types

[wikipedia page]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
