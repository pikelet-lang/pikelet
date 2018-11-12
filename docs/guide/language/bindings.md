---
id: bindings
title: Bindings
sidebar_label: Bindings
---

## Items

Let bindings are made up of items. At the moment these can either be _declarations_
or _definitions_.

A _declaration_ states the type for of an identifier that we should
expect to see in a subsequent _definition_ for that identifier. For example:

```pikelet
let
    greeting : String;              -- declaration
    greeting = "hello there!";      -- definition
in
    ...
```

We can make supply a number of forward declarations before providing their
associated definitions:

```pikelet
let
    one : S32;
    two : S32;

    one = 1;
    two = 2;
in
    ...
```

Values that can be inferred do not require a declaration, although sometimes a
declaration may be useful for documentations purposes!

```pikelet
let
    string = "hello"  -- ok!
    one = 1 : U16     -- ok!
    two = 2           -- error: is this an U8, U16, S64, etc?
in
    ...
```

## Function definitions

We have some nice syntactic sugar for defining functions. For example:

```pikelet
let
    const : (a b : Type) -> a -> b -> a;
    const = \a b x y => x;
in
    const String I32 "hello" 1
```

Could be expressed more cleanly as:

```pikelet
let
    const : (a b : Type) -> a -> b -> a;
    const a b x y = x;
in
    const String I32 "hello" 1
```

## Type aliases

Because Pikelet is dependently typed, we need no other mechanism for making
type aliases. Instead we just use definitions!

```pikelet
let
    Name : Type;
    Name = String;

    bobs-name : Name
    bobs-name = "bob"
in
    ...
```

## Doc comments

Documentation can be provided for above declarations, by using doc comments:

```pikelet
let
    ||| This is a documented definition
    |||
    ||| # Example
    |||
    ||| ```pikelet-repl
    ||| Pikelet> self-aware-string
    ||| "I am a string!" : String
    ||| ```
    self-aware-string : String;
    self-aware-string = "I am a string!";
in
    ...
```
