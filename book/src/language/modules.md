# Modules

## Contents

- [Items](#items)
- [Function definitions](#function-definitions)
- [Type aliases](#type-aliases)
- [Doc comments](#doc-comments)

## Items

Modules are made up of items. At the moment these can either be _declarations_
or _definitions_.

A _declaration_ states the type for of an identifier that we should
expect to see in a subsequent _definition_ for that identifier. For example:

```pikelet
greeting : String;              -- declaration
greeting = "hello there!";      -- definition
```

We can make supply a number of forward declarations before providing their
associated definitions:

```pikelet
one : S32;
two : S32;

one = 1;
two = 1;
```

Values that can be inferred do not require a declaration, although sometimes a
declaration may be useful for documentations purposes!

```pikelet
string = "hello"  -- ok!
one = 1 : U16     -- ok!
two = 2           -- error: is this an U8, U16, etc?
```

## Function definitions

We have some nice syntactic sugar for defining functions. For example:

```pikelet
const : (a b : Type) -> a -> b -> a;
const = \a b x y => x;
```

Could be expressed more cleanly as:

```pikelet
const : (a b : Type) -> a -> b -> a;
const a b x y = x;
```

## Type aliases

Because Pikelet is dependently typed, we need no other mechanism for making
type aliases. Instead we just use definitions!

```pikelet
name : Type;
name = String;
```

## Doc comments

Documentation can be provided for above declarations, by using doc comments:

```pikelet
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
```
