# Functions

## Contents

- [Simply typed functions](#simply-typed-functions)
- [Polymorphic functions](#polymorphic-functions)
- [Syntactic sugar for functions](#syntactic-sugar-for-functions)

## Simply typed functions

Here are some simple functions and their types:

```pikelet-repl
Pikelet> :t \x : I32 => x
I32 -> I32
Pikelet> :t \x : String => x
String -> String
Pikelet> :t \x : Char => x
Char -> Char
```

Note that all of these types follow the same pattern - they are the identity
function! This means that if you pass a value to them, they'll return the same
thing without alteration!

```pikelet-repl
Pikelet> (\x : I32 => x) 42
42 : I32
Pikelet> (\x : String => x) "hi"
"hi" : String
Pikelet> (\x : Char => x) 'b'
'b' : Char
```

## Polymorphic functions

Alas, we can't reuse one of these identity functions with other, incompatible
types:

```pikelet-repl
Pikelet> (\x : I32 => x) 4.0
error: found a floating point literal, but expected a type `I32`
- <repl>:1:17
1 | (\x : I32 => x) 4.0
  |                 ^^^ the literal
```

Let's make this identity function polymorphic by adding a parameter for the type
of the argument:

```pikelet-repl
Pikelet> :t \(a : Type) (x : a) => x
(a : Type) -> a -> a
```

We now have a polymorphic identity function! We can specialise this function by
applying a type to it:

```pikelet-repl
Pikelet> (\(x : Type) (x : a) => x) String "hello"
"hello" : String
Pikelet> (\(x : Type) (x : a) => x) I32 1
1 : I32
```

## Syntactic sugar for functions

In Pikelet all functions take a single argument - in order to pass multiple
arguments we use currying. The following functions are equivalent:

```pikelet
\(x : Type) (x : a) => x
\(x : Type) => \(x : a) => x
```

Non-dependent functions can be expressed without explicit parameter names. For
example the following function types are equivalent:

```pikelet
(a : Type) (x : a) -> a
(a : Type) -> (x : a) -> a
(a : Type) -> a -> a
```
