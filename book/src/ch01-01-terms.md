# Terms

## Contents

- [Comments](#comments)
- [Built-in types and their literals](#built-in-types-and-their-literals)
- [Type annotations](#type-annotations)
- [Types of types](#types-of-types)
- [Identifiers](#identifiers)
- [Functions](#functions)
  - [Function literals](#function-literals)
  - [Function Types](#function-types)
  - [Function application](#function-application)
  - [Syntactic sugar](#syntactic-sugar)

## Comments

In Pikelet, line comments are proceeded by a double dash:

```pikelet
-- this is a comment!
```

Doc comments are proceeded by a triple pipe:

```pikelet
||| This is a doc comment!
```

## Built-in types and their literals

Pikelet has a number of fundamental types:

| Type     | Literal                                |
|----------|----------------------------------------|
| `String` | `"hello there!"`                       |
| `Char`   | `'a'`, `'b'`, ..., `'\n'`, `'\t'`, ... |
| `U8`     | `1`, `2`, `3`, ...                     |
| `U16`    | `1`, `2`, `3`, ...                     |
| `U32`    | `1`, `2`, `3`, ...                     |
| `U64`    | `1`, `2`, `3`, ...                     |
| `I8`     | `1`, `2`, `3`, ...                     |
| `I16`    | `1`, `2`, `3`, ...                     |
| `I32`    | `1`, `2`, `3`, ...                     |
| `I64`    | `1`, `2`, `3`, ...                     |
| `F32`    | `1`, `2`, `3`, ..., `0.0`, `1.0`, ...  |
| `F64`    | `1`, `2`, `3`, ..., `0.0`, `1.0`, ...  |

> **Note:** You can't do much with these built-in types yet. In the future we
> will add some primitive operations to allow you to maniplate them.

## Type annotations

If you note [above](#built-in-types-and-their-literals), a number of the
built-in types share a literal representation. Pikelet will try to predictably
infer the types, but if it fails to do so you will get an error. In that case
you can use the type annotation operator, `(:)`, to specify the intended type:

```pikelet
Pikelet> 1           -- error: ambiguous integer literal
Pikelet> 1 : I32     -- ok!
Pikelet> 1 : F32     -- ok!
Pikelet> 1.0 : F32   -- ok!
Pikelet> 1.1 : U64   -- error: expected a floating point value
```

## Types of types

Types also have types!

```
Pikelet> :t I32
Type
```

You might then ask, “what is the type of `Type`?”

```
Pikelet> :t Type
Type 1
```

Note that `Type` is actually just syntactic sugar for `Type 0`:

```
Pikelet> :t Type 0
Type 1
```

In fact Pikelet has an infinte number of 'universes', each one 'bigger' than the
previous:

```
Type 0 : Type 1 : Type 2 : Type 3 : ...
```

You can think of these as larger and larger sets, with the smaller type
universes being contained within the larger type universes:

> TODO: Put a nice SVG diagram of the sets of types here
>
> ```
> (Type 2
>   (Type 1
>     (Type 0
>       (I32, F32, String,
>         (1, 4.0, "hello")))))
> ```

## Identifiers

> TODO

## Functions

> TODO

### Function literals

> TODO

### Function Types

> TODO

### Function application

> TODO

### Syntactic sugar

> TODO
