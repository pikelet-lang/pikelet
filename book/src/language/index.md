# Language

## Contents

- [Declarations](#declarations)
- [Comments](#comments)
- [Primitive types and their literals](#primitive-types-and-their-literals)
- [Type annotations](#type-annotations)
- [Identifiers](#identifiers)


## Declarations

Declarations are preceded by a type annotation, and followed by a definition:

```pikelet
greeting : String;
greeting = "hello there!";
```

We can also make type aliases in the same way:

```pikelet
name : Type;
name = String;
```

## Comments

Line comments are preceded by a double dash:

```pikelet
-- this is a comment!
```

Doc comments are preceded by a triple pipe, and are used to document
definitions:

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

## Primitive types and their literals

Pikelet has a number of primitive types:

| Type     | Literal                                |
|----------|----------------------------------------|
| `Bool`   | `true`, `false`                        |
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

> **Note:** You can't do much with these primitive types yet. In the future we
> will add some primitive functions to allow you to manipulate them.

## Type annotations

If you note [above](#primitive-types-and-their-literals), a number of the
primitive types share a literal representation. Pikelet will try to predictably
infer the types, but if it fails to do so you will get an error. In that case
you can use the type annotation operator, `(:)`, to specify the intended type:

```pikelet-repl
Pikelet> 1
error: ambiguous integer literal
Pikelet> 1 : I32
1 : I32
Pikelet> 1 : F32
1 : F32
Pikelet> 1.0 : F32
1.0 : F32
Pikelet> 1.1 : U64
error: found a floating point literal, but expected a type `U64`
- <repl>:1:1
1 | 1.1 : U64
  | ^^^ the literal
```

## Identifiers

> TODO
