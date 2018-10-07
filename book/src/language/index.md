# Language

## Contents

- [Comments](#comments)
- [Primitive types and their literals](#primitive-types-and-their-literals)
- [Type annotations](#type-annotations)
- [Identifiers](#identifiers)
- [Keywords](#keywords)

## Comments

Line comments are preceded by a double dash:

```pikelet
-- this is a comment!
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
| `S8`     | `1`, `2`, `3`, ...                     |
| `S16`    | `1`, `2`, `3`, ...                     |
| `S32`    | `1`, `2`, `3`, ...                     |
| `S64`    | `1`, `2`, `3`, ...                     |
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
Pikelet> 1 : S32
1 : S32
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

## Keywords
| Keyword  | Documentation                             |
|----------|-------------------------------------------|
| `as`     | [internal field names]                    |
| `case`   | [case expressions]                        |
| `else`   | [if-then-else-expressions]                |
| `extern` |                                           |
| `if`     | [if-then-else-expressions]                |
| `import` |                                           |
| `in`     | [bindings]                                |
| `let`    | [bindings]                                |
| `record` | [record] values                           |
| `Record` | [Record] types                            |
| `Type`   | [polymorphic functions], [types of types] |
| `where`  |                                           |

[if-then-else-expressions]: conditionals.html#if-then-else-expressions
[case expressions]: conditionals.html#case-expressions
[bindings]: bindings.html
[record]: records.html
[polymorphic functions]: functions.html
[types of types]: universes.html#types-of-types
[internal field names]: records.html#external-vs-internal-field-names
