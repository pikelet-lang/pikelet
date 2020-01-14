---
id: literals
title: Literals
keywords:
  - docs
  - reference
  - pikelet
---

## Numbers

```pikelet
0.0
+1
-25
0xAB342
1_000_000
```

:::note
See [Surface language - Lexical syntax - Numeric literals](../specification/surface/lexical-structure#numeric-literals)
:::

### Supported types

- Unsigned integers: [`U8`][unsigned-integers], [`U16`][unsigned-integers], [`U32`][unsigned-integers], [`U64`][unsigned-integers]
- Signed integers: [`S8`][signed-integers], [`S16`][signed-integers], [`S32`][signed-integers], [`S64`][signed-integers]
- Floating point numbers: [`F32`][floating-point-numbers], [`F64`][floating-point-numbers]

### Overloading

Overloaded number literals are not yet supported, but _are_ planned.

## Characters

```pikelet
'A'
'가'
'🥞'
```

### Supported types

- [`Char`][characters]

### Overloading

Overloaded character literals are not yet supported, but _are_ planned.

:::note
See [Surface language - Lexical syntax - Character and string literals](../specification/surface/lexical-structure#character-and-string-literals)
:::

## Strings

```pikelet
"hello"
```

### Supported types

- [`String`][strings]

### Overloading

Overloaded string literals are not yet supported, but _are_ planned.

:::note
See [Surface language - Lexical syntax - Character and string literals](../specification/surface/lexical-structure#character-and-string-literals)
:::


[booleans]: ./builtins#booleans
[unsigned-integers]: ./builtins#unsigned-integers
[signed-integers]: ./builtins#signed-integers
[floating-point-numbers]: ./builtins#floating-point-numbers
[strings]: ./builtins#strings
[characters]: ./builtins#characters
