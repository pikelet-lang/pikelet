# Literals

## Numbers

```pikelet
0.0
+1
-25
0xAB342
1_000_000
```

> **Specification**:
>
> - [Surface language - Lexical syntax - Numeric literals](../specification/surface/lexical-structure#Numeric-literals): <a href="../specification/surface/lexical-structure#var:number-literal"><var>number-literal</var></a>

### Supported types

- Unsigned integers: `U8`, `U16`, `U32`, `U64`
- Signed integers: `S8`, `S16`, `S32`, `S64`
- Floating point numbers: `F32`, `F64`

### Overloading

Overloaded number literals are not yet supported, but _are_ planned.

## Characters

```pikelet
'A'
'가'
'🥞'
```

### Supported types

- `Char`

### Overloading

Overloaded character literals are not yet supported, but _are_ planned.

> **Specification**:
>
> - [Surface language - Lexical syntax - Character and string literals](../specification/surface/lexical-structure#Character-and-string-literals): <a href="../specification/surface/lexical-structure#var:character-literal"><var>character-literal</var></a>

## Strings

```pikelet
"hello"
```

### Supported types

- `String`

### Overloading

Overloaded string literals are not yet supported, but _are_ planned.

> **Specification**:
>
> - [Surface language - Lexical syntax - Character and string literals](../specification/surface/lexical-structure#Character-and-string-literals): <a href="../specification/surface/lexical-structure#var:string-literal"><var>string-literal</var></a>
