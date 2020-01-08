# Builtin types

Pikelet has a number of builtin types, which we now describe here:

## Unsigned integers

```pikelet
U8 : Type
U16 : Type
U32 : Type
U64 : Type
```

## Signed integers

```pikelet
S8 : Type
S16 : Type
S32 : Type
S64 : Type
```

## Floating point

```pikelet
F32 : Type
F64 : Type
```

## Strings

```pikelet
String : Type
```

Characters can be constructed using string literals. For example:

```pikelet
"hello" : String
```

## Characters

```pikelet
Char : Type
```

Characters can be constructed using character literals. For example:

```pikelet
'A' : Char
'가' : Char
'🥞' : Char
```

## Lists

Lists are ordered sequences of terms.

```pikelet
List : Type -> Type
```

Lists can be constructed using sequences. For example:

```pikelet
[] : List F32
[1, 2, 3] : List F32
```

## Arrays

Arrays are ordered sequences of terms, with a length specified in the type.

```pikelet
Array : U32 -> Type -> Type
```

Arrays can be constructed using sequences. For example:

```pikelet
[] : Array 0 F32
[1, 2, 3] : Array 3 F32
```
