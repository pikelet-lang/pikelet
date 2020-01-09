# Builtin types

Pikelet has a number of builtin types, which we now describe here:

## Booleans

```pikelet
Bool : Type
```

Booleans have two constructors, `true` and `false`:

```pikelet
true : Type
false : Type
```

## Unsigned integers

Unsigned integers are defined via the following built-ins:

```pikelet
U8 : Type
U16 : Type
U32 : Type
U64 : Type
```

Unsigned integers can be constructed using numeric literals:

```pikelet
0 : S8
+42 : S32
0x2F : S16
```

## Signed integers

Two's complement, signed integers are defined via the following built-ins:

```pikelet
S8 : Type
S16 : Type
S32 : Type
S64 : Type
```

Signed integers can be constructed using numeric literals:

```pikelet
0 : S8
+42 : S32
-42 : S32
0x2F : S16
```

## Floating point numbers

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
