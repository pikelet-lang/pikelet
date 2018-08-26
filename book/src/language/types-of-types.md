# Types of types

Types also have types!

```pikelet-repl
Pikelet> :t I32
Type
```

Naturally, you might then wonder, “what is the type of `Type`?” That's a good
question! If `Type` were its own type, ie. `Type : Type`, it would lead
situations like [Girard's paradox][girards-paradox], (the type theory equivalent
of the more well-known [Russel's paradox][russels-paradox] in set theory).
Instead we create ourselves a new _level_ of types called `Type^1`:

```pikelet-repl
Pikelet> :t Type
Type^1
```

We keep incrementing the levels, giving us a hierarchy of _types_
(or _universes_):

```pikelet
Type : Type^1 : Type^2 : Type^3 : ...
```

[girards-paradox]: https://en.wikipedia.org/wiki/Girard%27s_paradox
[russels-paradox]: https://en.wikipedia.org/wiki/Russell%27s_paradox

We call the number given to each universe the _level_ of that universe. You can
think of these as larger and larger sets, with the smaller universes being
contained within the larger universes:

```
.- Type^2 -----------------------------------------------------------------------.
|                                                    Array n Type^1              |
|                                                                                |
|  .- Type^1 -----------------------------------------------------------------.  |
|  |                                                                          |  |
|  |  .- Type -------------------------------.                                |  |
|  |  |                Array n String        |                                |  |
|  |  |                                      |                                |  |
|  |  |            Record { x : F32 }        |          Array n Type          |  |
|  |  |                                      |                                |  |
|  |  |  .- I32 --------------.              |                                |  |
|  |  |  | ..., -1, 0, 1, ... |              |       Nat -> Type -> Type      |  |
|  |  |  '--------------------'              |                                |  |
|  |  |                                      |                                |  |
|  |  |  .- Record {} -.   .- Bool -------.  |                                |  |
|  |  |  |  record {}  |   |  true, false |  |          Type -> Type          |  |
|  |  |  '-------------'   '--------------'  |                                |  |
|  |  |                                      |                                |  |
|  |  |  .- String ----------------------.   |  .- Record { t : Type } ----.  |  |
|  |  |  | "hello", "byee!", "hoho", ... |   |  |  record { t = String },  |  |  |
|  |  |  '-------------------------------'   |  |  record { t = U32 }, ... |  |  |
|  |  '--------------------------------------'  '--------------------------'  |  |
|  '--------------------------------------------------------------------------'  |
'--------------------------------------------------------------------------------'
```

## Type^0

Note that `Type` is actually just sugar for `Type^0`:

```pikelet-repl
Pikelet> :t Type^0
Type^1
```

## Shifting universes

Often we'll write definitions in terms of `Type`, without worrying about the
universe levels. For example the identity function can be defined with a type
parameter in the universe of `Type`:

```
id : (a : Type) -> a -> a
id a x = x
```

This then allows us to use it with values:

```pikelet-repl
Pikelet> id String "hello"   -- ok!
Pikelet> id I32 1            -- ok!
```

Sadly because of our universe hierarchy, we can't use our identity function at
the type level!

```pikelet-repl
Pikelet> id Type String                 -- error, expected Type!
Pikelet> id ((a : Type) -> a -> a) id   -- error, expected Type!
Pikelet> id Type^1 Type                 -- error, expected Type!
```

This would be terrible for code reuse, but thankfully we have a simple solution!
We allow identifiers to be _shifted_ to the correct universe level using the `^`
notation. This shifts the given definition to the correct universe level:

```pikelet-repl
:t id^1
(a : Type^1) -> a^1 -> a^1
```

We can then use the shifted identity functions like so:

```pikelet-repl
Pikelet> id^1 Type String                    -- ok!
Pikelet> id^1 ((a : Type) -> a -> a) id      -- ok!
Pikelet> id^2 Type^1 Type                    -- ok!
```
