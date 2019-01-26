# Universes

## Contents

- [Types of types](#types-of-types)
- [Cumulativity](#cumulativity)
- [Syntactic sugar](#syntactic-sugar)
- [Shifting universes](#shifting-universes)

## Types of types

Types also have types!

```pikelet-repl
Pikelet> :t S32
Type
```

We call this special 'type of types' a _universe_.

You might then wonder, “what is the type of `Type`?” That's a good question!
Clever people have figured out that if `Type` was its own type, ie. `Type : Type`,
it would lead situations like [Girard's paradox][girards-paradox], (the type
theory equivalent of the more well-known [Russel's paradox][russels-paradox] in
set theory). There is nothing worse than paradoxes in a type system, so instead
we create ourselves a new universe called `Type^1`:

```pikelet-repl
Pikelet> :t Type
Type^1
```

We keep on going like this, giving us a hierarchy of _universes_, as many
as we need for a given program:

```pikelet
Type : Type^1 : Type^2 : Type^3 : ...
```

[girards-paradox]: https://en.wikipedia.org/wiki/Girard%27s_paradox
[russels-paradox]: https://en.wikipedia.org/wiki/Russell%27s_paradox

We call the number given to each universe the _level_ of that universe. You can
think of these universes as larger and larger collections of things, with the
smaller universes being contained within the larger universes:

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
|  |  |  .- S32 --------------.              |                                |  |
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

Note that in most regular programming you will rarely see anything above `Type`,
and even more rarely still will you see things above `Type^1`, so all of this might
seem a little excessive. That being said, we believe it is important enough to
plug this gap in our type system while we have the chance.

## Cumulativity

Because the level of a universe corresponds to some notion of it's 'size', this
suggests that larger universes should be able to contain all the other things
smaller than themselves. This is reflected in Pikelet too:

```pikelet-repl
Pikelet> Bool : Type        -- ok
Pikelet> Bool : Type^2      -- ok
Pikelet> Type^1 : Type^3    -- ok
Pikelet> Type^3 : Type^1    -- error!
```

## Syntactic sugar

Note that `Type` is actually just sugar for `Type^0`:

```pikelet-repl
Pikelet> :t Type^0
Type^1
```

## Shifting universes

Often we'll write definitions in terms of `Type`, without worrying about the
universe levels. For example the identity function can be defined with a type
parameter in the universe of `Type`:

```pikelet-repl
Pikelet> :let id = fun (a : Type) (x : a) => x
id : Fun (a : Type) (x : a) -> a
```

This then allows us to use it with values:

```pikelet-repl
Pikelet> id String "hello"   -- ok
Pikelet> id S32 1            -- ok
```

Sadly because of our universe hierarchy, we can't use our identity function at
the type level!

```pikelet-repl
Pikelet> id Type String                 -- error!
Pikelet> id (Fun (a : Type) -> a -> a) id   -- error!
Pikelet> id Type^1 Type                 -- error!
```

This would seem like it would be terrible for code reuse - you would need to
create a new `id` function for every universe level! Thankfully we have a simple
solution: We allow identifiers to be _shifted_ to the correct universe level
using the `^` notation. This shifts the given definition to the desired universe
level:

```pikelet-repl
Pikelet> :t id^1
Fun (a : Type^1) -> a -> a
```

We can then use the shifted identity functions like so:

```pikelet-repl
Pikelet> id^1 Type String                    -- ok
Pikelet> id^1 (Fun (a : Type) -> a -> a) id  -- ok
Pikelet> id^2 Type^1 Type                    -- ok
```

Field projections can also have shifts applied to them:

```pikelet-repl
Pikelet> prelude.id^1 Type String
```
