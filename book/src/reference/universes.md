# Universes

Having first class types naturally poses the question: what is the type of `Type`?

```pikelet
Type : ???
```

One idea would might be to have:

```pikelet
Type : Type
```

This is not a bad design, and many systems choose it pragmatically for its simplicity,
but it is, however, [_inconsistent_][consistency-wikipedia] for [subtle reasons][type-in-type-liamoc],
as seen in [Girard's Paradox][girards-paradox-wikipedia].
This means that allowing it would allow for the construction of a program that returned `Void`!
Pikelet avoids such paradoxical constructions by introducing a hierarchy of universes,
indexed by _universe levels_, as seen in the next section.

[consistency-wikipedia]: https://en.wikipedia.org/wiki/Consistency
[type-in-type-liamoc]: http://liamoc.net/posts/2015-09-10-girards-paradox/index.html
[girards-paradox-wikipedia]: https://en.wikipedia.org/wiki/System_U#Girard's_paradox

## Universe levels

> **Note:**
>
> This section is a work in progress.

```text
╭─ Type^2 ───────────────────────────────────────────────────────────────────────╮
│                                                    Array n Type^1              │
│                                                                                │
│  ╭─ Type^1 ─────────────────────────────────────────────────────────────────╮  │
│  │                                                                          │  │
│  │  ╭─ Type ───────────────────────────────╮                                │  │
│  │  │                Array n String        │                                │  │
│  │  │                                      │                                │  │
│  │  │            Record { x : F32 }        │          Array n Type          │  │
│  │  │                                      │                                │  │
│  │  │  ╭─ S32 ──────────────╮              │                                │  │
│  │  │  │ ..., -1, 0, 1, ... │              │       Nat -> Type -> Type      │  │
│  │  │  ╰────────────────────╯              │                                │  │
│  │  │                                      │                                │  │
│  │  │  ╭─ Record {} ─╮   ╭─ Bool ───────╮  │                                │  │
│  │  │  │  record {}  │   │  true, false │  │          Type -> Type          │  │
│  │  │  ╰────────────-╯   ╰──────────────╯  │                                │  │
│  │  │                                      │                                │  │
│  │  │  ╭─ String ──────────────────────╮   │  ╭─ Record { t : Type } ────╮  │  │
│  │  │  │ "hello", "byee!", "hoho", ... │   │  │  record { t = String },  │  │  │
│  │  │  ╰──────────────────────────────-╯   │  │  record { t = U32 }, ... │  │  │
│  │  ╰──────────────────────────────────────╯  ╰──────────────────────────╯  │  │
│  ╰──────────────────────────────────────────────────────────────────────────╯  │
╰────────────────────────────────────────────────────────────────────────────────╯
```

## Cumulativity

Because the level of a universe corresponds to some notion of it's 'size',
this suggests that larger universes should be able to contain all the other things smaller than themselves.
This is reflected in Pikelet too:

```pikelet
Bool : Type        -- ok
Bool : Type^2      -- ok
Type^1 : Type^3    -- ok
Type^3 : Type^1    -- error!
```

## Lifting terms

> **Note:**
>
> This section is a work in progress.
