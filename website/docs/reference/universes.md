---
id: universes
title: Universes (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

## Universe levels

:::warning
This section is a work in progress.
:::

```
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

:::warning
This section is a work in progress.
:::

## Lifting terms

:::warning
This section is a work in progress.
:::
