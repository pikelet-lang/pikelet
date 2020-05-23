---
id: records
title: Records (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

Records provide a way of grouping together data into [composite data types][composite-data-types-wikipedia].

[composite-data-types-wikipedia]: https://en.wikipedia.org/wiki/Composite_data_type

## Formation

A record type is a list of entries, consisting of an entry name, and an entry type.
For example, this is a record that defines `width` and `height` extents:

```pikelet
Record {
    width : U32,
    height : U32,
}
```

### Dependent records

Entries can be used to constrain the types of later entries.
For example:

```pikelet
Record {
  A : Type,
  a : A,
}
```

Here the type of `a` _depends_ on the type given to `A`.

### Universes

Record types are also types:

```pikelet
Record {
  first : U32,
} : Type
```

In order to find the universe level of a record type,
we use the universe level the largest entry type:

```
Record {
  first : U32,
  second : Type^2,
  third : Type,
} : Type^3
```

### Entry order

The order of entries in a record type are significant,
so the following record type is not the same as the one shown above:

```pikelet
Record {
    height : U32,
    width : U32,
}
```

Dependencies must be supplied from the roots to the leaves.
For example the following record would not type check because `A : Type` is not yet defined when `a : A` is declared:

```pikelet
Record {
  a : A,
  A : Type,
}
```

:::note
The entry order seems annoying!
It would be nice not to require this in the future,
but dependent record types make this a challenge!
:::

## Construction

:::note
This section is a work in progress.
:::

```pikelet
record {}
```

```pikelet
record {
    width = 24,
    height = 33,
} : Record {
    width : U32,
    height : U32,
}
```

## Elimination

:::note
This section is a work in progress.
:::

```pikelet
extents.width
```

### Computation

:::note
This section is a work in progress.
:::
