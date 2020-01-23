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

### Universes

Record types are also types:

```
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

### Field order

Note that the order of the fields matters, so this would be a different type to
the one defined above:

```pikelet
Record {
    height : U32,
    width : U32,
}
```

:::note
The field order seems annoying!
It would be nice not to require this in the future.
:::

## Construction

:::note
This section is a work in progress.
:::

```pikelet
record {
    width = 24,
    height = 33,
} : Record {
    width : U32,
    height : U32,
}
```

```pikelet
record {
    width = 24 : U32,
    height = 33 : U32,
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
