---
id: records
title: Records (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

Records provide a way of grouping together data.

## Formation

:::note
This section is a work in progress.
:::

```pikelet
Record {
    width : U32,
    height : U32,
}
```

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
