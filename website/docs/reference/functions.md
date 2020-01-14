---
id: functions
title: Functions (WIP)
keywords:
  - docs
  - reference
  - pikelet
---

A function is a way of relating an input to an output.

## Formation

Function types are written as `A -> B`.
Functions are [_curried_][currying-wikipedia], meaning that they take a single input, and return a single output.
Multi-argument functions can be created by creating functions that output other functions.

For example, the function type for adding two 32-bit signed integers together is:

```pikelet
S32 -> S32 -> S32
```

## Introduction

:::warning
This section is a work in progress.
:::

:::warning
This feature is not yet implemented!
:::

## Elimination

:::warning
This section is a work in progress.
:::

Functions are applied to arguments via [_juxtaposition_][juxtaposition-wikipedia].

For example, this is how the identity function might be applied:

```pikelet
id String "hello!"
```

```pikelet
Array 3 String
```

[currying-wikipedia]: https://en.wikipedia.org/wiki/Currying
[juxtaposition-wikipedia]: https://en.wikipedia.org/wiki/Juxtaposition#Mathematics
