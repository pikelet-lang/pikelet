---
id: comments
title: Comments
keywords:
  - docs
  - reference
  - pikelet
---

## Line comments

Line comments are preceded by a double dash (`--`):

```pikelet
-- This is a comment!
```

:::note
See [Surface language - Lexical syntax - Whitespace and comments](../specification/surface/lexical-structure#whitespace-and-comments)
:::

## Doc comments

Documentation comments are preceded by three pipes (`|||`):

```pikelet
||| A doc comment!
```

Multi-line doc comments can be created by 'stacking'. For example:

```pikelet
||| The unit type
|||
||| This is a synonym for the empty record,
||| and can be constructed using the `unit` function.
```

:::note
See [Surface language - Lexical syntax - Whitespace and comments](../specification/surface/lexical-structure#whitespace-and-comments)
:::
