# Types of types

Types also have types!

```pikelet-repl
Pikelet> :t I32
Type
```

You might then ask, “what is the type of `Type`?”

```pikelet-repl
Pikelet> :t Type
Type 1
```

Note that `Type` is actually just syntactic sugar for `Type 0`:

```pikelet-repl
Pikelet> :t Type 0
Type 1
```

In fact, Pikelet has an infinte number of 'universes', each one 'bigger' than the
previous:

```pikelet-repl
Pikelet> :t Type 0
Type 1
Pikelet> :t Type 1
Type 2
Pikelet> :t Type 2
Type 3
...
```

You can think of these as larger and larger sets, with the smaller type
universes being contained within the larger type universes:

> TODO: Put a nice SVG diagram of the sets of types here
>
> ```
> (Type 2
>   (Type 1
>     (Type 0
>       (I32, F32, String,
>         (1, 4.0, "hello")))))
> ```
