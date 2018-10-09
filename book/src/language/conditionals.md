# Conditionals

## If-then-else expressions

`if` expressions take an expression that evaluates to a `Bool` (the _condition_),
and two other expressions (the _consequent_ and the _alternative_) that evaluate
to the same type. If the condition evaluates to `true`, then the consequent will
be evaluated and returned, otherwise the alternative will be evaluated and
returned.

```pikelet-repl
Pikelet> if true then "hello!" else "goodbye!"
"hello!" : String
Pikelet> if false then "hello!" else "goodbye!"
"goodbye!" : String
```

## Case expressions

Pikelet supports case expressions on strings, and numbers:

```pikelet
case value {
    "hello" => "goodbye";
    "goodbye" => "hello";
    value => value; -- matches all strings
}
```

Note that we don't (yet) check that the series of patterns provided cover all
possible cases, leading to the following embarrassing error:

```pikelet-repl
Pikelet> case "hello" { "hi" => "oh dear" }
error: internal compiler error: no patterns matched the given expression
```

In the future we' plan to fix this, add support for matching on booleans, and
also support more complex patterns (eg. for records).
