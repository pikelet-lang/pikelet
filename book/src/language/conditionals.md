# Conditionals

## If-then-else expressions

If expressions take an expression that evaluates to a `Bool` (the _condition_),
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
