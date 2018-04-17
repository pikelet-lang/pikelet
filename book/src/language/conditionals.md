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

Case expressions can be used in place of a sequence of if expressions:

```pikelet
not : Bool -> Bool;
not x = case x of {
    true => false;
    false => true;
};
```

```pikelet-repl
Pikelet> not true
false : Bool
Pikelet> not false
true : Bool
```

Variables can be captured in case expressions:

```pikelet
case value of {
    "hello" => "goodbye";
    "goodbye" => "hello";
    value => value; -- matches all strings
}
```

In the future we hope to also support more complex pattern matching, for example
on records.
