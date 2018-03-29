# Pikelet 🥞

Pikelet is a small [dependently typed][dependent-type-wikipedia] language. It
doesn't do many interesting things yet, but hopefully that will change in the future!

- [Source code](https://github.com/brendanzab/pikelet)
- [Issues](https://github.com/brendanzab/pikelet/issues)
- [Gitter Chat](https://gitter.im/pikelet-lang/Lobby)

[dependent-type-wikipedia]: https://en.wikipedia.org/wiki/Dependent_type

## A small taste

Definitions:

```pikelet
module prelude;

id : (a : Type) -> a -> a;
id a x = x;

const : (a b : Type) -> a -> b -> a;
const a b x y = x;
```

Interactive REPL:

```
cargo run repl
    ____  _ __        __     __
   / __ \(_) /_____  / /__  / /_
  / /_/ / / //_/ _ \/ / _ \/ __/    Version 0.1.0
 / ____/ / ,< /  __/ /  __/ /_      https://github.com/brendanzab/pikelet
/_/   /_/_/|_|\___/_/\___/\__/      :? for help

Pikelet> (\(a : Type) (x: a) => x) String "hello"
"hello" : #String
Pikelet> :t Type
Type 1
Pikelet> 1 : I16
1 : #I16
Pikelet>
```

## What is a Pikelet?

A pikelet is an odd sort of small (often pre-made) pancake found in Australia
and New Zealand. Commonly sent in school lunches spread with jam and butter.
Handily it also has a name that includes 'pi' and 'let' as substrings! 😅
