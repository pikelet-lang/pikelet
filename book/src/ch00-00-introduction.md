# Introduction

Pikelet is a small dependently typed language. It doesn't do many interesting
things yet, but hopefully that will change in the future!

- [Source code](https://github.com/brendanzab/pikelet)
- [Issues](https://github.com/brendanzab/pikelet/issues)
- [Gitter Chat](https://gitter.im/pikelet-lang/Lobby)

```
id : (a : Type) -> a -> a;
id a x = x;

const : (a b : Type) -> a -> b -> a;
const a b x y = x;
```
