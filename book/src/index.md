# Pikelet!

[![Actions Status][actions-badge]][actions-url]
[![Matrix][matrix-badge]][matrix-lobby]
[![License][license-badge]][license-url]
[![GitHub stars][stars-badge]][github-url]

[actions-badge]: https://github.com/pikelet-lang/pikelet/workflows/ci/badge.svg
[actions-url]: https://github.com/pikelet-lang/pikelet/actions
[matrix-badge]: https://img.shields.io/matrix/pikelet:matrix.org?label=%23pikelet%3Amatrix.org
[matrix-lobby]: https://app.element.io/#/room/#pikelet:matrix.org
[license-badge]: https://img.shields.io/github/license/pikelet-lang/pikelet
[license-url]: https://github.com/pikelet-lang/pikelet/blob/master/LICENSE
[stars-badge]: https://img.shields.io/github/stars/pikelet-lang/pikelet?style=social
[github-url]: https://github.com/pikelet-lang/pikelet

![Pikelet Mascot][pikelet-mascot]

[pikelet-mascot]: ../assets/pikelet.png

Pikelet is a small [dependently typed][dependent-type-wikipedia] language. It
doesn't do many interesting things yet, but hopefully that will change in the future!

[dependent-type-wikipedia]: https://en.wikipedia.org/wiki/Dependent_type

> **Note:**
>
> Pikelet is still a work in progress! Many features are not implemented yet!
>
> If you'd like to see what we hope to work on next, have a look at [the roadmap](./development/roadmap).

## A small taste

Definitions:

```pikelet
record {
    id : Fun (A : Type) -> A -> A,
    id A a = a,

    always : Fun (A B : Type) -> A -> B -> A,
    always A B a b = a,
}
```

Interactive REPL:

```text
$ pikelet repl
    ____  _ __        __     __
   / __ \(_) /_____  / /__  / /_
  / /_/ / / //_/ _ \/ / _ \/ __/    Version 0.1.0
 / ____/ / ,< /  __/ /  __/ /_      https://github.com/pikelet-lang/pikelet
/_/   /_/_/|_|\___/_/\___/\__/      :? for help

> (fun A a => a : Fun (A : Type) -> A -> A) String "hello"
"hello" : String
```
[dependent-type-wikipedia]: https://en.wikipedia.org/wiki/Dependent_type

## Summary

- [Guide](./guide.md): For people new to Pikelet
- [Reference](./reference.md): For people who need a detailed descriptions of individual language features
- [Development](./development.md): For people wanting to contribute to the language
- [Specification](./specification.md): For developers and researchers
