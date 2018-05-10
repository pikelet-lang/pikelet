# Pikelet 🥞

[![Build Status][travis-badge]][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[travis-badge]: https://travis-ci.org/brendanzab/pikelet.svg?branch=master
[travis-url]: https://travis-ci.org/brendanzab/pikelet
[gitter-badge]: https://badges.gitter.im/pikelet-lang/pikelet.svg
[gitter-lobby]: https://gitter.im/pikelet-lang/Lobby

For more information, see [The Pikelet Book][pikelet-book].

This is an implementation of a small [dependently typed][dependent-type-wikipedia]
lambda calculus in Rust. This project is intended as an exploratory exercise,
and to serve as a basis for more fully-featured dependently typed languages.

Thanks to the hard work of our academics, it has become much easier for the
average developer to try their hand at building dependent type systems. Alas, the
materials required to tackle this are scattered throughout the literature, and
can be time-consuming to track down. Hopefully this project makes it easier to
see how this can be done in Rust!

We take a [bidirectional approach][bidirectional-typing-paper] to building the
type system, similar to the one descibed in the excellent
[LambdaPi paper][lambdapi-site]. This allows us to provide a good amount of
local type inference while still maintaining a simple, easy to understand type
checking algorithm.

[pikelet-book]: https://brendanzab.github.io/pikelet/
[dependent-type-wikipedia]: https://en.wikipedia.org/wiki/Dependent_type
[bidirectional-typing-paper]: http://www.davidchristiansen.dk/tutorials/bidirectional.pdf
[lambdapi-site]: https://www.andres-loeh.de/LambdaPi/

## Acknowledgments

[![YesLogic Logo][yeslogic-logo]][yeslogic]

This work was done with the generous support of [YesLogic][yeslogic].

[yeslogic]: http://yeslogic.com/
[yeslogic-logo]: assets/yeslogic-logo.png
