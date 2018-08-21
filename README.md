# Pikelet!

![Pikelet Mascot][pikelet-mascot]

[![Build Status][travis-badge]][travis-url]
[![Gitter][gitter-badge]][gitter-lobby]

[pikelet-mascot]: assets/pikelet.png
[travis-badge]: https://travis-ci.org/pikelet-lang/pikelet.svg?branch=master
[travis-url]: https://travis-ci.org/pikelet-lang/pikelet
[gitter-badge]: https://badges.gitter.im/pikelet-lang/pikelet.svg
[gitter-lobby]: https://gitter.im/pikelet-lang/Lobby

Pikelet is a small, functional, dependently typed programming language.

Dependent types allow us to do a bunch of really interesting things, like using
records for modules, declaring the length of arrays at the type level, and
much more — many of great utility for low-level and high-level code alike!

We hope to one day grow Pikelet into fully-fledged systems programming language,
with support for unboxed data types, control over memory layout and allocation
strategy, linear types, and a flexible phase distinction and support for calling
other languages. At the moment however we've only implemented a type checker and
very slow  interpreter, so don't get your hopes up too much yet! There's still a
whole lot to do before it is even remotely useful to anyone! 😅

For more information, see [The Pikelet Book][pikelet-book].

[pikelet-book]: https://pikelet-lang.github.io/pikelet/

## Contributing

We really want to encourage new contributors to help out! Please come chat with
us [on our Gitter channel][gitter-lobby] - if you have any questions about the
project, or just want to say hi! We sometimes get side-tracked on technical
discussions, but we're always more than happy to spend time explaining things.

## Acknowledgments

[![YesLogic Logo][yeslogic-logo]][yeslogic]

This work was done in part with the generous support of [YesLogic][yeslogic].

[yeslogic]: http://yeslogic.com/
[yeslogic-logo]: assets/yeslogic-logo.png
