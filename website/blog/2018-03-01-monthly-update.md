---
title: Monthly Update, March 2018
author: Brendan Zabarauskas
authorURL: http://twitter.com/brendanzab
authorImageURL: https://avatars3.githubusercontent.com/u/695077?s=460&v=4
---

News in [Pikelet](https://github.com/brendanzab/pikelet) land:

- extracted name binding into a separate [nameless](https://github.com/brendanzab/nameless/) crate.
- added some primitive types, like strings, characters, integers, floats. Bidirectional checking made it very easy to [add coercions](https://github.com/brendanzab/pikelet/blob/81ea7a4294d6b5e008e83e7342963376159f74f6/src/semantics/mod.rs#L140-L171) for numeric literals.
<!--truncate-->
- added [the beginnings of a book](https://brendanzab.github.io/pikelet/) ([deployed automatically](https://github.com/brendanzab/pikelet/blob/81ea7a4294d6b5e008e83e7342963376159f74f6/.travis.yml#L22-L31) via Travis CI, woohoo).
- added [a whole bunch of issues](https://github.com/brendanzab/pikelet/issues) to the github repo.
- did a [presentation at my local Haskell user group](https://www.meetup.com/Melbourne-Haskell-Users-Group/events/248041735/), alas there are no slides or recording - was pretty informal.

I'm thinking next up I will work on adding dependent records. This will then allow me to start building an API of type classes. I wanted to go with a structural approach, but it seems like it might be a tad challenging, especially when trying to do subtyping between records with different field orderings. I've found some prior art based on System LF from Luo and Feng, but I find the LF systems a little hard to understand after coming from LambdaPi and Norell's Agda paper…

As always, feel free to come chat in real time on [Gitter](https://gitter.im/pikelet-lang/Lobby)!
