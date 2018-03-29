# Installation

Pikelet is written in _[Rust][rust-site]_ and therefore needs
to be compiled with _Cargo_, because we don't yet offer prebuilt binaries.
If you haven't already installed Rust, please [install it][rust-install] now!

[rust-site]: https://www.rust-lang.org/
[rust-install]: https://www.rust-lang.org/downloads.html

## Cloning the source from Github

We've not yet published Pikelet on [crates.io][crates-io], so you'll first need
to clone [the repository][pikelet-repository] using git:

```sh
git clone https://github.com/brendanzab/pikelet.git
cd pikelet
```

[crates-io]: https://crates.io/
[pikelet-repository]: https://github.com/brendanzab/pikelet

## Running the Pikelet REPL

After cloning, you can now run the repl using cargo

```sh
cargo run repl
```

You will now need to wait for cargo to download and build the dependencies, but
sooner or later the REPL will be ready for you to interact with!

Try some commands now:

```
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
