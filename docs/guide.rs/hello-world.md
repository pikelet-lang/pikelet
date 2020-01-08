# Hello world!

## Using the REPL

If you have [installed pikelet][installation], you can run the REPL by running this command in the terminal:

```
$ pikelet repl
```

[installation]: ./installation

The REPL should appear like so:

```
$ cargo run repl
    ____  _ __        __     __
   / __ \(_) /_____  / /__  / /_
  / /_/ / / //_/ _ \/ / _ \/ __/    Version 0.1.0
 / ____/ / ,< /  __/ /  __/ /_      https://github.com/pikelet-lang/pikelet
/_/   /_/_/|_|\___/_/\___/\__/      :? for help

>
```

You can now enter Pikelet terms into the REPL after the `>`. For example:

```
> "Hello world!"
```

By pressing <kbd>Enter</kbd>, you can 'normalize' the term, and see its type:

```
> "Hello world!"
"Hello world!" : String
```
