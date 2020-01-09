# Hello world!

## Using the REPL

If you have [installed Pikelet][installation], you can run the REPL by running this command in the terminal:

```
pikelet repl
```

[installation]: ./installation

The REPL should appear in the terminal like so:

```
    ____  _ __        __     __
   / __ \(_) /_____  / /__  / /_
  / /_/ / / //_/ _ \/ / _ \/ __/    Version 0.1.0
 / ____/ / ,< /  __/ /  __/ /_      https://github.com/pikelet-lang/pikelet
/_/   /_/_/|_|\___/_/\___/\__/      :? for help

>
```

"REPL" stands for "Read-eval-print-loop" and is a nice way to experiment with Pikelet in an interactive way.
You can enter Pikelet terms into the REPL after the `>`. For example:

```
> "Hello world!"
```

By pressing <kbd>Enter</kbd>, you can 'normalize' the term, and see its type:

```
> "Hello world!"
"Hello world!" : String
```

## Effects

TODO: This is not yet implemented!

```
> print-line "Hello world!"
print-line "Hello world!" : Unit <{Std-Out}>
```

```
> :exec print-line "Hello world!"
Hello world!
```

## Compiling a standalone executable

TODO:  This is not yet implemented!
