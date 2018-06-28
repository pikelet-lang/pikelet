# Compiler Architecture

In order to create a separation of concerns, we break up our compiler into many
small stages, beginning with a source string, and ultimately ending up with
compiled machine code.

Below is a rough flow chart showing how source strings are currently lexed,
parsed, desugared, and type checked/elaborated:

```bob
         .------------.
         |   String   |
         '------------'
                |
      syntax::parse::lexer
                |
                v
  .-----------------------------.
  | syntax::parse::lexer::Token |
  '-----------------------------'
                |
     syntax::parse::grammar
                |
                v
    .------------------------.
    | syntax::concrete::Term |
    '------------------------'
                |
   syntax::translation::desugar
                |
                v
      .-------------------.
      | syntax::raw::Term |
      '-------------------'
                |                        .---------------------.
    semantics::{check,infer} <---------- | syntax::core::Value |
                |                        '---------------------'
                v                                    ^
      .--------------------.                         |
      | syntax::core::Term | - semantics::normalize -'
      '--------------------'
                |
                v
    TODO: compiler back end(s)
```

As you can see we have only built the front-end as of the time of writing. When
we begin to build out a [compiler back end](https://github.com/pikelet-lang/pikelet/issues/9),
more stages will be added after type checking and elaboration.

## Name binding

Name binding is a surprisingly challenging thing to implement in type checkers
and compilers. We use the [`moniker` crate](https://github.com/brendanzab/moniker)
for this. Unfortunately this uses a quite slow method of name binding, and could
result in performance blowouts in the future. This is something to keep an eye on!

## Performance considerations

As you can see from the diagram above, this architecture leads to an
easy-to-reason about pipeline. It does however result in the creation of lots of
intermediate allocations of heap-allocated tree data structures that will
ultimately be discarded. This is quite similar to the problem we face with
iterators:

```rust
// 'internal' iteration
vec![1, 2, 3].map(|x| x * x).filter(|x| x < 3)

// 'external' iteration
vec![1, 2, 3].iter().map(|x| x * x).filter(|x| x < 3).collect()
```

The first example, which uses 'internal' iteration allocates a new collection
after each operation, resulting in three allocated collections. We can improve
the performance however by using 'external' iteration - ie. returning a series
of chained iterator adaptors, that only perform the allocation on the call to
`collect`. This emulates the 'fusion' that languages like Haskell perform to
reduce intermediate allocations.

We could potentially get some fusion between the stages of our compiler by way
of the [visitor pattern](https://github.com/pikelet-lang/pikelet/issues/75).

## Support for interactive development

It would be interesting to see how Pikelet could be implemented using an
asynchronous, query-based architecture. This will become more important as the
demands of interactive development and incremental compilation become more
pressing. In this model we would have to think of compilation as less a pure
function from source code to machine code, and more as interacting with a
database. Perhaps a CQRS (Command-Query-Responsibility-Segregation) model would
be useful for this?

### Resources

- [Queries: demand-driven compilation (Rustc Book)](https://rust-lang-nursery.github.io/rustc-guide/query.html)
- [Anders Hejlsberg on Modern Compiler Construction (YouTube)](https://www.youtube.com/watch?v=wSdV1M7n4gQ)
