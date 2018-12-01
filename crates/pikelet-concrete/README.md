# Pikelet Concrete Syntax

This crate is responsible for:

- defining data structures for:
  - concrete terms
  - raw terms
- parsing the concrete syntax
- pretty printing
- desugaring from the concrete syntax
- resugaring to the concrete syntax
- bidirectional elaboration, involving:
  - bidirectional type checking
  - passing implicit arguments explicitly (TODO)
  - passing instance arguments explicitly (TODO)
  - returning the fully explicit core syntax

