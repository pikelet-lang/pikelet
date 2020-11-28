# Design

## Design Principles

- Empower our users through careful design, rather than being driven by familiarity.
- Surface level features should decompose into a simple, typed core.
- The top-level should not be [hopeless](https://gist.github.com/samth/3083053).
- Programs should not have to pay for things that they do not use.
- Pikelet should work well for high level and low level, resource constrained applications.
- It should be easy to bootstrap the language fro a new platform and cross-compile programs.
- Diagnostics should be clear and easy to understand.
- Features should behave predictably, without relying on complicated, hard to understand heuristics.
- Features should have a high power-to-weight ratio.

## Research to Watch

There are a number of exciting areas of research that are worth keeping an eye on:

- Dependent types
- High performance elaboration
- Effects and Coeffects
  - Algebraic Effects and Handlers
  - Effect Runners
  - Graded Modal Type Theory
    - Quantitative Type Theory
    - Multistage Programming
- Call by Push Value
- Codata vs. Data
- Modular Programming with Dependent Records
- Fancy Dependencies
  - Self Types
  - Dependent Intersection
  - Very Dependent Types
- Datatype Generic Programming
  - Levitated data descriptions
  - Data Layout Interpretations
  - Ornamented Data Types
- Projectional Editors
