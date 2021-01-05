# Concrete Syntax

This section defines the concrete syntax of the surface language.

## Terms

```text
term ::=
    | expr-term
    | expr-term ":" term

expr-term ::=
    | arrow-term
    | "fun" name+ "=>" expr-term

arrow-term ::=
    | app-term
    | "Fun" ("(" name+ ":" arrow-term ")")+ "->" arrow-term
    | app-term "->" arrow-term

app-term ::=
    | atomic-term
    | atomic-term atomic-term+

atomic-term ::=
    | "(" term ")"
    | name
    | "Record" "{" (type-entry ",")* type-entry? "}"
    | "record" "{" (term-entry ",")* term-entry? "}"
    | atomic-term "." name
    | "[" (term ",")* term? "]"
    | number-literal
    | character-literal
    | string-literal
```

## Entries

```text
type-entry ::=
    | doc-comment* name ("as" name)? ":" term

term-entry ::=
    | doc-comment* name ("as" name)? "=" term
```
