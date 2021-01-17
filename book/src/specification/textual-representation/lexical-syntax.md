# Lexical Syntax

## Input format

The textual surface language assigns meaning to a source string,
which consists of a sequence of _Unicode scalar values_ (as defined in Section 3.4 of [the Unicode Standard](www.unicode.org/versions/latest/)),
terminated with a virtual end-of-file symbol, `"\0"`:

```text
unicode-scalar-value ::=
    | "\u{00}" ... "\u{D7FF}"
    | "\u{E000}" ... "\u{10FFF}"

source ::=
    | unicode-scalar-value* "\0"
```

For convenience, we define a number of special values within the above `unicode-scalar-value` definition:

```text
horizontal-tab        ::= "\u{0009}"
line-feed             ::= "\u{000A}"
vertical-tab          ::= "\u{000B}"
form-feed             ::= "\u{000C}"
carriage-return       ::= "\u{000D}"
next-line             ::= "\u{0085}"
left-to-right-mark    ::= "\u{200E}"
right-to-left-mark    ::= "\u{200F}"
line-separator        ::= "\u{2028}"
paragraph-separator   ::= "\u{2029}"
```

## Whitespace

```text
line-break ::=
    | line-feed
    | carriage-return
    | carriage-return line-feed
    | "\0"

white-space ::=
    | horizontal-tab
    | vertical-tab
    | form-feed
    | line-break
    | next-line
    | left-to-right-mark
    | right-to-left-mark
    | line-separator
    | paragraph-separator
```

## Comments

```text
comment-data  ::= unicode-scalar-value - (line-feed | carriage-return)

comment       ::= "--" comment-data* line-break
doc-comment   ::= "|||" comment-data* line-break
```

## Keywords

```text
keyword ::=
    | "as"
    | "fun"
    | "Fun"
    | "Record"
    | "record"
```

## Names

```text
name-start    ::= "a" ... "z" | "A" ... "Z"
name-continue ::= "a" ... "z" | "A" ... "Z" | "0" ... "9" | "-"

name ::=
    | (name-start name-continue*) - keyword
```

### Punctuation

```text
delimiter ::=
    | "{"
    | "}"
    | "["
    | "]"
    | "("
    | ")"

symbol ::=
    | "."
    | ":"
    | ","
    | "="
    | "=>"
    | "->"

punctuation ::=
    | delimiter
    | symbol
```

### Numeric Literals

```text
sign            ::= "+" | "-"
digit-start     ::= "0" ... "9"
digit-continue  ::= "a" ... "z" | "A" ... "Z" | "0" ... "9" | "."

numeric-literal ::=
    | sign? digit-start digit-continue*
```

### Quoted Literals

```text
quoted-data(quote-end) ::=
    | unicode-scalar-value - (quote-end | "\\")
    | "\\" unicode-scalar-value

character-literal ::= "\'" quoted-data("\'")* "\'"
string-literal    ::= "\"" quoted-data("\"")* "\""
```

### Tokens

```text
ignored :=
    | white-space
    | comment

token ::=
    | doc-comment
    | keyword
    | name
    | punctuation
    | numeric-literal
    | character-literal
    | string-literal

tokens :=
    | (token | ignored)*
```
