# Lexical Syntax

The _lexical structure_ of the Pikelet programming langues is a description of what constitutes a valid sequence of tokens in the programming language.

## Characters

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

## Whitespace and comments

```text
line-break ::=
    | line-feed
    | carriage-return
    | carriage-return line-feed
    | "\0"

comment-text ::=
    | (~(line-feed | carriage-return) unicode-scalar-value)*

comment ::=
    | "--" comment-text line-break

doc-comment ::=
    | "|||" comment-text line-break

white-space ::=
    | horizontal-tab
    | comment
    | vertical-tab
    | form-feed
    | line-break
    | next-line
    | left-to-right-mark
    | right-to-left-mark
    | line-separator
    | paragraph-separator
```

## Keywords and names

```text
keyword ::=
    | "as"
    | "fun"
    | "Fun"
    | "Record"
    | "record"

name-or-keyword ::=
    | ("a" ... "z" | "A" ... "Z") ("a" ... "z" | "A" ... "Z" | "0" ... "9" | "-")*

name ::=
    | ~keyword name-or-keyword
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

### Literals

```text
number-literal ::=
    | ("+" | "-")? ("0" ... "9") ("a" ... "z" | "A" ... "Z" | "0" ... "9" | ".")*

character-literal ::=
    | "\"" ("\"" | ~"\"" unicode-scalar-value)* "\""

string-literal ::=
    | "'" ("'" | ~"'" unicode-scalar-value)* "'"
```

### Tokens

```text
token ::=
    | white-space
    | doc-comment
    | keyword
    | name
    | punctuation
    | number-literal
    | character-literal
    | string-literal
```
