# Lexical syntax

## Whitespace and comments

> <sub>Grammar:</sub>
>
> <var>horizontal-tab</var> ::=\
> &emsp;|&ensp;U+0009
>
> <var>line-feed</var> ::=\
> &emsp;|&ensp;U+000A
>
> <var>vertical-tab</var> ::=\
> &emsp;|&ensp;U+000B
>
> <var>form-feed</var> ::=\
> &emsp;|&ensp;U+000C
>
> <var>carriage-return</var> ::=\
> &emsp;|&ensp;U+000D
>
> <var>next-line</var> ::=\
> &emsp;|&ensp;U+0085
>
> <var>left-to-right-mark</var> ::=\
> &emsp;|&ensp;U+200E
>
> <var>right-to-left-mark</var> ::=\
> &emsp;|&ensp;U+200F
>
> <var>line-separator</var> ::=\
> &emsp;|&ensp;U+2028
>
> <var>paragraph-separator</var> ::=\
> &emsp;|&ensp;U+2029
>
> <var>line-break</var> ::=\
> &emsp;|&ensp;<var>line-feed</var>\
> &emsp;|&ensp;<var>carriage-return</var>\
> &emsp;|&ensp;<var>carriage-return</var> <var>line-feed</var>
>
> <var>comment-text</var> ::=\
> &emsp;|&ensp;Any Unicode scalar value except <var>line-feed</var> or <var>carriage-return</var>
>
> <var>comment</var> ::=\
> &emsp;|&ensp;`--` <var>comment-text</var> <var>line-break</var>
>
> <var>doc-comment</var> ::=\
> &emsp;|&ensp;`|||` <var>comment-text</var> <var>line-break</var>
>
> <var>white-space</var> ::=\
> &emsp;|&ensp;<var>horizontal-tab</var>\
> &emsp;|&ensp;<var>comment</var>\
> &emsp;|&ensp;<var>vertical-tab</var>\
> &emsp;|&ensp;<var>form-feed</var>\
> &emsp;|&ensp;<var>line-break</var>\
> &emsp;|&ensp;<var>next-line</var>\
> &emsp;|&ensp;<var>left-to-right-mark</var>\
> &emsp;|&ensp;<var>right-to-left-mark</var>\
> &emsp;|&ensp;<var>line-separator</var>\
> &emsp;|&ensp;<var>paragraph-separator</var>

## Keywords and identifiers

> <sub>Grammar:</sub>
>
> <var>keyword</var> ::=\
> &emsp;|&ensp; `Record`\
> &emsp;|&ensp; `record`
>
> <var>ident-or-keyword</var> ::=\
> &emsp;|&ensp;[`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `-`]<sup>\*</sup>
>
> <var>ident</var> ::=\
> &emsp;|&ensp;Any <var>ident-or-keyword</var> except <var>keyword</var>

### Punctuation

> <sub>Grammar:</sub>
>
> <var>punctuation</var> ::=\
> &emsp;|&ensp;`{`\
> &emsp;|&ensp;`}`\
> &emsp;|&ensp;`(`\
> &emsp;|&ensp;`)`\
> &emsp;|&ensp;`:`\
> &emsp;|&ensp;`,`\
> &emsp;|&ensp;`=`\
> &emsp;|&ensp;`->`\
> &emsp;|&ensp;`;`

### Literals

TODO

### Tokens

> <sub>Grammar:</sub>
>
> <var>token</var> ::=\
> &emsp;|&ensp;<var>white-space</var>\
> &emsp;|&ensp;<var>doc-comment</var>\
> &emsp;|&ensp;<var>keyword</var>\
> &emsp;|&ensp;<var>ident</var>\
> &emsp;|&ensp;<var>punctuation</var>\
> &emsp;|&ensp;<var>literal</var>
