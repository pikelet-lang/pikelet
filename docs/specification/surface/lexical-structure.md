# Lexical structure

The _lexical structure_ of the Pikelet programming langues is a description of what constitutes a valid sequence of tokens in the programming language.

## Whitespace and comments

> **Grammar**:
>
> <a href="#var:horizontal-tab"><var id="var:horizontal-tab">horizontal-tab</var></a> ::=\
> &emsp;|&ensp;U+0009
>
> <a href="#var:line-feed"><var id="var:line-feed">line-feed</var></a> ::=\
> &emsp;|&ensp;U+000A
>
> <a href="#var:vertical-tab"><var id="var:vertical-tab">vertical-tab</var></a> ::=\
> &emsp;|&ensp;U+000B
>
> <a href="#var:form-feed"><var id="var:form-feed">form-feed</var></a> ::=\
> &emsp;|&ensp;U+000C
>
> <a href="#var:carriage-return"><var id="var:carriage-return">carriage-return</var></a> ::=\
> &emsp;|&ensp;U+000D
>
> <a href="#var:next-line"><var id="var:next-line">next-line</var></a> ::=\
> &emsp;|&ensp;U+0085
>
> <a href="#var:left-to-right-mark"><var id="var:left-to-right-mark">left-to-right-mark</var></a> ::=\
> &emsp;|&ensp;U+200E
>
> <a href="#var:right-to-left-mark"><var id="var:right-to-left-mark">right-to-left-mark</var></a> ::=\
> &emsp;|&ensp;U+200F
>
> <a href="#var:line-separator"><var id="var:line-separator">line-separator</var></a> ::=\
> &emsp;|&ensp;U+2028
>
> <a href="#var:paragraph-separator"><var id="var:paragraph-separator">paragraph-separator</var></a> ::=\
> &emsp;|&ensp;U+2029
>
> <a href="#var:line-break"><var id="var:line-break">line-break</var></a> ::=\
> &emsp;|&ensp;<a href="#var:line-feed"><var>line-feed</var></a>\
> &emsp;|&ensp;<a href="#var:carriage-return"><var>carriage-return</var></a>\
> &emsp;|&ensp;<a href="#var:carriage-return"><var>carriage-return</var></a> <a href="#var:line-feed"><var>line-feed</var></a>
>
> <a href="#var:comment-text"><var id="var:comment-text">comment-text</var></a> ::=\
> &emsp;|&ensp;Any Unicode scalar value except <a href="#var:line-feed"><var>line-feed</var></a> or <a href="#var:carriage-return"><var>carriage-return</var></a>
>
> <a href="#var:comment"><var id="var:comment">comment</var></a> ::=\
> &emsp;|&ensp;`--` <a href="comment-text"><var>comment-text</var></a> <a href="#var:line-break"><var>line-break</var></a>
>
> <a href="#var:doc-comment"><var id="var:doc-comment">doc-comment</var></a> ::=\
> &emsp;|&ensp;`|||` <a href="comment-text"><var>comment-text</var></a> <a href="#var:line-break"><var>line-break</var></a>
>
> <a href="#var:white-space"><var id="var:white-space">white-space</var></a> ::=\
> &emsp;|&ensp;<a href="#var:horizontal-tab"><var>horizontal-tab</var></a>\
> &emsp;|&ensp;<a href="#var:comment"><var>comment</var></a>\
> &emsp;|&ensp;<a href="#var:vertical-tab"><var>vertical-tab</var></a>\
> &emsp;|&ensp;<a href="#var:form-feed"><var>form-feed</var></a>\
> &emsp;|&ensp;<a href="#var:line-break"><var>line-break</var></a>\
> &emsp;|&ensp;<a href="#var:next-line"><var>next-line</var></a>\
> &emsp;|&ensp;<a href="#var:left-to-right-mark"><var>left-to-right-mark</var></a>\
> &emsp;|&ensp;<a href="#var:right-to-left-mark"><var>right-to-left-mark</var></a>\
> &emsp;|&ensp;<a href="#var:line-separator"><var>line-separator</var></a>\
> &emsp;|&ensp;<a href="#var:paragraph-separator"><var>paragraph-separator</var></a>

## Keywords and identifiers

> **Grammar**:
>
> <a href="#var:keyword"><var id="var:keyword">keyword</var></a> ::=\
> &emsp;|&ensp;`Record`\
> &emsp;|&ensp;`record`
>
> <a href="#var:ident-or-keyword"><var id="var:ident-or-keyword">ident-or-keyword</var></a> ::=\
> &emsp;|&ensp;[`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `-`]<sup>\*</sup>
>
> <a href="#var:ident"><var id="var:ident">ident</var></a> ::=\
> &emsp;|&ensp;Any <a href="#var:ident-or-keyword"><var>ident-or-keyword</var></a> except <a href="#var:keyword"><var>keyword</var></a>

### Punctuation

> **Grammar**:
>
> <a href="#var:punctuation"><var id="var:punctuation">punctuation</var></a> ::=\
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

> **Grammar**:
>
> <a href="#var:number-literal"><var id="var:number-literal">number-literal</var></a> ::=\
> &emsp;|&ensp;[`+` `-`]<sup>?</sup> [`0`-`9`]<sup>\+</sup> (`.` [`0`-`9`]+)<sup>?</sup>
>
> <a href="#var:character-literal"><var id="var:character-literal">character-literal</var></a> ::=\
> &emsp;|&ensp;`"` TODO `"`
>
> <a href="#var:string-literal"><var id="var:string-literal">string-literal</var></a> ::=\
> &emsp;|&ensp;`'` TODO `'`
>
> <a href="#var:literal"><var id="var:literal">literal</var></a> ::=\
> &emsp;|&ensp;<a href="#var:number-literal"><var>number-literal</var></a>\
> &emsp;|&ensp;<a href="#var:character-literal"><var>character-literal</var></a>\
> &emsp;|&ensp;<a href="#var:string-literal"><var>string-literal</var></a>

### Tokens

> **Grammar**:
>
> <a href="#var:token"><var id="var:token">token</var></a> ::=\
> &emsp;|&ensp;<a href="#var:white-space"><var>white-space</var></a>\
> &emsp;|&ensp;<a href="#var:doc-comment"><var>doc-comment</var></a>\
> &emsp;|&ensp;<a href="#var:keyword"><var>keyword</var></a>\
> &emsp;|&ensp;<a href="#var:ident"><var>ident</var></a>\
> &emsp;|&ensp;<a href="#var:punctuation"><var>punctuation</var></a>\
> &emsp;|&ensp;<a href="#var:literal"><var>literal</var></a>\
> &emsp;|&ensp;<a href="#var:literal"><var>literal</var></a>
