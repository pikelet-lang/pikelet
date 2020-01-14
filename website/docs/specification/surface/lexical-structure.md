---
id: lexical-structure
title: Lexical Structure
keywords:
  - docs
  - specification
  - pikelet
---

The _lexical structure_ of the Pikelet programming langues is a description of what constitutes a valid sequence of tokens in the programming language.

## Characters

The textual surface language assigns meaning to a source string,
which consists of a sequence of _Unicode scalar values_ (as defined in Section 3.4 of [the Unicode Standard](www.unicode.org/versions/latest/)),
terminated with a virtual end-of-file symbol, &empty;:

> **Grammar**:
>
> <a href="#var:unicode-scalar-value"><var id="var:unicode-scalar-value">unicode-scalar-value</var></a> ::=\
> &emsp;|&ensp;<kbd>U+00</kbd> &hellip; <kbd>U+D7FF</kbd>\
> &emsp;|&ensp;<kbd>U+E000</kbd> &hellip; <kbd>U+10FFF</kbd>
>
> <a href="#var:source"><var id="var:source">source</var></a> ::=\
> &emsp;|&ensp;<a href="#var:unicode-scalar-value"><var>unicode-scalar-value</var></a><sup>\*</sup> &empty;

For convenience, we define a number of special values within the above <a href="#var:unicode-scalar-value"><var>unicode-scalar-value</var></a> definition:

> **Grammar**:
>
> <a href="#var:horizontal-tab"><var id="var:horizontal-tab">horizontal-tab</var></a> ::=\
> &emsp;|&ensp;<kbd>U+0009</kbd>
>
> <a href="#var:line-feed"><var id="var:line-feed">line-feed</var></a> ::=\
> &emsp;|&ensp;<kbd>U+000A</kbd>
>
> <a href="#var:vertical-tab"><var id="var:vertical-tab">vertical-tab</var></a> ::=\
> &emsp;|&ensp;<kbd>U+000B</kbd>
>
> <a href="#var:form-feed"><var id="var:form-feed">form-feed</var></a> ::=\
> &emsp;|&ensp;<kbd>U+000C</kbd>
>
> <a href="#var:carriage-return"><var id="var:carriage-return">carriage-return</var></a> ::=\
> &emsp;|&ensp;<kbd>U+000D</kbd>
>
> <a href="#var:next-line"><var id="var:next-line">next-line</var></a> ::=\
> &emsp;|&ensp;<kbd>U+0085</kbd>
>
> <a href="#var:left-to-right-mark"><var id="var:left-to-right-mark">left-to-right-mark</var></a> ::=\
> &emsp;|&ensp;<kbd>U+200E</kbd>
>
> <a href="#var:right-to-left-mark"><var id="var:right-to-left-mark">right-to-left-mark</var></a> ::=\
> &emsp;|&ensp;<kbd>U+200F</kbd>
>
> <a href="#var:line-separator"><var id="var:line-separator">line-separator</var></a> ::=\
> &emsp;|&ensp;<kbd>U+2028</kbd>
>
> <a href="#var:paragraph-separator"><var id="var:paragraph-separator">paragraph-separator</var></a> ::=\
> &emsp;|&ensp;<kbd>U+2029</kbd>

## Whitespace and comments

> **Grammar**:
>
> <a href="#var:line-break"><var id="var:line-break">line-break</var></a> ::=\
> &emsp;|&ensp;<a href="#var:line-feed"><var>line-feed</var></a>\
> &emsp;|&ensp;<a href="#var:carriage-return"><var>carriage-return</var></a>\
> &emsp;|&ensp;<a href="#var:carriage-return"><var>carriage-return</var></a> <a href="#var:line-feed"><var>line-feed</var></a>\
> &emsp;|&ensp;&empty;
>
> <a href="#var:comment-text"><var id="var:comment-text">comment-text</var></a> ::=\
> &emsp;|&ensp;~(<a href="#var:line-feed"><var>line-feed</var></a> | <a href="#var:carriage-return"><var>carriage-return</var></a>) <a href="#var:unicode-scalar-value"><var>unicode-scalar-value</var></a><sup>\*</sup>
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
> &emsp;|&ensp;`fun`\
> &emsp;|&ensp;`Record`\
> &emsp;|&ensp;`record`
>
> <a href="#var:ident-or-keyword"><var id="var:ident-or-keyword">ident-or-keyword</var></a> ::=\
> &emsp;|&ensp;(`a` &hellip; `z` | `A` &hellip; `Z`) (`a` &hellip; `z` | `A` &hellip; `Z` | `0` &hellip; `9` | `-`)<sup>\*</sup>
>
> <a href="#var:ident"><var id="var:ident">ident</var></a> ::=\
> &emsp;|&ensp;~<a href="#var:keyword"><var>keyword</var></a> <a href="#var:ident-or-keyword"><var>ident-or-keyword</var></a>

### Punctuation

> **Grammar**:
>
> <a href="#var:delimiter"><var id="var:delimiter">delimiter</var></a> ::=\
> &emsp;|&ensp;`{`\
> &emsp;|&ensp;`}`\
> &emsp;|&ensp;`[`\
> &emsp;|&ensp;`]`\
> &emsp;|&ensp;`(`\
> &emsp;|&ensp;`)`
>
> <a href="#var:symbol"><var id="var:symbol">symbol</var></a> ::=\
> &emsp;|&ensp;`:`\
> &emsp;|&ensp;`,`\
> &emsp;|&ensp;`=`\
> &emsp;|&ensp;`=>`\
> &emsp;|&ensp;`->`\
> &emsp;|&ensp;`;`
>
> <a href="#var:punctuation"><var id="var:punctuation">punctuation</var></a> ::=\
> &emsp;|&ensp;<a href="#var:delimiter"><var>delimiter</var></a>\
> &emsp;|&ensp;<a href="#var:symbol"><var>symbol</var></a>

### Numeric literals

> **Grammar**:
>
> <a href="#var:number-literal"><var id="var:number-literal">number-literal</var></a> ::=\
> &emsp;|&ensp;(`+` | `-`)<sup>?</sup> (`0` &hellip; `9`)<sup>+</sup> `.`<sup>?</sup> (`0` &hellip; `9`)<sup>+</sup>

### Character and string literals

> **Grammar**:
>
> <a href="#var:character-literal"><var id="var:character-literal">character-literal</var></a> ::=\
> &emsp;|&ensp;`"` (`\"` | ~`"` <a href="#var:unicode-scalar-value"><var>unicode-scalar-value</var></a>)<sup>\*</sup>  `"`
>
> <a href="#var:string-literal"><var id="var:string-literal">string-literal</var></a> ::=\
> &emsp;|&ensp;`'` (`\'` | ~`'` <a href="#var:unicode-scalar-value"><var>unicode-scalar-value</var></a>)<sup>\*</sup>  `'`

### Tokens

> **Grammar**:
>
> <a href="#var:token"><var id="var:token">token</var></a> ::=\
> &emsp;|&ensp;<a href="#var:white-space"><var>white-space</var></a>\
> &emsp;|&ensp;<a href="#var:doc-comment"><var>doc-comment</var></a>\
> &emsp;|&ensp;<a href="#var:keyword"><var>keyword</var></a>\
> &emsp;|&ensp;<a href="#var:ident"><var>ident</var></a>\
> &emsp;|&ensp;<a href="#var:punctuation"><var>punctuation</var></a>\
> &emsp;|&ensp;<a href="#var:number-literal"><var>number-literal</var></a>\
> &emsp;|&ensp;<a href="#var:character-literal"><var>character-literal</var></a>\
> &emsp;|&ensp;<a href="#var:string-literal"><var>string-literal</var></a>
