// Check out some other Prism highlighters for inspiration:
//
// https://github.com/PrismJS/prism/blob/master/components/prism-haskell.js
// https://github.com/PrismJS/prism/blob/master/components/prism-rust.js
// https://github.com/SassDoc/prism-scss-sassdoc/blob/master/prism-scss-sassdoc.js

Prism.languages.pikelet = {
  // Comments.
  comment: {
    pattern: /(^|[^\\:])--.*/,
    lookbehind: true
  },
  "doc-comment": {
    pattern: /(^|[^\\:])\|\|\|.*/,
    lookbehind: true,
    alias: "comment"
  },
  // String literals
  string: [
    {
      pattern: /b?r(#*)"(?:\\.|(?!"\1)[^\\\r\n])*"\1/,
      greedy: true
    },
    {
      pattern: /b?"(?:\\.|[^\\\r\n"])*"/,
      greedy: true
    }
  ],
  // Character literals
  char: {
    pattern: /b?'(?:\\(?:x[0-7][\da-fA-F]|u{(?:[\da-fA-F]_*){1,6}|.)|[^\\\r\n\t'])'/,
    alias: "string"
  },
  // Keywords
  keyword: /\b(?:fun|Record|record)\b/,
  // Builtins
  builtin: /\b(?:Type|Bool|true|false|U8|U16|U32|U64|S8|S16|S32|S64|F32|F64|String|Char|Array|List)\b/,
  // Numeric literals
  number: [
    /(-|\+)?\b(?:0b[01](?:_?[01])*)(?:_)?\b/,
    /(-|\+)?\b(?:0o[0-7](?:_?[0-7])*)(?:_)?\b/,
    /(-|\+)?\b(?:0x[\dA-Fa-f](?:_?[\dA-Fa-f])*)(?:_)?\b/,
    /(-|\+)?\b(?:(?:\d(?:_?\d)*)?\.?\d(?:_?\d)*(?:[Ee][+-]?\d+)?)(?:_)?\b/
  ],
  // Symbols
  delimiter: {
    pattern: /\[|\]|\(|\)|\{|\}/,
    alias: "punctuation"
  },
  punctuation: /;|,|:|=>?/,
  operator: /->|\.|\^/
};
