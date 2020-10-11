import hljs from "highlight.js/lib/core";

hljs.registerLanguage("pikelet", (hljs) => {
  const KEYWORDS = {
    keyword: "as fun Fun record Record",
    built_in: "Type Bool true false U8 U16 U32 U64 S8 S16 S32 S64 F32 F64 String Char Array List",
  };

  const CHARACTER = {
    className: "string",
    begin: /'([^'\\]|\\.)*'/,
  };
  const STRING = {
    className: "string",
    begin: /"([^"\\]|\\.)*"/,
  };
  const NUMBER = {
    className: "number",
    begin: /\b[-+]?[0-9][a-zA-Z0-9_\.]*\b/,
    relevance: 0,
  };

  const COMMENT = {
    variants: [
      hljs.COMMENT("--", "$"),
      hljs.COMMENT("|||", "$"),
    ],
  };

  return {
    name: "Fathom",
    keywords: KEYWORDS,
    contains: [
      STRING,
      CHARACTER,
      NUMBER,

      COMMENT,

      { begin: "->|<-" }, // No markup, relevance booster
    ],
  };
});

window.addEventListener("load", (event) => {
  document
    .querySelectorAll("code.language-pikelet")
    .forEach((block) => hljs.highlightBlock(block));
});
