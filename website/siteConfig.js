// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

// List of projects/orgs using your project for the users page.
const users = [
  // {
  //   caption: 'User1',
  //   // You will need to prepend the image path with your baseUrl
  //   // if it is not '/', like: '/test-site/img/docusaurus.svg'.
  //   image: '/img/docusaurus.svg',
  //   infoLink: 'https://www.facebook.com',
  //   pinned: true,
  // },
];

const siteConfig = {
  title: 'Pikelet' /* title for your website */,
  tagline: 'A friendly little systems language with first-class types.',
  url: 'https://pikelet.org' /* your website url */,
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'pikelet-site',
  organizationName: 'pikelet',

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { doc: 'guide', label: 'Guide' },
    { page: 'try', label: 'Try' },
    { doc: 'spec', label: 'Spec' },
    { blog: true, label: 'Blog' },
    { href: 'https://github.com/pikelet-lang/pikelet', label: 'GitHub' },
  ],

  blogSidebarCount: 'ALL',

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  headerIcon: 'img/pikelet.png',
  footerIcon: 'img/pikelet.png',
  favicon: 'img/favicon.png',

  /* Colors for website */
  colors: {
    primaryColor: '#ECB93E',
    secondaryColor: '#AB7E10',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Brendan Zabarauskas`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
    hljs: hljs => {
      hljs.registerLanguage('pikelet', hljs => {
        var KEYWORDS = {
          keyword:
            'as case else let if import in module record Record Type then where',
          built_in:
            'true false Bool String Char U8 U16 U32 U64 S8 S16 S32 S64 F32 F64',
        };

        var STRING = hljs.inherit(hljs.QUOTE_STRING_MODE, { illegal: null });

        var CHARACTER = {
          className: 'string',
          begin: /'\\?(x\w{2}|u\w{4}|U\w{8}|.)'/,
        };

        var NUMBER = {
          className: 'number',
          variants: [
            { begin: '\\b0b([01_]+)' },
            { begin: '\\b0o([0-7_]+)' },
            { begin: '\\b0x([A-Fa-f0-9_]+)' },
            { begin: '\\b(\\d[\\d_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?)' },
          ],
          relevance: 0,
        };

        var COMMENT = {
          variants: [
            hljs.COMMENT('--', '$'),
            hljs.COMMENT('\\|\\|\\|', '$'),
            hljs.COMMENT('{-', '-}', { contains: ['self'] }),
          ],
        };

        return {
          keywords: KEYWORDS,
          contains: [
            STRING,
            CHARACTER,
            NUMBER,

            COMMENT,

            { begin: '->|<-' }, // No markup, relevance booster
          ],
        };
      });

      hljs.registerLanguage('pikelet-repl', hljs => {
        var SHELL = {
          className: 'meta',
          begin: /^\$\s/,
          starts: {
            end: /$/,
            subLanguage: 'shell',
          },
        };

        var BANNER = {
          className: 'comment',
          begin: '____',
          end: ':? for help',
        };

        var PROMPT = {
          className: 'meta',
          begin: /^[\w][0-9\w\.-_]*>/,
          keywords: ':? :h :help :q :quit :t :type',
          starts: {
            end: /$/,
            subLanguage: 'pikelet',
          },
        };

        return {
          contains: [SHELL, BANNER, PROMPT],
        };
      });
    },
  },

  markdownPlugins: [
    md =>
      md.use(require('./lib/remarkable-katex'), {
        macros: {
          // Haskell-style append https://tex.stackexchange.com/questions/4194/how-to-typeset-haskell-operator-and-friends
          '\\doubleplus': '+\\kern-1.3ex+\\kern0.8ex',
          // Small caps https://github.com/mathjax/MathJax-docs/wiki/Small-caps-%5Ctextsc-in-MathJaxx
          '\\sc#1': '\\dosc#1\\csod',
          '\\dosc#1#2': '\\csod{{\\rm #1{\\small #2}}}',

          '\\rule[3]': '\\dfrac{ ~~#2~~ }{ ~~#3~~ } & \\Tiny{\\text{(#1)}}',

          // \DeclareMathOperator{\max}{max}
          // \DeclareMathOperator{\field}{field}
          // \DeclareMathOperator{\fieldty}{fieldty}
          // \DeclareMathOperator{\fieldsubst}{fieldsubst}
          // \DeclareMathOperator{\Match}{\sc{MATCH}}
          // \DeclareMathOperator{\shift}{shift}

          // Judgments
          '\\eval[3]': '#1 \\vdash #2 \\hookrightarrow #3',
          '\\check[4]': '#1 \\vdash #2 \\uparrow #3 \\leadsto #4',
          '\\infer[4]': '#1 \\vdash #2 downarrow #3 \\leadsto #4',
          '\\subty[3]': '#1 \\vdash #2 preccurlyeq #3',
          '\\match[3]': 'Match(#1,#2) \\Longrightarrow #3',
          '\\checkpat[5]':
            '#1 \\vdash #2 \\uparrow #3 \\leadsto #4 \\Longrightarrow #5',
          '\\inferpat[5]':
            '#1 \\vdash #2 downarrow #3 \\leadsto #4 \\Longrightarrow #5',

          // Metavariables
          '\\rexpr': 'r', // raw expressions
          '\\rtype': 'R', // raw types
          '\\rpat': 's', // raw patterns

          '\\texpr': 't', // expressions
          '\\ttype': 'T', // types
          '\\tpat': 'p', // patterns

          '\\vexpr': 'v', // value expressions
          '\\vtype': 'V', // value types
          '\\wexpr': 'w', // whnf expressions
          '\\wtype': 'W', // whnf types
          '\\nexpr': 'n', // neutral expressions
          '\\ntype': 'N', // neutral types

          '\\ctx': '\\Gamma', // contexts

          // Keywords
          '\\kw[1]': '\\mathsf{#1}',

          // Term and Type constructors
          '\\label': 'l',
          '\\binder': 'x',
          '\\var[1]': 'x^\\wedge{#1}',
          '\\Type[1]': '\\kw{Type}^\\wedge{#1}',
          '\\Arrow[2]': '#1 \\rightarrow #2',
          '\\Pi[2]': '\\Arrow{(#1)}{#2}',
          '\\lam[2]': '\\kw{\\lambda} #1 . #2',
          '\\app[2]': '#1 ~ #2',
          '\\case[2]': '\\kw{case} ~ #1 \\left{ #2 \\right}',
          '\\RecordCons[2]': '\\kw{Record} \\left{ #1; #2 \\right}',
          '\\RecordEmpty': '\\kw{Record} \\left{\\right}',
          '\\as': '~ \\kw{as} ~',
          '\\record[1]': '\\kw{record} \\left{ #1 \\right}',
          '\\proj[3]': '#1.#2^\\wedge{#3}',
          '\\subst[3]': '#1 ~ [#2 \\rightarrow #3]',

          // Items
          '\\declItem[2]': '#1 : #2',
          '\\defnItem[2]': '#1 = #2',

          // Contexts
          '\\emptyCtx': '\\varnothing',
          '\\composeCtx[2]': '#1 sim #2',
          '\\extendCtx[2]': '#1, #2',
        },
      }),
  ],

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // Add custom stylesheets here that would be placed in <link rel="stylesheet"> tags.
  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css',
      integrity:
        'sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH',
      crossOrigin: 'anonymous',
    },
  ],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/pikelet.png',
  twitterImage: 'img/pikelet.png',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  repoUrl: 'https://github.com/pikelet-lang/pikelet',
  chatUrl: 'https://gitter.im/pikelet-lang/',
};

module.exports = siteConfig;
