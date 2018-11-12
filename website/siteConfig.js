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

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

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
