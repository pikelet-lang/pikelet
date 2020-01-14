module.exports = {
  title: "Pikelet",
  tagline: "A friendly little systems language with first-class types.",
  url: "https://pikelet.org",
  baseUrl: "/",
  favicon: "img/favicon.ico",
  organizationName: "pikelet-lang", // Usually your GitHub org/user name.
  projectName: "pikelet", // Usually your repo name.
  themeConfig: {
    sidebarCollapsible: false,
    prism: {
      theme: require("prism-react-renderer/themes/palenight")
    },
    navbar: {
      title: "Pikelet",
      logo: {
        alt: "My Site Logo",
        src: "img/logo.svg"
      },
      links: [
        {
          label: "Guide",
          to: "docs/guide",
          position: "left"
        },
        {
          label: "Reference",
          to: "docs/reference",
          position: "left"
        },
        {
          label: "Specification",
          to: "docs/specification",
          position: "left"
        },
        {
          label: "Contributing",
          to: "docs/contributing",
          position: "left"
        },
        // {
        //   label: "Blog",
        //   to: "blog",
        //   position: "left"
        // },
        {
          label: "GitHub",
          href: "https://github.com/pikelet-lang/pikelet",
          position: "right"
        },
        {
          label: "Gitter",
          href: "https://gitter.im/pikelet-lang/Lobby",
          position: "right"
        }
      ]
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Docs",
          items: [
            { to: "docs/guide", label: "Guide" },
            { to: "docs/reference", label: "Reference" },
            { to: "docs/specification", label: "Specification" }
          ]
        },
        {
          title: "Community",
          items: [
            {
              label: "Gitter",
              href: "https://gitter.im/pikelet-lang/Lobby"
            },
            {
              label: "Discord (Unofficial)",
              href: "https://discord.gg/vQtyxjZ"
            }
          ]
        },
        {
          title: "Social",
          items: [
            {
              label: "Blog",
              to: "blog"
            },
            {
              label: "GitHub",
              href: "https://github.com/pikelet-lang/pikelet"
            },
            {
              label: "Twitter",
              href: "https://twitter.com/pikelet-lang"
            }
          ]
        }
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Brendan Zabarauskas. Built with Docusaurus.`
    }
  },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          sidebarPath: require.resolve("./sidebars.js"),
          editUrl:
            "https://github.com/pikelet-lang/pikelet/edit/master/website/",
          remarkPlugins: [[require("remark-admonitions"), {}]]
        },
        // blog: {
        //   feedOptions: {
        //     type: "all",
        //     description: "Pikelet Language Blog",
        //     copyright: `Copyright © ${new Date().getFullYear()} Brendan Zabarauskas.`,
        //     language: "en"
        //   }
        // },
        theme: {
          customCss: require.resolve("./src/css/custom.css")
        }
      }
    ]
  ]
};
