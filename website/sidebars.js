/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

module.exports = {
  docsSidebar: [
    {
      type: "category",
      label: "Pikelet",
      items: [
        "pikelet/introduction",
        "pikelet/roadmap",
        "pikelet/contributing",
      ],
    },
    {
      type: "category",
      label: "Guide",
      items: [
        "guide",
        "guide/getting-started",
        "guide/hello-world",
        "guide/data-structures",
        "guide/modular-programming",
      ],
    },
    {
      type: "category",
      label: "Reference",
      items: [
        "reference",
        {
          type: "category",
          label: "Language Features",
          items: [
            "reference/comments",
            "reference/names",
            "reference/literals",
            "reference/builtins",
            "reference/functions",
            "reference/records",
            "reference/universes",
          ],
        },
        "reference/bibliography",
        "reference/influences",
      ],
    },
    {
      type: "category",
      label: "Specification",
      items: [
        "specification",
        {
          type: "category",
          label: "Surface Language",
          items: [
            "specification/surface/lexical-structure",
            "specification/surface/grammar",
            "specification/surface/elaboration",
          ],
        },
        {
          type: "category",
          label: "Core Language",
          items: [
            "specification/core/operational-semantics",
            "specification/core/declarative-typing",
            "specification/core/bidirectional-typing",
          ],
        },
        "specification/inspiration",
      ],
    },
  ],
};
