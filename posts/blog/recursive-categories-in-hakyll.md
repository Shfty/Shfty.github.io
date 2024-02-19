---
title: Recursive Categories in Hakyll
author: Josh ρ
published: 2024-02-13
---

* Why recursive categories?
  * More granular organization
  * In my case, wanted to emulate the look of a filesystem

* How?
  * Can't simply make the compiler behave recursively,
    since it has to take care of various backend dependency management
  * Ergo, exploit the flat structure of Hakyll's built-in tags
    to categorize based on directory path, then interpret
    it (ex. to add indentation for subcats) while compiling final content
