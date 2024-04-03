---
title: Custom Syntax Highlighting in Hakyll
author: Josh ρ
icon: haskell
tags: Haskell
published: 2024-02-13
---

## Why?
  * Hakyll uses Pandoc's built-in code highlighting to generate
    styled code blocks in HTML
  * Pandoc supports loading custom themes via JSON,
    but doing this via its Haskell API is not well-documented

### How?
  * Load and parse a custom JSON theme using IO,
    and load it into a customized pandocCompiler before
    entering the Rules monad
  * Considered storing Style as an Item and processing
    it using Compiler to allow for hot-reloaded edits,
    but Style does not implement Writable, and thus is inadmissable
    * Potential for future work?
      * Hakyll has a semantic for storing items that won't be written
        out to the final site (ex. templates),
        but seems to enforce their writability -in case- they need to be written
      * Worth investigating to see if there's a mechanism
        for storing such data in a way that can be hot-reloaded
  * Turns out that pandoc exports this functionality directly via lookupHighlightStyle,
    but still needs to be bridged into Hakyll via the PandocIO typeclass;
    i.e. by using a combination of hakyll `preprocess` and pandoc `runIO` / `runIOorExplode`
