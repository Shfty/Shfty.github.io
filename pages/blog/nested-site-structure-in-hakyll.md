---
title: Nested site structure in Hakyll
author: Josh ρ
icon: haskell
published: 2024-02-13
---

## Why nested structure?
  * Fairly common pattern for sites with heavily-categorized content
  * Allows for more granular organization
  * In my case, wanted to emulate the appearance of a filesystem

## How?
  * Can't follow intuition and do a recursive descent into the filesystem,
    as Hakyll operates on a declarative monadic structure that automates the site compilation process
  * Categories are provided atop the tags system, but operate on a flat structure
  * Ergo, emulate nested structure by keying the existing flat structure
    on directory paths, and interpret it during compilation

## Implementation

### Breadcrumbs
  * Needed to modify tagsFieldWith so its renderLink function can
    take a tag Identifier instead of a pre-extracted String, and return a MonadFail + MonadMetadata type
    * This allows it to look up the tag's metadata (such as title) for rendering,
      as well as return an error if the requested metadata doesn't exist
  * Injected rendered HTML into the header via template
  * TODO

### Tag Pages
  * Needed to modify tagsRules to use match instead of create,
    in order to give pages static content
  * Created a combined post + posts template that renders the provided body,
    and an injected list of child pages
  * TODO

### Navigation Panel
  * Opted to overhaul structure to render a <details>-based nested navigation panel;
    the generated data is a tree, and the problem is ostensibly modeled on a tree,
    so using an actual tree - while remaining friendly to Hakyll - is an elegant solution
    * Emulating via tags is nice, but felt like it was being stretched too far.
