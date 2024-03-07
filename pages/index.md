---
icon: home
title: '~'
templates:
    - Self

versions:
    - header:
        template: templates/content/header.html

templates_new:
    - Self:
        - apply: true
    - FlexScroll: {}
    - FlexColumn:
        - before:
            - Self:
                - version: header
    - FlexRow:
        - before:
            - Self:
                - version: menu
    - FlexColumn: 
        - after:
            - Footer: {}
    - Document: {}
---

## Welcome

<img src="/images/haskell-logo.png" style="float: right; margin: 10px;" />

Welcome to my blog!

I've reproduced a list of recent posts here for your reading pleasure:

## Posts
$partial("templates/post-list.html")$

…or you can find more in the <a href="/archive.html">archives</a>.
