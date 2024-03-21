---
title: Elysian
git: Shfty/elysian
icon: rust
published: 2024-02-08
---

Pure functional row-polymorphic programming language.

Initially created as a domain-agnostic first-class representation for field functions,
with the intent of outputting GPU-friendly representations, such as transpiled GLSL / WGSL, or compiled SPIR-V.

Has since evolved into a fully-fledged language, with field function features represented within it as a library.

Goals:

* Simplicity / Unified Design
  * Decoupling between logic and implementation
    * All concrete implementations details are separated out into a compiler backend,
      which is free to make opinionated implementations and optimizations in order to output a final artifact
  * Minimal amount of built-in terms
    * Many PLs use arbitrary statement structure for compiler-level functionality
      like defining data, types, traits, and so on
      * Opaque compiler-level functionality is a necessity, but its interfaces should not be arbitrary
        * To wit, Elysian standardizes this interface using existing language primitives
      * In some cases, such as that of typeclasses / traits, such functionality can be
        implemented almost entirely in-language, with some small amount of compiler
        glue to enable ex. updating a global binding ahead of compilation
    * Lower cognitive footprint
    * Easier to create language backends
