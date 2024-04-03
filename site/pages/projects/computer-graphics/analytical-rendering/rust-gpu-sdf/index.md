---
title: Rust-GPU SDF
git: shfty-rust/rust-gpu-sdf
icon: rust
tags: Rust
published: 2024-02-08
---

Signed Distance Field rendering library based on bevy-rust-gpu.

Encodes field functions as native Rust functions, using various type-level trait machinery to represent category theoric abstractions.

Unfortunately, limitations in rust-gpu prevented it from reaching the desired level of abstraction,
thus giving rise to the creation of [Elysian](../elysian.html).

![Dynamic implicit surface](animated.gif)

