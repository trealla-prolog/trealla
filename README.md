# Trealla Prolog

This is a fork of [Trealla Prolog](https://github.com/trealla-prolog/trealla) for experimenting with WebAssembly/WASI.
For more info on Trealla, check out the parent repository.

We endeavor to keep this fork as close as possible to the upstream and contribute all stable changes upstream.
Ideally, when WASM support is better stablized, this fork won't need to exist.

## Binaries on WAPM
You can grab WASM binary builds from [guregu/trealla on WAPM](https://wapm.io/guregu/trealla).
These builds are uploaded automatically for each release.

## Differences from upstream
- `library(js)` JSON-based programmatic toplevel and WASM host interop (WIP).
- `library(pseudojson)` Very fast JSON parser/generator (but not validator).
- WASM system predicates: `'$host_call'/2` and `'$host_resume'/1` (see `js_eval/2` in `library/js.pl`).

![Trealla Logo](https://user-images.githubusercontent.com/131059/190109875-7eb65bf5-feef-41e1-b19c-7fbcab8887ae.png)
