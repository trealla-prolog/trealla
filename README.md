# Trealla Prolog

This is a fork of [Trealla Prolog](https://github.com/trealla-prolog/trealla) for experimenting with WebAssembly/WASI.
For more info on Trealla, check out the parent repository.

We endeavor to keep this fork as close as possible to the upstream and contribute all stable changes upstream.
Ideally, when WASM support is better stablized, this fork won't need to exist.

## Binaries on WAPM
You can grab WASM binary builds from [guregu/trealla on WAPM](https://wapm.io/guregu/trealla).
These builds are uploaded automatically for each release.

## Differences from upstream
- `library(wasm)` JSON-based programmatic toplevel.
- `library(wasm_*)` host-guest interop.
- `library(pseudojson)` Very fast JSON parser/generator (but not validator).
- WASM system predicates: `'$host_call'/2` and `'$host_resume'/1` (see `js_eval/2` in `library/wasm_js.pl`).

## Compile targets

There are two compile targets for WASM builds.

### wasm

`make wasm` will build a pure WASI version of Trealla: `tpl.wasm`. This binary can be executed by any runtime that supports WASI. For example, you can use it [with Spin to serve webpages](https://github.com/guregu/php/blob/519031a86a02b812962c264dd7037b82dc77e02d/spin.toml#L10). Currently the Go port of Trealla uses this.

### libtpl

`make libtpl` will build a WASM binary with host calls enabled: `libtpl.wasm`. This adds host-guest interop exports and imports that break pure WASI compatibility. This is currently used by trealla-js. When WASI libraries for browsers improve, we can get rid of this and use standard WASI APIs instead.

## See also

- [Trealla Prolog](https://github.com/trealla-prolog/trealla): parent repository
- [trealla-js](https://github.com/guregu/trealla-js): Trealla for Javascript
- [trealla-prolog/go](https://github.com/trealla-prolog/go): Trealla for Go

![Trealla Logo](https://user-images.githubusercontent.com/131059/190109875-7eb65bf5-feef-41e1-b19c-7fbcab8887ae.png)
