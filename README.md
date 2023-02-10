# Trealla Prolog

This is a fork of [Trealla Prolog](https://github.com/trealla-prolog/trealla) for experimenting with WebAssembly/WASI.
For more info on Trealla, check out the parent repository.

We endeavor to keep this fork as close as possible to the upstream and contribute all stable changes upstream.
Ideally, when WASM support is better stablized, this fork won't need to exist.

## Download

You can find binaries on [the Releases page](https://github.com/guregu/trealla/releases).

## Binaries on WAPM
~~You can grab WASM binary builds from [guregu/trealla on WAPM](https://wapm.io/guregu/trealla).~~
~~These builds are uploaded automatically for each release.~~

**Note**: currently WAPM builds are [temporarily disabled](https://github.com/guregu/trealla/issues/11).

## Differences from upstream
- `library(wasm)` JSON-based programmatic toplevel.
- `library(wasm_*)` host-guest interop.
- `library(pseudojson)` Very fast JSON parser/generator (but not validator).
- WASM system predicates: `'$host_call'/2` and `'$host_resume'/1` (see `js_eval/2` in `library/wasm_js.pl`).

## Compile targets

There's a bunch of new compile targets for WASM.

### wasm

`make wasm` will build a pure WASI version of Trealla: `tpl.wasm`. This binary can be executed by any runtime that supports WASI. (Mostly) upstreamed.

### libtpl

`make libtpl` will build a WASM binary with host calls enabled: `libtpl.wasm`. This adds host-guest interop exports and imports that break pure WASI compatibility. This is currently used by the Go port.

### libtpl-js

`make libtpl-js` builds the trealla-js version of libtpl. Includes [JS-specific predicates](https://github.com/guregu/trealla-js#predicate-reference).

### libtpl-spin

`make libtpl-spin` builds the [Spin](https://github.com/fermyon/spin)-flavored libtpl.

`make SPINDIR=path/to/spin/source wit` to generate the WASM component code via wit-bindgen.

#### Spin Components

See `library/spin.pl`.

- [x] Inbound HTTP (via `http_handler/4` multifile predicate)
- [x] Outbound HTTP (via `http_fetch/3`)
- [ ] Outbound PostgreSQL
- [ ] Inbound Redis
- [ ] Outbound Redis

## See also

- [Trealla Prolog](https://github.com/trealla-prolog/trealla): parent repository
- [trealla-js](https://github.com/guregu/trealla-js): Trealla for Javascript
- [trealla-prolog/go](https://github.com/trealla-prolog/go): Trealla for Go

![Trealla Logo](https://user-images.githubusercontent.com/131059/190109875-7eb65bf5-feef-41e1-b19c-7fbcab8887ae.png)
