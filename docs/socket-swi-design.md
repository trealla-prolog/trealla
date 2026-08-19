# SWI-compatible `library(socket)` for Trealla — design

Design only. No source is modified by this document.

Goal: a new `library/socket.pl` presenting SWI-Prolog's socket interface
(<https://www.swi-prolog.org/pldoc/man?section=socket>), built on Trealla's existing
builtins wherever possible. `library/sockets.pl` — Trealla's current, Scryer-shaped
socket library — stays as it is and serves as the guide for how the builtins behave.

**Source basis.** Read: `library/sockets.pl`, `src/bif_net.c` (all 9 net bifs),
`src/network.c` (`tpl_server`, `tpl_connect`), `src/bif_streams.c` (`set_stream/2`,
`stream_property/2`), and the SWI socket manual section. Facts below marked
**[checked]** were verified in the source during this pass.

---

## 1. The central mismatch

This is the whole design problem, and everything else follows from it.

**SWI is socket-first.** A socket is an opaque handle created empty, then
configured, bound, listened, connected, and only *later* turned into streams:

```prolog
tcp_socket(S), tcp_setopt(S, reuseaddr), tcp_bind(S, 8080),
tcp_listen(S, 5), tcp_accept(S, Slave, Peer), tcp_open_socket(Slave, Pair)
```

**Trealla is stream-first.** There is no socket object at all. The bif fuses
socket+bind+listen and hands back a *stream*: **[checked]**

| Trealla bif | does |
|---|---|
| `'$server'(+Addr, --Stream, +Opts)` | socket + bind + listen, in one call |
| `'$accept'(+Stream, --Stream)` | accept, yields a connected stream |
| `'$client'(+Addr, -Atom, -Atom, --Stream, +Opts)` | socket + connect, yields a stream |
| `'$peer_addr'(+Stream, -Atom, -Integer)` | peer host and port |
| `'$current_host'(-Atom)` | hostname |
| `'$server_tls'/2`, `'$client_tls'/4` | TLS upgrade |

There is **no** bif that creates an unbound, unconnected socket, and none that
binds without listening. So `tcp_socket/1` cannot map onto anything directly, and
the SWI call sequence cannot be executed in the order SWI executes it.

---

## 2. The handle: deferred materialisation

The only way to present SWI's shape on these builtins is to make a Socket a
**Prolog-side handle that accumulates intent and materialises late**.

```prolog
% Opaque to callers. N from gensym.
'$socket'(N)
```

State in a dynamic predicate keyed by `N`, holding domain, type, pending options,
pending address, and a phase:

| phase | meaning | holds |
|---|---|---|
| `fresh` | created, nothing done | domain, type, options |
| `listening(Stream)` | materialised by `tcp_bind/2` via `'$server'` | listening stream |
| `connected(Stream)` | materialised by `tcp_connect` or produced by `tcp_accept` | connected stream |
| `closed` | after `tcp_close_socket/1` | — |

Materialisation points:

- `tcp_bind(S, Addr)` calls `'$server'(Addr, Stream, Opts)` — socket, bind and
  listen all happen here.
- `tcp_listen(S, _Backlog)` validates the phase and returns. **No syscall.**
- `tcp_connect(S, Addr)` calls `'$client'(Addr, _, _, Stream, Opts)`.
- `tcp_accept(S, Slave, Peer)` calls `'$accept'`, then `'$peer_addr'`, and wraps the
  resulting stream in a *new* handle already in `connected/1`.
- `tcp_open_socket(S, Pair)` just returns the stream the handle already holds.

**Revised at phase 3.** This section originally deferred materialisation to
`tcp_listen/2` and accepted that bind errors would surface late. That is now
reversed, because `'$server'` was confirmed to **report an ephemeral port back**
through an unbound argument — and a variable stored by `assertz` loses its
binding, so the port can only reach the caller if the bind happens inside
`tcp_bind/2` itself. Binding eagerly is therefore forced by the ephemeral-port
case, and it happens to also put bind errors exactly where SWI puts them:

```prolog
?- tcp_socket(S), tcp_bind(S, '127.0.0.1':P).
P = 65422.

?- tcp_socket(S), tcp_bind(S, '127.0.0.1':3599).      % already in use
ERROR: socket_error(eaddrinuse, 'address already in use') in tcp_bind/2
```

**The consequence to be honest about** is now the mirror image, and much
smaller: a socket that is bound but never listened is nonetheless listening, so
connections to it are accepted into the backlog where SWI would refuse them. No
realistic program can observe this, since `tcp_listen/2` normally follows
immediately. `BackLog` itself is ignored either way — `tpl_server` hardcodes
`SOMAXCONN`.

A second consequence: `tcp_accept/3` in SWI returns a *socket* that the caller then
opens. Here the stream already exists before the handle does, so the handle is born
materialised. That direction is harmless — it just means `tcp_open_socket/2` on an
accepted socket is a lookup, not a syscall.

**Concurrency.** Trealla has threads. The handle table must not be a plain
`assertz`-per-socket global if two threads can create sockets concurrently. Options:
`bb_put/bb_get` (the blackboard, which is documented as handling this) or a
thread-keyed dynamic predicate. **Open question — see §9.**

---

## 3. Predicate map

### Implementable on existing builtins

| SWI predicate | via | notes |
|---|---|---|
| `tcp_socket/1` | handle only | no syscall until bind+listen or connect |
| `unix_domain_socket/1` | handle with `domain(unix)` | `'$server'`/`'$client'` accept `unix://Path` **[checked]** |
| `udp_socket/1` | handle with `type(dgram)` | creatable, but see §4 — the UDP *operations* are missing |
| `socket_create/2` | handle | only the `domain`/`type` combinations the bifs support |
| `tcp_bind/2` | `'$server'` | binds *and* listens; reports an unbound port back |
| `tcp_listen/2` | — | phase check only; **Backlog ignored** — `tpl_server` hardcodes it |
| `tcp_accept/3` | `'$accept'` + `'$peer_addr'` | Peer as `ip(A,B,C,D):Port` after conversion |
| `tcp_connect/2` | `'$client'` | |
| `tcp_connect/3` (Address, StreamPair, Options) | `'$client'` | the modern form; most-used in practice |
| `tcp_connect/4` (deprecated) | `'$client'` | returns the same stream twice, see §6 |
| `tcp_open_socket/2,3` | lookup | see §6 |
| `tcp_close_socket/1` | `close/1` | plus handle teardown; must be idempotent-safe on `fresh` |
| `tcp_setopt(S, nodelay)` | `'$client'`/`'$server'` option | must be set *before* materialisation |
| `tcp_setopt(S, reuseaddr)` | **already always on** | `SO_REUSEADDR` is set unconditionally in `network.c` **[checked]** — accept and ignore |
| `tcp_getopt(S, file_no(F))` | `stream_property(Stream, file_no(F))` | **[checked]** exists; only once materialised |
| `gethostname/1` | `'$current_host'/1` | |
| `ip_name/2` | pure Prolog | text ↔ `ip/4`, `ip/8` |
| `tcp_host_to_address/2` | partial | see §4 |

### Pure Prolog, no builtin needed

- Address term conversion: `ip(A,B,C,D)`, `ip(A,...,H)`, `Host:Port`, unix path atom
  ↔ the `Host:Port` atom form the bifs take.
- `negotiate_socks_connection/2` — SOCKS5 is a wire protocol spoken over an
  already-open stream. Sizeable but entirely doable in Prolog. Low priority.
- `proxy_for_url/3`, `try_proxy/4`, `rewrite_host/3` — multifile hooks; declare them
  and honour them in `tcp_connect/3`.

---

## 4. What cannot be done without new C

Listing these explicitly so the gap is visible rather than discovered later.

**UDP is the big one.** `udp_socket/1` can create a handle, but:

- `udp_receive(+Socket, -Data, -From, +Options)` needs `recvfrom()`
- `udp_send(+Socket, +Data, +To, +Options)` needs `sendto()`

Neither is exposed, and **there is no `recvfrom`/`sendto` anywhere in
`src/network.c`** **[checked]**. The `udp(true)` option on `'$server'`/`'$client'`
only sets `SOCK_DGRAM` on an otherwise connected socket, which is not the same
thing — it gives you a datagram socket you can read and write, but with no way to
learn who sent a datagram or to address one to a specific peer. That is the entire
point of the UDP API.

*Needed:* two bifs, `'$udp_recv'(+Stream, -Data, -Host, -Port, +Opts)` and
`'$udp_send'(+Stream, +Data, +Host, +Port, +Opts)`. **Both now exist** — added in
`network.c`/`bif_net.c`, loopback-verified in both directions.

**Phase 5 correction.** The phasing table called UDP "Prolog-side only" now that
the bifs exist. That was wrong about `encoding`: the text path is UTF-8, so
sending byte 255 puts *two* bytes on the wire and no arrangement of Prolog can
prevent it. `encoding(octet)` was therefore added to both bifs — the datagram
becomes a list of raw byte values in each direction. UDP is mostly binary
protocols, so this is not an edge case. Verified byte-exact for `[0,255,128,7]`,
which the text path mangles.

Both bifs also used to `return false` on a failed syscall. They now throw
`socket_error/2` off errno like the rest of §5.

**`as(term)` interns permanently.** Reading a term interns the functor and atom
names it contains, and *that* grows the symbol table where ordinary atom
construction does not — measured at 950 new symbols for 1000 distinct
datagrams, against a control of 1000 distinct atoms built with
`format(atom(...))` which grew it by 0. So `as(term)` on an untrusted peer is a
slow leak. SWI has the same property; it is documented in the module header
rather than prevented.

The receive path otherwise builds no atom unless `as(atom)` was asked for.
`read_term_from_atom/3` accepts a string — though *not* a bare char list, which
is a distinction worth knowing — so the term case needs no intermediate atom,
and `number_codes/2` plus `format(string(S), ...)` cover the send side.

**Socket options with no plumbing:** `broadcast`, `bindtodevice/1`, `sndbuf/1`,
`ip_add_membership/1,2,3`, `ip_drop_membership/1,2,3`. All need `setsockopt` calls
that do not exist. `dispatch/1` is GUI-related and should simply be accepted and
ignored.

**`tcp_fcntl(+Stream, setfl, nonblock)`** — no `O_NONBLOCK` handling **[checked]**.
`set_stream(S, timeout(T))` exists and may cover the common use, but it is a
*timeout*, not non-blocking mode, and conflating them would be wrong. Needs a
decision (§9).

**`tcp_select/3`** — deprecated in SWI in favour of `wait_for_input/3`, and Trealla
has **neither** **[checked]**. Leave unimplemented; document the omission.

**`host_address/3`** (the modern resolver) returns a *dict*. Trealla has no dicts.
Provide `tcp_host_to_address/2` only, and even that needs a resolver bif:
`'$client'`'s two output atoms are populated from URL parsing, not DNS
**[checked]**, so there is no way to resolve a name without opening a connection.

*Needed:* `'$host_address'(+Host, -IpAtom)` wrapping `getaddrinfo`. **Now exists.**

**Unix domain sockets were not actually implemented** — found at phase 4.
`parse_host` recognised the `unix://` prefix and set a `domain` flag that
**nothing ever read**, and `tpl_domain_server`/`tpl_domain_connect` were fully
written but had *zero callers*. `library(sockets)`'s documented `unix(Path)`
support therefore opened an ordinary TCP socket on the wildcard address, and
because that works, the substitution was invisible: data flowed, round-trips
passed, and no socket file was ever created.

Two things needed fixing:

- the `domain` flag is now read, dispatching to `tpl_domain_*`;
- `parse_host` takes the remainder of a `unix://` URL verbatim. The general
  path would split it at the first slash and then strip the leading one — a
  rule that suits `http://host/path` but silently turned `/tmp/x.sock` into
  the relative `tmp/x.sock`.

The regression test asserts a **socket inode exists at the path**, not merely
that a round-trip succeeds — the round-trip passes on the broken build too.
Note that `exists_file/1` is false for a socket, so the check deletes it
instead.

---

## 5. Errors

SWI raises `error(socket_error(Code, Message), _)` where `Code` is the lowercased
errno macro (`epipe`, `econnrefused`, …).

Trealla's bifs raise ordinary ISO errors and, in places, just fail. Mapping is
therefore *lossy in one direction*: we can wrap what we get, but we cannot
manufacture an errno we were never told.

Implemented as a single `'$sock_call'(Goal, Context)` wrapper that catches what
the bifs throw and re-raises as `socket_error(Code, Message)` with `Context` set
to the *library* predicate the caller invoked, not the bif underneath it.

**Done at phase 3** — the "out of scope, worth doing eventually" note below was
resolved, because the coarse errors made the phase-3 tests unable to distinguish
a real bind conflict from a typo'd hostname. `tpl_server` and `tpl_connect` now
preserve `errno` across their cleanup paths (`freeaddrinfo` and `close` are both
free to clobber it, so it is saved at the point of failure), and
`tpl_socket_errname` maps it to the lowercase symbol SWI reports. The platform
`#if` guards stay in `network.c`; `bif_net.c` only calls the helper.

The two blanket errors are gone:

| was | now |
|---|---|
| `existence_error(_, server_failed)` | `socket_error(eaddrinuse, 'address already in use')` |
| `resource_error(could_not_connect)` | `socket_error(econnrefused, 'connection refused')` |

Both previously collapsed every cause into one term — a port conflict and an
unresolvable host were indistinguishable. Neither old term was referenced
anywhere in the tree, so nothing depended on them. `tpl_server` also no longer
`perror`s a failed bind to stderr, since the cause now travels in the exception.

The mapping is still lossy in one direction: bifs that simply *fail* tell us
nothing, and those still yield `socket_error(unknown, 'operation failed')`.

---

## 6. Stream pairs

Trealla has **no `stream_pair/3`** **[checked]**, and its socket streams are already
bidirectional — one stream handles both directions.

Proposal:

- `tcp_open_socket(S, StreamPair)` returns the single bidirectional stream.
- `tcp_open_socket(S, In, Out)` (deprecated form) returns **the same stream twice**.

Most SWI code uses the pair opaquely with `format/3`, `read_term/3`, `close/1`, and
that all works. Code that calls `stream_pair(P, In, Out)` to split it will not, and
that should be stated plainly in the module header. Closing is *simpler* here, not
harder — there is one stream, so the double-close hazard SWI warns about does not
arise.

---

## 7. Addresses

Accept and produce, converting to the `Host:Port` atom the bifs want:

- `ip(A,B,C,D)` — IPv4, fields 0..255
- `ip(A,B,C,D,E,F,G,H)` — IPv6, fields 0..65535. `network.c` uses `AF_UNSPEC` with
  `getaddrinfo` **[checked]**, so IPv6 works at the transport level; only the term
  form needs writing.
- `Host:Port` — pass through
- bare `Port` — as `'$server'` already accepts
- unix path atom — via `unix://Path`, as `library/sockets.pl` does **[checked]**

---

## 8. Testing

The DCG work's lesson applies directly: **a differential oracle beats hand-written
expectations**, and tests must be shown to fail.

1. **Loopback round-trip** — server on an ephemeral port, client connects, bytes
   both ways, both closed. Covers the whole materialisation path.
2. **Behaviour parity with `library/sockets.pl`** — both libraries drive the same
   underlying bifs, so a test that opens the same server two ways and compares
   observable behaviour is cheap and catches handle-layer bugs.
3. **Handle-state errors** — `tcp_listen/2` on a `fresh` socket, `tcp_accept/3` on a
   connected one, double `tcp_close_socket/1`, use-after-close. These are pure
   Prolog logic and are where the bugs will actually be.
4. **Resource leak** — N failed opens must not exhaust `MAX_STREAMS`.
   `library/sockets.pl` carries a comment about exactly this bug **[checked]**: a bad
   option once left the socket open with no reachable handle, and 1024 failed opens
   exhausted the stream table. The new layer has *more* places to leak, because a
   handle can exist without a stream and vice versa.
5. **Threads** — two threads creating sockets concurrently, to exercise §2's table.

Tests go in `tests/misc/`, no `halt/0` (the runner supplies `-g halt`). Anything
needing a real network must be loopback-only.

---

## 9. Open questions

1. ~~**Ephemeral ports.**~~ **Answered: `'$server'` with a var port does bind and
   report the port back.** So `tcp_bind(S, Addr)` with `Addr` unbound can work, and
   the deferred-bind design of §2 is viable.
2. **Handle table and threads.** Blackboard or dynamic predicate? Needs to survive
   concurrent socket creation, and handles must not leak on thread exit.
3. **`tcp_fcntl` non-blocking.** Map onto `set_stream(S, timeout(0))`, or refuse?
   Refusing is more honest; mapping is more compatible. Leaning refuse.
4. **Naming collision.** `library(socket)` and `library(sockets)` differ by one
   character and do the same job differently. Worth a note in both headers.
5. **Scope of UDP.** Ship without UDP and document it, or add the two bifs first?
   Adding them is a modest, self-contained piece of C — arguably better than shipping
   an API where a documented third of it throws.

---

## 9a. Interface binding — fixed

`tpl_server()` used to pass **NULL** as the host to `getaddrinfo`, binding the
wildcard and discarding the hostname, so `tcp_bind(S, '127.0.0.1':Port)` could not
have restricted the socket to loopback. **Fixed**: an explicit host now binds that
interface, while no host still means the wildcard. `tcp_bind/2` can therefore be
implemented faithfully.

The remaining consequence stands: with **no** host and `AF_UNSPEC`, `getaddrinfo`
resolves IPv6 first, so a wildcard server socket is normally **AF_INET6** and peers
arrive as v4-mapped addresses (`::ffff:127.0.0.1`). `library(socket)` must normalise
those to `ip(A,B,C,D)` before handing them to callers expecting SWI's IPv4 term.

---

## 10. Phasing

| Phase | Change | Risk |
|---|---|---|
| 0 | Answer open question 1. It determines the design. | — |
| 1 | Handle representation, state table, address conversion, `socket_error` wrapper. No I/O yet; unit-testable on its own. | Low |
| 2 | TCP client path: `tcp_socket/1`, `tcp_connect/2,3,4`, `tcp_open_socket/2,3`, `tcp_close_socket/1`. Loopback test against `library/sockets.pl`'s server. | Low |
| 3 | **Done.** TCP server path: `tcp_bind/2`, `tcp_listen/2`, `tcp_accept/3`. Full round-trip test. Materialisation moved to bind — see §2. Errno now carried out of `tpl_server`/`tpl_connect` so failures name their cause — see §5. |
| 4 | **Done.** Unix domain sockets - which turned out to need the C wiring above, not just Prolog. `gethostname/1`, `ip_name/2`, `tcp_host_to_address/2` had already landed with phase 1. |
| 5 | **Done.** UDP: `udp_receive/4`, `udp_send/4`, `as(Type)`, `max_message_size`, address normalisation. Not Prolog-side only after all — `encoding(octet)` needed C, see §4. |
| 6 | **Dropped.** SOCKS and proxy hooks — see §11. |

Phases 1–3 are the useful core: they cover what almost all SWI socket code actually
does. Phases 4–5 are completeness and are done. Phase 6 was dropped.

---

## 11. Not doing

- **Dicts.** `host_address/3` returns one; Trealla has none. `tcp_host_to_address/2`
  covers the need.
- **`tcp_select/3`.** Deprecated upstream, and the replacement (`wait_for_input/3`)
  does not exist here either.
- **GUI dispatch.** `tcp_setopt(S, dispatch(_))` accepted and ignored.
- **Replacing `library/sockets.pl`.** It stays. This is an addition, not a migration.
- **SOCKS and HTTP proxies** (was phase 6). The `proxy_for_url/3` and `try_proxy/4`
  hooks would be small — roughly 60–80 lines, no new C, since `multifile` works and
  socket streams do byte-exact I/O via `set_stream(type(binary))` with
  `put_byte`/`get_byte` (verified; note `peek_byte/2` raises `permission_error` on a
  socket, so any protocol parser must work without lookahead). SOCKS5 itself is the
  expensive part: address-type handling in both the request and the reply, eight
  status codes, and — the real cost — a SOCKS5 server written in Prolog to test
  against, because a protocol like this passes a naive round-trip and fails on a
  real proxy. Not attempted. It was also never established whether SWI implements
  SOCKS inside `library(socket)` or in a separate library; that is worth settling
  before anyone picks this up, since it decides whether it belongs in this file.
