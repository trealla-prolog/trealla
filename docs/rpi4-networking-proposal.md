# Proposal: networking for the Raspberry Pi 4 freestanding port

*Status: proposal. Nothing here is implemented.*

Give the bare-metal Pi 4 image an Ethernet driver, a small IPv4/UDP stack, and
enough of a socket surface that TFTP can be written in Prolog rather than C.

The point is a network stack driven from Prolog on the bare metal. It is not
the cheap way to get images onto the board - the firmware already does TFTP
netboot with no code at all, and `docs/` covers serial chain-loading. Anyone
reaching for this to speed up iteration should use one of those instead.

## Shape

Three layers, deliberately handled three different ways:

| Layer | Approach | Why |
| --- | --- | --- |
| GENET driver | **Crib** an existing BSD-licensed driver | Undocumented hardware; all the risk lives here |
| ARP/IPv4/ICMP/UDP | **Hand-roll**, ~600 lines | Textbook, and smaller than integrating a stack |
| TFTP | **Prolog**, over new builtins | The reason for doing any of this |

The asymmetry is the proposal. Cribbing the driver removes the part that is
genuinely hard to get right without documentation; hand-rolling the stack
avoids vendoring forty thousand lines to use five percent of them.

## Layer 1: the GENET driver

The Pi 4's Ethernet is a Broadcom GENET v5 MAC at `0xFD580000` - memory-mapped
with DMA rings, not the USB-attached part the Pi 3 had. That is the single
biggest thing in this proposal's favour: it is an ordinary DMA device, and no
USB stack is needed.

There is **no public documentation for GENET on the BCM2711**. It is not in the
peripherals datasheet. The driver gets written by reading other people's.

**Crib from:** FreeBSD's `sys/arm64/broadcom/genet/if_genet.c`, a working Pi 4
driver under a BSD licence - permissive enough to adapt into this MIT tree with
attribution. Verify the exact terms in the file header before copying anything.

**Read but do not copy** (all GPL): u-boot's `drivers/net/bcmgenet.c` has the
clearest minimal structure - polled, one TX ring, one RX ring, which is exactly
the shape wanted here; Circle (`rsta2/circle`) proves a GENET driver works in a
bare-metal Pi environment; Linux's `bcmgenet.c` is the authority on the magic
constants and unreadable for anything else.

Two landmines others have already hit and documented on the Raspberry Pi
forums:

- **Link-change interrupts do not work.** Link state has to be found by polling
  the PHY registers. No loss here - this port takes no interrupts anyway.
- **The Pi 4 requires the smaller DMA max burst size, `0x8`.** The kind of
  undocumented constant that costs days.

The MAC address lives in OTP and is normally read over the VideoCore mailbox.
Defer that: hardcode a locally-administered address for bring-up and add the
mailbox later.

### The main risk: DMA cache coherency

`ports/rpi4/mmu.c` currently maps **all** RAM as Normal write-back,
inner-shareable, cacheable. GENET's DMA is not coherent with the ARM caches, so
descriptor rings and packet buffers must either live in non-cacheable memory or
have explicit `DC CIVAC`/`DC IVAC` maintenance around every packet.

The proposed change is the former: add a Normal Non-Cacheable attribute to
`MAIR_EL1`, carve a region out of the level-2 table, and allocate rings and
buffers from it. Perhaps forty lines on top of what is there.

This is called out first because its failure mode is the worst kind. It does
not fail cleanly - it *mostly works*, until a descriptor write is still sitting
in a dirty cache line or a receive returns the previous packet's contents.
Anyone debugging intermittent packet loss here should suspect the mapping
before the driver.

## Layer 2: the stack

Scope, and nothing beyond it:

- Ethernet II framing
- ARP, with a small cache and ageing
- IPv4, no fragmentation - 512-byte TFTP blocks make ~558-byte frames, well
  under MTU, so it never arises
- ICMP echo, for the sake of being able to ping the board
- UDP; checksums are optional over IPv4 and can be zero until everything works

No TCP, no DHCP, no DNS, no routing beyond "same subnet, or send it to the
gateway". Static address configuration to begin with.

Around 600 lines. One thing that does *not* need fighting: the IP header lands
at offset 14 in the frame, so its 32-bit fields are unaligned - and the port
already runs with `SCTLR_EL1.A` clear over Normal memory, so unaligned loads
work.

### Why not lwIP

lwIP is BSD-licensed, `NO_SYS=1` is built for exactly this kind of bare-metal
main loop, and its `sys_now()` hook maps straight onto
`tpl_platform_monotonic_usec()`. It is a good stack.

It is still the wrong choice here. The integration surface - a `lwipopts.h`
with hundreds of knobs, `pbuf` pools wanting to be wired to `pl_set_allocator`
or a static arena, TCP and DHCP and DNS to be configured off - is likely more
work than writing the five headers this needs, and it puts forty thousand lines
in the tree to use a small fraction of them.

The exception is TCP. If TCP is ever wanted, adopt lwIP at that point rather
than growing a hand-rolled stack into one; those are different projects.

Either way, the structure and constants are worth reading from lwIP and uIP -
checksum routines, ARP cache ageing, header layouts - while writing our own.

### Where it should live

The driver is Pi 4-specific and belongs under `ports/rpi4/`. The stack above it
is not: ARP and IPv4 know nothing about a board. It could reasonably live
somewhere shared from the start, with only the driver behind a port-specific
interface - the same split that keeps `src/bif_gpio_linux.c` out of
`ports/rpi4/`. Worth deciding before the first line rather than after.

## Layer 3: TFTP in Prolog

RFC 1350 is a toy protocol and that is the appeal: five packet types (RRQ, WRQ,
DATA, ACK, ERROR), 512-byte blocks, lockstep acknowledgement, and a short block
means end of file. Lockstep request/response with timeouts and retransmission
is comfortable ground for Prolog, and `delay_ms/1` already exists for the
retransmit timer.

Three new builtins, in the port's `g_port_bifs` table:

```
udp_open(+Port, -Socket)
udp_send(+Socket, +Host, +Port, +Data)
udp_recv(+Socket, -Host, -Port, -Data, +TimeoutMs)
```

Payloads should be strings, not lists of codes: a 512-byte block as a code list
is 512 cells for no benefit.

**Build trap:** these go in the port table, *not* `src/network.c`. A
freestanding build sets `NONETWORK=1` and links `network_none.c`, so anything
added to `network.c` needs a matching stub there or the freestanding build
breaks.

## Milestones

Each is independently verifiable, which matters because the feedback loop is
poor.

0. Debug loop first: second machine, direct cable or dumb switch, Wireshark
   running. Do not start without this.
1. Driver brings the link up; PHY status polled and reported over the UART.
2. **Transmit only.** A hardcoded broadcast ARP, in a loop, visible on the
   other machine. This one frame proves the cache attributes, descriptor
   layout, DMA and MAC together, and nothing before it proves anything.
3. Receive: ARP request answered.
4. ICMP echo - the board answers `ping`.
5. UDP echo, in C.
6. The three UDP builtins, exercised from Prolog.
7. TFTP client in Prolog, fetching a file from the other machine.

## Risks

| Risk | Severity | Mitigation |
| --- | --- | --- |
| DMA cache coherency | High - fails intermittently, not cleanly | Non-cacheable region up front; suspect it first |
| PHY bring-up | High - most of the schedule variance | FreeBSD driver as reference |
| No GENET documentation | Medium | Three independent drivers to read |
| Licence terms on the cribbed driver | Low, but blocking | Check the file header before copying |
| **No QEMU coverage** | Medium | See below |

### This work leaves the tested envelope

Everything in the Pi 4 port so far has been verifiable under QEMU's `raspi4b`
machine, and CI boots it on every push. **QEMU does not emulate GENET** - it is
explicitly listed among that machine's missing devices, along with PWM and the
PCIe root port.

So none of this can be smoke-tested the way the rest of the port is. It cannot
be exercised in CI at all, and it cannot be developed without the board on the
desk. That is a real change in the character of the work, and it is the reason
the milestones above are structured so tightly around what can be seen on
another machine's Wireshark.

## Vendoring hygiene

Follow the `src/imath/` pattern: its own directory, its own `LICENSE`,
attribution recorded, objects added to the freestanding object list. Check
whether anything vendored needs a `network_none.c`-style stub so that a
freestanding build without networking still links.

## Effort

Two to three weekends, with nearly all the variance in PHY bring-up and cache
attributes. First ARP frame on the wire within an evening or two if the PHY
cooperates, several more if it does not. The stack is one or two evenings and
mechanical. The Prolog TFTP client is an evening, and is the fun part.

## Decisions wanted before starting

1. Adapt the FreeBSD driver in place, or vendor it under its own directory?
2. Does the IPv4/UDP stack live under `ports/rpi4/`, or somewhere shared from
   the outset?
3. Static IP configuration only, or DHCP in scope? (DHCP is perhaps another
   150 lines and can wait.)
4. Is leaving CI coverage behind acceptable for this subsystem?
