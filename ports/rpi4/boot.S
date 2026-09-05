// Raspberry Pi 4 (BCM2711, Cortex-A72) startup for a freestanding Trealla
// image. Firmware enters kernel8.img at 0x80000 with the MMU off and all four
// cores running, so this parks the secondaries, drops to EL1, gives the
// primary core a stack and zeroed BSS, and turns the MMU on before any C
// library code runs. Keeping startup here is part of the porting boundary:
// nothing under src/ knows about the board.

.section .text.boot, "ax"
.globl _start

_start:
	// Cores 1-3 are parked. Real firmware holds them in its own spin table
	// and QEMU's board stub does the same, but neither is guaranteed.
	mrs	x1, mpidr_el1
	and	x1, x1, #0xff
	cbz	x1, primary_core

park_secondary:
	wfe
	b	park_secondary

primary_core:
	mrs	x0, CurrentEL
	lsr	x0, x0, #2
	cmp	x0, #3
	b.eq	from_el3
	cmp	x0, #2
	b.eq	from_el2
	b	at_el1

	// Non-secure EL2 in AArch64, with HVC enabled and the RES1 bits set.
from_el3:
	mov	x0, #0x531
	msr	scr_el3, x0
	mov	x0, #0x3c9			// EL2h, DAIF masked
	msr	spsr_el3, x0
	adr	x0, from_el2
	msr	elr_el3, x0
	eret

from_el2:
	// Let EL1 read the generic counters, and leave virtual time unskewed.
	mrs	x0, cnthctl_el2
	orr	x0, x0, #3
	msr	cnthctl_el2, x0
	msr	cntvoff_el2, xzr

	mov	x0, #0x80000000			// HCR_EL2.RW: EL1 is AArch64
	msr	hcr_el2, x0

	// SCTLR_EL1 with the MMU, caches and alignment checking off, and the
	// architecturally reserved bits set.
	mov	x0, #0x0800
	movk	x0, #0x30d0, lsl #16
	msr	sctlr_el1, x0

	mov	x0, #0x3c5			// EL1h, DAIF masked
	msr	spsr_el2, x0
	adr	x0, at_el1
	msr	elr_el2, x0
	eret

at_el1:
	// Vectors first, so that a fault from here on says so instead of
	// jumping into whatever VBAR_EL1 happened to contain at reset.
	adrp	x0, vectors
	add	x0, x0, :lo12:vectors
	msr	vbar_el1, x0
	isb

	// Prolog arithmetic is full of doubles, so FP/SIMD has to be enabled
	// before any engine code runs or the first float traps.
	mov	x0, #(3 << 20)			// CPACR_EL1.FPEN
	msr	cpacr_el1, x0
	isb

	adrp	x0, __stack_top
	add	x0, x0, :lo12:__stack_top
	mov	sp, x0

	// Single-register stores: with the MMU still off this is Device
	// memory, where the pair variants are best left alone.
	adrp	x0, __bss_start
	add	x0, x0, :lo12:__bss_start
	adrp	x1, __bss_end
	add	x1, x1, :lo12:__bss_end

zero_bss:
	cmp	x0, x1
	b.hs	enter_c
	str	xzr, [x0], #8
	b	zero_bss

enter_c:
	bl	rpi4_mmu_init

	// Devices, once memory behaves. Does nothing unless the image was
	// built with something to bring up.
	bl	rpi4_board_init

	bl	main

	// main() returns only when the embedding sample gives up early.
	mov	w0, #1
	bl	tpl_platform_halt

hang:
	wfe
	b	hang

// The AArch64 vector table: sixteen entries of 128 bytes, four groups of
// four. Every one lands in the same place - this port has no interrupts to
// service, so anything arriving here is a fault to be reported, not handled.

.align 11
vectors:
	.set entry_num, 0
.rept 16
	.align 7
	mov	x3, #entry_num
	b	fault_entry
	.set entry_num, entry_num + 1
.endr

fault_entry:
	mrs	x0, esr_el1
	mrs	x1, far_el1
	mrs	x2, elr_el1
	bl	rpi4_fault

hang_fault:
	wfe
	b	hang_fault
