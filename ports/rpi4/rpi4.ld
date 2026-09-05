/* The Pi 4 firmware loads kernel8.img at 0x80000 with the MMU off; everything
 * below that belongs to the firmware and the device tree. The image, its stack
 * and the Trealla heap therefore all sit above it, and the heap stops well
 * short of the smallest board's usable RAM. Startup lives out here in the port
 * rather than in the engine. */

ENTRY(_start)

__stack_size = 8M;
/* Stops short of 0x20000000: the last 2MiB is the non-cacheable DMA window
 * mmu.c maps for devices. Keep this in step with RPI4_DMA_BASE. */
__heap_end = 0x1fe00000;

SECTIONS
{
	. = 0x80000;

	.text : {
		KEEP(*(.text.boot))
		*(.text .text.*)
	}

	.rodata : { *(.rodata .rodata.*) }

	.data : {
		*(.data .data.*)
		. = ALIGN(8);
		__init_array_start = .;
		KEEP(*(.init_array .init_array.*))
		__init_array_end = .;
	}

	. = ALIGN(16);
	__bss_start = .;
	.bss (NOLOAD) : {
		*(.bss .bss.*)
		*(COMMON)
	}
	. = ALIGN(16);
	__bss_end = .;

	/* The stack grows down from __stack_top towards BSS and the heap grows
	 * up from the same address, so the two never meet. There is no guard
	 * page: a stack overflow runs into BSS, which holds the page tables. */
	. = ALIGN(4096);
	__stack_bottom = .;
	. += __stack_size;
	__stack_top = .;
	__heap_start = .;

	/DISCARD/ : { *(.comment) *(.note.*) }
}
