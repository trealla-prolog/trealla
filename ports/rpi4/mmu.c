#include <stdint.h>

// Identity-mapped page tables for the BCM2711. This is not an optimisation:
// with the MMU off every access is Device-nGnRnE, where unaligned accesses
// fault whatever SCTLR_EL1.A says and nothing is cached. The engine needs
// ordinary Normal write-back memory to be either correct or quick.

// Low-peripheral mode: RAM below 0xfc000000, ARM and VideoCore peripherals
// above it. Mapping 8 GiB covers the largest board; entries beyond a given
// board's RAM are simply never touched.
#define RPI4_DEVICE_BASE 0xfc000000ull
#define RPI4_MAPPED_BYTES (8ull << 30)

#define DESC_BLOCK 0x1ull
#define DESC_TABLE 0x3ull
#define DESC_ATTR(index) ((uint64_t)(index) << 2)
#define DESC_AF (1ull << 10)
#define DESC_INNER_SHAREABLE (3ull << 8)
#define DESC_NO_EXECUTE ((1ull << 53) | (1ull << 54))

#define ATTR_DEVICE 0
#define ATTR_NORMAL 1

#define L1_BLOCK_BYTES (1ull << 30)
#define L2_BLOCK_BYTES (1ull << 21)
#define TABLE_ENTRIES 512

// Level 1 maps 512 GiB in 1 GiB blocks. The fourth gigabyte is split into
// 2 MiB blocks because RAM and the peripheral window share it.
static volatile uint64_t level1[TABLE_ENTRIES] __attribute__((aligned(4096)));
static volatile uint64_t level2_gb3[TABLE_ENTRIES] __attribute__((aligned(4096)));

static uint64_t normal_block(uint64_t pa)
{
	return pa | DESC_BLOCK | DESC_AF | DESC_INNER_SHAREABLE
		| DESC_ATTR(ATTR_NORMAL);
}

static uint64_t device_block(uint64_t pa)
{
	return pa | DESC_BLOCK | DESC_AF | DESC_ATTR(ATTR_DEVICE)
		| DESC_NO_EXECUTE;
}

// The tables are volatile so this stays a plain store loop. Left to itself the
// compiler would reach for memset, and newlib's aarch64 memset uses DC ZVA,
// which faults on the Device memory we are still running in.

void rpi4_mmu_init(void)
{
	const uint64_t gb3 = 3 * L1_BLOCK_BYTES;

	for (unsigned i = 0; i < TABLE_ENTRIES; i++) {
		uint64_t pa = (uint64_t)i * L1_BLOCK_BYTES;

		if (pa == gb3)
			continue;

		level1[i] = pa < RPI4_MAPPED_BYTES ? normal_block(pa) : 0;
	}

	for (unsigned i = 0; i < TABLE_ENTRIES; i++) {
		uint64_t pa = gb3 + (uint64_t)i * L2_BLOCK_BYTES;

		level2_gb3[i] = pa >= RPI4_DEVICE_BASE
			? device_block(pa) : normal_block(pa);
	}

	level1[3] = (uint64_t)(uintptr_t)level2_gb3 | DESC_TABLE;

	// attr0 Device-nGnRnE, attr1 Normal write-back read/write allocate.
	uint64_t mair = (0x00ull << (8 * ATTR_DEVICE))
		| (0xffull << (8 * ATTR_NORMAL));

	uint64_t tcr = (25ull << 0)		// T0SZ: 39-bit address space
		| (1ull << 8) | (1ull << 10)	// walk write-back cacheable
		| (3ull << 12)			// walk inner shareable
		| (0ull << 14)			// 4 KiB granule
		| (1ull << 23)			// no TTBR1 walks
		| (2ull << 32);			// 40-bit physical addresses

	__asm__ volatile("msr mair_el1, %0" :: "r"(mair));
	__asm__ volatile("msr tcr_el1, %0" :: "r"(tcr));
	__asm__ volatile("msr ttbr0_el1, %0"
		:: "r"((uint64_t)(uintptr_t)level1));
	__asm__ volatile("dsb ish; tlbi vmalle1; dsb ish; ic iallu; isb"
		::: "memory");

	uint64_t sctlr;
	__asm__ volatile("mrs %0, sctlr_el1" : "=r"(sctlr));
	sctlr |= (1ull << 0) | (1ull << 2) | (1ull << 12);	// M, C, I
	sctlr &= ~(1ull << 1);					// A: allow unaligned
	__asm__ volatile("msr sctlr_el1, %0; isb" :: "r"(sctlr) : "memory");
}
