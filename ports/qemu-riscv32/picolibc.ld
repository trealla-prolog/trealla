/* QEMU virt starts an RV32 bare-metal image at 0x80000000. Picolibc supplies
 * crt0 and the generic section layout; this file describes the reference
 * machine's 128 MiB RAM as a 16 MiB image region and a 112 MiB data/heap
 * region. Keeping startup here, outside the engine, is part of the porting
 * boundary. */

__flash = 0x80000000;
__flash_size = 0x01000000;
__ram = 0x81000000;
__ram_size = 0x07000000;
__stack_size = 1M;

INCLUDE picolibc.ld
