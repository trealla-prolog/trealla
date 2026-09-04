#include "prolog.h"
#include "query.h"

// Stand-in for src/bif_ffi.c in a build without USE_FFI, the way
// bif_net_none.c and bif_os_none.c stand in for theirs.
//
// The real file reserves g_ffi_bifs[MAX_FFI] for predicates registered at
// run time by use_foreign_module/2. Each builtins entry is 680 bytes -
// mostly the types[MAX_FFI_ARGS] and names[MAX_FFI_ARGS] arrays that only
// FFI uses - so those 1000 slots cost 680,000 bytes of BSS. In a build with
// no FFI nothing can ever register, and on the Raspberry Pi 4 freestanding
// image that table was 97% of all the BSS the engine asked the board for.
//
// Everything else bif_ffi.c defines - do_dlopen, do_register_predicate and
// the rest - is declared and called only under #if USE_FFI, so an empty
// table is the whole of what a non-FFI build needs.

builtins g_ffi_bifs[] =
{
	{0}
};
