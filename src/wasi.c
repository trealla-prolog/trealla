#ifdef __wasi__
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

// WASI memory manipulation ABI

#ifndef WASI_TARGET_SPIN
__attribute__((weak, export_name("canonical_abi_realloc")))
void *canonical_abi_realloc(void *ptr, size_t orig_size, size_t org_align, size_t new_size) {
  void *ret = realloc(ptr, new_size);
  if (!ret)
  	abort();
  return ret;
}

__attribute__((weak, export_name("canonical_abi_free")))
void canonical_abi_free(void *ptr, size_t size, size_t align) {
  free(ptr);
}
#endif


#ifdef WASI_IMPORTS

// Communication with host.
// Host (WASM runtime) will allocate reply.
// Guest (Trealla) is responsible for freeing msg and reply.
int32_t host_call(int32_t subquery, const char *msg, size_t msg_size, char **reply, size_t *reply_size);
bool host_resume(int32_t subquery, char **reply, size_t *reply_size);

#endif

#endif