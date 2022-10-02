#ifdef __wasi__
#include <stdlib.h>

// WASI memory manipulation ABI

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

#ifdef WASI_IMPORTS
// Communication with host
// Guest (Trealla) is responsible for freeing msg and reply.
char *host_call(const char *msg, size_t msg_size, size_t *reply_size);
#endif

#endif