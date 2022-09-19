#ifdef __wasi__
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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

// Extras for the WASM shim libraries to use

__attribute__((weak, export_name("memset")))
void _memset(void *dest, int ch, size_t count) {
  memset(dest, ch, count);
}

__attribute__((export_name("test_print")))
void test_print(char* str) {
  fprintf(stdout, "%s", str);
  fflush(stdout);
}
#endif