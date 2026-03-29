#include "baremetal/memory_baremetal.h"

#include <stdlib.h>

void *pl4bm_malloc_baremetal(size_t size) {
#error "Unimplemented"
  return NULL;
}

void *pl4bm_calloc_baremetal(size_t nmemb, size_t size) { return NULL; }

void pl4bm_free_baremetal(void *ptr) {}

void *pl4bm_realloc_baremetal(void *ptr, size_t new_size) { return NULL; }
