#pragma once

#include <stddef.h>

void *pl4bm_malloc_baremetal(size_t size);

void *pl4bm_calloc_baremetal(size_t nmemb, size_t size);

void *pl4bm_realloc_baremetal(void *ptr, size_t new_size);

void pl4bm_free_baremetal(void *ptr);

#define PL4BM_MALLOC_IMPL(size) pl4bm_malloc_baremetal((size))

#define PL4BM_CALLOC_IMPL(nmemb, size) pl4bm_calloc_baremetal((nmemb), (size))

#define PL4BM_REALLOC_IMPL(ptr, size) pl4bm_realloc_baremetal((ptr), (size))

#define PL4BM_FREE_IMPL(ptr) pl4bm_free_baremetal((ptr))
