#pragma once

#include <stddef.h>

void *pl4bm_malloc_picolibc(size_t size);

void *pl4bm_calloc_picolibc(size_t nmemb, size_t size);

void *pl4bm_realloc_picolibc(void *ptr, size_t new_size);

void pl4bm_free_picolibc(void *ptr);

#define PL4BM_MALLOC_IMPL(size) pl4bm_malloc_picolibc((size))

#define PL4BM_CALLOC_IMPL(nmemb, size) pl4bm_calloc_picolibc((nmemb), (size))

#define PL4BM_REALLOC_IMPL(ptr, size) pl4bm_realloc_picolibc((ptr), (size))

#define PL4BM_FREE_IMPL(ptr) pl4bm_free_picolibc((ptr))
