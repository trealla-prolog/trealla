#pragma once

#include <stddef.h>

void *pl4bm_malloc_nolibc(size_t size);

void *pl4bm_calloc_nolibc(size_t nmemb, size_t size);

void *pl4bm_realloc_nolibc(void *ptr, size_t new_size);

void pl4bm_free_nolibc(void *ptr);

#define PL4BM_MALLOC_IMPL(size) pl4bm_malloc_nolibc((size))

#define PL4BM_CALLOC_IMPL(nmemb, size) pl4bm_calloc_nolibc((nmemb), (size))

#define PL4BM_REALLOC_IMPL(ptr, size) pl4bm_realloc_nolibc((ptr), (size))

#define PL4BM_FREE_IMPL(ptr) pl4bm_free_nolibc((ptr))
