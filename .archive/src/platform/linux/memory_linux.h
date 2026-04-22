#pragma once

#include <stddef.h>

void *pl4bm_malloc_linux(size_t size, const char *file, const char *func, int line);

void *pl4bm_calloc_linux(size_t nmemb, size_t size, const char *file, const char *func, int line);

void *pl4bm_realloc_linux(void *ptr, size_t new_size, const char *file, const char *func, int line);

void pl4bm_free_linux(void *ptr, const char *file, const char *func, int line);

#define PL4BM_MALLOC_IMPL(size) pl4bm_malloc_linux((size), __FILE__, __func__, __LINE__)

#define PL4BM_CALLOC_IMPL(nmemb, size)                                                             \
    pl4bm_calloc_linux((nmemb), (size), __FILE__, __func__, __LINE__)

#define PL4BM_REALLOC_IMPL(ptr, size)                                                              \
    pl4bm_realloc_linux((ptr), (size), __FILE__, __func__, __LINE__)

#define PL4BM_FREE_IMPL(ptr) pl4bm_free_linux((ptr), __FILE__, __func__, __LINE__)
