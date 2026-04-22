#pragma once

#include <stddef.h>

typedef struct {
    size_t alloc_calls;
    size_t calloc_calls;
    size_t realloc_calls;
    size_t free_calls;

    size_t total_allocated_bytes;
    size_t total_callocated_bytes;
    size_t total_reallocated_bytes;
} pl4bm_memory_stats;

void pl4bm_memory_stats_on_alloc(void *ptr, size_t size, const char *file, const char *func,
                                 int line);

void pl4bm_memory_stats_on_calloc(void *ptr, size_t nmemb, size_t size, const char *file,
                                  const char *func, int line);

void pl4bm_memory_stats_on_realloc(void *old_ptr, void *new_ptr, size_t new_size, const char *file,
                                   const char *func, int line);

void pl4bm_memory_stats_on_free(void *ptr, const char *file, const char *func, int line);

const pl4bm_memory_stats *pl4bm_memory_stats_get(void);
void pl4bm_memory_stats_dump(void);
void pl4bm_memory_stats_reset(void);

#if defined(TPL_MEMORY_LOGGING)
#define PL4BM_STATS_ON_ALLOC(ptr, size, file, func, line)                                          \
    pl4bm_memory_stats_on_alloc((ptr), (size), (file), (func), (line))

#define PL4BM_STATS_ON_CALLOC(ptr, nmemb, size, file, func, line)                                  \
    pl4bm_memory_stats_on_calloc((ptr), (nmemb), (size), (file), (func), (line))

#define PL4BM_STATS_ON_REALLOC(old_ptr, new_ptr, new_size, file, func, line)                       \
    pl4bm_memory_stats_on_realloc((old_ptr), (new_ptr), (new_size), (file), (func), (line))

#define PL4BM_STATS_ON_FREE(ptr, file, func, line)                                                 \
    pl4bm_memory_stats_on_free((ptr), (file), (func), (line))
#else
#define PL4BM_STATS_ON_ALLOC(ptr, size, file, func, line) ((void)0)
#define PL4BM_STATS_ON_CALLOC(ptr, nmeb, size, file, func, line) ((void)0)
#define PL4BM_STATS_ON_REALLOC(old_ptr, new_ptr, new_size, file, func, line) ((void)0)
#define PL4BM_STATS_ON_FREE(ptr, file, func, line) ((void)0)
#endif
