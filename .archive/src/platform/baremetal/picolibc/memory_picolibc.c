#include "baremetal/picolibc/memory_picolibc.h"

#include <stdlib.h>

#include "common/memory_stats.h"

void *pl4bm_malloc_picolibc(size_t size)
{
    void *ptr = malloc(size);
    if (ptr)
        PL4BM_STATS_ON_ALLOC(ptr, size, NULL, NULL, 0);
    return ptr;
}

void *pl4bm_calloc_picolibc(size_t nmemb, size_t size)
{
    void *ptr = calloc(nmemb, size);
    if (ptr)
        PL4BM_STATS_ON_CALLOC(ptr, nmemb, size, NULL, NULL, 0);
    return ptr;
}

void pl4bm_free_picolibc(void *ptr)
{
    PL4BM_STATS_ON_FREE(ptr, NULL, NULL, 0);
    free(ptr);
}

void *pl4bm_realloc_picolibc(void *ptr, size_t new_size)
{
    void *new_ptr = realloc(ptr, new_size);
    if (new_ptr)
        PL4BM_STATS_ON_REALLOC(ptr, new_ptr, new_size, NULL, NULL, 0);
    return new_ptr;
}
