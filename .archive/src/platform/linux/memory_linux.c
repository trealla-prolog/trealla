#include "linux/memory_linux.h"

#include <stdlib.h>

#include "common/memory_stats.h"

void *pl4bm_malloc_linux(size_t size, const char *file, const char *func, int line)
{
    void *ptr = malloc(size);

    if (ptr)
        PL4BM_STATS_ON_ALLOC(ptr, size, file, func, line);

    return ptr;
}

void *pl4bm_calloc_linux(size_t nmemb, size_t size, const char *file, const char *func, int line)
{
    void *ptr = calloc(nmemb, size);

    if (ptr)
        PL4BM_STATS_ON_CALLOC(ptr, nmemb, size, file, func, line);

    return ptr;
}

void pl4bm_free_linux(void *ptr, const char *file, const char *func, int line)
{
    PL4BM_STATS_ON_FREE(ptr, file, func, line);
    free(ptr);
}

void *pl4bm_realloc_linux(void *ptr, size_t new_size, const char *file, const char *func, int line)
{
    void *new_ptr = realloc(ptr, new_size);

    if (new_ptr)
        PL4BM_STATS_ON_REALLOC(ptr, new_ptr, new_size, file, func, line);

    return new_ptr;
}
