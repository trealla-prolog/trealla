#include "memory_stats.h"

#include <stdio.h>

static pl4bm_memory_stats g_stats;

#if defined(TPL_MEMORY_LOGGING)
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#define LOG(...) ((void)0)
#endif

void pl4bm_memory_stats_on_alloc(void *ptr, size_t size, const char *file, const char *func,
                                 int line)
{
    g_stats.alloc_calls++;
    g_stats.total_allocated_bytes += size;

    LOG("[pl4bm] malloc  ptr=%p size=%zu (%s:%s:%d) total_alloc=%zu\n", ptr, size,
        file ? file : "?", func ? func : "?", line, g_stats.total_allocated_bytes);
}

void pl4bm_memory_stats_on_calloc(void *ptr, size_t nmemb, size_t size, const char *file,
                                  const char *func, int line)
{
    size_t total = nmemb * size;

    g_stats.calloc_calls++;
    g_stats.total_callocated_bytes += total;

    LOG("[pl4bm] calloc  ptr=%p nmemb=%zu size=%zu total=%zu (%s:%s:%d) "
        "total_calloc=%zu\n",
        ptr, nmemb, size, total, file ? file : "?", func ? func : "?", line,
        g_stats.total_callocated_bytes);
}

void pl4bm_memory_stats_on_realloc(void *old_ptr, void *new_ptr, size_t new_size, const char *file,
                                   const char *func, int line)
{
    g_stats.realloc_calls++;
    g_stats.total_reallocated_bytes += new_size;

    LOG("[pl4bm] realloc ptr=%p -> %p new=%zu (%s:%s:%d) total_realloc=%zu\n", old_ptr, new_ptr,
        new_size, file ? file : "?", func ? func : "?", line, g_stats.total_reallocated_bytes);
}

void pl4bm_memory_stats_on_free(void *ptr, const char *file, const char *func, int line)
{
    g_stats.free_calls++;

    LOG("[pl4bm] free    ptr=%p (%s:%s:%d)\n", ptr, file ? file : "?", func ? func : "?", line);
}

const pl4bm_memory_stats *pl4bm_memory_stats_get(void)
{
    return &g_stats;
}

void pl4bm_memory_stats_dump(void)
{
#if defined(TPL_MEMORY_LOGGING)
    fprintf(stderr,
            "\n[pl4bm] ===== MEMORY STATS =====\n"
            "alloc calls           : %zu\n"
            "calloc calls          : %zu\n"
            "realloc calls         : %zu\n"
            "free calls            : %zu\n"
            "total alloc bytes     : %zu\n"
            "total calloc bytes    : %zu\n"
            "total realloc bytes   : %zu\n"
            "===============================\n\n",
            g_stats.alloc_calls, g_stats.calloc_calls, g_stats.realloc_calls, g_stats.free_calls,
            g_stats.total_allocated_bytes, g_stats.total_callocated_bytes,
            g_stats.total_reallocated_bytes);
#endif
}

void pl4bm_memory_stats_reset(void)
{
    g_stats = (pl4bm_memory_stats){0};
}
