#include "baremetal/nolibc/memory_nolibc.h"

#include <stdlib.h>

void *pl4bm_malloc_nolibc(size_t size)
{
#error "Unimplemented"
    return NULL;
}

void *pl4bm_calloc_nolibc(size_t nmemb, size_t size)
{
    return NULL;
}

void pl4bm_free_nolibc(void *ptr)
{
}

void *pl4bm_realloc_nolibc(void *ptr, size_t new_size)
{
    return NULL;
}
