#pragma once

#include <stddef.h>

void *tpl_malloc(size_t size);
void *tpl_calloc(size_t count, size_t size);
void *tpl_realloc(void *ptr, size_t size);
void tpl_free(void *ptr);
char *tpl_strdup(const char *src);

#define TPL_malloc tpl_malloc
#define TPL_calloc tpl_calloc
#define TPL_realloc tpl_realloc
#define TPL_free tpl_free
#define TPL_strdup tpl_strdup
