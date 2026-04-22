#pragma once

#if defined(TPL_BACKEND_LINUX)
#include "linux/memory_linux.h"
#elif defined(TPL_LIBC_BACKEND_PICOLIBC)
#include "baremetal/picolibc/memory_picolibc.h"
#elif defined(TPL_LIBC_BACKEND_NONE)
#include "baremetal/nolibc/memory_nolibc.h"
#else
#error                                                                                             \
    "No memory backend selected. Define exactly one TPL_BACKEND_* or TPL_LIBC_BACKEND_* compile definition."
#endif

#ifndef PL4BM_MALLOC_IMPL
#error "PL4BM_MALLOC_IMPL is not defined by the selected memory backend."
#endif

#ifndef PL4BM_CALLOC_IMPL
#error "PL4BM_CALLOC_IMPL is not defined by the selected memory backend."
#endif

#ifndef PL4BM_FREE_IMPL
#error "PL4BM_FREE_IMPL is not defined by the selected memory backend."
#endif

#ifndef PL4BM_REALLOC_IMPL
#error "PL4BM_REALLOC_IMPL is not defined by the selected memory backend."
#endif
