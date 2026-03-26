#pragma once

#if defined(TPL_PLATFORM_LINUX)
  #include "linux/memory_linux.h"
#elif defined(TPL_PLATFORM_BAREMETAL)
  #include "memory_baremetal.h"
#else
  #error "No memory backend selected. Define exactly one TPL_PLATFORM_* compile definition."
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
