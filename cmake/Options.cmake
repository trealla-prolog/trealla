option(TPL_MEMORY_LOGGING "Enable memory allocation logging and stats" OFF)
option(TPL_USE_COMPAT_STUB_HEADERS "Use compat stub headers to more easily find os dependencies" OFF)

option(TPL_USE_EDITLINE  "Use editline" OFF)
option(TPL_USE_READLINE  "Use readline (default on non-WASI if no other editor is selected)" OFF)

option(TPL_USE_FFI       "Enable libffi" OFF)
option(TPL_USE_OPENSSL   "Enable OpenSSL" OFF)
option(TPL_USE_THREADS   "Enable threads" OFF)

option(TPL_LTO           "Enable link-time optimization (IPO/LTO)" OFF)

set(TPL_EXTRA_CFLAGS "" CACHE STRING "Extra C compiler flags (like Makefile OPT)")
set(TPL_EXTRA_LDFLAGS "" CACHE STRING "Extra linker flags")

set(MAIN_PL "" CACHE FILEPATH "Path to main.pl used by compile_main target")


if(TPL_USE_ISOCLINE)
  set(TPL_NEEDS_READLINE OFF)
elseif(TPL_USE_EDITLINE)
  set(TPL_NEEDS_READLINE OFF)
else()
    set(TPL_NEEDS_READLINE ON)
endif()
