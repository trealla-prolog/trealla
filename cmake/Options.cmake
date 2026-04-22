option(TPL_MEMORY_LOGGING "Enable memory allocation logging and stats" OFF)
option(TPL_LTO "Enable link-time optimization (IPO/LTO)" OFF)

set(TPL_PROGRAM_PL
    ""
    CACHE FILEPATH "Path to a Prolog file to embed as the program (sets program_pl / program_pl_len)")

set(TPL_EXCLUDED_BIF_SRCS
    ""
    CACHE STRING "Semicolon-separated list of src/bif_*.c stems to exclude (e.g. bif_ffi;bif_posix)")

set(TPL_EXTRA_CFLAGS
    ""
    CACHE STRING "Extra C compiler flags")
set(TPL_EXTRA_LDFLAGS
    ""
    CACHE STRING "Extra linker flags")
