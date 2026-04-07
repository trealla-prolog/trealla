cmake_minimum_required(VERSION 3.25)

project(tpl LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

include(Options)
include(Version)
include(CodegenLibraryPl)
include(Dependencies)
include(Testing)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

set(COMPILE_DIAGNOSTICS_FORMAT "text" CACHE STRING  "Format of diagnositcs")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(WARNING "No build or configuration type defined, defaulting to 'Release'")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Generic")
    message(DEBUG "Detected 'Generic' system name, defaulting to baremetal backend")
    set(_tpl_backend_default "baremetal")
else()
    message(DEBUG "Defaulting to linux backend")
    set(_tpl_backend_default "linux")
endif()


set(TPL_BACKEND "${_tpl_backend_default}" CACHE STRING "Runtime backend")
set_property(CACHE TPL_BACKEND PROPERTY STRINGS linux baremetal)

separate_arguments(_tpl_extra_cflags UNIX_COMMAND "${TPL_EXTRA_CFLAGS}")
separate_arguments(_tpl_extra_ldflags UNIX_COMMAND "${TPL_EXTRA_LDFLAGS}")

set(TPL_CORE_SRC
  tpl.c
  src/base64.c
  src/bif_atts.c
  src/bif_bboard.c
  src/bif_control.c
  src/bif_csv.c
  src/bif_database.c
  src/bif_ffi.c
  src/bif_format.c
  src/bif_functions.c
  src/bif_maps.c
  src/bif_os.c
  src/bif_posix.c
  src/bif_predicates.c
  src/bif_sort.c
  src/bif_sregex.c
  src/bif_streams.c
  src/bif_tasks.c
  src/bif_threads.c
  src/compile.c
  src/heap.c
  src/history.c
  src/library.c
  src/list.c
  src/module.c
  src/network.c
  src/parser.c
  src/print.c
  src/prolog.c
  src/query.c
  src/skiplist.c
  src/terms.c
  src/toplevel.c
  src/unify.c
  src/utf8.c
  src/version.c
)

set(TPL_VENDOR_SRC
  src/imath/imath.c
  src/imath/imrat.c
  src/sre/re.c
)

set(TPL_PLATFORM_COMMON_SRC
  src/platform/common/memory_stats.c
)

set(TPL_LINUX_SRC
    src/platform/linux/memory_linux.c
)

set(TPL_BAREMETAL_SRC
    src/platform/baremetal/memory_baremetal.c
)

set(TPL_ALL_SRC
    ${TPL_CORE_SRC}
  ${TPL_VENDOR_SRC}
  ${TPL_PLATFORM_COMMON_SRC}
  ${TPL_LIBRARY_GEN_C}
)

set(TPL_INCLUDE_DIRS
  "${CMAKE_SOURCE_DIR}/src"
  "${CMAKE_SOURCE_DIR}/src/platform"
  "${CMAKE_SOURCE_DIR}/src/platform/api"
  "${CMAKE_BINARY_DIR}/generated"
)

if(TPL_BACKEND STREQUAL "linux")
    list(APPEND TPL_ALL_SCR ${TPL_LINUX_SRC})
    set(TPL_BACKEND_DEFINE TPL_BACKEND_LINUX)
elseif(TPL_BACKEND STREQUAL "baremetal")
    list(APPEND TPL_ALL_SCR ${TPL_BAREMETAL_SRC})
    set(TPL_BACKEND_DEFINE TPL_BACKEND_BAREMETAL)
else()
    message(FATAL_ERROR "Unsupported TPL_BACKEND='${TPL_BACKEND}'")
endif()

add_executable(tpl ${TPL_ALL_SRC})

target_include_directories(tpl PRIVATE ${TPL_INCLUDE_DIRS})

target_compile_definitions(tpl
    PRIVATE
    _GNU_SOURCE
    VERSION="${TPL_GIT_VERSION}"
    BACKEND="${TPL_BACKEND}"
    ${TPL_PLATFORM_DEFINE}
)

if(TPL_MEMORY_LOGGING)
  target_compile_definitions(tpl PRIVATE TPL_MEMORY_LOGGING)
endif()

target_compile_options(tpl PRIVATE
    -Wall
    -Wextra
    -Wno-unused-but-set-variable
    -Wno-unused-parameter
    -Wno-unused-variable
    -fmacro-prefix-map=${CMAKE_SOURCE_DIR}/=
    ${_tpl_extra_cflags}

	# for parsing logs
	-fdiagnostics-format=${COMPILE_DIAGNOSTICS_FORMAT}
	-fdiagnostics-color=auto

)

target_link_options(tpl PRIVATE ${_tpl_extra_ldflags})
target_link_libraries(tpl PRIVATE ${TPL_LINK_LIBS})

if(TPL_USE_COMPAT_STUB_HEADERS)
    message(WARNING "Using compat stubs")
    target_include_directories(tpl PRIVATE
      "${CMAKE_SOURCE_DIR}/compat"
      ${TPL_INCLUDE_DIRS}
)
endif()

if(TPL_BACKEND STREQUAL "baremetal")
    if(TPL_ENTRY)
        target_sources(tpl PRIVATE "${TPL_ENTRY}")
    else()
        message(WARNING "Selected baremetal backend but no entry source is defined.")
    endif()

    target_link_options(tpl PRIVATE
        -Wl,-Map=${CMAKE_BINARY_DIR}/tpl.map
    )

    if(TPL_LINKER_SCRIPT)
        target_link_options(tpl PRIVATE
            -T${TPL_LINKER_SCRIPT}
        )
    else()
        message(WARNING "Selected baremetal backend but no linker script is defined.")
    endif()
endif()

