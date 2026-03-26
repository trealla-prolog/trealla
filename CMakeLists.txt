cmake_minimum_required(VERSION 3.25)

project(tpl C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

include(Options)
include(Version)
include(CodegenLibraryPl)
include(Dependencies)
include(Testing)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

set(TPL_PLATFORM "linux" CACHE STRING "Target platform backend")
set_property(CACHE TPL_PLATFORM PROPERTY STRINGS linux baremetal)

add_compile_definitions(
  _GNU_SOURCE
  VERSION="${TPL_GIT_VERSION}"
  TARGET="${TPL_PLATFORM}"
)

add_compile_options(
  -Wall -Wextra
  -Wno-unused-but-set-variable
  -Wno-unused-parameter
  -Wno-unused-variable
)

separate_arguments(_TPL_EXTRA_CFLAGS UNIX_COMMAND "${TPL_EXTRA_CFLAGS}")
separate_arguments(_TPL_EXTRA_LDFLAGS UNIX_COMMAND "${TPL_EXTRA_LDFLAGS}")

set(TPL_SRC
  src/compile.c
  src/heap.c
  src/module.c
  src/parser.c
  src/print.c
  src/prolog.c
  src/query.c
  src/terms.c
  src/unify.c
  src/version.c
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
  src/base64.c
  src/list.c
  src/skiplist.c
  src/utf8.c
  src/history.c
  src/library.c
  src/network.c
  src/toplevel.c
)

set(TPL_VENDOR_SRC
  src/imath/imath.c
  src/imath/imrat.c
  src/sre/re.c
)

set(TPL_PLATFORM_COMMON_SRC
	src/platform/common/memory_stats.c
)

if(TPL_PLATFORM STREQUAL "linux")
	set(TPL_PLATFORM_SRC
		src/platform/linux/memory_linux.c
	)
	set(TPL_PLATFORM_DEFINE TPL_PLATFORM_LINUX)
elseif(TPL_PLATFORM STREQUAL "baremetal")
	set(TPL_PLATFORM_SRC
		# src/platform/baremetal/...
	)
	set(TPL_PLATFORM_DEFINE TPL_PLATFORM_BAREMETAL)
	#TODO: Board specific implementations
else()
	message(FATAL_ERROR("Unsupported TPL_PLATFORM='${TPL_PLATFORM}'"))
endif()

add_library(tpl_core OBJECT
  ${TPL_SRC}
  ${TPL_VENDOR_SRC}
  ${TPL_PLATFORM_COMMON_SRC}
  ${TPL_PLATFORM_SRC}
  ${TPL_LIBRARY_GEN_C}
)

target_compile_definitions(tpl_core PRIVATE ${TPL_PLATFORM_DEFINE})

# I would prefer to only have src/ and src/platform/api as include directories
# and then reference e.g. with include "core/polog.c", but that would mean having
# to go through all sources and updating imports.
set(TPL_INCLUDE_DIRS
	"${CMAKE_SOURCE_DIR}/src"
	"${CMAKE_SOURCE_DIR}/src/platform"
	"${CMAKE_SOURCE_DIR}/src/platform/api"
	"${CMAKE_SOURCE_DIR}/generated"
)

target_include_directories(tpl_core PRIVATE ${TPL_INCLUDE_DIRS})

target_compile_options(tpl_core PRIVATE
	${_TPL_EXTRA_CFLAGS}
	-fmacro-prefix-map=${CMAKE_SOURCE_DIR}/=
)
target_link_options(tpl_core PRIVATE ${_TPL_EXTRA_LDFLAGS})

if(TPL_MEMORY_LOGGING)
  target_compile_definitions(tpl_core PRIVATE TPL_MEMORY_LOGGING)
endif()

add_executable(tpl
	tpl.c
	$<TARGET_OBJECTS:tpl_core>
)

if(TPL_MEMORY_LOGGING)
  target_compile_definitions(tpl PRIVATE TPL_MEMORY_LOGGING)
endif()

target_link_libraries(tpl PRIVATE ${TPL_LINK_LIBS})
target_include_directories(tpl PRIVATE ${TPL_INCLUDE_DIRS})


set(MAIN_C "${CMAKE_BINARY_DIR}/generated/main.c")

if(MAIN_PL)
  add_custom_command(
    OUTPUT "${MAIN_C}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/generated"
    COMMAND ${CMAKE_COMMAND} -E echo "#include <stddef.h>" > "${MAIN_C}"
    COMMAND "${XXD_EXECUTABLE}" -i "${MAIN_PL}" >> "${MAIN_C}"
    DEPENDS "${MAIN_PL}"
    VERBATIM
  )

  add_library(tpl_core OBJECT
    ${TPL_SRC}
    ${TPL_LIBRARY_GEN_C}
  )
  target_include_directories(tpl_core PRIVATE
    "${CMAKE_SOURCE_DIR}/src"
    "${CMAKE_BINARY_DIR}/generated"
  )
  target_compile_options(tpl_core PRIVATE ${_TPL_EXTRA_CFLAGS})

  add_executable(compile_main
    $<TARGET_OBJECTS:tpl_core>
    "${MAIN_C}"
  )
  target_compile_definitions(compile_main PRIVATE USE_MAIN=1)
  target_link_options(compile_main PRIVATE ${_TPL_EXTRA_LDFLAGS})
  target_link_libraries(compile_main PRIVATE ${TPL_LINK_LIBS})

  add_custom_target(compile DEPENDS compile_main)
else()
  add_custom_target(compile
    COMMAND ${CMAKE_COMMAND} -E echo "Set -DMAIN_PL=/abs/path/to/main.pl to enable compile_main"
  )
endif()

# include(GNUInstallDirs)
# install(TARGETS tpl RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
