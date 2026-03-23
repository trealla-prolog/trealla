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

add_compile_definitions(
  _GNU_SOURCE
  VERSION="${TPL_GIT_VERSION}"
)

add_compile_options(
  -Wall -Wextra
  -Wno-unused-but-set-variable
  -Wno-unused-parameter
  -Wno-unused-variable
)

include_directories("${CMAKE_SOURCE_DIR}/src")
include_directories("/usr/local/include")
link_directories("/usr/local/lib")

set(TPL_SRC
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
  src/imath/imath.c
  src/imath/imrat.c
  src/sre/re.c
)

if(TPL_USE_ISOCLINE)
  list(APPEND TPL_SRC src/isocline/src/isocline.c)
endif()


separate_arguments(_TPL_EXTRA_CFLAGS UNIX_COMMAND "${TPL_EXTRA_CFLAGS}")
separate_arguments(_TPL_EXTRA_LDFLAGS UNIX_COMMAND "${TPL_EXTRA_LDFLAGS}")

add_executable(tpl
  ${TPL_SRC}
  ${TPL_LIBRARY_GEN_C}
)

target_include_directories(tpl PRIVATE
  "${CMAKE_SOURCE_DIR}/src"
  "${CMAKE_BINARY_DIR}/generated"
)

target_compile_options(tpl PRIVATE ${_TPL_EXTRA_CFLAGS})
target_link_options(tpl PRIVATE ${_TPL_EXTRA_LDFLAGS})
target_link_libraries(tpl PRIVATE ${TPL_LINK_LIBS})

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
