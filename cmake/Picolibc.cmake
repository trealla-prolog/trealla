include(ExternalProject)

find_program(MESON_EXECUTABLE meson REQUIRED)
find_program(NINJA_EXECUTABLE ninja REQUIRED)

set(TPL_PICOLIBC_CROSS_FILE
    ""
    CACHE FILEPATH "Meson cross file for building picolibc")
set(TPL_USE_SEMIHOST
    ON
    CACHE BOOL "Enable semihosting support in picolibc")

if(NOT TPL_PICOLIBC_CROSS_FILE)
  message(FATAL_ERROR "TPL_PICOLIBC_CROSS_FILE must be set")
endif()

set(_picolibc_prefix "${CMAKE_BINARY_DIR}/_picolibc")
set(_picolibc_build "${_picolibc_prefix}/build")
set(_picolibc_install "${_picolibc_prefix}/install")

# TODO: A lot of duplication :(
if(TPL_USE_SEMIHOST)
  ExternalProject_Add(
    picolibc_ext
    SOURCE_DIR "${CMAKE_SOURCE_DIR}/picolibc"
    BINARY_DIR "${_picolibc_build}"
    CONFIGURE_COMMAND
      "${MESON_EXECUTABLE}" setup "${_picolibc_build}"
      "${CMAKE_SOURCE_DIR}/picolibc" --cross-file "${TPL_PICOLIBC_CROSS_FILE}"
      --prefix "${_picolibc_install}" -Dmultilib=false -Dsemihost=true
      -Dpicocrt=true -Dpicocrt-lib=true -Denable-malloc=true
      -Dthread-local-storage=false -Dthread-local-storage-api=false
      -Dspecsdir=${_picolibc_install}/lib -Doptimization=s
    BUILD_COMMAND "${MESON_EXECUTABLE}" compile -C "${_picolibc_build}"
    INSTALL_COMMAND "${MESON_EXECUTABLE}" install -C "${_picolibc_build}"
    BUILD_BYPRODUCTS
      "${_picolibc_install}/lib/libc.a"
      "${_picolibc_install}/lib/libsemihost.a"
      "${_picolibc_install}/lib/libcrt0.a"
      "${_picolibc_install}/lib/picolibc.specs")
else()
  ExternalProject_Add(
    picolibc_ext
    SOURCE_DIR "${CMAKE_SOURCE_DIR}/picolibc"
    BINARY_DIR "${_picolibc_build}"
    CONFIGURE_COMMAND
      "${MESON_EXECUTABLE}" setup "${_picolibc_build}"
      "${CMAKE_SOURCE_DIR}/picolibc" --cross-file "${TPL_PICOLIBC_CROSS_FILE}"
      --prefix "${_picolibc_install}" -Dmultilib=false -Dsemihost=false
      -Dpicocrt=true -Dpicocrt-lib=true -Denable-malloc=true
      -Dthread-local-storage=false -Dthread-local-storage-api=false
      -Dspecsdir=${_picolibc_install}/lib -Doptimization=s
    BUILD_COMMAND "${MESON_EXECUTABLE}" compile -C "${_picolibc_build}"
    INSTALL_COMMAND "${MESON_EXECUTABLE}" install -C "${_picolibc_build}"
    BUILD_BYPRODUCTS
      "${_picolibc_install}/lib/libc.a"
      "${_picolibc_install}/lib/picolibc.specs")
endif()

set(PICOLIBC_INSTALL_DIR
    "${_picolibc_install}"
    CACHE INTERNAL "")
set(PICOLIBC_SPECS_FILE
    "${_picolibc_install}/lib/picolibc.specs"
    CACHE INTERNAL "")
