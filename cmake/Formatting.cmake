find_program(CLANG_FORMAT NAMES clang-format)

if(NOT CLANG_FORMAT)
  message(
    STATUS "clang-format not found -- format/format-check targets disabled")
  return()
endif()

message(STATUS "clang-format: ${CLANG_FORMAT}")

# Collect first-party sources (vendor imath/ and sre/ excluded)
set(_fmt_sources
    ${TPL_CORE_SRC} ${TPL_PLATFORM_COMMON_SRC} ${TPL_LINUX_SRC}
    ${TPL_NOLIBC_SRC} ${TPL_PICOLIBC_SRC} ${TPL_PICOLIBC_STUBS_SRC})
list(FILTER _fmt_sources EXCLUDE REGEX ".*(imath|sre)/.*")

file(GLOB_RECURSE _fmt_headers "${CMAKE_SOURCE_DIR}/src/*.h")
list(FILTER _fmt_headers EXCLUDE REGEX ".*(imath|sre)/.*")

list(APPEND _fmt_sources ${_fmt_headers})

# Normalise to absolute paths
set(_fmt_abs)
foreach(_src ${_fmt_sources})
  if(IS_ABSOLUTE "${_src}")
    list(APPEND _fmt_abs "${_src}")
  else()
    list(APPEND _fmt_abs "${CMAKE_SOURCE_DIR}/${_src}")
  endif()
endforeach()

add_custom_target(
  format
  COMMAND ${CLANG_FORMAT} -i ${_fmt_abs}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMENT "Formatting sources with clang-format"
  VERBATIM)

add_custom_target(
  format-check
  COMMAND ${CLANG_FORMAT} --dry-run --Werror ${_fmt_abs}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMENT "Checking source formatting with clang-format"
  VERBATIM)
