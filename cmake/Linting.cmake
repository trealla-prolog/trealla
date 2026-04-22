find_program(CPPCHECK NAMES cppcheck)

if(NOT CPPCHECK)
  message(STATUS "cppcheck not found -- lint target disabled")
  return()
endif()

message(STATUS "cppcheck: ${CPPCHECK}")

set(_lint_sources
    ${TPL_CORE_SRC} ${TPL_PLATFORM_COMMON_SRC} ${TPL_LINUX_SRC}
    ${TPL_NOLIBC_SRC} ${TPL_PICOLIBC_SRC} ${TPL_PICOLIBC_STUBS_SRC})
list(FILTER _lint_sources EXCLUDE REGEX ".*(imath|sre)/.*")

set(_lint_abs)
foreach(_src ${_lint_sources})
  if(IS_ABSOLUTE "${_src}")
    list(APPEND _lint_abs "${_src}")
  else()
    list(APPEND _lint_abs "${CMAKE_SOURCE_DIR}/${_src}")
  endif()
endforeach()

add_custom_target(
  lint
  COMMAND
    ${CPPCHECK} --enable=warning,style,performance,portability --std=c99
    --suppress=missingIncludeSystem --error-exitcode=1 --quiet -I
    "${CMAKE_SOURCE_DIR}/src" -I "${CMAKE_SOURCE_DIR}/src/platform/api"
    ${_lint_abs}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMENT "Running cppcheck on sources"
  VERBATIM)
