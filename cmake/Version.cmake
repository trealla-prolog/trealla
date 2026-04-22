find_package(Git QUIET)
if(Git_FOUND)
  execute_process(
    COMMAND "${GIT_EXECUTABLE}" describe --abbrev=4 --dirty --always --tags
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    OUTPUT_VARIABLE TPL_GIT_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
endif()

if(NOT TPL_GIT_VERSION)
  set(TPL_GIT_VERSION "unknown")
endif()
