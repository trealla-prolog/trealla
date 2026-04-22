set(_hook_src "${CMAKE_SOURCE_DIR}/.hooks/pre-commit")
set(_hook_dst "${CMAKE_SOURCE_DIR}/.git/hooks/pre-commit")

if(NOT IS_DIRECTORY "${CMAKE_SOURCE_DIR}/.git")
  return()
endif()

# Ensure the hook script is executable (e.g. after a fresh checkout)
file(
  CHMOD
  "${_hook_src}"
  FILE_PERMISSIONS
  OWNER_READ
  OWNER_WRITE
  OWNER_EXECUTE
  GROUP_READ
  GROUP_EXECUTE
  WORLD_READ
  WORLD_EXECUTE)

if(NOT EXISTS "${_hook_dst}")
  file(CREATE_LINK "${_hook_src}" "${_hook_dst}" SYMBOLIC)
  message(STATUS "Installed pre-commit hook: ${_hook_dst}")
endif()
