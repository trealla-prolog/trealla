find_program(XXD_EXECUTABLE xxd)
if(NOT XXD_EXECUTABLE)
  message(
    FATAL_ERROR
      "xxd not found (needed to generate library/*.c from library/*.pl).")
endif()

# TPL_EMBEDDED_LIBRARIES: semicolon-separated list of library/*.pl stems to
# embed. When empty, all library files are embedded.
set(TPL_EMBEDDED_LIBRARIES
    ""
    CACHE STRING
          "Library *.pl stems to embed, semicolon-separated (empty = all)")

file(GLOB TPL_LIBRARY_PL "${CMAKE_SOURCE_DIR}/library/*.pl")

set(TPL_LIBRARY_GEN_C)
foreach(pl ${TPL_LIBRARY_PL})
  get_filename_component(stem "${pl}" NAME_WE)

  if(TPL_EMBEDDED_LIBRARIES)
    list(FIND TPL_EMBEDDED_LIBRARIES "${stem}" _idx)
    if(_idx EQUAL -1)
      continue()
    endif()
  endif()

  set(gen_c "${CMAKE_BINARY_DIR}/generated/library/${stem}.c")
  list(APPEND TPL_LIBRARY_GEN_C "${gen_c}")

  set(sym "library_${stem}_pl")

  add_custom_command(
    OUTPUT "${gen_c}"
    COMMAND ${CMAKE_COMMAND} -E make_directory
            "${CMAKE_BINARY_DIR}/generated/library"
    COMMAND ${CMAKE_COMMAND} -E echo "#include <stddef.h>" > "${gen_c}"
    COMMAND "${XXD_EXECUTABLE}" -i -n "${sym}" "${pl}" >> "${gen_c}"
    DEPENDS "${pl}"
    VERBATIM)
endforeach()
