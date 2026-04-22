enable_testing()

add_test(
  NAME tpl_tests
  COMMAND "${CMAKE_SOURCE_DIR}/tests/run.sh"
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME tpl_valgrind
  COMMAND "${CMAKE_SOURCE_DIR}/tests/run_valgrind.sh"
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME tpl_valgrind_leaks
  COMMAND "${CMAKE_SOURCE_DIR}/tests/run_valgrind_leaks.sh"
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
