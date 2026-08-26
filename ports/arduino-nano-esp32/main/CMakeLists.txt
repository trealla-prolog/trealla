set(TREALLA_ROOT "${CMAKE_CURRENT_LIST_DIR}/../../..")

idf_component_register(
	SRCS "app_main.c" "platform.c"
	INCLUDE_DIRS "${TREALLA_ROOT}/src"
	EMBED_TXTFILES "program.pl"
	REQUIRES esp_timer esp_system esp_psram heap
	LDFRAGMENTS "trealla.lf")

add_prebuilt_library(trealla "${TREALLA_ROOT}/libtrealla.a")
target_link_libraries(${COMPONENT_LIB} PRIVATE trealla)
