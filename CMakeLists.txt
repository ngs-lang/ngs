cmake_minimum_required(VERSION 3.0)
project(NGS)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR})
add_executable(ngs version.h ngs.c syntax.include syntax.auto.h pcre_constants.include obj.c vm.c compile.c debug.c ast.c)
find_file(PCRE_H pcre.h)
# --- version - start ---
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/VERSION
	COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/make-version.sh ${CMAKE_CURRENT_SOURCE_DIR}
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/make-version.sh
)
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/version.h
	# TODO: Find cleaner way. $$ instead of $ because of make is not that pretty.
	COMMAND bash -euc 'v=`cat ${CMAKE_CURRENT_BINARY_DIR}/VERSION` && echo "V=$$v" && sed "s/@/$$v/g" ${CMAKE_CURRENT_SOURCE_DIR}/version.h.in >${CMAKE_CURRENT_BINARY_DIR}/version.h'
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/VERSION
)
# --- version - end ---
add_custom_command(
	OUTPUT
		${CMAKE_CURRENT_BINARY_DIR}/syntax.include
	COMMAND
		cat ${CMAKE_CURRENT_SOURCE_DIR}/syntax.leg |
		sed -f ${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-input.sed |
		leg |
		sed 's/<stdin>/syntax.leg/' |
		sed -f ${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-output.sed |
		awk -f ${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-output.awk
		>${CMAKE_CURRENT_BINARY_DIR}/syntax.include
	DEPENDS
		${CMAKE_CURRENT_SOURCE_DIR}/syntax.leg
		${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-input.sed
		${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-output.sed
		${CMAKE_CURRENT_SOURCE_DIR}/patch-leg-output.awk
)
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/syntax.auto.h
	COMMAND cat ${CMAKE_CURRENT_BINARY_DIR}/syntax.include | ${CMAKE_CURRENT_SOURCE_DIR}/make-syntax-auto.sh >${CMAKE_CURRENT_BINARY_DIR}/syntax.auto.h
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/syntax.include
)
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/pcre_constants.include
	COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/make-pcre-constants.sh ${PCRE_H} >${CMAKE_CURRENT_BINARY_DIR}/pcre_constants.include
)
target_link_libraries(ngs m pthread gc ffi dl json-c pcre)
install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION bin)
