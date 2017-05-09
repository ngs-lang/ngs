cmake_minimum_required(VERSION 3.0)
include(FindPkgConfig)
project(NGS)

pkg_check_modules(LIBFFI libffi REQUIRED)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LIBFFI_INCLUDE_DIRS} /opt/local/include/uthash)

add_executable(ngs version.h ngs.c syntax.include syntax.auto.h pcre_constants.include obj.c vm.c compile.c debug.c ast.c)

include(CheckFunctionExists)
check_function_exists(fmemopen FMEMOPEN)
IF(NOT FMEMOPEN)
	target_sources(ngs PRIVATE fmemopen.c)
ENDIF()

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
add_custom_target(
	doc
	COMMAND make
#	DEPENDS doc/*.1.md
 	${CMAKE_CURRENT_SOURCE_DIR}/doc
	)
target_link_libraries(ngs m pthread gc ffi dl json-c pcre)
install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION bin)

enable_testing()
add_test(all bash -c "NGS_BOOTSTRAP=lib/bootstrap.ngs NGS_DIR=lib ./ngs test.ngs")

install(PROGRAMS ngs DESTINATION bin)
install(DIRECTORY ${PROJECT_BINARY_DIR}/lib/ DESTINATION lib/ngs)
install(DIRECTORY ${PROJECT_BINARY_DIR}/bin/ DESTINATION bin FILES_MATCHING PATTERN "*.ngs"  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_WRITE WORLD_EXECUTE)
install(DIRECTORY ${PROJECT_BINARY_DIR}/doc/ DESTINATION man/man1 FILES_MATCHING PATTERN "*.1")
install(FILES ${PROJECT_BINARY_DIR}/LICENSE DESTINATION doc/ngs)
