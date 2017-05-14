cmake_minimum_required(VERSION 3.0)
include(FindPkgConfig)
project(NGS)

pkg_check_modules(LIBFFI libffi REQUIRED)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LIBFFI_INCLUDE_DIRS} /opt/local/include/uthash /usr/local/include)

add_executable(ngs version.h ngs.c syntax.include syntax.auto.h pcre_constants.include obj.c vm.c compile.c debug.c ast.c malloc.c)

include(CheckFunctionExists)
check_function_exists(fmemopen FMEMOPEN)
IF(NOT FMEMOPEN)
	target_sources(ngs PRIVATE fmemopen.c)
ENDIF()

find_file(PCRE_H pcre.h)
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

add_custom_target(man ALL WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc COMMAND make man DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/doc/*.1.md)


install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/ngs DESTINATION bin)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib/ DESTINATION lib/ngs)
install(
	DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/
	DESTINATION bin FILES_MATCHING PATTERN "*.ngs"
	PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_WRITE WORLD_EXECUTE
)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE DESTINATION doc/ngs)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/ DESTINATION man/man1 FILES_MATCHING PATTERN "*.1")

enable_testing()
add_test(all bash -c "NGS_BOOTSTRAP=${CMAKE_CURRENT_SOURCE_DIR}/lib/bootstrap.ngs NGS_DIR=${CMAKE_CURRENT_SOURCE_DIR}/lib ${CMAKE_CURRENT_BINARY_DIR}/ngs ${CMAKE_CURRENT_SOURCE_DIR}/test.ngs")
