# TODO: check what should be changed in the file after migration from cmake_minimum_required(VERSION 3.0)
cmake_minimum_required(VERSION 3.10)

project(NGS)

# TOOD: check how not to use hard coded include and lib directories.

include(CheckFunctionExists)
include(CheckIncludeFile)
include(FindPkgConfig)
include(ExternalProject)
include(FindBacktrace)

# -D_DARWIN_C_SOURCE - SIGWINCH and friends on MacOS
# -D_XOPEN_SOURCE - strptime on Linux
# -D_DEFAULT_SOURCE - MAP_ANONYMOUS on Linux
# -D__BSD_VISIBLE - AF_UNIX on FreeBSD
add_definitions(-D_XOPEN_SOURCE=700 -D_DARWIN_C_SOURCE=1 -D_DEFAULT_SOURCE=1 -D_BSD_SOURCE -D__BSD_VISIBLE)

# This is workaround for boehm GC library bug or NGS usage of it
# which cases sporadic SIGSEGV and other issues after fork() in child process.
# Use test-gc.ngs to determine if you can remove this setting (test on MacOS and Linux).
add_definitions(-DNGS_STUPID_MALLOC_AFTER_FORK)

add_executable(ngs
	version.h
	ngs.c
	syntax.include syntax.auto.h
	pcre_constants.include errno.include
	obj.c vm.c compile.c debug.c ast.c malloc.c decompile.c
	stdlib.ngs.h
)

# peg/leg - start
find_program(LEG leg)
IF(LEG)
	message(STATUS "leg program found, will use installed one")
ELSE()
	message(STATUS "leg program not found, will download and build it")
	ExternalProject_Add(
		leg
		URL https://www.piumarta.com/software/peg/peg-0.1.18.tar.gz
		URL_HASH SHA1=2390bcf91299aa61c5fa93895151ffeb988357a5
		CONFIGURE_COMMAND ""
		BUILD_COMMAND make
		BUILD_IN_SOURCE true
		INSTALL_COMMAND ""
	)
	ExternalProject_Get_property(leg SOURCE_DIR)
	SET(LEG "${SOURCE_DIR}/leg")
	add_dependencies(ngs leg)
ENDIF()
# peg/leg - end

check_function_exists(fmemopen FMEMOPEN)
IF(NOT FMEMOPEN)
	target_sources(ngs PRIVATE fmemopen.c)
ENDIF()

check_include_file(execinfo.h EXECINFO_H)
IF(EXECINFO_H)
	add_definitions(-DHAVE_EXECINFO_H)
ENDIF()

check_include_file(sys/poll.h POLL_H)
IF(POLL_H)
	add_definitions(-DHAVE_POLL_H)
ENDIF()


find_program(SED NAMES gsed sed) # gsed - MacOS and FreeBSD, sed - all the rest

pkg_search_module(PCRE REQUIRED libpcre)
pkg_search_module(JSONC REQUIRED json-c)
pkg_search_module(LIBGC REQUIRED bdw-gc-threaded bdw-gc)
pkg_search_module(LIBFFI REQUIRED libffi)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LIBFFI_INCLUDE_DIRS} /usr/local/include /opt/homebrew/include)
link_directories(/usr/local/lib /opt/homebrew/lib)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

add_custom_command(
	OUTPUT
		${CMAKE_CURRENT_BINARY_DIR}/syntax.include
	COMMAND
		cat ${CMAKE_CURRENT_SOURCE_DIR}/syntax.leg |
		${SED} -f ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-input.sed |
		${LEG} |
		${SED} 's/<stdin>/syntax.leg/' |
		${SED} -f ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-output.sed |
		awk -f ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-output.awk
		>${CMAKE_CURRENT_BINARY_DIR}/syntax.include
	DEPENDS
		${CMAKE_CURRENT_SOURCE_DIR}/syntax.leg
		${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-input.sed
		${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-output.sed
		${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/patch-leg-output.awk
)
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/syntax.auto.h
	COMMAND cat ${CMAKE_CURRENT_BINARY_DIR}/syntax.include | ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/make-syntax-auto.sh ${SED} >${CMAKE_CURRENT_BINARY_DIR}/syntax.auto.h
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/syntax.include
)
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/pcre_constants.include
	COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/make-pcre-constants.sh ${PCRE_INCLUDEDIR}/pcre.h >${CMAKE_CURRENT_BINARY_DIR}/pcre_constants.include
)
add_custom_command(
	OUTPUT
		${CMAKE_CURRENT_BINARY_DIR}/errno.include
	COMMAND
		${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/make-errno-include.sh ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/include_errno.c >${CMAKE_CURRENT_BINARY_DIR}/errno.include
	DEPENDS
		${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/include_errno.c
)
add_custom_command(
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/stdlib.ngs.h
	COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/bin2c.sh ${SED} lib/stdlib.ngs >${CMAKE_CURRENT_BINARY_DIR}/stdlib.ngs.h
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lib/stdlib.ngs
)


target_link_libraries(ngs m Threads::Threads ${CMAKE_DL_LIBS} ${LIBGC_LIBRARIES} ${LIBFFI_LIBRARIES} ${JSONC_LIBRARIES} ${PCRE_LIBRARIES} ${Backtrace_LIBRARY})

add_custom_target(man ALL WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc COMMAND make man DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/doc/*.1.md)


install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/ngs DESTINATION bin)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib/ DESTINATION lib/ngs)
install(
	DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/
	DESTINATION bin FILES_MATCHING PATTERN "*.ngs"
	PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_WRITE WORLD_EXECUTE
)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE DESTINATION share/doc/ngs)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/ DESTINATION share/man/man1 FILES_MATCHING PATTERN "*.1")

enable_testing()
add_test(all bash -c "${CMAKE_CURRENT_BINARY_DIR}/ngs ${CMAKE_CURRENT_SOURCE_DIR}/test.ngs")
