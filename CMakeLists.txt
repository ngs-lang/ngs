cmake_minimum_required(VERSION 3.10)

project(NGS)

option(INSTALL_EXAMPLE_SCRIPTS "Install example scripts" OFF)

set(BUILD_MAN AUTO CACHE STRING "Build documentation")
set_property(CACHE BUILD_MAN PROPERTY STRINGS AUTO ON OFF)

include(CheckFunctionExists)
include(CheckIncludeFile)
include(FindPkgConfig)
include(ExternalProject)
include(FindBacktrace)
include(GNUInstallDirs)

# -D_DARWIN_C_SOURCE - SIGWINCH and friends on MacOS
# -D_XOPEN_SOURCE - strptime on Linux
# -D_DEFAULT_SOURCE - MAP_ANONYMOUS on Linux
# -D__BSD_VISIBLE - AF_UNIX on FreeBSD
add_definitions(-D_XOPEN_SOURCE=700 -D_DARWIN_C_SOURCE=1 -D_DEFAULT_SOURCE=1 -D_BSD_SOURCE -D__BSD_VISIBLE -DINSTALL_LIBDIR=${CMAKE_INSTALL_FULL_LIBDIR})

# This is workaround for boehm GC library bug or NGS usage of it
# which cases sporadic SIGSEGV and other issues after fork() in child process.
# Use test-gc.ngs to determine if you can remove this setting (test on MacOS and Linux).
add_definitions(-DNGS_STUPID_MALLOC_AFTER_FORK)

pkg_search_module(JSONC REQUIRED json-c)
pkg_search_module(LIBFFI REQUIRED libffi)
pkg_search_module(LIBGC REQUIRED bdw-gc-threaded bdw-gc)
pkg_search_module(PCRE REQUIRED libpcre)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LIBFFI_INCLUDE_DIRS} ${LIBGC_INCLUDE_DIRS} ${PCRE_INCLUDE_DIRS} ${Backtrace_INCLUDE_DIR} ${JSONC_INCLUDE_DIRS})

link_directories(${LIBFFI_LIBRARY_DIRS} ${LIBGC_LIBRARY_DIRS} ${PCRE_LIBRARY_DIRS} ${JSONC_LIBRARY_DIRS})

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

set(DO_BUILD_MAN_PAGES ON)
if(BUILD_MAN STREQUAL "AUTO")
	find_program(PANDOC pandoc)
	if(PANDOC)
		message(STATUS "BUILD_MAN is AUTO - pandoc program found, building man pages.")
	else()
		message(STATUS "BUILD_MAN is AUTO - pandoc program not found, not building man pages.")
	endif()
elseif(BUILD_MAN)
	message(STATUS "BUILD_MAN is ON - looking for pandoc, it's required for building the man pages")
	find_program(PANDOC pandoc REQUIRED)
else()
	message(STATUS "BUILD_MAN is OFF - not building man pages")
	set(DO_BUILD_MAN_PAGES OFF)
endif()

if(DO_BUILD_MAN_PAGES)
	add_custom_target(man ALL WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc COMMAND make man DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/doc/*.1.md)
	find_program(PANDOC pandoc)
	install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/ DESTINATION ${CMAKE_INSTALL_MANDIR}/man1 FILES_MATCHING PATTERN "*.1")
endif()


install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION ${CMAKE_INSTALL_BINDIR})
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/ngs DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/ngs)

if(INSTALL_EXAMPLE_SCRIPTS)
	install(
		DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/
		DESTINATION ${CMAKE_INSTALL_BINDIR} FILES_MATCHING PATTERN "*.ngs"
		PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_WRITE WORLD_EXECUTE
	)
endif()
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE DESTINATION ${CMAKE_INSTALL_DOCDIR})

enable_testing()
# NGS_PATH is set because the files are not installed yet.
add_test(all bash -c "NGS_PATH=${CMAKE_CURRENT_SOURCE_DIR}/lib ${CMAKE_CURRENT_BINARY_DIR}/ngs ${CMAKE_CURRENT_SOURCE_DIR}/test.ngs")
