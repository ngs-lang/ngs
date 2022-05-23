cmake_minimum_required(VERSION 3.0)
project(NGS)

# TOOD: check how not to use hard coded include and lib directories.
#       Do something like pkg_check_modules(LIBFFI libffi REQUIRED) but for all libraries?

include(CheckFunctionExists)
include(CheckIncludeFile)
include(FindPkgConfig)
include(ExternalProject)

option(BUILD_DOCS "Build documentation" ON)
option(INSTALL_EXAMPLE_SCRIPTS "Install example scripts" OFF)

# -D_DARWIN_C_SOURCE - SIGWINCH and friends on MacOS
# -D_XOPEN_SOURCE - strptime on Linux
# -D_DEFAULT_SOURCE - MAP_ANONYMOUS on Linux
add_definitions(-D_XOPEN_SOURCE=700 -D_DARWIN_C_SOURCE=1 -D_DEFAULT_SOURCE=1 -D_BSD_SOURCE)

# This is workaround for boehm GC library bug or NGS usage of it
# which cases sporadic SIGSEGV and other issues after fork() in child process.
# Use test-gc.ngs to determine if you can remove this setting (test on MacOS and Linux).
add_definitions(-DNGS_STUPID_MALLOC_AFTER_FORK)

pkg_check_modules(LIBFFI libffi REQUIRED)
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${LIBFFI_INCLUDE_DIRS} /usr/local/include /opt/homebrew/include)
link_directories(/usr/local/lib /opt/homebrew/lib)

add_executable(
  ngs
  version.h
  ngs.c
  syntax.include syntax.auto.h
  pcre_constants.include errno.include
  obj.c vm.c compile.c debug.c ast.c malloc.c decompile.c
  stdlib.ngs.h
)

# peg/leg - start
find_program(LEG leg)
if(LEG)
  message(STATUS "leg program found, will use installed one")
else()
  message(STATUS "leg program not found, will download and build it")
  externalproject_add(
    leg
    URL https://www.piumarta.com/software/peg/peg-0.1.18.tar.gz
    URL_HASH SHA1=2390bcf91299aa61c5fa93895151ffeb988357a5
    CONFIGURE_COMMAND ""
    BUILD_COMMAND make
    BUILD_IN_SOURCE true
    INSTALL_COMMAND ""
  )
  externalproject_get_property(leg SOURCE_DIR)
  set(LEG "${SOURCE_DIR}/leg")
  add_dependencies(ngs leg)
endif()
# peg/leg - end

check_function_exists(fmemopen FMEMOPEN)
if(NOT FMEMOPEN)
  target_sources(ngs PRIVATE fmemopen.c)
endif()

check_include_file(execinfo.h EXECINFO_H)
if(EXECINFO_H)
  add_definitions(-DHAVE_EXECINFO_H)
endif()

check_include_file(sys/poll.h POLL_H)
if(POLL_H)
  add_definitions(-DHAVE_POLL_H)
endif()

if(CMAKE_INSTALL_LIBDIR)
  file(READ lib/stdlib.ngs FILE_CONTENTS)
  string(REPLACE "#@INSTALL_LIBDIR@" ${CMAKE_INSTALL_LIBDIR}/ngs FILE_CONTENTS "${FILE_CONTENTS}")
  file(WRITE lib/stdlib.ngs "${FILE_CONTENTS}")
endif()

find_program(SED NAMES gsed sed) # gsed - MacOS, sed - all the rest
find_file(PCRE_H pcre.h)
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
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build-scripts/make-pcre-constants.sh ${PCRE_H} >${CMAKE_CURRENT_BINARY_DIR}/pcre_constants.include
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

target_link_libraries(ngs m pthread gc ffi dl json-c pcre)

if(BUILD_DOCS)
  find_program(PANDOC pandoc)
  if(NOT PANDOC)
    message(FATAL_ERROR "Could not find pandoc")
  endif()
  add_custom_target(man ALL WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc COMMAND make man DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/doc/*.1.md)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE DESTINATION doc/ngs)
endif()

install(FILES "${PROJECT_BINARY_DIR}/ngs" DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/ngs DESTINATION bin)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib/ DESTINATION lib/ngs)
if(INSTALL_EXAMPLE_SCRIPTS)
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin/
    DESTINATION bin FILES_MATCHING PATTERN "*.ngs"
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_WRITE WORLD_EXECUTE
  )
endif()

if(APPLE)
  set(MAN_DIR share/man/man1)
else()
  set(MAN_DIR man/man1)
endif()
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc/ DESTINATION ${MAN_DIR} FILES_MATCHING PATTERN "*.1")

enable_testing()
add_test(all bash -c "NGS_PATH=${CMAKE_CURRENT_SOURCE_DIR}/lib ${CMAKE_CURRENT_BINARY_DIR}/ngs ${CMAKE_CURRENT_SOURCE_DIR}/test.ngs")
