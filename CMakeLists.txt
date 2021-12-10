## -------------------------------------------------------------------------- ##
#                                                                              #
#                     EaFort: Easy Fortran Handling Library                    #
#                                                                              #
#    * Author : Young-Myung CHOI                                               #
#                                                                              #
## -------------------------------------------------------------------------- ##

## Default Settings --------------------------------------------------------- ##

##... Set the minimum version of cmake
cmake_minimum_required(VERSION 2.9 FATAL_ERROR)

##... CMake Project Name
project(EaFort)

##... Set Default Fortran Compiler & Eable the Fortran Compilation Environment
enable_language(Fortran)
if (NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER gfortran)
endif()

## CMake Configuration ------------------------------------------------------ ##

##... Load Default CMake Configuration
include( config/config.cmake )

##... Set Fortran Compiler Flags
include( config/fortranFlag.cmake )

##... Auto Compilation Functions
include( config/Func_AutoCompile.cmake )

## Print CMake Project Information ------------------------------------------ ##
message("")
message("${BoldYellow}Project: ${BoldWhite}${CMAKE_PROJECT_NAME}")
message("")
message("  - ${Green}Projct Path       ${White}: ${PROJECT_COMPILE_PATH} ${Green}")
message("  - ${Green}GIT_BRANCH        ${White}: ${GIT_BRANCH} ${Green}")
message("  - ${Green}GIT_COMMIT_HASH   ${White}: ${GIT_COMMIT_HASH} ${Green}")
message("")
message("  - ${Green}Lib. Install Path ${White}: ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${Green}")
message("  - ${Green}Exe. Install Path ${White}: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} ${Green}")
message("")

if( ${FORTRAN_COMPILE_MODE} STREQUAL "Release" )
message("  - ${Green}Compilation Mode  ${White}: Release ${Green}")
elseif( ${FORTRAN_COMPILE_MODE} STREQUAL "Debug" )
message("  - ${Green}Compilation Mode  ${White}: Debug ${Green}")
endif()

message("${Green}")
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
message("  - ${Green}Fortran Compiler  ${White}: Intel Fortran ${Green}")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
message("  - ${Green}Fortran Compiler  ${White}: GNU Fortran ${Green}")
endif()
message("  - ${Green}Global Flag       ${White}: ${CMAKE_Fortran_FLAGS} ${Green}")
message("${ColourReset}")

## Math Kernel Library ------------------------------------------------------ ##

## Compile ThirdParty Library ----------------------------------------------- ##

set(LIB_NAME_StringiFor StringiFor)
add_subdirectory(ThirdParty/StringiFor)

set(LIB_NAME_json-fortran json-fortran)
add_subdirectory(ThirdParty/json-fortran)


## Compile EaFort Library --------------------------------------------------- ##

##... Set Library name
set( LIB_NAME_EaFort EaFort )
set( DEPEND_EaFort StringiFor json-fortran )

##... Set dependencies
add_subdirectory(src)

## Compile Testing EaFort Programs ------------------------------------------ ##

set(TEST_NAME_string testString)
add_subdirectory(test/testString)

set(TEST_NAME_GURU testGURU)
add_subdirectory(test/testGURU)
