# CMake project file for forts
cmake_minimum_required (VERSION 2.8)

# project name and details
project (TOAST)
enable_language (Fortran)
set(PROJECT_DESCRIPTION "TOAST")
set(PROJECT_URL "https://git.ccfe.ac.uk/thomasms/toast")

# set source directories
set (TOAST_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/toast)
set (TOAST_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/toast/include)
set (TOAST_EXTERNAL_DIR ${CMAKE_CURRENT_LIST_DIR}/external)
set (TOAST_EXAMPLES_DIR ${CMAKE_CURRENT_LIST_DIR}/examples)
set (TOAST_TEST_DIR ${CMAKE_CURRENT_LIST_DIR}/tests)
set (TOAST_BUILD_DIR ${CMAKE_BINARY_DIR})

# library names
set (TOAST_LIB_NAME toast)

# Set release build by default
set (CMAKE_BUILD_TYPE Release CACHE STRING "Select which configuration to build.")

# set the general fortran Debug/Release compiler flags
set(TOAST_RELEASE_F90_FLAGS "-O3")
set(TOAST_RELEASE_CXX_FLAGS "-O3")
set(TOAST_DEBUG_F90_FLAGS "-g -O0")
set(TOAST_DEBUG_CXX_FLAGS "-g -O0")

set(CMAKE_Fortran_FLAGS_RELEASE ${TOAST_RELEASE_FLAGS})
set(CMAKE_Fortran_FLAGS_DEBUG ${TOAST_DEBUG_FLAGS})

# C++ flags
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -fvisibility=hidden -fvisibility-inlines-hidden")
set(CMAKE_CXX_FLAGS_DEBUG ${TOAST_DEBUG_CXX_FLAGS})
set(CMAKE_CXX_FLAGS_RELEASE ${TOAST_RELEASE_CXX_FLAGS})

# Fortran OS specific compiler flags
if (CMAKE_Fortran_COMPILER_ID MATCHES MSVC)
    set ( TOAST_DEBUG_F90_FLAGS "${TOAST_DEBUG_F90_FLAGS} -fp-model")
    set ( TOAST_RELEASE_FLAGS  "${TOAST_RELEASE_F90_FLAGS} -fp-model" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set ( TOAST_DEBUG_F90_FLAGS "${TOAST_DEBUG_F90_FLAGS} -debug  -CB -CA -CU -std08 -fpp -fp-model source")
    set ( TOAST_RELEASE_FLAGS  "${TOAST_RELEASE_F90_FLAGS} -std08 -unroll -fpp -fp-model source -diag-disable 8291,8577" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set ( TOAST_DEBUG_F90_FLAGS "${TOAST_DEBUG_F90_FLAGS} -cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none")
    set ( TOAST_RELEASE_FLAGS  "${TOAST_RELEASE_F90_FLAGS} -cpp -ffree-form -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES NAG)
    set ( TOAST_DEBUG_F90_FLAGS "${TOAST_DEBUG_F90_FLAGS} -fpp -free -float-store -f2008")
    set ( TOAST_RELEASE_FLAGS  "${TOAST_RELEASE_F90_FLAGS} -fpp -free -float-store -f2008")
else ()
    message(FATAL_ERROR "OS >>> Fortran Compiler not supported!!")
endif ()

# Release or Debug build
if(CMAKE_BUILD_TYPE MATCHES Release)
    message(STATUS "Release build")
    set(F90_FLAGS ${TOAST_RELEASE_F90_FLAGS})
elseif(CMAKE_BUILD_TYPE MATCHES Debug)
    message(STATUS "Debug build")
    set(F90_FLAGS ${TOAST_DEBUG_F90_FLAGS})
else()
    message(STATUS "Default build (Release)")
    set(F90_FLAGS ${TOAST_RELEASE_F90_FLAGS})
endif()

# set the output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)

# Collect all the mod files into their own include directory
set(TOAST_MODULE_DIR ${TOAST_BUILD_DIR}/include)

# set include and link directories
include_directories (${TOAST_MODULE_DIR} ${TOAST_INCLUDE_DIR})
link_directories (${TOAST_BUILD_DIR}/lib)

# external sources
set (FORK_SOURCE_DIR ${TOAST_EXTERNAL_DIR}/fork)

# sources
add_subdirectory(${TOAST_SOURCE_DIR})
add_subdirectory(${TOAST_EXAMPLES_DIR})
add_subdirectory(${TOAST_TEST_DIR})


