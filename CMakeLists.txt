# CMake project file for forts
cmake_minimum_required (VERSION 2.8)

# Set package metadata
set(CPACK_PACKAGE_NAME "TOAST")
set(CPACK_PACKAGE_VERSION_MAJOR "0")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A straightforward lightweight unit testing library for modern Fortran")
set(CPACK_PACKAGE_VENDOR "Thomas Stainer")
set(CPACK_PACKAGE_CONTACT "Thomas Stainer <me@physicstom.com>")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://git.ccfe.ac.uk/thomasms/toast")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}")

# project name and details
project (TOAST)
enable_language (Fortran)
set(PROJECT_DESCRIPTION "${CPACK_PACKAGE_NAME}")
set(PROJECT_URL "${CPACK_PACKAGE_HOMEPAGE_URL}")

# Set default installation paths; should be invoked after setting project language(s)
include(GNUInstallDirs)

# CTest setup
include(CTest)
enable_testing()

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
set (JSON_FORTRAN_SOURCE_DIR ${TOAST_EXTERNAL_DIR}/json-fortran/src)

# sources
add_subdirectory(${TOAST_SOURCE_DIR})
add_subdirectory(${TOAST_EXAMPLES_DIR})
add_subdirectory(${TOAST_TEST_DIR})

###############################################################################
## Packaging ##################################################################
###############################################################################

### Select what to install

# Libraries
# This may require CMake 3.14 or later
install(TARGETS ${TOAST_LIB_NAME})

# Fortran module (.mod) files
# This list may contain unnecessary files; is more than toast.mod needed?
# Verify this reduced list works
install(FILES
    # "${TOAST_MODULE_DIR}/fork_m.mod"
    # "${TOAST_MODULE_DIR}/json_file_module.mod"
    # "${TOAST_MODULE_DIR}/json_kinds.mod"
    # "${TOAST_MODULE_DIR}/json_module.mod"
    # "${TOAST_MODULE_DIR}/json_parameters.mod"
    # "${TOAST_MODULE_DIR}/json_serializer_m.mod"
    # "${TOAST_MODULE_DIR}/json_string_utilities.mod"
    # "${TOAST_MODULE_DIR}/json_value_module.mod"
    # "${TOAST_MODULE_DIR}/stdout_ansi_colours_m.mod"
    # "${TOAST_MODULE_DIR}/stdout_writer_m.mod"
    "${TOAST_MODULE_DIR}/toast.mod"
    # "${TOAST_MODULE_DIR}/toast_util_m.mod"
    DESTINATION finclude/toast
)

# Examples
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/examples/"
    DESTINATION examples)

# Documentation
install(FILES LICENSE README.md DESTINATION doc)

### Select archives and installers to generate

list(APPEND CPACK_GENERATOR ZIP)

if(WIN32)
    # Set up NSIS
    find_package(NSIS)
    if(NSIS_FOUND)
        # set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/SOFIRE2.ico")
        # set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/img/SOFIRE2.ico")
        set(CPACK_NSIS_INSTALLED_ICON_NAME "Uninstall.exe")
        set(CPACK_NSIS_HELP_LINK "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_URL_INFO_ABOUT "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_MODIFY_PATH ON)
        set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)

        list(APPEND CPACK_GENERATOR NSIS)
    endif()

    # NuGet
    # TODO: Find a more robust means of detecting whether NuGet is available
    find_program(NUGET_EXECUTABLE nuget)
    if(NUGET_EXECUTABLE)
        message(STATUS "NuGet found at ${NUGET_EXECUTABLE}; needs configuration")

        # list(APPEND CPACK_GENERATOR NuGet)
    endif()

    # Set up WIX
    # TODO: Make this user-configurable or otherwise deal with version and architecture in WIX path
    set(WIX_ROOT_DIR "/Program Files (x86)/WiX Toolset v3.11")
    set(WIX_FIND_REQUIRED OFF)
    set(WIX_FIND_QUIETLY ON)
    find_package(WIX)
    # message("WIX found? ${WIX_FOUND}")
    if(WIX_FOUND)
        message(STATUS "WIX was found: WIX_FOUND = ${WIX_FOUND}")
        # The variable holding the WIX root directory used by CPack is different from the
        # variable populated by find_package(WIX) i.e. cmake/FindWix.cmake
        # Manually tell CPack where find_package() found WIX...
        set(CPACK_WIX_ROOT "${WIX_ROOT_DIR}")
        # Manually convert LICENSE to RTF format because WIX/CPack is stupid
        # set(WIX_LICENSE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.rtf")
        # set(CPACK_WIX_LICENSE_RTF "${WIX_LICENSE_FILE}")
        # install(FILES "${WIX_LICENSE_FILE}" DESTINATION doc)
        # set(CPACK_WIX_PRODUCT_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/SOFIRE2.ico")
        # Note: This should not change for the life of the product
        # Generated from guidgen.exe
        # set(CPACK_WIX_UPGRADE_GUID "")

        if(EXISTS "${WIX_CANDLE}")
            list(APPEND CPACK_GENERATOR WIX)
        else()
            message(STATUS "Why is WIX_FOUND = ${WIX_FOUND} if WIX_CANDLE (${WIX_CANDLE}) is not found?")
        endif()
    else()
        message(STATUS "WIX was not found: WIX_FOUND = ${WIX_FOUND}")
    endif()
else()
    list(APPEND CPACK_GENERATOR TGZ TBZ2)
    if(APPLE)
        # Set up DRAGNDROP
        # Add DragNDrop properties
        set(CPACK_DMG_VOLUME_NAME "${CPACK_PACKAGE_NAME} v${CPACK_PACKAGE_VERSION}")
#        set(CPACK_DMG_FORMAT "UDZO")
        set(CPACK_DMG_FORMAT "UDBZ")
#*       CPACK_DMG_DS_STORE
#*       CPACK_DMG_DS_STORE_SETUP_SCRIPT
#*       CPACK_DMG_BACKGROUND_IMAGE
#        CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK
#        CPACK_DMG_SLA_DIR
#        CPACK_DMG_SLA_LANGUAGES
#        CPACK_DMG_<component>_FILE_NAME
#        CPACK_COMMAND_HDIUTIL
#        CPACK_COMMAND_SETFILE
#        CPACK_COMMAND_REZ
        list(APPEND CPACK_GENERATOR DragNDrop)
    else()
        # Set up DEB
        # TODO: Find a more robust means of detecting whether debian packaging should be enabled
        # Note that readelf is not strictly necessary but platform is assumed
        # Debian-ish if it's present
        find_program(READELF_EXECUTABLE readelf)
        if(READELF_EXECUTABLE)
            set(CPACK_DEBIAN_PACKAGE_DESCRIPTION
                "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}")
            # set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "any")
            # Auto-detect dependencies
            set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
            # Build multiple packages
            set(CPACK_DEB_COMPONENT_INSTALL ON)
            set(CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS ON)
            # set(CPACK_DEBIAN_PACKAGE_DEBUG ON)
            # Turn off for executable-only; only needed for packaging libraries
            set(CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS ON)

            list(APPEND CPACK_GENERATOR DEB)
        endif()

        # Set up RPM
        # TODO: Find a more robust means of detecting whether RPM generator is available
        find_program(RPMBUILD_EXECUTABLE rpmbuild)
        if(RPMBUILD_EXECUTABLE)
            set(CPACK_RPM_PACKAGE_LICENSE "BSD-3")
            set(CPACK_RPM_FILE_NAME RPM-DEFAULT)
            list(APPEND CPACK_GENERATOR RPM)
        endif()
    endif()
endif()

# This must be last
include(CPack)