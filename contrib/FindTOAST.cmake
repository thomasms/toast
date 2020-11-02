set(TOAST_LIB_NAME toast)
set(TOAST_FOUND OFF)

# Set TOAST_ROOT and TOAST_MODULE_PATH on the command line, e.g.:
# cmake -D TOAST_ROOT:PATH=/home/myname/.local -D TOAST_MODULE_PATH:PATH=/home/myname/.local/fortran_modules/toast ..

# The following are defined in the root CMakeLists.txt file
# set(TOAST_ROOT "" CACHE PATH "Installation root of TOAST library")
# set(TOAST_MODULE_PATH "" CACHE PATH "Directory containing TOAST Fortran module (.mod) files")

# Set custom search paths for TOAST installation
if(IS_DIRECTORY "${TOAST_ROOT}")
    set(SEARCH_TOAST_LIB ${TOAST_ROOT}/lib)
    set(SEARCH_TOAST_MOD ${TOAST_ROOT}/include ${TOAST_ROOT}/module
        ${TOAST_ROOT}/finclude ${TOAST_ROOT}/finclude/toast)
endif()

# Set even-more-custom search path for TOAST .mod file because
# absolutely nobody knows where .mod files get installed...
if(IS_DIRECTORY "${TOAST_MODULE_PATH}")
    list(APPEND SEARCH_TOAST_MOD "${TOAST_MODULE_PATH}")
endif()

find_library(TOAST_LIBRARY
    NAMES toast
    PATHS ${SEARCH_TOAST_LIB}
)

# message(STATUS "Debug: SEARCH_TOAST_MOD=${SEARCH_TOAST_MOD}")
find_file(TOAST_MODULE_FILE
    NAMES toast.mod
    PATHS ${SEARCH_TOAST_MOD}
)

# Set TOAST_FOUND if both TOAST_LIBRARY and TOAST_MODULE_FILE are found
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(TOAST DEFAULT_MSG
    TOAST_LIBRARY TOAST_MODULE_FILE)

if(TOAST_FOUND)
    ##### Set Output Variables #####

    # Set the following:
    # - TOAST_LIB_NAME (at top; "toast")
    # - TOAST_LIBRARY_DIR
    # - TOAST_MODULE_DIR
    # - TOAST_MODULE_FILE (from find_file())
    get_filename_component(TOAST_LIBRARY_DIR "${TOAST_LIBRARY}" DIRECTORY)
    get_filename_component(TOAST_MODULE_DIR "${TOAST_MODULE_FILE}" DIRECTORY)
    message(STATUS "Found TOAST library under ${TOAST_LIBRARY_DIR}")
else()
    message(STATUS "Cannot find TOAST (is TOAST_ROOT set? '${TOAST_ROOT}')")
endif()

# These variables are set to be compatible with the naming scheme used
# in original TOAST example CMake setup; see
# build/TOAST-source/examples/example1/CMakeLists.txt
# - TOAST_LIB_NAME (= "toast")
# - TOAST_LIBRARY_DIR
# - TOAST_MODULE_DIR

# Note: This needs to be manually added to the list of source files
# required for unit tests
# - TOAST_MODULE_FILE
