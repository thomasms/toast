# Overview:
#
# Create a test target and executable with name specified by TARGET
# composed of source files listed in SOURCES with the targets specified
# by DEPENDENCIES set as target/test dependencies. A test is added with
# the TARGET name under the configurations Debug, Release, and ""
# (the latter to allow the test to run when no configuration is
# specified). The test will be run in the working directory
# specified by RUN_DIRECTORY. TIMEOUT and WILL_FAIL may be specified to
# change allowable test runtime (30 seconds by default) or the
# expectation the test will pass. DEBUG shows parsed function arguments
# but has no other effect.
#
# Arguments:
#
# - TARGET specifies the test target and executable name. Single value,
#   required.
# - SOURCES indicates a list of source files required to build the test
#   application. Multi-value, required.
# - RUN_DIRECTORY specifies the directory in which the test application
#   should be run; this directory should exist prior to the test run.
#   Single value. Optional but recommended; if omitted,
#   CMAKE_CURRENT_BINARY_DIR will be specified as the test's
#   WORKING_DIRECTORY.
# - DEPENDENCIES indicates a list of target names which are required to
#   be built prior to running the test application. Multivalue, may be
#   empty. Note there is an implied dependency on TOAST_external which
#   is handled automatically in this function so it is not necessary to
#   specify TOAST_external as a dependency here.
# - TIMEOUT specifies the maximum expected run time of the test, in
#   seconds. Single value. Optional; will default to 30 seconds if
#   omitted.
# - WILL_FAIL - if present as an argument, WILL_FAIL will be set to
#   TRUE for the test (i.e. test is expected to fail). No value.
#   Optional; default is WILL_FAIL FALSE (test expected to pass)
# - DEBUG - if present, will print the parsed value of each argument
#   passed to this function
#
# Notes:
#
# When compiling the test, Fortran_MODULE_DIRECTORY is set to
# ${CMAKE_CURRENT_BINARY_DIR}/${test_name}_include
# (e.g. ./build/mytest_include) to prevent collisions with .mod
# (module) files built by other targets.
#
# Note that this is decoupled from BuildTOAST.cmake to allow TOAST unit
# tests if TOAST is installed via a different method, e.g. FindTOAST.cmake.
# See the limitation regarding expectations of variables TOAST_LIB_NAME,
# etc. being set prior to using this function.
#
# Limitations:
#
# - This function depends on TOAST_MODULE_FILE, TOAST_MODULE_DIR,
#   TOAST_LIBRARY_DIR, and TOAST_LIB_NAME which it inherits from current
#   context. See BuildTOAST.cmake and FindTOAST.cmake for details.
# - No provision is made for specifying library or include directories
#   or specific libraries to link the test application against;
#   manually override settings made here by using
#   `set_target_properties(${test_name} ...)` after calling this
#   function
# - Unit tests are expected be single commands with no arguments
#   (bare executable name)
# - If TOAST is installed locally, the target TOAST_external may not be
#   defined.
function(add_toast_unit_test)
    # Define the supported set of keywords
    set(prefix ARG)
    set(noValues WILL_FAIL DEBUG)
    set(singleValues TARGET RUN_DIRECTORY TIMEOUT)
    set(multiValues SOURCES DEPENDENCIES)

    cmake_parse_arguments(
        ${prefix}
        "${noValues}" "${singleValues}" "${multiValues}"
        ${ARGN}
    )

    if(${ARG_DEBUG})
        # Log details for each supported keyword
        message("Option summary:")
        foreach(arg IN LISTS noValues)
            if(${prefix}_${arg})
                message(" ${arg} enabled")
            else()
                message(" ${arg} disabled")
            endif()
        endforeach()

        foreach(arg IN LISTS singleValues multiValues)
            # Single argument values will print as a string
            # Multiple argument values will print as a list
            message(" ${arg} = ${${prefix}_${arg}}")
        endforeach()
    endif()

    if(TOAST_FOUND)
        message(STATUS "Adding unit test ${ARG_TARGET}")
        set(TEST_FORTRAN_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/${ARG_TARGET}_include")
        file(MAKE_DIRECTORY "${TEST_FORTRAN_MODULE_DIR}")
        add_executable(${ARG_TARGET} ${ARG_SOURCES} ${TOAST_MODULE_FILE})
        set_target_properties(
            ${ARG_TARGET}
            PROPERTIES
            OUTPUT_NAME ${ARG_TARGET}
            DEPENDS TOAST_external
            Fortran_MODULE_DIRECTORY ${TEST_FORTRAN_MODULE_DIR}
            INCLUDE_DIRECTORIES ${TOAST_MODULE_DIR}
            LINK_DIRECTORIES ${TOAST_LIBRARY_DIR}
            LINK_LIBRARIES ${TOAST_LIB_NAME}
        )

        add_test(NAME ${ARG_TARGET}
            COMMAND $<TARGET_FILE:${ARG_TARGET}>
            CONFIGURATIONS Debug Release ""
        )

        if("${ARG_TIMEOUT}" GREATER 1)
            # Set timeout if specified
            set_tests_properties(${ARG_TARGET} PROPERTIES
                TIMEOUT "${ARG_TIMEOUT}")
        else()
            # Set default timeout of 30 seconds
            set_tests_properties(${ARG_TARGET} PROPERTIES TIMEOUT 30)
        endif()

        if("${ARG_RUN_DIRECTORY}" STREQUAL "")
            # Default to current binary directory if ARG_RUN_DIRECTORY
            # looks undefined
            set_tests_properties(${ARG_TARGET} PROPERTIES
                WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
        else()
            # Set test WORKING_DIRECTORY to ARG_RUN_DIRECTORY
            set_tests_properties(${ARG_TARGET} PROPERTIES
                WORKING_DIRECTORY "${ARG_RUN_DIRECTORY}")
        endif()

        if(${ARG_WILL_FAIL})
            set_tests_properties(${ARG_TARGET} PROPERTIES WILL_FAIL TRUE)
        endif()

        foreach(dep IN LISTS ${ARG_DEPENDENCIES})
            add_dependencies(${ARG_TARGET} "${dep}")
        endforeach()
    else()
        message(WARNING "TOAST is not available; add_toast_unit_test(${ARG_TARGET} ...) is ignored")
    endif()

endfunction()
# __END__