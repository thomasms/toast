project (TOAST_TEST_TESTCASE_RESET)
enable_language (Fortran)

set (TOAST_TEST_NAME test_testcase_reset)
set (TOAST_TEST_SOURCES test.F90)

set_source_files_properties(${TOAST_TEST_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

link_directories (${TOAST_BUILD_DIR}/lib)

add_executable(${TOAST_TEST_NAME} ${TOAST_TEST_SOURCES})

target_link_libraries (${TOAST_TEST_NAME}
                       ${TOAST_LIB_NAME})

set_target_properties(${TOAST_TEST_NAME}
                      PROPERTIES OUTPUT_NAME ${TOAST_TEST_NAME}
                      RUNTIME_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/bin/tests
                      LIBRARY_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      ARCHIVE_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      Fortran_MODULE_DIRECTORY ${TOAST_MODULE_DIR})

add_test(NAME ${TOAST_TEST_NAME} COMMAND ${TOAST_TEST_NAME})