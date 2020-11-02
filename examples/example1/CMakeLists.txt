project (TOASTEXAMPLE)
enable_language (Fortran)

set (TOASTEXAMPLE_NAME toastexample1)
set (TOASTEXAMPLE_SOURCES example.F90)

set_source_files_properties(${TOASTEXAMPLE_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

link_directories (${TOAST_BUILD_DIR}/lib)

add_executable(${TOASTEXAMPLE_NAME} ${TOASTEXAMPLE_SOURCES})
target_link_libraries (${TOASTEXAMPLE_NAME}
                       ${TOAST_LIB_NAME})
set_target_properties(${TOASTEXAMPLE_NAME}
                      PROPERTIES OUTPUT_NAME ${TOASTEXAMPLE_NAME}
                      RUNTIME_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/bin
                      LIBRARY_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      ARCHIVE_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      Fortran_MODULE_DIRECTORY ${TOAST_MODULE_DIR})

add_test(NAME ${TOASTEXAMPLE_NAME} COMMAND ${TOASTEXAMPLE_NAME})

set_tests_properties(${TOASTEXAMPLE_NAME}
    PROPERTIES
    WILL_FAIL TRUE
)