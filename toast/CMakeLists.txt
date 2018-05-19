project (TOASTLIB)
enable_language (Fortran)

set (TOASTJSON_SOURCES ${JSON_FORTRAN_SOURCE_DIR}/json_kinds.F90
                       ${JSON_FORTRAN_SOURCE_DIR}/json_parameters.F90
                       ${JSON_FORTRAN_SOURCE_DIR}/json_string_utilities.F90
                       ${JSON_FORTRAN_SOURCE_DIR}/json_value_module.F90
                       ${JSON_FORTRAN_SOURCE_DIR}/json_file_module.F90
                       ${JSON_FORTRAN_SOURCE_DIR}/json_module.F90
                       json/json_serializer_m.f90)

set (TOASTLIB_SOURCES ${TOASTJSON_SOURCES}
                      ${FORK_SOURCE_DIR}/fork_m.F90
                      toast_util_m.F90
                      toast_test_case_m.F90
                      toast_test_suite_m.F90
                      toast.F90)



set_source_files_properties(${TOASTLIB_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

add_library(${TOAST_LIB_NAME} ${TOASTLIB_SOURCES})
set_target_properties(${TOAST_LIB_NAME}
                      PROPERTIES OUTPUT_NAME ${TOAST_LIB_NAME}
                      RUNTIME_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/bin
                      LIBRARY_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      ARCHIVE_OUTPUT_DIRECTORY ${TOAST_BUILD_DIR}/lib
                      Fortran_MODULE_DIRECTORY ${TOAST_MODULE_DIR})

