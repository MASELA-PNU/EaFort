##... Source Files
set(ITEM_TYPE "LIB")
set(ITEM_NAME "json-fortran")
set(SRC_${ITEM_NAME}
    ##.... Wrapper Module of EaFort
    src/json_file_module.F90

    src/json_kinds.F90
    src/json_module.F90
    src/json_parameters.F90
    src/json_string_utilities.F90
    src/json_value_module.F90
)

##... Compile
AutoCompile(
    IS_COMPILE_LIB      True
    IS_DYNAMIC_LIB      False
    INSTALL_NAME        "${LIB_NAME_${ITEM_NAME}}"
    INSTALL_PATH        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
)
