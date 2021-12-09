##... Source Files
set(ITEM_TYPE "LIB")
set(ITEM_NAME "StringiFor")
set(SRC_${ITEM_NAME}
    ##.... Wrapper Module of EaFort
    src/BeFoR64/befor64_pack_data_m.F90
    src/BeFoR64/befor64.F90

    src/FACE/face.F90

    src/PENF/penf_b_size.F90
    src/PENF/penf.F90
    src/PENF/penf_global_parameters_variables.F90
    src/PENF/penf_stringify.F90

    src/StringiFor/stringifor.F90
    src/StringiFor/stringifor_string_t.F90
)

##... Compile
AutoCompile(
    IS_COMPILE_LIB      True
    IS_DYNAMIC_LIB      False
    INSTALL_NAME        "${LIB_NAME_${ITEM_NAME}}"
    INSTALL_PATH        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
)
