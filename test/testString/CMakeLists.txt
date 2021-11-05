set(ITEM_TYPE "test")
set(ITEM_NAME "string")
set(SRC_${ITEM_NAME}
    src/testString.F90
)

set(DEPEND_${ITEM_NAME}
    EaFort
)

##... Compile
AutoCompile(
    IS_COMPILE_EXE      True
    INSTALL_NAME        "${TEST_NAME_${ITEM_NAME}}"
    INSTALL_PATH        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
    INSTALL_DEPEND      "${DEPEND_${ITEM_NAME}}"
)
