##... Source Files
set(ITEM_TYPE "LIB")
set(ITEM_NAME "EaFort")
set(SRC_${ITEM_NAME}
    ##.... Wrapper Module of EaFort
    pkgEaFort.F90
)

set(LIST_SUB_ITEM_NAME
    JSON
)

foreach( SUB_ITEM_NAME IN LISTS LIST_SUB_ITEM_NAME )
    message( "${SUB_ITEM_NAME}" )
    add_subdirectory( ${SUB_ITEM_NAME} )

    # message("${SUB_ITEM_NAME}")
    # message("${SRC_${SUB_ITEM_NAME}}")

    # set( SRC_${ITEM_NAME} ${SRC_${ITEM_NAME}} ${SRC_${SUB_ITEM_NAME}} )

endforeach()

message("${SRC_${ITEM_NAME}}")

##... Compile
AutoCompile(
    IS_COMPILE_LIB      True
    IS_DYNAMIC_LIB      False
    INSTALL_NAME        "${LIB_NAME_${ITEM_NAME}}"
    INSTALL_PATH        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
)
