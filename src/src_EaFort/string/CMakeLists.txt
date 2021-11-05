set(ITEM_NAME "string")
set(SRC_CURRENT_${ITEM_NAME}
    /src/modString.F90
    /src/ThirdParty/PENF/penf_global_parameters_variables.F90
    /src/ThirdParty/PENF/penf_stringify.F90
    /src/ThirdParty/PENF/penf_b_size.F90
    /src/ThirdParty/PENF/penf.F90
    /src/ThirdParty/FACE/face.F90
    /src/ThirdParty/StringiFor/stringifor_string_t.F90
    /src/ThirdParty/StringiFor/stringifor.F90
    /src/ThirdParty/BeFoR64/befor64.F90
    /src/ThirdParty/BeFoR64/befor64_pack_data_m.F90
)

foreach( SUB_SRC_NAME IN LISTS SRC_CURRENT_${ITEM_NAME} )

    list( APPEND SRC_GATHER_${ITEM_NAME}
            ${CMAKE_CURRENT_SOURCE_DIR}/${SUB_SRC_NAME}
        )

endforeach()

set( SRC_${ITEM_NAME} "${SRC_GATHER_${ITEM_NAME}}" PARENT_SCOPE )
