!! -------------------------------------------------------------------------- !!
!                                   EaFort:JSON                               !!
!! -------------------------------------------------------------------------- !!
!   Wrapper Module of JSON-Fortran for easy use.
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modJSON
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use modGURU         !!... GURU Module
    Use modString       !!... String Module

    Use json_module, only: &
    &   typJSONCore => json_core, &
    &   typJSON     => json_value, &
    &   typJSONFile => json_file

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... JSON Core
    Type(typJSONCore) :: jsonCore

!!... Functions associated with JSON Core, File, Print, Clone, Close
#include "routines/jsonCore.proc"

!!... Functions associated with basic manipulations
#include "routines/jsonFunc.proc"

!!... Functions associated with variable manipulations
#include "routines/jsonVarFunc.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Functions associated with JSON Core, File, Print, Clone, Close
#include "routines/jsonCore.inc"

!!... Functions associated with basic manipulations
#include "routines/jsonFunc.inc"

!!... Functions associated with variable manipulations
#include "routines/jsonVarFunc.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
