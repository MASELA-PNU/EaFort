!                                   EaFort:String                             !!
!! -------------------------------------------------------------------------- !!
!   Info to handle the fortran program
!
!    - Error / Warning Message
!    - Argument by command line
!    - Explanation of program
!    - File I/O Handling
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modInfoFortran
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort

    Use FACE, only: Colorize    !!... Third Party String Module (Colorize)
    Use stringifor              !!... Third Party String Module

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Prefix before message
    Character(len=7), Parameter, Private :: PREIX_ERROR = "[ERROR]"

    Character(len=9), Parameter, Private :: PREIX_WARN  = "[WARNING]"

    Character(len=7), Parameter, Private :: PREIX_DEBUG = "[DEBUG]"

    Character(len=2), Parameter, Private :: INDENT  = "  "
    Character(len=4), Parameter, Private :: INDENT2 = "    "
    Character(len=6), Parameter, Private :: INDENT3 = "      "
    Character(len=8), Parameter, Private :: INDENT4 = "        "

!!... Arguments Class Definition
#include "InfoArg/CommandArgument/CommandArgument.typ"

!!... GURU defintion
#include "InfoArg/InfoArg.typ"

    Type(typInfoArg), target :: InfoArg

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Arguments Class Functions
#include "InfoArg/CommandArgument/CommandArgument.inc"

!!... GURU Functions associated with arguments
#include "InfoArg/InfoArg.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
