!                                   EaFort:String                             !!
!! -------------------------------------------------------------------------- !!
!   GURU to handle the fortran program
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
Module modGURU
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
#include "GURU/CommandArgument/CommandArgument.typ"

!!... GURU defintion
#include "GURU/GURU.typ"

    Type(typGURU), target :: GURU

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Arguments Class Functions
#include "GURU/CommandArgument/CommandArgument.inc"

!!... GURU Functions associated with arguments
#include "GURU/GURU_Argument.inc"

!!... GURU Functions associated with messages
#include "GURU/GURU_Message.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
