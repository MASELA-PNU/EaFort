!                                   EaFort:FileIO                             !!
!! -------------------------------------------------------------------------- !!
!   File IO Module
!
!    - File I/O Handling
!
!   author: Young-Myung Choi
!   date: 2021-03-07
!
!! -------------------------------------------------------------------------- !!
Module modFileIO
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use modGURU         !!... GURU Module

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... file index (input port)
    Integer, Parameter :: FID_IN_DEFAULT = 5

    !!... file index (output port)
    Integer, Parameter :: FID_OUT_DEFAULT = 6

!!... Global File IO Class definitions
#include "globalFileIO/globalFileIO.typ"

    !!... Global File IO
    Type(typGlobalFileIO) :: globalFileIO

!!... File IO Class definitions
#include "fileIO/fileIO.typ"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Global File IO Class definitions
#include "globalFileIO/globalFileIO.inc"

!!... File IO Class functions
#include "fileIO/fileIO.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
