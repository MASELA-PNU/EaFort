!! -------------------------------------------------------------------------- !!
!                                   EaFort:Global                             !!
!! -------------------------------------------------------------------------- !!
!   Wrapper Module of JSON-Fortran for easy use.
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modEaFort
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Integer Precision
    Integer, Parameter :: IP = KIND(1)

    !!... Real Precision
    Integer, Parameter :: RP = KIND(1.D0)

    !!... Complex Precision
    Integer, Parameter :: CP = RP

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!