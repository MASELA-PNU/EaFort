!! -------------------------------------------------------------------------- !!
!   EaFort-JSON
!   Wrapper Module of JSON-Fortran for easy use.
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modJSON
!! -------------------------------------------------------------------------- !!

    Use json_module

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... JSON Core
    Type(json_core) :: JSON


!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

Subroutine JSON_ReadFile( filePath, json )
    Implicit None
    Character(len=*), intent(in) :: filePath
    Type(json_value)             :: json

End Subroutine

Subroutine JSON_WriteFile( filePath, json )
    Implicit None
    Character(len=*), intent(in) :: filePath
    Type(json_value)             :: json

End Subroutine

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
