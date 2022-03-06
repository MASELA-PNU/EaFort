!! -------------------------------------------------------------------------- !!
Subroutine GetStrFromClass( arg, str, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(*), intent(in) :: arg
    type(string)         :: str
    Logical, Optional    :: isError
!! -------------------------------------------------------------------------- !!

    isError = .FALSE.
    Select Type (arg)

        Type is ( Character(len=*) )
            str = arg
        Type is ( Integer )
            str = arg
        Type is ( real(4) )
            str = arg
        Type is ( Double Precision )
            str = arg
        Type is ( Logical )
            if (arg.eqv..true.) then
                str = ".true."
            else
                str = ".false."
            end if
        Class default
            str = ""
            isError = .TRUE.
    End Select

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
