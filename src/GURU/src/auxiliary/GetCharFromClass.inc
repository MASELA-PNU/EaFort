!! -------------------------------------------------------------------------- !!
Subroutine GetStrFromClass( arg, str )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(*), intent(in) :: arg
    type(string)         :: str
!! -------------------------------------------------------------------------- !!

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
    End Select

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
