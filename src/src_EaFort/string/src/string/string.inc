!!... Get Integer
Function GetInt_typString(this) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Integer(IP)                  :: funcValue
    !!... Local Variables
    integer :: ios
    read( this%raw, *, iostat = ios ) funcValue
    if ( ios.NE.0 ) then
        write(*, "(a)") "[ERROR] GetInt_typString"
        write(*, "(a)") "  Conversion to integer fails."
        write(*, "(a)") this%raw
        stop
    end if
End Function

!!... Get Real
Function GetReal_typString(this) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP)                     :: funcValue
    !!... Local Variables
    integer :: ios
    read( this%raw, *, iostat = ios ) funcValue
    if ( ios.NE.0 ) then
        write(*, "(a)") "[ERROR] GetReal_typString"
        write(*, "(a)") "  Conversion to real fails."
        write(*, "(a)") this%raw
        stop
    end if
End Function
