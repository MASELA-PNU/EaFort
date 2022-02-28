!! -------------------------------------------------------------------------- !!
Subroutine WriteMessage(msg, unit, color)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: msg
    Integer, intent(in), optional           :: unit
    Character(len=*), intent(in), optional  :: color
!! -------------------------------------------------------------------------- !!
    Integer :: unit_
    Character(len=LEN_COLOR) :: color_

    unit_  = 6
    color_ = ""
    If ( Present(unit) )  unit_ = unit
    If ( Present(color) ) color_ = color

    If ( unit_.ne.6 ) then
        write(unit_, msg) msg
        write(*, msg) Colorize(msg, color_)
    else
        write(*, msg) Colorize(msg, color_)
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine WritePrefixMessage( prefix, prefixColor, head, msg, &
    &   errValue, refValue, unit, isColor, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: prefix
    Character(len=*), intent(in)            :: head
    Character(len=*), intent(in)            :: msg
    Character(len=*), intent(in), Optional  :: errValue
    Character(len=*), intent(in), Optional  :: refValue
    Integer, intent(in), optional           :: unit
    Logical, intent(in), optional           :: isColor
    Character(len=*), optional, intent(in)  :: prefixColor
    Logical, intent(in), optional           :: isStop
!! -------------------------------------------------------------------------- !!
    Logical :: isColor_
    Character(LEN_COLOR) :: prefixColor_, headColor
    Integer :: unit_
    type(string)              :: str
    type(string), Allocatable :: strVect(:)
    Integer :: nMsgLine, iMsg
    Logical :: isStop_

    isColor_ = .FALSE.
    if ( Present(isColor) ) isColor_ = isColor

    isStop_ = .FALSE.
    If ( Present(isStop) ) isStop_ = isStop

    prefixColor_ = ""
    headColor    = ""
    If (isColor_) then
        headColor   = HEAD_COLOR
        If ( Present(prefixColor) ) prefixColor_ = prefixColor
    end if

    unit_ = 6
    if ( Present(unit) ) unit_ = unit

    str = msg
    Call str%Split( tokens = strVect, sep = lineSep )
    nMsgLine =  size(strVect)

    If ( unit_.ne.6 ) then

        !!----- File

        !!... Write Header
        Write(unit_,"(a)") prefix//head
        write(unit_,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(unit_,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(unit_,"(a)")

        !!... Write Given Value
        If ( Present(errValue) ) then
            write(unit_,"(a)") INDENT//PREFIX_GIVEN//errValue
        End if

        !!... Write Reference Value
        If ( Present(errValue) ) then
            write(unit_,"(a)") INDENT//PREFIX_REF//refValue
        End if

        !!----- Command Windows

        !!... Write Header
        Write(*,"(a)") Colorize(prefix, prefixColor_)//Colorize(head, headColor)
        write(*,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(*,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(*,"(a)")

        !!... Write Given Value
        If ( Present(errValue) ) then
            write(*,"(a)") INDENT//PREFIX_GIVEN//errValue
        End if

        !!... Write Reference Value
        If ( Present(errValue) ) then
            write(*,"(a)") INDENT//PREFIX_REF//refValue
        End if

    else

        !!... Write Header
        Write(*,"(a)") Colorize(prefix, prefixColor)//Colorize(head, headColor)
        write(*,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(*,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(*,"(a)")

        !!... Write Given Value
        If ( Present(errValue) ) then
            write(*,"(a)") INDENT//PREFIX_GIVEN//errValue
        End if

        !!... Write Reference Value
        If ( Present(errValue) ) then
            write(*,"(a)") INDENT//PREFIX_REF//refValue
        End if
    end if
    write(*,"(a)")

    If ( isStop_ ) then
        write(*,"(a)") "Please insert any key to exit."
        read(*,*)
        Stop
    End If

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
