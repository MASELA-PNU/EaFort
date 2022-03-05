!! -------------------------------------------------------------------------- !!
Subroutine WriteMessage(msg, unit, color, isAdvance)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: msg
    Integer, intent(in), optional           :: unit
    Character(len=*), intent(in), optional  :: color
    Logical, intent(in), Optional           :: isAdvance
!! -------------------------------------------------------------------------- !!
    Integer :: unit_
    Character(len=LEN_COLOR) :: color_
    Logical :: isAdvance_
    Character(len=:), Allocatable :: wrtMessage

    unit_      = 6
    color_     = ""
    isAdvance_ = .TRUE.
    If ( Present(unit) )  unit_          = unit
    If ( Present(color) ) color_         = color
    If ( Present(isAdvance) ) isAdvance_ = isAdvance

    If ( unit_.ne.6 ) then
        If ( isAdvance_.EQV..TRUE. ) then
            write(unit_, "(a)" ) msg
        else
            write(unit_, "(a)", advance = "no") msg
        end if
    end if


    if ( present(color) ) then
        wrtMessage = Colorize(msg, color)
    else
        wrtMessage = msg
    end if

    If ( isAdvance_.EQV..TRUE. ) then
        write(*, "(a)" ) wrtMessage
    else
        write(*, "(a)", advance = "no") wrtMessage
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine WritePrefixMessage( prefix, prefixColor, msg, head, &
    &   errValue, refValue, unit, isColor, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: prefix
    Character(len=*), intent(in)            :: msg
    Character(len=*), intent(in), Optional  :: head
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
    type(string)              :: str, headStr
    type(string), Allocatable :: strVect(:)
    Integer :: nMsgLine, iMsg
    Logical :: isStop_, isEndEmptyLine

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

    headStr = ""
    if ( present(head) ) headStr = head

    isEndEmptyLine = .FALSE.
    if (present(errValue)) then
        if (len_trim(errValue).ge.1) isEndEmptyLine = .TRUE.
    end if

    if (present(refValue)) then
        if (len_trim(refValue).ge.1) isEndEmptyLine = .TRUE.
    end if

    !!----- File
    If ( unit_.ne.6 ) then

        !!... Write Header
        Write(unit_,"(a)") prefix//headStr%Chars()
        write(unit_,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(unit_,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(unit_,"(a)")

        !!... Write Given Value
        If ( Present(errValue) ) then
            if (len_trim(errValue).ge.1) write(unit_,"(a)") INDENT//PREFIX_GIVEN//errValue

        End if

        !!... Write Reference Value
        If ( Present(refValue) ) then
            if (len_trim(refValue).ge.1) write(unit_,"(a)") INDENT//PREFIX_REF//refValue
        End if

        if ( isEndEmptyLine ) then
            write(unit_,"(a)")
        end if

    end if

    !!----- Command Windows
    Block

        !!... Write Header
        Write(*,"(a)") Colorize(prefix, prefixColor_)//Colorize(headStr%Chars(), headColor)
        write(*,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(*,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(*,"(a)")

        !!... Write Given Value
        If ( Present(errValue) ) then
            if (len_trim(errValue).ge.1) write(*,"(a)") INDENT//PREFIX_GIVEN//errValue
        End if

        !!... Write Reference Value
        If ( Present(refValue) ) then
            if (len_trim(refValue).ge.1) write(*,"(a)") INDENT//PREFIX_REF//refValue
        End if

        if ( isEndEmptyLine ) then
            write(*,"(a)")
        end if

    End Block

    If ( isStop_ ) then
        write(*,"(a)") "Please insert any key to exit."
        read(*,*)
        Stop
    End If

    !!... Free the memory
    Call str%Free()
    Call headStr%Free()
    do iMsg = 1, nMsgLine
        Call strVect%Free()
    end do
    If (Allocated(strVect)) deallocate(strVect)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
