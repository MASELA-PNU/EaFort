!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typInfoArg( &
    &   this, projectName, description, authors, date, version)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(inout)          :: this
    Character(len=*), intent(in)           :: projectName
    Character(len=*), intent(in), Optional :: description
    Character(len=*), intent(in), Optional :: authors
    Character(len=*), intent(in), Optional :: date
    Character(len=*), intent(in), Optional :: version
!! -------------------------------------------------------------------------- !!

    if ( len_trim(projectName).LE.0 ) then
        write(*,"(a)") PREIX_WARN//" Initialize_typInfoArg"
        write(*,"(a)")
        write(*,"(a)") INDENT//"The length of 'projectName' is less than 1."
        write(*,"(a)")
    end if

    this%projectName = projectName

    if ( present(description) ) this%description = description
    if ( present(authors) )     this%authors     = authors
    if ( present(date) )        this%date        = date
    if ( present(version) )     this%version     = version

    !!... Set basic arguments
    this%nArguments = 1
    Allocate( this%arguments(this%nArguments) )

    Call this%arguments(1)%Initialize( &
    &   key           = "help",                   &
    &   description   = "Print help messages",    &
    &   isRequiredKey = .FALSE.                   )

    ! Call this%arguments(2)%Initialize( &
    ! &   key           = "log",               &
    ! &   description   = "Log File Path",     &
    ! &   isRequiredKey = .FALSE.,             &
    ! &   nRequiredArg  = 1                    )
    !
    ! Call this%arguments(3)%Initialize( &
    ! &   key           = "color",             &
    ! &   isRequiredKey = .FALSE.,             &
    ! &   description   = "Color Mode"         )
    !
    ! Call this%arguments(4)%Initialize( &
    ! &   key           = "debug",             &
    ! &   isRequiredKey = .FALSE.,             &
    ! &   description   = "Debug Mode"         )

    !!... Update Basic arguments
    Call this%Update()

    !!... Color & Debug Mode
    ! If ( this%IsFoundKey( key = "color" ) ) this%isColor_ = .TRUE.
    ! If ( this%IsFoundKey( key = "debug" ) ) this%isDebug_ = .TRUE.

    !!... Logger: See with GURU_Message.inc

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function IsFoundKey_typInfoArg( this, key ) result(isFound)
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(in) :: this
    Character(len=*), intent(in)  :: key
    Logical :: isFound
!! -------------------------------------------------------------------------- !!
    Integer :: iArg
    isFound = .FALSE.
    do iArg = 1, this%nArguments
        if ( this%arguments(iArg)%key.EQ.key ) then
            if ( this%arguments(iArg)%isFound ) then
                isFound = .TRUE.
                return
            end if
        end if
    end do
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!




!! -------------------------------------------------------------------------- !!
Subroutine Destroy_typInfoArg(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer :: nArg, iArg
    Call this%projectName%Free()
    Call this%description%Free()
    Call this%authors%Free()
    Call this%date%Free()
    Call this%version%Free()

    ! this%isColor_ = .TRUE.
    ! this%isDebug_ = .FALSE.

    this%nArguments = 0

    If ( Allocated(this%arguments) ) then
        nArg = size(this%arguments)
        do iArg =1, nArg
            Call this%arguments(iArg)%Destroy()
        end do
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine UpdateAndPrint_typInfoArg(this)
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
Class(typInfoArg), intent(inout) :: this
!! -------------------------------------------------------------------------- !!

    !!... Update Key
    Call this%Update()

    !!... Print Description
    Call this%PrintDescription()

    !!... If --help is given, print help message and stop
    If ( this%IsFoundKey( key = "help" ) ) then
        Call this%PrintHelp()
        stop
    end if

    !!... Print Given Key Information
    Call this%PrintGivenKey()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine PrintDescription_typInfoArg(this, ioUnit)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(in) :: this
    Integer, optional             :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: ioUnit_

    !!... Set Print Mode
    ! isColor = this%isColor_
    ioUnit_ = 6
    if ( present(ioUnit) ) then
        if (ioUnit.EQ.6) then
            ! isColor = this%isColor_
            isColor = .FALSE.
        else
            isColor = .FALSE.
        end if
        ioUnit_ = ioUnit
    end if

    !!... Write Project Name
    blkWriteProjectName : Block
        Character(len=17) :: color1, color2
        color1 = ""
        color2 = ""
        If ( isColor ) then
            color1 = "Green"
            color2 = "Yellow"
        end if

        Write(ioUnit_, "(a)") ""
        Write(ioUnit_, "(a)") Colorize("Project", color1)// " : "//Colorize(this%projectName%Chars(), color2)
        Write(ioUnit_, "(a)") ""

    End Block blkWriteProjectName

    !!... Write Description
    blkWriteDescription : Block
        Type(string), Allocatable :: multipleLines(:)
        Integer :: nStr, iStr
        Character(len=17) :: color1

        If ( Allocated(this%description%raw) ) then

            !!... Split the description into the multiple lines and print
            Call this%description%Split( multipleLines, "\\" )
            nStr = size( multipleLines )

            color1 = ""
            If ( isColor ) color1 = "CYAN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Description", color1)
            Write(ioUnit_, "(a)") ""

            If ( nStr.GE.1 ) then
                do iStr = 1, nStr
                    Write(ioUnit_, "(a)") INDENT2//multipleLines(iStr)%Chars()
                end do
            end if
            Write(ioUnit_, "(a)") ""

        end if

    End Block blkWriteDescription

    !!... Write Authors
    blkWriteAuthors : Block
        Type(string), Allocatable :: multipleLines(:)
        Integer :: nStr, iStr
        Character(len=17) :: color1

        If ( Allocated(this%authors%raw) ) then

            !!... Split the description into the multiple lines and print
            Call this%authors%Split( multipleLines, "\\" )
            nStr = size( multipleLines )

            color1 = ""
            If ( isColor ) color1 = "CYAN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Authors", color1)
            Write(ioUnit_, "(a)") ""

            If ( nStr.GE.1 ) then
                do iStr = 1, nStr
                    Write(ioUnit_, "(a)") INDENT2//multipleLines(iStr)%Chars()
                end do
            end if
            Write(ioUnit_, "(a)") ""

        end if

    End Block blkWriteAuthors

    !!... Write Version & Date
    blkWriteVersionDate : Block
        Character(len=17) :: color1

        color1 = ""
        If ( isColor ) color1 = "CYAN"

        If ( Allocated(this%version%raw) ) then
            Write(ioUnit_, "(a)") INDENT//Colorize("Version", color1)//" : "//this%version%Chars()
            Write(ioUnit_, "(a)") ""
        End if

        If ( Allocated(this%date%raw) ) then
            Write(ioUnit_, "(a)") INDENT//Colorize("Date   ", color1)//" : "//this%date%Chars()
            Write(ioUnit_, "(a)") ""
        end if

    End Block blkWriteVersionDate

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine PrintGivenKey_typInfoArg(this, ioUnit)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(in) :: this
    Integer, optional             :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: ioUnit_

    !!... Set Print Mode
    ! isColor = this%isColor_
    ioUnit_ = 6
    if ( present(ioUnit) ) then
        if (ioUnit.EQ.6) then
            ! isColor = this%isColor_
        else
            isColor = .FALSE.
        end if
        ioUnit_ = ioUnit
    end if

    !!... Write Required Key Information
    blkWriteRequiredKey : Block
        Integer           :: iArg, nArg
        Character(len=17) :: color

        If ( this%nArguments.GE.1 ) then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color = ""
                If ( isColor ) color = "MAGENTA_INTENSE"
                Write(ioUnit_, "(a)") INDENT//Colorize("Required", color)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintInfo(ioUnit, isColor)
                    end if
                end do
            end if

        End If

    End Block blkWriteRequiredKey

    !!... Write Optional Key Information
    blkWriteOptionalKey : Block
        Integer :: iArg, nArg
        Character(len=17) :: color

        if ( this%nArguments.GE.1 ) then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color = ""
                If ( isColor ) color = "MAGENTA_INTENSE"
                Write(ioUnit_, "(a)") INDENT//Colorize("Optional", color)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( .NOT.this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintInfo(ioUnit, isColor)
                    end if
                end do
            end if

        end if

    End Block blkWriteOptionalKey

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Update_typInfoArg(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer :: iArg

    do iArg = 1, this%nArguments
        Call this%arguments(iArg)%Update()
    end do

    do iArg = 1, this%nArguments
        if ( this%arguments(iArg)%isRequiredKey.AND.&
        &    (.NOT.this%arguments(iArg)%isFound) ) then
            write(*,"(a)") PREIX_ERROR//" Update_typInfoArg"
            write(*,"(a)")
            write(*,"(a)") INDENT//"Required key is not given."
            write(*,"(a)") INDENT//"key: --"//this%arguments(iArg)%key
            write(*,"(a)")
            stop
        end if
    end do

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine AddKey_typInfoArg( &
    &   this, key, singleKey, description, isRequiredKey, nRequiredArg )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(inout)         :: this
    Character(len=*), intent(in)             :: key
    Character(len=*), Optional, intent(in)   :: singleKey
    Character(len=*), Optional, intent(in)   :: description
    Logical, intent(in), Optional            :: isRequiredKey
    Integer, intent(in), Optional            :: nRequiredArg
!! -------------------------------------------------------------------------- !!
    Logical :: isRequiredKey_
    Integer :: iArg, nPreviousArg
    Type(typCommandArgument), allocatable :: tmpArguments(:)

    isRequiredKey_ = .TRUE.
    If (present(isRequiredKey)) isRequiredKey_ = isRequiredKey

    !!... Check key is already exist

    !!... Required Key
    nPreviousArg  = this%nArguments
    If ( nPreviousArg.GE.1 ) then
        do iArg = 1, nPreviousArg
            if ( this%arguments(iArg)%key.EQ.key ) then
                write(*,"(a)") PREIX_ERROR//" AddKey_typInfoArg"
                write(*,"(a)")
                write(*,"(a)") INDENT//"Given 'key' exist already."
                write(*,"(a)") INDENT//"  key: --"//trim(key)
                write(*,"(a)")
                stop
            else if ( this%arguments(iArg)%singleKey.EQ.singleKey ) then
                write(*,"(a)") PREIX_ERROR//" AddKey_typInfoArg"
                write(*,"(a)")
                write(*,"(a)") INDENT//"Given 'singleKey' exist already."
                write(*,"(a)") INDENT//"  singleKey: -"//trim(singleKey)
                write(*,"(a)")
                stop
            end if
        end do
    end if

    !!... Copy into temporal array
    If ( nPreviousArg.GE.1 ) then
        Allocate( tmpArguments(nPreviousArg) )
        do iArg = 1, nPreviousArg
            tmpArguments(iArg) = this%arguments(iArg)
            Call this%arguments(iArg)%Destroy()
        end do
        Deallocate( this%arguments )
    End if

    !!... Set a new set
    this%nArguments = this%nArguments + 1
    Allocate( this%arguments(this%nArguments) )

    !!... Copy previous keys
    Do iArg = 1, nPreviousArg
        this%arguments(iArg) = tmpArguments(iArg)
    End Do

    !!... Delete temporal keys
    If ( nPreviousArg.GE.1 ) then
        do iArg = 1, nPreviousArg
            Call tmpArguments(iArg)%Destroy()
        End do
        Deallocate( tmpArguments )
    End If

    !!... Add new key
    Call this%arguments(this%nArguments)%Initialize( &
    &   key           = key,              &
    &   singleKey     = singleKey,        &
    &   description   = description,      &
    &   isRequiredKey = isRequiredKey,    &
    &   nRequiredArg  = nRequiredArg      )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine PrintHelp_typInfoArg(this, ioUnit)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typInfoArg), intent(in) :: this
    Integer, optional             :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: ioUnit_, iArg

    !!... Set Print Mode
    ! isColor = this%isColor_
    ioUnit_ = 6
    if ( present(ioUnit) ) then
        if (ioUnit.EQ.6) then
            ! isColor = this%isColor_
        else
            isColor = .FALSE.
        end if
        ioUnit_ = ioUnit
    end if

    blkWriteRequiredKey: Block
        Integer :: iArg, nArg
        Character(len=17) :: color1

        If ( this%nArguments.GE.1 ) Then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color1 = ""
                If ( isColor ) color1 = "GREEN"
                Write(ioUnit_, "(a)") INDENT//Colorize("Required", color1)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintHelp( ioUnit, isColor )
                    End if
                end do

            end if

        End if

    End Block blkWriteRequiredKey

    blkWriteOptionalKey: Block
        Integer :: iArg, nArg
        Character(len=17) :: color1

        If ( this%nArguments.GE.1 ) Then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            color1 = ""
            If ( isColor ) color1 = "GREEN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Optional", color1)
            Write(ioUnit_, "(a)") ""

            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) then
                    Call this%arguments(iArg)%PrintHelp( ioUnit, isColor )
                End if
            end do

        End if

    End Block blkWriteOptionalKey

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
