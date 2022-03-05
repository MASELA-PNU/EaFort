!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typLogGURU( &
    & this, logFilePath, isDebug, isColor, MPI_isMaster )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typLogGURU), intent(inout)        :: this
    Character(len=*), intent(in), Optional  :: logFilePath
    Logical, intent(in), Optional           :: isDebug
    Logical, intent(in), Optional           :: isColor
    Logical, intent(in), Optional           :: MPI_isMaster
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: baseDir
    character(len=:), Allocatable :: baseName
    character(len=:), Allocatable :: ext
    Logical :: isBaseDirExist, isError, isCreate

    !!... Set Debug Mode & Color Mode
    If ( Present(isDebug) ) this%isDebug_ = isDebug
    If ( Present(isColor) ) this%isColor_ = isColor

    !!... TODO: Set the MPI Envi
    !this%MPI_isMaster = .FALSE.
    if ( present(MPI_isMaster) ) this%MPI_isMaster = MPI_isMaster

    If ( this%MPI_isMaster ) then

    !!... Set log file
    this%isLogFile_ = .FALSE.

    !!... Deallocate log file path
    If (Allocated(this%logFilePath_)) Deallocate(this%logFilePath_)

    !!... Set log file path if it is given.
    If ( Present(logFilePath) ) then

        !!... Set a new log file path
        this%logFilePath_ = trim(adjustl(logFilePath))

        If ( len_trim(this%logFilePath_).GE.1 ) then
            this%logFilePath_ = logFilePath
        else
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,              &
            &   prefixColor = ERROR_COLOR,              &
            &   head        = "Initialize_typLogGURU",  &
            &   msg         = "Given 'logFilePath' is less than 0.", &
            &   isColor     = this%isColor_,            &
            &   isStop      = .TRUE.                    )
        End if

    end If

    !!... Open Log file & Ready to write log
    blcCreateLogFile: Block
        Logical :: isFileOpen, isFileExist
        Logical :: isPrevFileExist

        !!... Check file exist already or opened by other program
        Inquire( file   = this%logFilePath_, &
        &        exist  = isFileExist,       &
        &        opened = isFileOpen         )

        isPrevFileExist = .FALSE.
        If ( isFileExist.EQV..TRUE. ) isPrevFileExist = .TRUE.

        If ( isFileOpen.EQV..TRUE. ) then
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,              &
            &   prefixColor = ERROR_COLOR,              &
            &   head        = "Initialize_typLogGURU",  &
            &   msg         = "Given log file is already opened.", &
            &   isColor     = this%isColor_,            &
            &   isStop      = .TRUE.                    )
        end if

        !!... Separate Path
        Call SeparatePath( this%logFilePath_, baseDir, baseName, ext)

        If ( len_trim(baseDir).ge.1 ) then

            isBaseDirExist = IsExistDir( baseDir )

            If (isBaseDirExist.EQV..FALSE.) then

                Call CreateDir( &
                &   dirPath  = baseDir, &
                &   isError  = isError, &
                &   isCreate = isCreate )

                If ( (isError.EQV..TRUE.).OR.(isCreate.EQV..FALSE.) ) then
                    Call WritePrefixMessage( &
                    &   prefix      = PREIX_ERROR,              &
                    &   prefixColor = ERROR_COLOR,              &
                    &   head        = "Initialize_typLogGURU",  &
                    &   msg         = "Failed to create the directory for log file.", &
                    &   errValue    = baseDir,                  &
                    &   isColor     = this%isColor_,            &
                    &   isStop      = .TRUE.                    )
                End if

            End if
        end if

        !!... If Prev Log file exist, rename them
        If ( isPrevFileExist ) then
            blkMoveFiles: Block
                !type(string) :: filePath
                !!...
                Logical :: isExist, isOpened, isOrgExist
                Integer :: iFile, ios
                Character(len=:), Allocatable :: prevFilePath
                Character(len=30) :: iFileChar


                ! type(string) :: baseDir, baseName, ext
                Integer :: iStr1, iStr2, iStr3, nChar
                Integer :: iStrSep, iStrExt
                Integer :: iStrBase1, iStrBase2
                Logical :: isDirSep, isExtension

                !!... Check log file exist and move !
                Inquire( &
                &   file   = this%logFilePath_, &
                &   exist  = isExist,           &
                &   opened = isOpened           )

                if ( isOpened.EQV..TRUE. ) then
                    Call WritePrefixMessage( &
                    &   prefix      = PREIX_ERROR,              &
                    &   prefixColor = ERROR_COLOR,              &
                    &   head        = "Initialize_typLogGURU",  &
                    &   msg         = "Given 'logFilePath' is already opened.", &
                    &   errValue    = this%logFilePath_,        &
                    &   isColor     = this%isColor_,            &
                    &   isStop      = .TRUE.                    )
                end if

                isOrgExist = .FALSE.
                if ( isExist.EQV..TRUE. ) isOrgExist = .TRUE.

                If ( isOrgExist ) then

                    !!... Check existing log file index
                    do iFile = 1, N_MAX_LOG_FILE

                        Write(iFileChar, *, iostat = ios) iFile

                        If (ios.ne.0) then
                            Call WritePrefixMessage( &
                            &   prefix      = PREIX_ERROR,              &
                            &   prefixColor = ERROR_COLOR,              &
                            &   head        = "Initialize_typLogGURU",  &
                            &   msg         = "Failed to convert the integer.", &
                            &   isColor     = this%isColor_,            &
                            &   isStop      = .TRUE.                    )
                        end if

                        prevFilePath = baseDir//baseName//"_"//trim(adjustl(iFileChar))//ext

                        Inquire( &
                        &   file   = prevFilePath, &
                        &   exist  = isExist,      &
                        &   opened = isOpened      )

                        If ( (isExist.EQV..FALSE.).AND.(isOpened.EQV..FALSE.) ) then
                            exit
                        End if

                        If ( iFIle.EQ.N_MAX_LOG_FILE ) then
                            Call WritePrefixMessage( &
                            &   prefix      = PREIX_ERROR,              &
                            &   prefixColor = ERROR_COLOR,              &
                            &   head        = "Initialize_typLogGURU",  &
                            &   msg         = "Too many log files. Please delete them.", &
                            &   isColor     = this%isColor_,            &
                            &   isStop      = .TRUE.                    )
                        End If

                    end do

                    !!... Move the file
                    Call RenameDirFile( this%logFilePath_, prevFilePath )

                end if

                If ( Allocated(prevFilePath) )  Deallocate(prevFilePath)
                !If ( Allocated(iFileChar) )     Deallocate(iFileChar)

            End Block blkMoveFiles
        End if

        !!... Create Log File
        Open( &
        &   newunit = this%fid_,         &
        &   file    = this%logFilePath_, &
        &   status  = "replace"          )

    End Block blcCreateLogFile

    If ( Allocated(baseDir) )       Deallocate(baseDir)
    If ( Allocated(baseName) )      Deallocate(baseName)
    If ( Allocated(ext) )           Deallocate(ext)

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ToggleDebugMode_typLogGURU( this, isDebug )
    Implicit None
    Class(typLogGURU), intent(inout) :: this
    Logical, intent(in)              :: isDebug
    this%isDebug_ = isDebug
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ToggleColorMode_typLogGURU( this, isColor )
    Implicit None
    Class(typLogGURU), intent(inout) :: this
    Logical, intent(in)              :: isColor
    this%isColor_ = isColor
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ToggleIsMaster_typLogGURU( this, isMaster )
    Implicit None
    Class(typLogGURU), intent(inout) :: this
    Logical, intent(in)              :: isMaster
    this%MPI_isMaster = isMaster
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Write_typLogGURU( this, msg, color, nSpace, isAdvance )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typLogGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: color
    Integer, intent(in), Optional          :: nSpace
    Logical, intent(in), Optional          :: isAdvance
!! -------------------------------------------------------------------------- !!
    type(string)              :: str
    type(string), Allocatable :: strVect(:)
    Character(len=:), Allocatable :: prefix, tmpMsg
    Logical :: isSpace
    Integer :: iMsg, nMsgLine

    !!... Separate message with line separator
    str = msg
    Call str%Split( tokens = strVect, sep = lineSep )
    nMsgLine =  size(strVect)

    !!... Set space before writing the message
    isSpace = .FALSE.
    if ( present(nSpace) ) then
        if (nSpace.ge.1) then
            Allocate( character(nSpace) :: prefix )
            prefix(:) = " "
            isSpace   = .TRUE.
        end if
    end if

    !!... Write each lines
    do iMsg = 1, nMsgLine

        tmpMsg = strVect(iMsg)%Chars()
        if ( isSpace ) tmpMsg = prefix//tmpMsg

        if ( iMsg.lt.nMsgLine ) then
            If ( this%MPI_isMaster ) then
                Call WriteMessage( &
                &   msg       = tmpMsg,         &
                &   unit      = this%fid_,      &
                &   color     = color           )
            else
                Call WriteMessage(              &
                &   msg       = tmpMsg,         &
                &   color     = color           )
            end if
        else
            If ( this%MPI_isMaster ) then
                Call WriteMessage( &
                &   msg       = tmpMsg,         &
                &   unit      = this%fid_,      &
                &   color     = color,          &
                &   isAdvance = isAdvance       )
            else
                Call WriteMessage(              &
                &   msg       = tmpMsg,         &
                &   color     = color,          &
                &   isAdvance = isAdvance       )
            end if
        end if

        If (Allocated(tmpMsg)) Deallocate(tmpMsg)

    end do

    If (Allocated(prefix)) Deallocate(prefix)

    !!... Free the memory
    Call str%Free()
    do iMsg = 1, nMsgLine
        Call strVect%Free()
    end do
    If (Allocated(strVect)) deallocate(strVect)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine WriteDebug_typLogGURU( this, msg, color, nSpace, isAdvance )
!! -------------------------------------------------------------------------- !!
Implicit None
Class(typLogGURU), intent(in)          :: this
Character(len=*), intent(in)           :: msg
Character(len=*), intent(in), Optional :: color
Integer, intent(in), Optional          :: nSpace
Logical, intent(in), Optional          :: isAdvance
!! -------------------------------------------------------------------------- !!
    If ( this%isDebug_ ) then
        Call this%Write( &
        &   msg       = msg,        &
        &   color     = color,      &
        &   nSpace    = nSpace,     &
        &   isAdvance = isAdvance   )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Error_typLogGURU( this, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typLogGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
    Logical, intent(in), Optional          :: isStop
!! -------------------------------------------------------------------------- !!
    Type(string) :: valueStr, refValueStr
    Character(len=:), Allocatable :: valueChar, refValueChar

    !!... Convert values
    valueChar = ""
    if ( present(value) ) then
        Call GetStrFromClass(value, valueStr)
        valueChar    = valueStr%Chars()
    end if

    if ( present(refValue) ) then
        Call GetStrFromClass(refValue, refValueStr)
        refValueChar = refValueStr%Chars()
    end if

    !!... Write Message
    Call WritePrefixMessage( &
    &   prefix      = PREIX_ERROR,      &
    &   prefixColor = ERROR_COLOR,      &
    &   msg         = msg,              &
    &   head        = head,             &
    &   errValue    = valueChar,        &
    &   refValue    = refValueChar,     &
    &   unit        = this%fid_,        &
    &   isColor     = this%isColor_,    &
    &   isStop      = isStop            )

    !!... Free the memory
    If ( Allocated(valueChar) ) Deallocate(valueChar)
    If ( Allocated(refValueChar) ) Deallocate(refValueChar)
    Call valueStr%Free()
    Call refValueStr%Free()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Warn_typLogGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typLogGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
!! -------------------------------------------------------------------------- !!
    Type(string) :: valueStr, refValueStr
    Character(len=:), Allocatable :: valueChar, refValueChar

    !!... Convert values
    valueChar = ""
    if ( present(value) ) then
        Call GetStrFromClass(value, valueStr)
        valueChar    = valueStr%Chars()
    end if

    if ( present(refValue) ) then
        Call GetStrFromClass(refValue, refValueStr)
        refValueChar = refValueStr%Chars()
    end if

    !!... Write Message
    Call WritePrefixMessage( &
    &   prefix      = PREIX_WARN,       &
    &   prefixColor = WARN_COLOR,       &
    &   msg         = msg,              &
    &   head        = head,             &
    &   errValue    = valueChar,        &
    &   refValue    = refValueChar,     &
    &   unit        = this%fid_,        &
    &   isColor     = this%isColor_,    &
    &   isStop      = .FALSE.           )

    !!... Free the memory
    If ( Allocated(valueChar) ) Deallocate(valueChar)
    If ( Allocated(refValueChar) ) Deallocate(refValueChar)
    Call valueStr%Free()
    Call refValueStr%Free()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Debug_typLogGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typLogGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
!! -------------------------------------------------------------------------- !!
    Type(string) :: valueStr, refValueStr
    Character(len=:), Allocatable :: valueChar, refValueChar

    If ( this%isDebug_ ) then

        !!... Convert values
        valueChar = ""
        if ( present(value) ) then
            Call GetStrFromClass(value, valueStr)
            valueChar    = valueStr%Chars()
        end if

        if ( present(refValue) ) then
            Call GetStrFromClass(refValue, refValueStr)
            refValueChar = refValueStr%Chars()
        end if

        !!... Write Message
        Call WritePrefixMessage( &
        &   prefix      = PREIX_DEBUG,      &
        &   prefixColor = DEBUG_COLOR,      &
        &   msg         = msg,              &
        &   head        = head,             &
        &   errValue    = valueChar,        &
        &   refValue    = refValueChar,     &
        &   unit        = this%fid_,        &
        &   isColor     = this%isColor_,    &
        &   isStop      = .FALSE.           )

        !!... Free the memory
        If ( Allocated(valueChar) ) Deallocate(valueChar)
        If ( Allocated(refValueChar) ) Deallocate(refValueChar)
        Call valueStr%Free()
        Call refValueStr%Free()

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine Flush_typLogGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typLogGURU), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    if ( this%isLogFile_ ) Flush(this%fid_)
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine Destroy_typLogGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typLogGURU), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Call this%Flush()   !!... Write Log file

    !!... Write the message to the log file
    this%isLogFile_ = .FALSE.

    !!... Log File Path
    this%logFilePath_ = ""

    !!... Default Output
    this%fid_ = 11

    !!... Is Debug Mode (Write the message when the debug mode is enabled)
    this%isDebug_ = .FALSE.

    !!... Is Color Mode (Write the message when the debug mode is enabled)
    this%isColor_ = .FALSE.

    !!... MPI Variable representing the master
    this%MPI_isMaster = .TRUE.

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
