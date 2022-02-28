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
