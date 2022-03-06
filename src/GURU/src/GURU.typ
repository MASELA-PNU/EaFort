!! -------------------------------------------------------------------------- !!
Type typGURU
!! -------------------------------------------------------------------------- !!

    Private

!! -------------------------------------------------------------------------- !!
!!  Arguments Variables
!! -------------------------------------------------------------------------- !!

    !!... Project name
    Type(string) :: projectName

    !!... Application Description
    Type(string) :: description

    !!... Authors
    Type(string) :: authors

    !!... Date
    Type(string) :: date

    !!... Date
    Type(string) :: version

    !!... The number of arguments
    Integer :: nArguments = 0

    !!... Required Arguments
    Type(typCommandArgument), allocatable :: arguments(:)

!! -------------------------------------------------------------------------- !!
!!  Logging Variables
!! -------------------------------------------------------------------------- !!

    !!... Write the message to the log file
    Logical :: isLogFile_ = .FALSE.

    !!... Log File Path
    character(len=:), Allocatable :: logFilePath_

    !!... Default Output
    Integer :: logUnit_ = 11

    !!... Is Debug Mode (Write the message when the debug mode is enabled)
    Logical :: isDebug_ = .FALSE.

    !!... Is Color Mode (Write the message when the debug mode is enabled)
    Logical :: isColor_ = .FALSE.

    !!... MPI Variable representing the master
    Logical :: MPI_isMaster = .TRUE.

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
!!  Arguments Routines
!! -------------------------------------------------------------------------- !!

    !!... Initialize the description
    Procedure :: Initialize_ARG => ARG_Initialize_typGURU

    !!... Add Argument
    Procedure :: AddKey => ARG_AddKey_typGURU

    !!... Update Arguments
    Procedure :: Update => ARG_Update_typGURU

    !!... Found Key
    Procedure :: IsFoundKey => ARG_IsFoundKey_typGURU

    !!... Get the number of argument
    Procedure :: GetNArg => ARG_GetNArg_typGURU

    !!... Get key for given index
    Procedure :: GetKey => ARG_GetKey_typGURU

    !!... Get Arguments
    Procedure :: GetArgInteger => ARG_GetArgInteger_typGURU
    ! Procedure :: GetArgReal    => ARG_GetArg_Integer_typGURU
    ! Procedure :: GetArgDble    => ARG_GetArg_Dble_typGURU
    ! Procedure :: GetArgLogical => ARG_GetArg_Logical_typGURU
    ! Procedure :: GetArgChar    => ARG_GetArg_Char_typGURU
    ! Procedure :: GetArgString  => ARG_GetArg_String_typGURU

    ! !!... Get argument
    ! Procedure :: GetArg => GetArg_typInfoArg
    !
    ! !!... Get argument or default
    ! Procedure :: GetArgOrDefault => GetArgOrDefault_typInfoArg

    !!... Update and Print
    Procedure :: UpdateAndPrint => ARG_UpdateAndPrint_typGURU

    !!... Print the description
    Procedure :: PrintDescription => ARG_PrintDescription_typGURU

    !!... Print Help
    Procedure :: PrintHelp => ARG_PrintHelp_typGURU

    !!... Print the description
    Procedure :: PrintGivenKey => ARG_PrintGivenKey_typGURU

    !!... Destroy the class
    Procedure :: Destroy_ARG => ARG_Destroy_typGURU

    !!... Get Argument
    ! Generic :: GetArg => GetArgInteger,     &
    ! &                    GetArgReal,        &
    ! &                    GetArgDble,        &
    ! &                    GetArgLogical,     &
    ! &                    GetArgChar,        &
    ! &                    GetArgString

!! -------------------------------------------------------------------------- !!
!!  Logging Routines
!! -------------------------------------------------------------------------- !!

    !!... Initialize the Log GURU
    Procedure :: Initialize_LOG => LOG_Initialize_typGURU

    !!... Toggle Debug Mode
    Procedure :: ToggleDebugMode => LOG_ToggleDebugMode_typGURU

    !!... Toggle Color Mode
    Procedure :: ToggleColorMode => LOG_ToggleColorMode_typGURU

    !!... Toggle isMaster
    Procedure :: ToggleIsMaster => LOG_ToggleIsMaster_typGURU

    !!... Write Message
    Procedure :: Write => LOG_Write_typGURU

    !!... Write Message
    Procedure :: WriteDebug => LOG_WriteDebug_typGURU

    !!... Write Message
    Procedure :: Error => LOG_Error_typGURU

    !!... Write Message
    Procedure :: Warn => LOG_Warn_typGURU

    !!... Write Message
    Procedure :: Debug => LOG_Debug_typGURU

    !!... Flush File IO
    Procedure :: Flush => LOG_Flush_typGURU

    !!... Destroy
    Procedure :: Destroy_LOG => LOG_Destroy_typGURU

!! -------------------------------------------------------------------------- !!
End Type
!! -------------------------------------------------------------------------- !!
