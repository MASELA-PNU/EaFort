!! -------------------------------------------------------------------------- !!
Type typLogGURU
!! -------------------------------------------------------------------------- !!

    Private

    !!... Write the message to the log file
    Logical :: isLogFile_ = .FALSE.

    !!... Log File Path
    character(len=:), Allocatable :: logFilePath_

    !!... Default Output
    Integer :: fid_ = 11

    !!... Is Debug Mode (Write the message when the debug mode is enabled)
    Logical :: isDebug_ = .FALSE.

    !!... Is Color Mode (Write the message when the debug mode is enabled)
    Logical :: isColor_ = .FALSE.

    !!... MPI Variable representing the master
    Logical :: MPI_isMaster = .TRUE.

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

    !!... Initialize the Log GURU
    Procedure :: Initialize => Initialize_typLogGURU

    !!... Toggle Debug Mode
    Procedure :: ToggleDebugMode => ToggleDebugMode_typLogGURU

    !!... Toggle Color Mode
    Procedure :: ToggleColorMode => ToggleColorMode_typLogGURU

    !!... Toggle isMaster
    Procedure :: ToggleIsMaster => ToggleIsMaster_typLogGURU

    !!... Write Message
    Procedure :: Write => Write_typLogGURU

    !!... Write Message
    Procedure :: WriteDebug => WriteDebug_typLogGURU

    !!... Write Message
    Procedure :: Error => Error_typLogGURU

    !!... Write Message
    Procedure :: Warn => Warn_typLogGURU

    !!... Write Message
    Procedure :: Debug => Debug_typLogGURU

    !!... Flush File IO
    Procedure :: Flush => Flush_typLogGURU

    !!... Destroy
    Procedure :: Destroy => Destroy_typLogGURU

!! -------------------------------------------------------------------------- !!
End Type
!! -------------------------------------------------------------------------- !!
