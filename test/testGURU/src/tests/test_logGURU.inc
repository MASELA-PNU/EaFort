!! -------------------------------------------------------------------------- !!
Subroutine Test_LogGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Call logGURU%Initialize( &
    &   logFilePath = "./log/testLogGURU.log",    &
    &   isDebug     = .TRUE.,               &
    &   isColor     = .TRUE.                )

    ! Call logGURU%Initialize( &
    ! &   logFilePath = ".\log\testLogGURU.log",    &
    ! &   isDebug     = .TRUE.,               &
    ! &   isColor     = .TRUE.                )
    !
    ! Call logGURU%Initialize( &
    ! &   logFilePath = "./testLogGURU.log",    &
    ! &   isDebug     = .TRUE.,               &
    ! &   isColor     = .TRUE.                )
    !
    ! Call logGURU%Initialize( &
    ! &   logFilePath = "testLogGURU.log",    &
    ! &   isDebug     = .TRUE.,               &
    ! &   isColor     = .TRUE.                )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
