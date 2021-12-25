Type typInfoArg

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

    ! !!... Writing the message with color (Only in Linux Env.)
    ! Logical :: isColor_ = .FALSE.
    !
    ! !!... Debugging Mode is On
    ! Logical :: isDebug_ = .FALSE.

    !!... The number of arguments
    Integer :: nArguments = 0

    !!... Required Arguments
    Type(typCommandArgument), allocatable :: arguments(:)

Contains

!!... Argument Handling --------------------------------------------------------

    !!... Initialize the description
    Procedure :: Initialize => Initialize_typInfoArg

    !!... Add Argument
    Procedure :: AddKey => AddKey_typInfoArg

    !!... Update Arguments
    Procedure :: Update => Update_typInfoArg

    !!... Found Key
    Procedure :: IsFoundKey => IsFoundKey_typInfoArg

    ! !!... Get the number of argument
    ! Procedure :: GetNArgument => GetNArgument_typInfoArg
    !
    ! !!... Get argument
    ! Procedure :: GetArgument => GetArgument_typInfoArg
    !
    ! !!... Get argument or default
    ! Procedure :: GetArgumentOrDefault => GetArgumentOrDefault_typInfoArg

    !!... Update and Print
    Procedure :: UpdateAndPrint => UpdateAndPrint_typInfoArg

    !!... Print the description
    Procedure :: PrintDescription => PrintDescription_typInfoArg

    !!... Print Help
    Procedure :: PrintHelp => PrintHelp_typInfoArg

    !!... Print the description
    Procedure :: PrintGivenKey => PrintGivenKey_typInfoArg

    !!... Destroy the class
    Procedure :: Destroy => Destroy_typInfoArg

End Type
