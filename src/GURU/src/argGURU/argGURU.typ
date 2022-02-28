Type typArgGURU

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
    Procedure :: Initialize => Initialize_typArgGURU

    !!... Add Argument
    Procedure :: AddKey => AddKey_typArgGURU

    !!... Update Arguments
    Procedure :: Update => Update_typArgGURU

    !!... Found Key
    Procedure :: IsFoundKey => IsFoundKey_typArgGURU

    ! !!... Get the number of argument
    ! Procedure :: GetNArgument => GetNArgument_typInfoArg
    !
    ! !!... Get argument
    ! Procedure :: GetArgument => GetArgument_typInfoArg
    !
    ! !!... Get argument or default
    ! Procedure :: GetArgumentOrDefault => GetArgumentOrDefault_typInfoArg

    !!... Update and Print
    Procedure :: UpdateAndPrint => UpdateAndPrint_typArgGURU

    !!... Print the description
    Procedure :: PrintDescription => PrintDescription_typArgGURU

    !!... Print Help
    Procedure :: PrintHelp => PrintHelp_typArgGURU

    !!... Print the description
    Procedure :: PrintGivenKey => PrintGivenKey_typArgGURU

    !!... Destroy the class
    Procedure :: Destroy => Destroy_typArgGURU

End Type
