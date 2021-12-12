Type typGURU

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

    !!... Writing the message with color (Only in Linux Env.)
    Logical :: isColor_ = .FALSE.

    !!... Debugging Mode is On
    Logical :: isDebug_ = .FALSE.

    !!... The number of arguments
    Integer :: nArguments = 0

    !!... Required Arguments
    Type(typCommandArgument), allocatable :: arguments(:)

Contains

!!... Argument Handling --------------------------------------------------------

    !!... Initialize the description
    Procedure :: InitializeDescription => InitializeDescription_typGURU

    !!... Add Argument
    Procedure :: AddCommandArgument => AddCommandArgument_typGURU

    !!... Update Arguments
    Procedure :: UpdateCommandArgument => UpdateCommandArgument_typGURU

    !!... Found Key
    Procedure :: IsFoundKey => IsFoundKey_typGURU

    !!... Update and Print
    Procedure :: UpdateAndPrintDescription => UpdateAndPrintDescription_typGURU

    !!... Print the description
    Procedure :: PrintDescription => PrintDescription_typGURU

    !!... Print Help
    Procedure :: PrintHelp => PrintHelp_typGURU

    !!... Print the description
    Procedure :: PrintGivenKey => PrintGivenKey_typGURU

!!... Error/Warning Message ----------------------------------------------------

    Procedure :: Write => Write_typGURU

!!... File IO ------------------------------------------------------------------


!!... Routines

    Procedure :: Destroy => Destroy_typGURU

End Type
