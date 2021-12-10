Type typCommandArgument

    !!.... Key
    Character(len=:), Allocatable :: key

    !!... Is Single Key Given
    Logical :: isSingleKeyGiven = .FALSE.

    !!... Single Key
    Character(len=1) :: singleKey = " "

    !!... Description
    Type(string) :: description

    !!... isRequireKey
    Logical :: isRequiredKey = .TRUE.

    !!... is keyword found
    Logical :: isFound = .FALSE.

    !!... The number of required argument
    Integer :: nRequiredArg = 0

    !!... The number of arguments
    Integer :: nArguments = 0

    !!... Required Argument
    Type(string), Allocatable :: arguments(:)

Contains

    !!... Initialize the Command Argument
    Procedure :: Initialize => Initialize_typCommandArgument

    !!... Print Information
    Procedure :: PrintInfo => PrintInfo_typCommandArgument

    !!... Print Help
    Procedure :: PrintHelp => PrintHelp_typCommandArgument

    !!... Update Argument
    Procedure :: Update => Update_typCommandArgument

    !!... Copy Routine
    Procedure :: Copy => Copy_typCommandArgument

    !!... Copy Routine
    Procedure :: Destroy => Destroy_typCommandArgument

    !!... Check the key and single key
    Procedure, Private :: CheckAndCleanKey => CheckAndCleanKey_typCommandArgument

    !!... Copy Operator
    Generic, Public :: Assignment(=) => Copy

End Type
