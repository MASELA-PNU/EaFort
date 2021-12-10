!! -------------------------------------------------------------------------- !!
Subroutine Test_GURU_Argument()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initizlize Description
    Call GURU%InitializeDescription( &
    &   projectName = "Test_GURU_Argument", &
    &   description = "Testing the GURU argment \\&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI \\&
                    &  Dept. Naval Arch. and Ocean Eng., Pusan National University, Korea", &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call GURU%AddCommandArgument( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   isRequiredKey = .TRUE.,               &
    &   nRequiredArg  = 1                     )

    !!... Add Arguments
    Call GURU%AddCommandArgument(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path",   &
    &   isRequiredKey = .FALSE.               )

    !!... Update Key
    Call GURU%UpdateCommandArgument()

    !!... Print Description
    Call GURU%PrintDescription()

    ! !!... Print Help
    ! Call GURU%PrintHelp()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
