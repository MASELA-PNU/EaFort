!! -------------------------------------------------------------------------- !!
Subroutine Test_ArgGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initizlize Description
    Call GURU%Initialize_ARG( &
    &   projectName = "Test_GURU_Argument", &
    &   description = "Testing the GURU argment \\&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI \\&
                    &  Dept. Naval Arch. and Ocean Eng., Pusan National University, Korea", &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call GURU%AddKey( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   isRequiredKey = .TRUE.,               &
    &   nRequiredArg  = 1                     )

    !!... Add Arguments
    Call GURU%AddKey(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path",   &
    &   isRequiredKey = .FALSE.               )

    !!... Add Arguments
    Call GURU%AddKey(                   &
    &   key           = "print",                    &
    &   singleKey     = "p",                        &
    &   description   = "Test another argument",    &
    &   isRequiredKey = .FALSE.                     )

    !!... Update Key
    Call GURU%UpdateAndPrint()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
