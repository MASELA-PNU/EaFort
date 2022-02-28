!! -------------------------------------------------------------------------- !!
Subroutine Test_ArgGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initizlize Description
    Call argGURU%Initialize( &
    &   projectName = "Test_GURU_Argument", &
    &   description = "Testing the GURU argment \\&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI \\&
                    &  Dept. Naval Arch. and Ocean Eng., Pusan National University, Korea", &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call argGURU%AddKey( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   isRequiredKey = .TRUE.,               &
    &   nRequiredArg  = 1                     )

    !!... Add Arguments
    Call argGURU%AddKey(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path",   &
    &   isRequiredKey = .FALSE.               )

    !!... Add Arguments
    Call argGURU%AddKey(                   &
    &   key           = "print",                    &
    &   singleKey     = "p",                        &
    &   description   = "Test another argument",    &
    &   isRequiredKey = .FALSE.                     )

    !!... Update Key
    Call argGURU%UpdateAndPrint()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
