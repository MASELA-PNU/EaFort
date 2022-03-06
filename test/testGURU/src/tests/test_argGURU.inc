!! -------------------------------------------------------------------------- !!
Subroutine Test_ArgGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initizlize Description
    Call GURU%Initialize_ARG( &
    &   projectName = "Test_GURU_Argument", &
    &   description = "Testing the GURU argment \n&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI\n \n&
                    &  Dept. Naval Architecture and Ocean Engineering,\n&
                    &  Pusan National University, Republic of Korea.",  &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    ! Call GURU%AddKey( &
    ! &   key           = "input",              &
    ! &   singleKey     = "i",                  &
    ! &   description   = "Input File Path",    &
    ! &   isRequiredKey = .TRUE.,               &
    ! &   nRequiredArg  = 1                     )
    !
    ! !!... Add Arguments
    ! Call GURU%AddKey(             &
    ! &   key           = "output",             &
    ! &   singleKey     = "o",                  &
    ! &   description   = "Output File Path",   &
    ! &   isRequiredKey = .FALSE.               )
    !
    ! !!... Add Arguments
    ! Call GURU%AddKey(                   &
    ! &   key           = "print",                    &
    ! &   singleKey     = "p",                        &
    ! &   description   = "Test another argument",    &
    ! &   isRequiredKey = .FALSE.                     )

    !!... Update Key
    Call GURU%UpdateAndPrint()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
