!! -------------------------------------------------------------------------- !!
Subroutine Test_ArgGURUVector()
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
    Call GURU%AddKey( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   isRequiredKey = .TRUE.,               &
    &   nRequiredWord = 1                     )

    !!... Add Arguments
    Call GURU%AddKey(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path",   &
    &   isRequiredKey = .FALSE.               )


    !!... Update Key
    Call GURU%UpdateAndPrint()

    Block
        Integer, Allocatable :: intValue(:)
        Real(8), Allocatable :: dbleValue(:)
        Logical, Allocatable :: logValue(:)
        Character(len=:), Allocatable, dimension(:) :: charValue
        Logical :: isError, isFound

        Call GURU%GetArgIntVector( "input", intValue, isError )
        write(*,*) " intValue : ", intValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgIntVector( "output", intValue, isError )
        write(*,*) " intValue : ", intValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgDbleVector( "output", dbleValue, isError )
        write(*,*) " dbleValue : ", dbleValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgLogicalVector( "output", logValue, isError )
        write(*,*) " logValue : ", logValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgCharVector( "output", charValue, isError )
        write(*,*) " charValue : ", charValue
        write(*,*) " isError  : ", isError
        write(*,*)

    End Block

    Block
        Integer, Allocatable :: intValue(:)
        Real(4), Allocatable :: realValue(:)
        Real(8), Allocatable :: dbleValue(:)
        Logical, Allocatable :: logValue(:)
        Character(len=:), Allocatable, dimension(:) :: charValue
        Character(len=:), Allocatable, dimension(:) :: refChar
        Logical :: isError

        write(*,*) "Testing GetOrDefault"
        write(*,*)

        Call GURU%ToggleDebugMode(.TRUE.)

        Call GURU%GetArgIntVectorOrDefault( "input", intValue, [1, 2, 4], isError )
        write(*,*) " intValue : ", intValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgRealVectorOrDefault( "output", realValue, [1.0, 2.2, 4.5], isError )
        write(*,*) " realValue : ", realValue
        write(*,*) " isError   : ", isError
        write(*,*)

        Call GURU%GetArgDbleVectorOrDefault( "output", dbleValue, [5.D0, 1.D0], isError )
        write(*,*) " dbleValue : ", dbleValue
        write(*,*) " isError   : ", isError
        write(*,*)

        Call GURU%GetArgLogicalVectorOrDefault( "output", logValue, [.true.], isError )
        write(*,*) " logValue : ", logValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Allocate( Character(5):: refChar(2) )
        refChar(1) = "a"
        refChar(2) = "bxa"

        Call GURU%GetArgCharVectorOrDefault( "output", charValue, refChar)
        write(*,*) " charValue : ", charValue
        write(*,*)

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
