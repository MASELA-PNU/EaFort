!! -------------------------------------------------------------------------- !!
Subroutine Test_String_SingleConversion()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Block
        Type(typString) :: str
        Integer(IP)     :: intValue
        Real(RP)        :: realValue

        write(*,"(a)") "Testing Numeric Values"

        str       = 3
        intValue  = str%GetInt()
        realValue = str%GetReal()

        write(*,*) "intValue", intValue
        write(*,*) "realValue", realValue

        write(*,*)
        str       = 4.0
        intValue  = str%GetInt()
        realValue = str%GetReal()

        write(*,*) "intValue", intValue
        write(*,*) "realValue", realValue
    End Block

    Block
        Type(typString) :: str
        Logical         :: logValue

        write(*,*)
        write(*,"(a)") "Testing Logical Values (TRUE)"

        str = 'T'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue

        str = 'TRue'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue

        str = '.TRue.'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue

    End Block

    Block
        Type(typString) :: str
        Logical         :: logValue

        write(*,*)
        write(*,"(a)") "Testing Logical Values (FALSE)"

        str = 'F'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue

        str = 'FALse'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue

        str = '.FALSe.'
        logValue = str%GetLogical()
        write(*,*) "logValue", logValue
    End Block

    Block
        Type(typString)   :: str
        Character(len=10) :: charValue

        write(*,*)
        write(*,"(a)") "Testing Char Values"

        str = 'asds'
        charValue = str%GetChar()
        write(*,*) 'charValue', charValue
    End Block

    Block
        Type(typString)   :: str
        Integer           :: intValue
        Real(RP)          :: realValue
        Logical           :: logValue
        Character(len=10) :: charValue

        write(*,*)
        write(*,"(a)") "Testing Get Or Default"

        str       = "tmp"
        intValue  = str%GetIntOrDefault( 5 )
        write(*,*) "intValue", intValue

        realValue  = str%GetRealOrDefault( 2.0_RP )
        write(*,*) "realValue", realValue

        logValue  = str%GetLogicalOrDefault( .TRUE. )
        write(*,*) "logValue", logValue

        charValue = str%GetCharOrDefault('Tested')
        write(*,*) 'charValue', charValue
    End Block



!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
