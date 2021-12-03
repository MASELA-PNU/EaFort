!! -------------------------------------------------------------------------- !!
Subroutine Test_String_VectorConversion()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    blkIntegerVector: Block
        Type(typString)   :: str
        Integer, Allocatable   :: intVec1(:)
        Integer, Dimension(10) :: intVec2
        Integer :: iVar, nVar

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "1 2 3 4 5 6 7"
        intVec1 = str%GetIntVector( )
        nVar    = size(intVec1)
        do iVar = 1, nVar
            write(*,*) iVar, intVec1(iVar)
        end do
        write(*,*) " "

        str = "7, 6, 5, 4, 3 2 1"
        intVec2 = str%GetIntVector( )
        nVar    = size(intVec2)
        do iVar = 1, nVar
            write(*,*) iVar, intVec2(iVar)
        end do
        write(*,*) " "

    End Block blkIntegerVector


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
