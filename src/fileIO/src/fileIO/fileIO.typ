!! -------------------------------------------------------------------------- !!
Type :: typFileIO
!! -------------------------------------------------------------------------- !!

    !!... File Path
    Character(len=:), Allocatable :: filePath

    !!... Base path
    Character(len=:), Allocatable :: basePath

    !!... File Name
    Character(len=:), Allocatable :: fileName

    !!... Extension
    Character(len=:), Allocatable :: ext

    !!... File index
    Integer :: unit = -1

    !!... File Status = ("replace", "old", "new", ...)
    Character(len=30) :: status

    !1... File Format
    Character(len=30) :: form

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

    !!... Initialize the class
    Procedure :: Initialize => Initialize_typFileIO

    !!... Get FID
    !Procedure :: GetUnit => GetUnit_typFileIO

    !!...
    !Procedure :: OpenFile =>


!! -------------------------------------------------------------------------- !!
End Type
!! -------------------------------------------------------------------------- !!
