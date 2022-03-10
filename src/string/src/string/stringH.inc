!! -------------------------------------------------------------------------- !!
Function isStringEqual_CharChar(str1, str2, isStrong) result(isEqual)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)  :: str1
    Character(len=*), intent(in)  :: str2
    Logical, intent(in), optional :: isStrong
    Logical :: isEqual
!! -------------------------------------------------------------------------- !!
    isEqual = IsCharEqual( char1 = str1, char2 = str2, isStrong = isStrong)
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function isStringEqual_CharString(str1, str2, isStrong) result(isEqual)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)  :: str1
    Type(typString), intent(in)   :: str2
    Logical, intent(in), optional :: isStrong
    Logical :: isEqual
!! -------------------------------------------------------------------------- !!
    isEqual = IsCharEqual( char1 = str1, char2 = str2%raw, isStrong = isStrong)
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function isStringEqual_StringChar(str1, str2, isStrong) result(isEqual)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typString), intent(in)   :: str1
    Character(len=*), intent(in)  :: str2
    Logical, intent(in), optional :: isStrong
    Logical :: isEqual
!! -------------------------------------------------------------------------- !!
    isEqual = IsCharEqual( char1 = str1%raw, char2 = str2, isStrong = isStrong)
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function isStringEqual_StringString(str1, str2, isStrong) result(isEqual)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typString), intent(in)   :: str1
    Type(typString), intent(in)   :: str2
    Logical, intent(in), optional :: isStrong
    Logical :: isEqual
!! -------------------------------------------------------------------------- !!
    isEqual = IsCharEqual( char1 = str1%raw, char2 = str2%raw, isStrong = isStrong)
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Int2Char(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Integer, Intent(in)           :: var
    Character(len=:), allocatable :: varChar
!! -------------------------------------------------------------------------- !!
    type(string) :: str
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)
    Call GetStrFromClass( var, str, isError )
    varChar = str%raw
    if ( isError ) then
        Call GURU%Error( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Int2Char", &
        &   value = var )
    end if
    Call str%Free()
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Int2Char_Vector(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Integer, dimension(:), Intent(in) :: var
    Character(len=:), allocatable     :: varChar
!! -------------------------------------------------------------------------- !!
    Integer :: nVar, iVar
    type(string) :: str, tmpStr
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)

    nVar = size(var)
    if (nVar.le.0) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "Int2Char_Vector" )
    end if

    ifGenStr: if ( nVar.GE.1 ) then

        str%raw = "[ "
        do iVar = 1, nVar-1
            Call GetStrFromClass( var(iVar), tmpStr, isError )
            if (isError) exit ifGenStr
            str%raw = str%raw//tmpStr%raw//", "
        end do

        Call GetStrFromClass( var(nVar), tmpStr, isError )
        if (isError) exit ifGenStr
        str%raw = str%raw//tmpStr%raw
        str%raw = str%raw//" ]"

    end if ifGenStr

    if ( isError ) then
        Call GURU%ErrorVector( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Int2Char", &
        &   value = var )
    end if

    varChar = str%raw

    Call str%Free()
    Call tmpStr%Free()

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Real2Char(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Real(4), Intent(in)           :: var
    Character(len=:), allocatable :: varChar
!! -------------------------------------------------------------------------- !!
    type(string) :: str
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)
    Call GetStrFromClass( var, str, isError )
    varChar = str%raw
    if ( isError ) then
        Call GURU%Error( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Real2Char", &
        &   value = var )
    end if
    Call str%Free()
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Real2Char_Vector(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Real(4), dimension(:), Intent(in) :: var
    Character(len=:), allocatable     :: varChar
!! -------------------------------------------------------------------------- !!
    Integer :: nVar, iVar
    type(string) :: str, tmpStr
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)

    nVar = size(var)
    if (nVar.le.0) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "Real2Char_Vector" )
    end if

    ifGenStr: if ( nVar.GE.1 ) then

        str%raw = "[ "
        do iVar = 1, nVar-1
            Call GetStrFromClass( var(iVar), tmpStr, isError )
            if (isError) exit ifGenStr
            str%raw = str%raw//tmpStr%raw//", "
        end do

        Call GetStrFromClass( var(nVar), tmpStr, isError )
        if (isError) exit ifGenStr
        str%raw = str%raw//tmpStr%raw
        str%raw = str%raw//" ]"

    end if ifGenStr

    if ( isError ) then
        Call GURU%ErrorVector( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Real2Char_Vector", &
        &   value = var )
    end if

    varChar = str%raw

    Call str%Free()
    Call tmpStr%Free()

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Dble2Char(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Real(8), Intent(in)           :: var
    Character(len=:), allocatable :: varChar
!! -------------------------------------------------------------------------- !!
    type(string) :: str
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)
    Call GetStrFromClass( var, str, isError )
    varChar = str%raw
    if ( isError ) then
        Call GURU%Error( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Dble2Char", &
        &   value = var )
    end if
    Call str%Free()
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Dble2Char_Vector(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Real(8), dimension(:), Intent(in) :: var
    Character(len=:), allocatable     :: varChar
!! -------------------------------------------------------------------------- !!
    Integer :: nVar, iVar
    type(string) :: str, tmpStr
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)

    nVar = size(var)
    if (nVar.le.0) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "Dble2Char_Vector" )
    end if

    ifGenStr: if ( nVar.GE.1 ) then

        str%raw = "[ "
        do iVar = 1, nVar-1
            Call GetStrFromClass( var(iVar), tmpStr, isError )
            if (isError) exit ifGenStr
            str%raw = str%raw//tmpStr%raw//", "
        end do

        Call GetStrFromClass( var(nVar), tmpStr, isError )
        if (isError) exit ifGenStr
        str%raw = str%raw//tmpStr%raw
        str%raw = str%raw//" ]"

    end if ifGenStr

    if ( isError ) then
        Call GURU%ErrorVector( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Dble2Char_Vector", &
        &   value = var )
    end if

    varChar = str%raw

    Call str%Free()
    Call tmpStr%Free()

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Logical2Char(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Logical, Intent(in)           :: var
    Character(len=:), allocatable :: varChar
!! -------------------------------------------------------------------------- !!
    type(string) :: str
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)
    Call GetStrFromClass( var, str, isError )
    varChar = str%raw
    if ( isError ) then
        Call GURU%Error( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Logical2Char", &
        &   value = var )
    end if
    Call str%Free()
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function Logical2Char_Vector(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Logical, dimension(:), Intent(in) :: var
    Character(len=:), allocatable     :: varChar
!! -------------------------------------------------------------------------- !!
    Integer :: nVar, iVar
    type(string) :: str, tmpStr
    Logical :: isError
    If ( Allocated(varChar) ) deallocate(varChar)

    nVar = size(var)
    if (nVar.le.0) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "Logical2Char_Vector" )
    end if

    ifGenStr: if ( nVar.GE.1 ) then

        str%raw = "[ "
        do iVar = 1, nVar-1
            Call GetStrFromClass( var(iVar), tmpStr, isError )
            if (isError) exit ifGenStr
            str%raw = str%raw//tmpStr%raw//", "
        end do

        Call GetStrFromClass( var(nVar), tmpStr, isError )
        if (isError) exit ifGenStr
        str%raw = str%raw//tmpStr%raw
        str%raw = str%raw//" ]"

    end if ifGenStr

    if ( isError ) then
        Call GURU%ErrorVector( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Dble2Char_Vector", &
        &   value = var )
    end if

    varChar = str%raw

    Call str%Free()
    Call tmpStr%Free()

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function String2Char(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typString), Intent(in)   :: var
    Character(len=:), allocatable :: varChar
!! -------------------------------------------------------------------------- !!
    Logical :: isError

    If ( Allocated(varChar) ) deallocate(varChar)

    isError = .FALSE.
    if (len(var%raw).le.0) isError = .TRUE.

    varChar = ""
    if (isError) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "String2Char" )
    else
        varChar = var%raw
    end if
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function String2Char_Vector(var) result( varChar )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typString), dimension(:), Intent(in) :: var
    Character(len=:), allocatable             :: varChar
!! -------------------------------------------------------------------------- !!
    Integer :: nVar, iVar
    Logical :: isError
    type(string) :: str

    If ( Allocated(varChar) ) deallocate(varChar)

    nVar = size(var)

    if (nVar.le.0) then
        Call GURU%Error( &
        &   msg   = "The size of given 'var' is less than 1.", &
        &   head  = "String2Char_Vector" )
    end if

    isError = .FALSE.
    ifGenStr: if ( nVar.GE.1 ) then

        str%raw = "[ "
        do iVar = 1, nVar-1
            if (len(var(iVar)%raw).le.0) isError = .TRUE.
            if (isError) exit ifGenStr
            str%raw = str%raw//var(iVar)%raw//", "
        end do

        if (len(var(nVar)%raw).le.0) isError = .TRUE.
        if (isError) exit ifGenStr
        str%raw = str%raw//var(iVar)%raw
        str%raw = str%raw//" ]"

    end if ifGenStr

    if ( isError ) then
        Call GURU%ErrorVector( &
        &   msg   = "Failed to convert to character.", &
        &   head  = "Dble2Char_Vector", &
        &   value = var )
    end if

    varChar = str%raw

    Call str%Free()

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!
