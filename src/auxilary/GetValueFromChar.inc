!! -------------------------------------------------------------------------- !!
Subroutine GetIntFromChar( char, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Integer, intent(out)           :: value
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Integer :: ios
    Logical :: isError_
    Real(8) :: dbleValue
    read(char, *, iostat = ios) dbleValue

    isError_ = .FALSE.
    If (ios.ne.0) then
        isError_  = .TRUE.
    else
        !!... Check the value is closed to the integer
        if ( dabs( dbleValue - nint(dbleValue) ).le.1.d-24 ) then
            value = nint(dbleValue)
        else
            isError_  = .TRUE.
        end if
    End if

    if ( isError_ ) value   = 0

    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetRealFromChar( char, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(4), intent(out)           :: value
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Integer :: ios
    Logical :: isError_
    read(char, *, iostat = ios) value

    isError_ = .FALSE.
    If (ios.ne.0) then
        value   = 0
        isError_  = .TRUE.
    End if

    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetDbleFromChar( char, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(8), intent(out)           :: value
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Integer :: ios
    Logical :: isError_
    read(char, *, iostat = ios) value

    isError_ = .FALSE.
    If (ios.ne.0) then
        value   = 0
        isError_  = .TRUE.
    End if

    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetLogicalFromChar( char, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Logical, intent(out)           :: value
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Character( len=len(char) ) :: upChar
    Logical :: isError_

    isError_ = .FALSE.
    If ( IsCharEqual(char, ".TRUE.").OR.&
    &    IsCharEqual(char, "TRUE").OR.&
    &    IsCharEqual(char, "T")     ) then
        value = .TRUE.
    else if ( IsCharEqual(char, ".FALSE.").OR.&
    &    IsCharEqual(char, "FALSE").OR.&
    &    IsCharEqual(char, "F")     ) then
        value = .FALSE.
    else
        value    = .FALSE.
        isError_ = .TRUE.
    end if

    if ( present(isError) ) isError = isError_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetIntFromCharOrDefault( char, value, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Integer, intent(out)           :: value
    Integer, intent(in)            :: refValue
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call GetIntFromChar( char, value, isError_ )
    if ( isError_ ) value = refValue
    If ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetRealFromCharOrDefault( char, value, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(4), intent(out)           :: value
    Real(4), intent(in)            :: refValue
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call GetRealFromChar( char, value, isError_ )
    if ( isError_ ) value = refValue
    If ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetDbleFromCharOrDefault( char, value, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(8), intent(out)           :: value
    Real(8), intent(in)            :: refValue
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call GetDbleFromChar( char, value, isError_ )
    if ( isError_ ) value = refValue
    If ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetLogicalFromCharOrDefault( char, value, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Logical, intent(out)           :: value
    Logical, intent(in)            :: refValue
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call GetLogicalFromChar( char, value, isError_ )
    if ( isError_ ) value = refValue
    If ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetSeperatedString( char, strVector, nString )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in) :: char
    type(string), allocatable    :: strVector(:)
    Integer                      :: nString
!! -------------------------------------------------------------------------- !!
    type(string) :: str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11

    if ( allocated(strVector) ) deallocate(strVector)
    nString =0

    if (len_trim(adjustl(char)).ge.1) then
        str1 = char
        !!... Replace , with the space
        str2 = str1%Replace(",", " ")
        str3 = str2%Replace("(", " ")
        str4 = str3%Replace(")", " ")
        str5 = str4%Replace("{", " ")
        str6 = str5%Replace("}", " ")
        str7 = str6%Replace("[", " ")
        str8 = str7%Replace("]", " ")
        str9 = str8%Replace("|", " ")

        str10= str9%Replace("(/", " ")
        str11= str10%Replace("/)", " ")
        Call str11%Split( tokens = strVector, sep = " ")
        nString = size( strVector )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetIntVectorFromChar( char, vector, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Integer, allocatable           :: vector(:)
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    type(string), allocatable :: strVector(:)
    Integer :: nString, iString
    Logical :: isError_
    Integer :: singleValue

    If ( Allocated(vector) ) Deallocate(vector)

    !!... Get Separated String
    Call GetSeperatedString( char, strVector, nString )

    isError_ = .FALSE.
    if ( nString.ge.1 ) then

        Allocate( vector(nString) ); vector(:) = 0

        loopString: do iString = 1, nString

            !!... Single Conversion
            Call GetIntFromChar( strVector(iString)%raw, singleValue, isError_ )
            if ( isError_ ) exit loopString
            vector(iString) = singleValue

        end do loopString
    end if

    If ( isError_ ) then
        If ( Allocated(vector) ) Deallocate(vector)
    end if

    If ( present(isError) ) isError = isError_
    If ( Allocated(strVector) ) Deallocate(strVector)

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetRealVectorFromChar( char, vector, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(4), allocatable           :: vector(:)
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    type(string), allocatable :: strVector(:)
    Integer :: nString, iString
    Logical :: isError_
    Real(4) :: singleValue

    If ( Allocated(vector) ) Deallocate(vector)

    !!... Get Separated String
    Call GetSeperatedString( char, strVector, nString )

    isError_ = .FALSE.
    if ( nString.ge.1 ) then

        Allocate( vector(nString) ); vector(:) = 0

        loopString: do iString = 1, nString

            !!... Single Conversion
            Call GetRealFromChar( strVector(iString)%raw, singleValue, isError_ )
            if ( isError_ ) exit loopString
            vector(iString) = singleValue

        end do loopString
    end if

    If ( isError_ ) then
        If ( Allocated(vector) ) Deallocate(vector)
    end if

    If (present(isError)) isError = isError_
    If ( Allocated(strVector) ) Deallocate(strVector)

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetDbleVectorFromChar( char, vector, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(8), allocatable           :: vector(:)
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    type(string), allocatable :: strVector(:)
    Integer :: nString, iString
    Logical :: isError_
    Real(8) :: singleValue

    If ( Allocated(vector) ) Deallocate(vector)

    !!... Get Separated String
    Call GetSeperatedString( char, strVector, nString )

    isError_ = .FALSE.
    if ( nString.ge.1 ) then

        Allocate( vector(nString) ); vector(:) = 0

        loopString: do iString = 1, nString

            !!... Single Conversion
            Call GetDbleFromChar( strVector(iString)%raw, singleValue, isError_ )
            if ( isError_ ) exit loopString
            vector(iString) = singleValue

        end do loopString
    end if

    If ( isError_ ) then
        If ( Allocated(vector) ) Deallocate(vector)
    end if

    If (present(isError)) isError = isError_
    If ( Allocated(strVector) ) Deallocate(strVector)

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetLogicalVectorFromChar( char, vector, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Logical, allocatable           :: vector(:)
    Logical, intent(out), Optional :: isError
!! -------------------------------------------------------------------------- !!
    type(string), allocatable :: strVector(:)
    Integer :: nString, iString
    Logical :: isError_
    Logical :: singleValue

    If ( Allocated(vector) ) Deallocate(vector)

    !!... Get Separated String
    Call GetSeperatedString( char, strVector, nString )

    isError_ = .FALSE.
    if ( nString.ge.1 ) then

        Allocate( vector(nString) ); vector(:) = 0

        loopString: do iString = 1, nString

            !!... Single Conversion
            Call GetLogicalFromChar( strVector(iString)%raw, singleValue, isError_ )
            if ( isError_ ) exit loopString
            vector(iString) = singleValue

        end do loopString
    end if

    If ( isError_ ) then
        If ( Allocated(vector) ) Deallocate(vector)
    end if

    If (present(isError)) isError = isError_
    If ( Allocated(strVector) ) Deallocate(strVector)

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetIntVectorFromCharOrDefault( char, vector, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)     :: char
    Integer, allocatable             :: vector(:)
    Integer, dimension(:),intent(in) :: refValue
    Logical, Optional                :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElement
    Call GetIntVectorFromChar( char, vector, isError_ )
    If ( isError_ ) then
        if ( Allocated(vector) ) Deallocate(vector)
        nElement = size( refValue )
        if (nElement.ge.1) Allocate(vector, source = refValue )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetRealVectorFromCharOrDefault( char, vector, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(4), allocatable           :: vector(:)
    Real(4), dimension(:)          :: refValue
    Logical, Optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElement
    Call GetRealVectorFromChar( char, vector, isError_ )
    If ( isError_ ) then
        if ( Allocated(vector) ) Deallocate(vector)
        nElement = size( refValue )
        if (nElement.ge.1) Allocate(vector, source = refValue )
    end if
    if (present(isError)) isError = isError_
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetDbleVectorFromCharOrDefault( char, vector, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Real(8), allocatable           :: vector(:)
    Real(8), dimension(:)          :: refValue
    Logical, Optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElement
    Call GetDbleVectorFromChar( char, vector, isError_ )
    If ( isError_ ) then
        if ( Allocated(vector) ) Deallocate(vector)
        nElement = size( refValue )
        if (nElement.ge.1) Allocate(vector, source = refValue )
    end if
    if (present(isError)) isError = isError_
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine GetLogicalVectorFromCharOrDefault( char, vector, refValue, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)   :: char
    Logical, allocatable           :: vector(:)
    Logical, dimension(:)          :: refValue
    Logical, Optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElement
    Call GetLogicalVectorFromChar( char, vector, isError_ )
    If ( isError_ ) then
        if ( Allocated(vector) ) Deallocate(vector)
        nElement = size( refValue )
        if (nElement.ge.1) Allocate(vector, source = refValue )
    end if
    if (present(isError)) isError = isError_
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
