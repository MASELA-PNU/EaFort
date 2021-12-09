!!... Get Integer
Function GetInt_typString(this, isError) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Logical, Optional            :: isError
    Integer(IP)                  :: funcValue
    !!... Local Variables
    Real(RP)                     :: numValue
    integer                      :: ios
    Logical                      :: isError_

    isError_ = .FALSE.
    read( this%raw, *, iostat = ios ) numValue
    funcValue = idnint(numValue)

    if ( ios.NE.0 ) isError_ = .TRUE.

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        write(*, "(a)") "[ERROR] GetInt_typString"
        write(*, "(a)") "  Conversion to integer variable fails."
        write(*, "(a)") this%raw
        stop
    end if

End Function

!!... Get Real
Function GetReal_typString(this, isError) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP)                     :: funcValue
    Logical, Optional            :: isError
    !!... Local Variables
    integer :: ios
    Logical :: isError_

    isError_ = .FALSE.
    read( this%raw, *, iostat = ios ) funcValue
    if ( ios.NE.0 ) isError_ = .TRUE.

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        write(*, "(a)") "[ERROR] GetReal_typString"
        write(*, "(a)") "  Conversion to real variable fails."
        write(*, "(a)") this%raw
        stop
    end if

End Function

!!... Get Logical
Function GetLogical_typString(this, isError) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Logical                      :: funcValue
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isCorrect

    funcValue = .FALSE.
    isCorrect = .FALSE.
    if ( IsCharCharEqual( this%raw, 'T') ) then
        funcValue = .TRUE.
        isCorrect = .TRUE.
    else if ( IsCharCharEqual( this%raw, 'TRUE') ) then
        funcValue = .TRUE.
        isCorrect = .TRUE.
    else if ( IsCharCharEqual( this%raw, '.TRUE.') ) then
        funcValue = .TRUE.
        isCorrect = .TRUE.
    end if

    if ( IsCharCharEqual( this%raw, 'F') ) then
        funcValue = .FALSE.
        isCorrect = .TRUE.
    else if ( IsCharCharEqual( this%raw, 'FALSE') ) then
        funcValue = .FALSE.
        isCorrect = .TRUE.
    else if ( IsCharCharEqual( this%raw, '.FALSE.') ) then
        funcValue = .FALSE.
        isCorrect = .TRUE.
    end if

    if ( present(isError) ) isError = .NOT.isCorrect

    if ( (.NOT.isCorrect).AND.(.NOT.present(isError)) ) then
        write(*, "(a)") "[ERROR] GetLogical_typString"
        write(*, "(a)") "  Conversion to logical variable fails."
        write(*, "(a)") this%raw
        stop
    end if

End Function

Function GetChar_typString(this, isError) Result(funcValue)
    Implicit None
    Class(typString), intent(in)           :: this
    Character(kind=CK, len=:), allocatable :: funcValue
    Logical, Optional                      :: isError
    Logical :: isError_

    funcValue = ''

    isError_   = .FALSE.
    if ( allocated(this%raw) ) then
        funcValue = this%raw
    else
        isError_ = .TRUE.
    endif

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        write(*, "(a)") "[ERROR] GetChar_typString"
        write(*, "(a)") "  Conversion to char variable fails."
        write(*, "(a)") this%raw
        stop
    end if

End Function

!!... Get Integer
Function GetIntOrDefault_typString(this, default) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Integer                      :: default
    Integer(IP)                  :: funcValue
    !!... Local Variables
    Logical                      :: isError

    funcValue = this%GetInt( isError )
    if ( isError ) then
        funcValue = default
        ! write(*, "(a)") "[ERROR] GetIntOrDefault_typString"
        ! write(*, "(a)") "  'default' is not given."
    end if

End Function

!!... Get Real
Function GetRealOrDefault_typString(this, default) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP)                     :: funcValue
    Real(RP)                     :: default
    !!... Local Variables
    Logical                      :: isError

    funcValue = this%GetReal( isError )
    if ( isError ) then
        funcValue = default
        ! write(*, "(a)") "[ERROR] GetRealOrDefault_typString"
    end if

End Function

!!... Get Logical
Function GetLogicalOrDefault_typString(this, default) Result(funcValue)
    Implicit None
    Class(typString), intent(in) :: this
    Logical                      :: default
    Logical                      :: funcValue
    !!... Local Variables
    Logical                      :: isError

    funcValue = this%GetLogical( isError )
    if ( isError ) then
        funcValue = default
        ! write(*, "(a)") "[ERROR] GetLogicalOrDefault_typString"
    end if

End Function

Function GetCharOrDefault_typString(this, default) Result(funcValue)
    Implicit None
    Class(typString), intent(in)           :: this
    Character(len=*)                       :: default
    Character(kind=CK, len=:), allocatable :: funcValue
    !!... Local Variables
    Logical                      :: isError

    funcValue = this%GetChar( isError )
    if ( isError ) then
        funcValue = default
        ! write(*, "(a)") "[ERROR] GetCharOrDefault_typString"
    end if

End Function

Subroutine Split_typString(this, strVector, sep, maxNVector )
    Implicit None
    Class(typString), intent(in)              :: this
    Type(typString), Allocatable, intent(out) :: strVector(:)
    Character(kind=CK, len=*), Optional       :: sep
    Integer, Optional                         :: maxNVector
    !!... Local Variables
    Type(string), Allocatable :: stringVector(:)
    Integer :: nString, iString

    !!... Call Parent's Subroutine
    Call this%string%Split( tokens = stringVector, sep = sep, max_tokens = maxNVector )

    nString = size( stringVector )
    if ( nString.GE.1 ) then
        Allocate( strVector(nString) )
        do iString = 1, nString
            strVector(iString) = stringVector(iString)%raw
        end do
    end if

    If ( Allocated(stringVector) ) Then
        do iString = 1, nString
            If ( Allocated(stringVector(iString)%raw) ) then
                Deallocate( stringVector(iString)%raw )
            end if
        end do
        Deallocate( stringVector )
    end if

End Subroutine

Subroutine SplitSimple_typString(this, strVector)
    Implicit None
    Class(typString), intent(in)              :: this
    Type(typString), Allocatable, intent(out) :: strVector(:)
    !!... Local Variables
    Type(string)    :: replacedStrOrg
    Type(typString) :: replacedStr

    !!... Replace ',' into ' ' (blank)
    replacedStrOrg = this%string%Replace( old = ",", new = " ")
    replacedStr    = replacedStrOrg%raw

    !!... Split
    Call replacedStr%Split_typString( strVector = strVector )

    If ( Allocated(replacedStrOrg%raw) ) Deallocate( replacedStrOrg%raw )
    If ( Allocated(replacedStr%raw) ) Deallocate( replacedStr%raw )

End Subroutine

Function GetIntVector_typString( this, isError ) Result( intVector )
    Implicit None
    Class(typString), intent(in) :: this
    Integer, Allocatable         :: intVector(:)
    Logical, Optional            :: isError
    !!... Local Variables
    Type(typString), Allocatable :: strVector(:)
    Integer :: nStr, iStr
    Logical :: isError_, isLocalError

    !!... Split into string vector
    isError_ = .FALSE.
    Call this%SplitSimple( strVector )

    nStr = size( strVector )
    if ( nStr.GE.1 ) then

        If ( Allocated(intVector) ) Deallocate( intVector )
        Allocate( intVector(nStr) )

        do iStr = 1, nStr
            intVector(iStr) = strVector(iStr)%GetInt( isLocalError )

            if ( isLocalError ) then
                isError_ = .TRUE.
            end if

        end do

    else
        isError_ = .TRUE.
    end if

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        write(*, "(a)") "[ERROR] GetIntVector_typString"
        write(*, "(a)") "  Conversion to integer vector fails."
        write(*, "(a)") this%raw
        stop
    end if


End Function
