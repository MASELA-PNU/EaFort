Function IsCharCharEqual( char1, char2, isStrongCheck ) Result( isEqual )
    Implicit None
    Character(len=*), intent(in) :: char1
    Character(len=*), intent(in) :: char2
    Logical                      :: isEqual
    Logical, optional            :: isStrongCheck
    !!... Local variables
    Logical :: isStrongCheck_

    isStrongCheck_ = .FALSE.
    if ( present(isStrongCheck) ) isStrongCheck_ = isStrongCheck
    isEqual = .FALSE.

    if ( isStrongCheck_ ) then
        if ( trim(char1).EQ.trim(char2) ) isEqual = .TRUE.
    else
        if ( trim( ToUpperChar(char1) ).EQ.trim( ToUpperChar(char2) ) ) isEqual = .TRUE.
    end if

End Function

Function ToUpperChar( var ) result( outVar )
    Implicit None
    Integer, Parameter           :: PARAM_ICHAR = ICHAR('A') - ICHAR('a')
    Character(len=*), intent(in) :: var
    Character( len(var) )        :: outVar
    !!... Local Variables
    Character                    :: tmpChar
    Integer                      :: ii, nChar
    nChar = len(var)
    do ii = 1, nChar
        tmpChar = var(ii:ii)
        if ( (tmpChar>='a').AND.(tmpChar<='z') ) tmpChar = CHAR( ICHAR(tmpChar) + PARAM_ICHAR )
        outVar(ii:ii) = tmpChar
    end do
End Function

Function ToLowerChar( var ) result( outVar )
    Implicit None
    Integer, Parameter           :: PARAM_ICHAR = ICHAR('A') - ICHAR('a')
    Character(len=*), intent(in) :: var
    Character( len(var) )        :: outVar
    !!... Local Variables
    Character                    :: tmpChar
    Integer                      :: ii, nChar
    nChar = len(var)
    do ii = 1, nChar
        tmpChar = var(ii:ii)
        if ( (tmpChar>='A').AND.(tmpChar<='Z') ) tmpChar = CHAR( ICHAR(tmpChar) - PARAM_ICHAR )
        outVar(ii:ii) = tmpChar
    end do
End Function
