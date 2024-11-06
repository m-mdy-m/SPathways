PROGRAM E_O
    IMPLICIT NONE
    REAL :: num
    CHARACTER(LEN=4) :: result  ! Declare result as a string of length 4 (for "even" or "odd")

    PRINT *, "Enter a number: "
    READ *, num

    ! Check if the integer part of the number is even or odd
    IF (MOD(INT(num), 2) == 0) THEN
        result = "even"
    ELSE
        result = "odd"
    END IF

    PRINT *, num, "is ", result
END PROGRAM E_O
