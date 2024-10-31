PROGRAM Calculator
    IMPLICIT NONE
    REAL :: first_num, second_num
    REAL :: sum, sub, mul, div

    PRINT *, "Enter the first number: "
    READ *, first_num

    PRINT *, "Enter the second number: "
    READ *, second_num

    sum = first_num + second_num
    sub = first_num - second_num
    mul = first_num * second_num
    div = first_num / second_num
    PRINT *, first_num, " + ", second_num, " = ", sum
    PRINT *, first_num, " - ", second_num, " = ", sub
    PRINT *, first_num, " * ", second_num, " = ", mul
    PRINT *, first_num, " / ", second_num, " = ", div

END PROGRAM Calculator
