! Implicit Declarations 
program typeBinding
    implicit none  ! This ensures all variables must be explicitly declared
    integer :: i    ! Explicitly declare i as an integer
    i = 5          ! Assign 5 to i
    print *, i     ! Print the value of i
end program
