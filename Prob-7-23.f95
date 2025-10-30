subroutine derivative(func, x_i, x_i_1, x_prime)
    real, intent(in) :: x_i, x_i_1
    real, intent(out) :: x_prime
    real, external :: func 

    x_prime = (func(x_i_1) - func(x_i)) / (x_i_1 - x_i)   
end subroutine derivative

function f(x) result(res)
    real :: res 
    real, intent(in) :: x 

    res = sin(x)

end function f 

program derivative_of_function
    implicit none
    integer, parameter :: nsamp = 100
    real :: start_val, step_size , ans
    integer :: i 
    real :: vec(nsamp)
    real, external :: f 

    start_val = 0. 
    step_size = 0.05 

    do i = 1, nsamp
        vec(i) = start_val + i * step_size
    end do 
    
    call derivative(f, vec(1), vec(2), ans)

    print *, 'derivative = ', ans 
    print *, cos(vec(1))

end program derivative_of_function