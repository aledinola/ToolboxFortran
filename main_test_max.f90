program main
    
    use mod_numerical, only: max_nonconvex
    implicit none    
    
    real(8) :: a,b,max_val,arg_min,true_min
    
    !max_nonconvex(f, a, b, xmax, fmax, mytol, mymaxit, mynx)
    
    a = 0d0
    b = 5d0
    true_min = 4.691893d0 ! global minimum (argmin)
    
    call max_nonconvex(fun, a, b, arg_min, max_val, mynx=15)
    
    write(*,*) "Interval [a,b]", a,b
    write(*,*) "Arg min       = ", arg_min
    write(*,*) "Minimum value = ", -max_val
    write(*,*) "True global arg-min = ", true_min
    
    if (abs(arg_min-true_min)<1d-4) then
        write(*,*) "Test passed!"
    else
        write(*,*) "Test failed!"
    endif
    
    write(*,*) "Program will terminate.."
    pause
    
    contains
    
    function fun(x) result(F)
        ! This function has many local minima in the interval [0,5]
        ! The unique global minimum is at x = 4.691893 
        ! Variables
        real(8), intent(in) :: x
        real(8) :: F
        ! Body of fun
        F = x*cos(x*x)
        
        F = -F
        
    end function fun
    
end program main
    