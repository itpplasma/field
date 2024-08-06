program test_example_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_example_field_init


contains


subroutine test_example_field_init
    use example_field, only: example_field_t

    real(dp), parameter :: tol = 1.0e-9

    type(example_field_t) :: example_field_instance

    call print_test("test_example_field_t")

    if (abs(example_field_instance%ampl) .le. tol) then
        print *, "example_field_instance%ampl = ", example_field_instance%ampl
        call print_fail
        error stop
    end if

    call print_ok
end subroutine test_example_field_init

    
end program test_example_field