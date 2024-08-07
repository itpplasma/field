program test_example_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_example_field_init
call test_example_field_compute_afield


contains


subroutine test_example_field_init
    use example_field, only: example_field_t

    call print_ok
end subroutine test_example_field_init


subroutine test_example_field_compute_afield
    use example_field, only: example_field_t

    call print_ok
end subroutine test_example_field_compute_afield

    
end program test_example_field