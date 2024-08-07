program test_create_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_create_field_example


contains


subroutine test_create_field_example
    use create_field, only: create_field_from_string
    use field, only: field_t

    class(field_t), allocatable :: field_instance

    call print_test("test_create_field_example")

    field_instance = create_field_from_string("example")
    if (.not.allocated(field_instance)) then
        call print_fail
        !error stop
    end if

    call print_ok
end subroutine test_create_field_example

    
end program test_create_field

