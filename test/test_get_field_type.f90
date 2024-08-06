program test_create_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_get_field_type_example


contains


subroutine test_get_field_type_example
    use get_field_type, only: field_type_from_string
    use field, only: field_t

    class(field_t), allocatable :: field_instance

    call print_test("test_get_field_type_example")

    field_instance = field_type_from_string("example")
    if (.not.allocated(field_instance)) then
        call print_fail
        !error stop
    end if

    call print_ok
end subroutine test_get_field_type_example

    
end program test_create_field

