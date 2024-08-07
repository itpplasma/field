program test_libneo_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_libneo_field_example


contains


subroutine test_libneo_field_example
    use libneo_field, only: create_field_from_string
    use libneo_base_field, only: base_field_t

    class(base_field_t), allocatable :: field

    call print_test("test_libneo_field_example")

    field = create_field_from_string("example")
    if (.not.allocated(field)) then
        call print_fail
        !error stop
    end if

    call print_ok
end subroutine test_libneo_field_example

    
end program test_libneo_field

