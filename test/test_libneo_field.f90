program test_libneo_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_create_field_from_string
call test_create_biotsavart_field


contains


subroutine test_create_field_from_string
    use libneo_field, only: create_field_from_string, destroy_field
    use libneo_base_field, only: base_field_t

    class(base_field_t), allocatable :: field

    call print_test("test_create_field_from_string")

    field = create_field_from_string("example")
    if (.not.allocated(field)) then
        call print_fail
        return
    end if

    call destroy_field(field)

    if (allocated(field)) then
        call print_fail
        return
    end if

    call print_ok
end subroutine test_create_field_from_string


subroutine test_create_biotsavart_field
    use libneo_field, only: create_field_from_string, destroy_field
    use libneo_base_field, only: base_field_t

    class(base_field_t), allocatable :: field

    call print_test("test_create_biotsavart_field")

    field = create_field_from_string("biotsavart")
    if (.not.allocated(field)) then
        call print_fail
        return
    end if

    call destroy_field(field)

    if (allocated(field)) then
        call print_fail
        return
    end if

    call print_ok
end subroutine test_create_biotsavart_field
    

end program test_libneo_field

