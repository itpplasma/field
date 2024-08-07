module libneo_field
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use libneo_base_field, only: base_field_t

    implicit none

    contains

    function create_field_from_string(field_type) result(field)
        character(*), intent(in) :: field_type
        class(base_field_t), allocatable :: field

        select case(field_type)
        case("example")
            field = create_example_field()
        case default
            print *, "create_field_from_string: Unknown field type"
            error stop
        end select
    end function create_field_from_string

    function create_example_field(ampl, ampl2) result(example_field_instance)
        use libneo_example_field, only: example_field_t

        real(dp), intent(in), optional :: ampl, ampl2
        type(example_field_t) :: example_field_instance

        call example_field_instance%init_field(ampl, ampl2)
    end function create_example_field

end module libneo_field