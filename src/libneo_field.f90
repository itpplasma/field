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
        case("biotsavart")
            field = create_biotsavart_field()
        case default
            print *, "create_field_from_string: Unknown field type"
            error stop
        end select
    end function create_field_from_string


    function create_example_field(ampl, ampl2) result(example_field)
        use libneo_example_field, only: example_field_t

        real(dp), intent(in), optional :: ampl, ampl2
        type(example_field_t) :: example_field

        call example_field%example_field_init(ampl, ampl2)
    end function create_example_field


    function create_biotsavart_field() result(biotsavart_field)
        use libneo_biotsavart_field, only: biotsavart_field_t

        type(biotsavart_field_t) :: biotsavart_field

        call biotsavart_field%biotsavart_field_init()
    end function create_biotsavart_field


    subroutine destroy_field(field)
        class(base_field_t), allocatable, intent(inout) :: field

        call field%field_deinit()
        deallocate(field)
    end subroutine destroy_field


end module libneo_field