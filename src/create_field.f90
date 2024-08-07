module create_field
    use field, only: field_t
    use example_field, only: example_field_t

    implicit none

    contains

    function create_field_from_string(field_type) result(field_instance)
        character(*), intent(in) :: field_type
        class(field_t), allocatable :: field_instance

        select case(field_type)
        case("example")
            field_instance = example_field_t()
        case default
            print *, "create_field: Unknown field type"
            error stop
        end select
    end function create_field_from_string

end module create_field