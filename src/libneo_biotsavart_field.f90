module libneo_biotsavart_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use libneo_base_field, only: base_field_t
use biotsavart, only: coils_t

implicit none


type, extends(base_field_t) :: biotsavart_field_t
    type(coils_t) :: coils
    contains
        procedure :: biotsavart_field_init
        procedure :: compute_abfield
        procedure :: compute_afield
        procedure :: compute_bfield
        procedure :: field_deinit
end type biotsavart_field_t


contains


subroutine biotsavart_field_init(self, coils_file)
    use biotsavart, only: load_coils_from_file, coils_init

    class(biotsavart_field_t), intent(out) :: self
    character(*), intent(in), optional :: coils_file

    if (present(coils_file)) then
        call load_coils_from_file(coils_file, self%coils)
    else
        call coils_init(x = (/0.0_dp,0.0_dp/), &
                        y = (/0.0_dp,0.0_dp/), &
                        z = (/0.0_dp,1.0_dp/), &
                        current = (/1.0_dp,0.0_dp/), &
                        coils = self%coils)
    end if
end subroutine biotsavart_field_init


subroutine compute_abfield(self, x, A, B)
    class(biotsavart_field_t), intent(in) :: self
    real(dp), intent(in) :: x(3)
    real(dp), intent(out) :: A(3), B(3)

    call self%compute_afield(x, A)
    call self%compute_bfield(x, B)
end subroutine compute_abfield


subroutine compute_afield(self, x, A)
    use biotsavart, only: compute_vector_potential

    class(biotsavart_field_t), intent(in) :: self
    real(dp), intent(in) :: x(3)
    real(dp), intent(out) :: A(3)

    A = compute_vector_potential(self%coils, x)
end subroutine compute_afield


subroutine compute_bfield(self, x, B)
    use biotsavart, only: compute_magnetic_field

    class(biotsavart_field_t), intent(in) :: self
    real(dp), intent(in) :: x(3)
    real(dp), intent(out) :: B(3)

    B = compute_magnetic_field(self%coils, x)
end subroutine compute_bfield


subroutine field_deinit(self)
    use biotsavart, only: coils_deinit

    class(biotsavart_field_t), intent(inout) :: self

    call coils_deinit(self%coils)
end subroutine field_deinit


end module libneo_biotsavart_field