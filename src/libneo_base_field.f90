module libneo_base_field
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none


type, abstract :: base_field_t
    contains
    procedure(compute_afield), deferred :: compute_afield
    procedure(compute_bfield), deferred :: compute_bfield
    procedure(compute_abfield), deferred :: compute_abfield
    procedure(field_deinit), deferred :: field_deinit
end type base_field_t


interface
    subroutine compute_afield(self, x, A)
        import :: base_field_t, dp
        class (base_field_t), intent(in) :: self
        real(dp), intent(in) :: x(3)
        real(dp), intent(out) :: A(3)
    end subroutine
end interface


interface
    subroutine compute_bfield(self, x, B)
        import :: base_field_t, dp
        class (base_field_t), intent(in) :: self
        real(dp), intent(in) :: x(3)
        real(dp), intent(out) :: B(3)
    end subroutine
end interface
    

interface
    subroutine compute_abfield(self, x, A, B)
        import :: base_field_t, dp
        class (base_field_t), intent(in) :: self
        real(dp), intent(in) :: x(3)
        real(dp), intent(out) :: A(3), B(3)
    end subroutine
end interface


interface
    subroutine field_deinit(self)
        import :: base_field_t
        class (base_field_t), intent(inout) :: self
    end subroutine
end interface


end module libneo_base_field
