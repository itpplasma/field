program test_example_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_biotsavart_field_init
call test_curla_equal_b


contains


subroutine test_biotsavart_field_init
    use libneo_biotsavart_field, only: biotsavart_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(biotsavart_field_t) :: biotsavart_field
    real(dp) :: x(3), A(3), B(3)

    call print_test("test_biotsavart_field_init")

    call biotsavart_field%biotsavart_field_init()

    x = [1.0_dp, 1.0_dp, 1.0_dp]
    call biotsavart_field%compute_abfield(x, A, B)
    if (maxval(abs(A)) > tol) then
        print *, "A != 0 of default biotsavart field"
        call print_fail
        return
    end if
    if (maxval(abs(B)) > tol) then
        print *, "B != 0 of default biotsavart field"
        call print_fail
        return
    end if

    call print_ok
end subroutine test_biotsavart_field_init


subroutine test_curla_equal_b
    use libneo_biotsavart_field, only: biotsavart_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(biotsavart_field_t) :: biotsavart_field
    real(dp) :: x(3), A(3), B(3), B_from_A(3)
    real(dp) :: temp(3)

    call print_test("test_curla_equal_b")

    print*, "Not implemented"
    call print_fail
    return

    call biotsavart_field%biotsavart_field_init()

    x = [1.0_dp, 1.0_dp, 1.0_dp]

    call biotsavart_field%compute_bfield(x, B)
    B_from_A = compute_curla(biotsavart_field, x, tol)

    if (maxval(abs(B - B_from_A)) > tol) then
        print *, "curl A != B"
        call print_fail
        return
    end if

    call biotsavart_field%compute_abfield(x, A, B)
    call biotsavart_field%compute_afield(x, temp)
    if (maxval(abs(A - temp)) > tol) then
        print *, "A from afield != from abfield"
        call print_fail
        return
    end if
    call biotsavart_field%compute_bfield(x, temp)
    if (maxval(abs(B - temp)) > tol) then
        print *, "B from bfield != from abfield"
        call print_fail
        return
    end if

    call print_ok
end subroutine test_curla_equal_b


function compute_curla(field, x, tol) result(curla)
    use libneo_field_base, only: field_t

    class(field_t), intent(in) :: field
    real(dp), intent(in) :: x(3)
    real(dp), intent(in) :: tol
    real(dp) :: curla(3)

    real(dp) :: x_temp(3), A_temp1(3), A_temp2(3)
    real(dp) :: dA_dx(3,3), delta
    integer :: i

    delta = sqrt(tol)
    x_temp = x
    dA_dx = 0.0_dp
    do i = 1, 3
        x_temp = x
        x_temp(i) = x(i) + delta
        call field%compute_afield(x_temp, A_temp1)
        x_temp(i) = x(i) - delta
        call field%compute_afield(x_temp, A_temp2)
        dA_dx(i,:) = (A_temp1 - A_temp2) / (2.0_dp * delta)
    end do

    curla(1) = dA_dx(2,3) - dA_dx(3,2)
    curla(2) = dA_dx(3,1) - dA_dx(1,3)
    curla(3) = dA_dx(1,2) - dA_dx(2,1)
end function compute_curla

    
end program test_example_field