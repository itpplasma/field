program test_example_field
use, intrinsic :: iso_fortran_env, only: dp => real64
use test_util, only: print_test, print_ok, print_fail

implicit none


call test_field_init
call test_curla_equal_b
call test_divb_0
call test_compute_abfield


contains


subroutine test_field_init
    use libneo_example_field, only: example_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(example_field_t) :: example_field

    call print_test("test_field_init")

    call example_field%init_field(1.0_dp, 2.0_dp)

    if (abs(example_field%ampl - 1.0_dp) > tol) then
        call print_fail
        return
    end if
    if (abs(example_field%ampl2 - 2.0_dp) > tol) then
        call print_fail
        return
    end if

    call print_ok
end subroutine test_field_init


subroutine test_curla_equal_b
    use libneo_example_field, only: example_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(example_field_t) :: example_field
    real(dp) :: x(3), A(3), B(3), B_from_A(3)
    real(dp) :: temp(3)

    call print_test("test_curla_equal_b")

    call example_field%init_field(1.0_dp, 2.0_dp)

    x = [1.0_dp, 1.0_dp, 1.0_dp]

    call example_field%compute_bfield(x, B)
    B_from_A = compute_curla(example_field, x, tol)

    if (maxval(abs(B - B_from_A)) > tol) then
        print *, "curl A != B"
        call print_fail
        return
    end if

    call example_field%compute_abfield(x, A, B)
    call example_field%compute_afield(x, temp)
    if (maxval(abs(A - temp)) > tol) then
        print *, "A from afield != from abfield"
        call print_fail
        return
    end if
    call example_field%compute_bfield(x, temp)
    if (maxval(abs(B - temp)) > tol) then
        print *, "B from bfield != from abfield"
        call print_fail
        return
    end if

    call print_ok
end subroutine test_curla_equal_b


function compute_curla(field, x, tol) result(curla)
    use libneo_base_field, only: base_field_t

    class(base_field_t), intent(in) :: field
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


subroutine test_divb_0
    use libneo_example_field, only: example_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(example_field_t) :: example_field
    real(dp) :: x(3), divb

    call print_test("test_divb_0")

    call example_field%init_field(1.0_dp, 2.0_dp)

    x = [1.0_dp, 1.0_dp, 1.0_dp]

    divb = compute_divb(example_field, x, tol)

    if (abs(divb) > tol) then
        print *, "div B = ", divb
        call print_fail
        return
    end if

    call print_ok
end subroutine test_divb_0


function compute_divb(field, x, tol) result(divb)
    use libneo_base_field, only: base_field_t

    class(base_field_t), intent(in) :: field
    real(dp), intent(in) :: x(3)
    real(dp), intent(in) :: tol

    real(dp) :: divb

    real(dp) :: x_temp(3), B_temp1(3), B_temp2(3)
    real(dp) :: dB_dx(3), delta
    integer :: i

    delta = sqrt(tol)
    x_temp = x
    dB_dx = 0.0_dp
    do i = 1, 3
        x_temp = x
        x_temp(i) = x(i) + delta
        call field%compute_bfield(x_temp, B_temp1)
        x_temp(i) = x(i) - delta
        call field%compute_bfield(x_temp, B_temp2)
        dB_dx(i) = (B_temp1(i) - B_temp2(i)) / (2.0_dp * delta)
    end do

    divb = sum(dB_dx)
end function compute_divb


subroutine test_compute_abfield
    use libneo_example_field, only: example_field_t

    real(dp), parameter :: tol = 1.0e-9_dp

    type(example_field_t) :: example_field
    real(dp) :: x(3), A(3), B(3), temp(3)

    call print_test("test_compute_abfield")

    call example_field%init_field(1.0_dp, 2.0_dp)

    x = [1.0_dp, 1.0_dp, 1.0_dp]

    call example_field%compute_abfield(x, A, B)
    call example_field%compute_afield(x, temp)
    if (maxval(abs(A - temp)) > tol) then
        print *, "A from afield != from abfield"
        call print_fail
        return
    end if
    call example_field%compute_bfield(x, temp)
    if (maxval(abs(B - temp)) > tol) then
        print *, "B from bfield != from abfield"
        call print_fail
        return
    end if

    call print_ok
end subroutine test_compute_abfield
    
end program test_example_field