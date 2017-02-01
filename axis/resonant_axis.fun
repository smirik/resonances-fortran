test_suite resonant_axis

! Global variables can be declared here
!real, parameter :: radius = 1.5d0

setup
    ! Place code here that should run before each test
end setup

teardown
    ! This code runs immediately after each test
end teardown

test a_from_n_test
    real :: a, n, res
    n = 1.72020309E-02
    res = (0.017202098795/n)**(2.0/3)
    a = a_from_n(n)
    assert_equal_within(a, 1.0, 1e-4)
end test

test n_from_a_test
    real :: a, n, res
    a = 1.00000261
    res = (0.017202098795/n)**(2.0/3)
    n = n_from_a(a)
    assert_equal_within(n, 1.72020309E-02, 1e-4)
end test

test n_from_a_reverse_test
    real :: a, n
    a = 1.00000261
    n = n_from_a(a)
    a = a_from_n(n)
    assert_equal_within(a, 1.0, 1e-4)

end test_suite

! assert_true(expression)
! assert_false(expression)
! assert_equal(a, b)
! assert_real_equal(a, b)
! assert_equal_with(a, b, tol)
! assert_array_equal(a,b