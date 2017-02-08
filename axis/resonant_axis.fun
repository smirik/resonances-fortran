test_suite resonant_axis

! Global variables can be declared here
!real, parameter :: radius = 1.5d0
real, parameter :: a_default = 1.00000261, k_default = 0.017202098795, n_default = 1.72020309E-02 

setup
    ! Place code here that should run before each test
end setup

teardown
    ! This code runs immediately after each test
end teardown

test a_from_n_test
    real :: a, n, res
    n = n_default 
    res = (0.017202098795/n)**(2.0/3)
    a = a_from_n(n)
    assert_equal_within(a, 1.0, 1e-4)
end test

test n_from_a_test
    real :: a, n, res
    a = a_default 
    res = (k_default/n)**(2.0/3)
    n = n_from_a(a)
    assert_equal_within(n, n_default, 1e-4)
end test

test n_from_a_reverse_test
    real :: a, n
    a = a_default 
    n = n_from_a(a)
    a = a_from_n(n)
    assert_equal_within(a, 1.0, 1e-4)
end test

test count_axis_2body_test
    real(8):: a1,m1,a
    integer,dimension(4)::resonance
    
    resonance=(/ 5, -2, 0, -3 /) 
    a1=5.20248019d0
    m1=9.547919384243222d-4
    a=count_axis_2body(resonance,a1,m1)
    assert_equal_within(a,2.8253d0,1d-2)
! Kirkwood gap case test    
    resonance=(/ 3, -1, 0, -2 /) 
    a=count_axis_2body(resonance,a1,m1)
    assert_equal_within(a,2.5020d0,1d-2)

end test

test count_axis_3body_test
    real :: a
    real :: a_a, n_a, l_a
    real, dimension(6) :: resonance
    resonance = (/ 5.0, -2.0, -2.0, 0.0, 0.0, -1.0 /)
    a = count_axis_3body(resonance, 5.204267, 0.00145024678779705, 5.65063905672262e-08, 9.58201720, &
                   0.000583991090866933, 3.74890765513312e-07)
    assert_equal_within(a, 3.17, 1e-2) ! resonance value for 5J-2S-2
end test

end test_suite

! assert_true(expression)
! assert_false(expression)
! assert_equal(a, b)
! assert_real_equal(a, b)
! assert_equal_with(a, b, tol)
! assert_array_equal(a,b
