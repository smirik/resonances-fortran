test_suite global_parameters

! Global variables can be declared here
!real, parameter :: radius = 1.5d0
real, parameter :: a_default = 1.00000261, k_default = 0.017202098795, n_default = 1.72020309E-02 

setup
    ! Place code here that should run before each test
end setup

teardown
    ! This code runs immediately after each test
end teardown

test planet_id_test
    integer :: p1=5,p
    character(8):: pl1= 'JUPITER'
    p=planet_id(pl)
    assert_equal(p1,p)
end test

test planet_name_test
    integer :: p1=5
    character(8):: pl= 'JUPITER',pl
    pl=planet_name(p1)
    assert_equal(pl1,p)
end test

test planet_id_reverse_test
    integer :: p1=5,p
    character(8):: pl1= 'JUPITER',pl
    pl=planet_anrt_name(p1)
    p=planet_id(pl)
    assert_equal(p1,p)
end test

test gcd_test
    integer :: a1=5,b1=2,g1=1
    integer :: a2=60,b2=12,g2=12
    integer :: g
    g=gcd(a1,b1)
    assert_equal(g1,g)
    g=gcd(a2,b2)
    assert_equal(g2,g)
end test

end test_suite

! assert_true(expression)
! assert_false(expression)
! assert_equal(a, b)
! assert_real_equal(a, b)
! assert_equal_with(a, b, tol)
! assert_array_equal(a,b
