test_suite global_parameters

! Global variables can be declared here

setup
    ! Place code here that should run before each test
end setup

teardown
    ! This code runs immediately after each test
end teardown

test planet_id_test
    integer :: p1=5,p
    character(8):: pl1= 'JUPITER'
    p=planet_id(pl1)
    assert_equal(p1,p)
end test

test planet_name_test
    integer :: p1=5
    character(8):: pl1= 'JUPITER',pl
    pl=planet_name(p1)
    assert_equal(pl1,pl)
end test

test planet_id_reverse_test
    integer :: p1=5,p
    character(8):: pl1= 'JUPITER',pl
    pl=planet_name(p1)
    p=planet_id(pl)
    assert_equal(p1,p)
end test

test gcd_test
    assert_equal(1,gcd(5,2))
    assert_equal(1,gcd(2,5))
    assert_equal(12,gcd(60,12))
    assert_equal(12,gcd(12,60))
end test

end test_suite
