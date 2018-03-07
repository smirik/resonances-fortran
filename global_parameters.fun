test_suite global_parameters

test planet_id_test
    assert_equal(5,planet_id('JUPITER'))
end test

test planet_name_test
    character(8):: pl1 = 'JUPITER'

    assert_equal(pl1,planet_name(5))
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
    assert_equal(1,gcd(3,gcd(5,9)))
end test

test norm_ang_test
    real(8):: pi=dacos(-1d0)

    assert_equal(pi/4d0, norm_ang(pi/4d0))
    assert_equal(pi/4d0, norm_ang(pi/4d0+2d0*pi))
    assert_equal(pi/4d0, norm_ang(pi/4d0-2d0*pi))
end test

test idm_index_test
    assert_equal(1,idm_index(1,2))
    assert_equal(36,idm_index(8,9))
    assert_equal(25,idm_index(4,8))
end test

end test_suite
