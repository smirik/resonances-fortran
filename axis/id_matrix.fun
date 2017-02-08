test_suite id_matrix

setup
    ! Place code here that should run before each test
end setup

teardown
    ! This code runs immediately after each test
end teardown

test get_idmatrix_2body_status_test
    integer :: pl_id=5, p
    integer :: status=-1
    
    p=get_idmatrix_2body_status(pl_id)
    assert_true(p>=-1)
end test

test build_idmatrix_2body_test
    integer :: s
    integer :: max_order = 20
    character(8):: pl_name'=JUPITER'
    
    call build_idmatrix_2body(pl_name,max_order)
    open(unit=108,file='id_matrix_JUPITER.dat',action='read',iostat=s)
    assert_true(s==0)
end test


end test_suite

! assert_true(expression)
! assert_false(expression)
! assert_equal(a, b)
! assert_real_equal(a, b)
! assert_equal_with(a, b, tol)
! assert_array_equal(a,b
