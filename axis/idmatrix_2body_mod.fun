test_suite idmatrix_2body_mod

test idmatrix_name_2body_test
    assert_true(trim(idmatrix_name_2body('MARS    ')) == 'id_matrix_MARS.dat')
end test

test get_idmatrix_2body_status_test
    assert_true(get_idmatrix_2body_status(5)>=-1)
end test

end test_suite

!test build_idmatrix_2body_test
!    integer:: s
!
!    call build_idmatrix_2body('JUPITER ')
!    open(unit=108,file='../id_matrices/id_matrix_JUPITER.dat',action='read',iostat=s)
!    assert_true(s==0)
!end test

