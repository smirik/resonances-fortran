test_suite idmatrix_3body_mod

test idmatrix_name_3body_test
    assert_true(trim(idmatrix_name_3body('MARS    ','JUPITER ')) == 'id_matrix_MARS_JUPITER.dat')
end test

test get_idmatrix_3body_status_test
    assert_true(get_idmatrix_3body_status(5,6)>=-1)
end test

test build_idmatrix_3body_test
    integer:: s

    call build_idmatrix_3body('JUPITER ','SATURN  ')
    open(unit=108,file='../id_matrices/id_matrix_JUPITER_SATURN.dat',action='read',iostat=s)
    assert_true(s==0)
end test

end test_suite
