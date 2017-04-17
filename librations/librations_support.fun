test_suite librations_support

test mbessel0_test
    assert_equal_within(mbessel0(2d0), 2.25d0, 1d-1)
end test
test count_aei_file_records_test
    open(unit = 9, file = 'test.aei', action = 'read')
    assert_equal(11,count_aei_file_records(9))
    close(9)
end test

end test_suite
