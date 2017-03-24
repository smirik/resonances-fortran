test_suite librations

test count_aei_file_records_test
    open(unit = 9, file = 'test.aei', action = 'read')
    assert_equal(11,count_aei_file_records(9))
    close(9)
end test

end test_suite
