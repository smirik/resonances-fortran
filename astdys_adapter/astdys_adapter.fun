test_suite astdys_adapter

test check_source_test
    assert_equal(check_source('test.bin'), 5)
end test

test greater_test
    assert_true(greater('55','500','lex'))
    assert_true(greater('500','55','num'))
end test

end test_suite
