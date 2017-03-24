test_suite resonant_axis

real(8), parameter:: a_default = 1.00000261d0, k_default = 0.017202098795d0, n_default = 1.72020309d-02 

test a_from_n_test
    assert_equal_within(a_from_n(n_default), a_default, 1d-4)
end test

test n_from_a_test
    assert_equal_within(n_from_a(a_default), n_default, 1d-4)
end test

test n_from_a_reverse_test
    real(8):: a, n
    
    n = n_from_a(a_default)
    a = a_from_n(n)
    assert_equal_within(a, a_default, 1d-4)
end test

test count_axis_2body_test
    real(8):: a1, m1, a
    integer,dimension(4):: resonance = (/ 5, -2, 0, -3 /) 

    a1 = 5.20248019d0
    m1 = 9.547919384243222d-4
    a = count_axis_2body(resonance, a1, m1)
    assert_equal_within(a, 2.8253d0, 1d-2)
! Kirkwood gap case test    
    resonance=(/ 3, -1, 0, -2 /) 
    a = count_axis_2body(resonance, a1, m1)
    assert_equal_within(a, 2.5020d0, 1d-2)
end test

test count_axis_3body_test
    real(8):: a
    real(8):: a_a, n_a, l_a
    integer, dimension(6):: resonance = (/ 5, -2, -2, 0, 0, -1 /)

! Resonance value for 5J-2S-2
    a = count_axis_3body(resonance, 5.204267d0, 0.00145024678779705d0, 5.65063905672262d-08, &
        0.000583991090866933d0, 3.74890765513312d-07)
    assert_equal_within(a, 3.17d0, 1d-2)
end test

end test_suite
