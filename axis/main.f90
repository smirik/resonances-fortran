program resonances
    use resonant_axis
    implicit none
    ! a_j — semimajor axe of Jupiter, n_j — mean motion of Jupiter, l_j — mean longitude of Jupiter
    real, parameter :: a_j = 5.204267, n_j = 0.00145024678779705, l_j = 5.65063905672262e-08
    real, parameter :: a_s  = 5.204267, n_s = 0.000583991090866933, l_s = 3.74890765513312e-07
    ! order of resonance
    integer, parameter :: order = 7

    real :: a_a, n_a, l_a
    real :: tmp, tmp2, tmp3, diff
    integer :: i,j,k
    real, dimension(6) :: resonance 
  
    ! cycle: we consider all possible combinations
    ! let's take the first element in resonance array as positive integer
    ! the other 2 elements are from -order to order
    ! From all subresonances we consider just the main subresonance [i,j,k,0,0,-i-k-j]
    do i=1,8
        do j=-order,order
            k_loop: do k=-order,order
              diff = (0.0 - i - j - k)
              resonance = (/ i*1.0, j*1.0, k*1.0, 0.0, 0.0, diff /)
              ! i,j,k = 0 — no resonance
              if ( (i == 0) .AND. (j==0) .AND. (k==0) ) then
                  exit k_loop
              end if

              tmp = count_axis(resonance, a_j, n_j, l_j, a_s, n_s, l_s)

              ! Order of resonance should be less than 7 and the resonant semi-axis should not be equal to 0
              if ( abs(diff) < order .AND. tmp > 0.0 ) then
                  write (*, 20) resonance, tmp
              end if

            end do k_loop
        end do
    end do  
  
20 format(3f10.1, 3f10.1, 3f10.4)
  
end program
