module resonant_axis
    implicit none

contains
    ! find axis by resonance (Nesvorny 1998, Gallardo 2006)
    ! Parameters: resonance — array of Integer (6 numbers)
    ! a_* — semimajor axis of body, n_* — mean motion, l_* — mean longitude
    ! return float — the corresponding value for given resonance of semimajor axis
    
    
    
real(8) function count_axis_2body(resonance, a1,m1)
! Find axis by resonance (Kepler et al.)
! Given:
!   resonance - array of integer (4 numbers: m1,m,p1,p)
!   a1 - semimajor axis of the planet (in AU)
!   m1 - mass of the planet (in MSun)
! Returns:
!   <real(8)> - the corresponding semimajor axis for a given resonance

    real(8):: a1,m1
    integer,dimension(4):: resonance
    
    count_axis_2body= a1*(1d0+m1)**(-1d0/3d0)*abs(dble(resonance(2))/resonance(1))**(2d0/3d0)
end function count_axis_2body




    function count_axis_3body(resonance, a_j, n_j, l_j, a_s, n_s, l_s)
        implicit none

        real :: a_j, n_j, l_j, a_s, n_s, l_s, a_a, n_a, l_a, eps
        real :: count_axis_3body

        real, dimension(6) :: resonance

        real, parameter :: k = 0.017202098795, pi = 3.141592653589

        n_a = (-resonance(1)*n_j - resonance(2)*n_s - resonance(4)*l_j - resonance(5)*l_s)/resonance(3)
        if (n_a < 0) then
            count_axis_3body = -1.0
        else
            a_a = a_from_n(n_a) ! formula for s/a
            eps = (a_j - a_a)/a_j
            l_a = k/(2*pi)*(a_a/a_j)**0.5*(eps**2)*n_j
            n_a = (-resonance(1)*n_j - resonance(2)*n_s - resonance(4)*l_j - resonance(5)*l_s - resonance(3)*l_a)/resonance(3)
            a_a = a_from_n(n_a) ! formula for s/a
            count_axis_3body = a_a
        end if
        if (isnan(count_axis_3body)) then
            count_axis_3body = 0.0
        end if
    end function count_axis_3body

    ! get semimajor axis from mean motion
    ! n — mean motion of object
    function a_from_n(n)
        implicit none

        real, parameter :: k = 0.017202098795
        real :: a, n

        real :: a_from_n

        a = (k/n)**(2.0/3.0)
        a_from_n = a
    end function a_from_n

    ! get mean motion from semimajor axis
    ! a — semimajor axis
    function n_from_a(a) result(mean_motion)
        implicit none

        real, parameter :: k2 = 0.0002959122082855911025
        real :: a, n
        real :: mean_motion

        n = (k2/(a**3))**0.5
        mean_motion = n
    end function n_from_a

end module resonant_axis
