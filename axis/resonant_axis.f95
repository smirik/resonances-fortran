module resonant_axis

    use global_parameters
    implicit none

contains

!----------------------------------------------------------------------------------------------
    real(8) function count_axis_2body(resonance, a1, m1)
! Find axis by resonance (Kepler et al.)
! Given:
!   resonance - array of integer (4 numbers: m1,m,p1,p)
!   a1 - semimajor axis of the planet (in AU)
!   m1 - mass of the planet (in MSun)
! Returns:
!   <real(8)> - the corresponding semimajor axis for a given resonance
        real(8):: a1, m1
        integer, dimension(4):: resonance

        count_axis_2body = a1*(1d0 + m1)**(-1d0/3d0)*abs(dble(resonance(2))/resonance(1))**(2d0/3d0)
    end function count_axis_2body

!----------------------------------------------------------------------------------------------
    real(8) function count_axis_3body(resonance, a1, n1, l1, a2, n2, l2)
! Find axis by resonance (Nesvorny 1998, Gallardo 2006)
! Given:
!   resonance - array of integer (6 numbers: m1,m2,m,p1,p2,p)
!   a* - semimajor axis of body
!   n* - mean motion
!   l* - mean longitude
! Returns:
!    <real(8)> - the corresponding semimajor axis for a given resonance

        real(8) :: a1, n1, l1, a2, n2, l2, aa, na, la, eps
        integer, dimension(6):: resonance

        na = (-resonance(1)*n1 - resonance(2)*n2 - resonance(4)*l1 - resonance(5)*l2)/resonance(3)
        if (na < 0d0) then
            count_axis_3body = -1d0
        else
            aa = a_from_n(na) ! formula for s/a
            eps = (a1 - aa)/a1
            la = gp_k/(2d0*pi)*(aa/a1)**0.5d0*(eps*eps)*n1
            na = (-resonance(1)*n1 - resonance(2)*n2 - resonance(4)*l1 - resonance(5)*l2 - resonance(3)*la)/resonance(3)
            aa = a_from_n(na) ! formula for s/a
            count_axis_3body = aa
        endif
        if (isnan(count_axis_3body)) count_axis_3body = 0d0
    end function count_axis_3body

!----------------------------------------------------------------------------------------------
    real(8) function a_from_n(n)
! Get semimajor axis from mean motion
! Given:
!   n - mean motion of object
! Returns:
!   <real(8)> - semimajor axis
        real(8) :: a, n

        a = (gp_k/n)**(2d0/3d0)
        a_from_n = a
    end function a_from_n

!----------------------------------------------------------------------------------------------
    real(8) function n_from_a(a)
! Get mean motion from semimajor axis
! Given:
!   a — semimajor axis
! Returns:
!   <real(8)> - mean motion

!real(8), parameter :: k2 = k*k ! or 0.0002959122082855911025 ? Commented as non-used
        real :: a, n

        n = dsqrt(gp_k*gp_k/(a*a*a))
        n_from_a = n
    end function n_from_a

!----------------------------------------------------------------------------------------------
end module resonant_axis
