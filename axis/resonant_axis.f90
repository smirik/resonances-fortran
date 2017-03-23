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

        count_axis_2body = a1*(1d0 + m1)**(-1d0/3d0) &
        *abs(dble(resonance(2))/resonance(1))**(2d0/3d0)
    end function count_axis_2body

!----------------------------------------------------------------------------------------------
    real(8) function count_axis_3body(resonance, a1, n1, l1, n2, l2)
    ! Find axis by resonance (Nesvorny 1998, Gallardo 2006)
    ! Given:
    !   resonance - array of integer (6 numbers: m1,m2,m,p1,p2,p)
    !   a* - semimajor axis of body
    !   n* - mean motion
    !   l* - mean longitude
    ! Returns:
    !    <real(8)> - the corresponding semimajor axis for a given resonance
        real(8):: a1, n1, l1, n2, l2, aa, na, la, eps
        integer, dimension(6):: resonance

        na = (-resonance(1)*n1 - resonance(2)*n2 - resonance(4)*l1 - resonance(5)*l2) &
        /resonance(3)
        if (na < 0d0) then
            count_axis_3body = -1d0
            return
        else
            aa = (gp_k*gp_k/na/na)**(1d0/3d0) ! formula for s/a
            eps = (a1 - aa)/a1
            la = gp_k/(2d0*pi)*dsqrt(aa/a1)*(eps*eps)*n1
            na = (-resonance(1)*n1 - resonance(2)*n2 - resonance(4)*l1 - resonance(5)*l2 &
            - resonance(3)*la)/resonance(3)
            aa = (gp_k*gp_k/na/na)**(1d0/3d0) ! formula for s/a
            count_axis_3body = aa
        endif
        if (isnan(count_axis_3body)) count_axis_3body = 0d0
        if (count_axis_3body > 1d3) count_axis_3body = 0d0
    end function count_axis_3body

!----------------------------------------------------------------------------------------------
    real(8) function a_from_n(n)
    ! Get semimajor axis from mean motion
    ! This function is left for support. It is not recommended to use it anymore.
    ! Given:
    !   n - mean motion of object
    ! Returns:
    !   <real(8)> - semimajor axis
        real(8):: n

        a_from_n = (gp_k/n)**(2d0/3d0)
    end function a_from_n

!----------------------------------------------------------------------------------------------
    real(8) function n_from_a(a)
    ! Get mean motion from semimajor axis
    ! Given:
    !   a - semimajor axis
    ! Returns:
    !   <real(8)> - mean motion
        real(8):: a

        n_from_a = dsqrt(gp_k*gp_k/(a*a*a))
    end function n_from_a

!----------------------------------------------------------------------------------------------
end module resonant_axis
