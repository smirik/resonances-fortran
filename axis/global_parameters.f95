module global_parameters
implicit none

real(8),parameter:: pi=dacos(-1d0)
real(8),parameter:: twopi=pi*2
real(8),parameter:: gp_k=0.017202098795d0! or 1.720209895d−2 by IERS Conv. 2010

real(8),dimension(9)::a_pl=(/ 0.38709843d0,0.72332102d0,1.00000018d0,&
                              1.52371243d0,5.20248019d0,9.54149883d0,&
                              19.18797948d0,30.06952752d0,39.48686035d0 /)
! Taken from http://ssd.jpl.nasa.gov/?planet_pos
! Link case for "Keplerian elements for 3000 BC to 3000 AD"

real(8),dimension(9)::m_pl=(/ &
    1.6601367952719304D-07,2.4478383396645447D-06,3.0404326462685257D-06,&
    3.2271514450538743D-07,9.547919384243222D-04,2.858859806661029D-04,&
    4.3662440433515637D-05,5.151389020535497D-05,7.407407407407407D-09 /)
! Used: http://ssd.jpl.nasa.gov/?planet_phys_par

integer,parameter:: max_order = 20

contains


integer function planet_id(s)
! Get planet ID
! Given:
!   s - planet name IN CAPITALS
! Returns:
!   <integer> - ID
    character(*) s
    integer i

    select case (s)
        case('MERCURY')
            i=1
        case('VENUS')
            i=2
        case('EARTHMOO')
            i=3
        case('MARS')
            i=4
        case('JUPITER')
            i=5
        case('SATURN')
            i=6
        case('URANUS')
            i=7
        case('NEPTUNE')
            i=8
        case('PLUTO')
            i=9
        case default
            i=0
            write(*,*)'Warning! Detected incorrect planet name, be careful.'
    end select
    planet_id=i
end function planet_id

end module global_parameters