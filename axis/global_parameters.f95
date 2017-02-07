module global_parameters
    implicit none

    real(8), parameter:: pi = dacos(-1d0)
    real(8), parameter:: twopi = pi*2
!----------------------------------------------------------------------------------------------
    real(8), parameter:: gp_k = 0.017202098795d0 ! or 1.720209895d−2 by IERS Conv. 2010

!----------------------------------------------------------------------------------------------
    real(8), dimension(0:9)::a_pl = (/0d0, &
                                      0.38709843d0, 0.72332102d0, 1.00000018d0, &
                                      1.52371243d0, 5.20248019d0, 9.54149883d0, &
                                      19.18797948d0, 30.06952752d0, 39.48686035d0/)
! Taken from http://ssd.jpl.nasa.gov/?planet_pos
! Link case for "Keplerian elements for 3000 BC to 3000 AD"

!----------------------------------------------------------------------------------------------
    real(8), dimension(0:9)::res_a_std_delta = (/0d0, &
                                                 1d-2, 1d-2, 1d-2, &
                                                 1d-2, 1d-2, 1d-1, &
                                                 2.5d-1, 4d-1, 1.4d0/)

!----------------------------------------------------------------------------------------------
    real(8), dimension(0:9)::m_pl = (/0d0, &
                                      1.6601367952719304D-07, 2.4478383396645447D-06, 3.0404326462685257D-06, &
                                      3.2271514450538743D-07, 9.547919384243222D-04, 2.858859806661029D-04, &
                                      4.3662440433515637D-05, 5.151389020535497D-05, 7.407407407407407D-09/)
! Used: http://ssd.jpl.nasa.gov/?planet_phys_par

!----------------------------------------------------------------------------------------------
    integer, parameter:: gp_max_order = 20

!----------------------------------------------------------------------------------------------
    type idmrow_2body
        integer, dimension(4)::resonance
        real(8):: res_a
    end type idmrow_2body

    type idmrow_3body
        integer, dimension(6)::resonance
        real(8):: res_a
    end type idmrow_3body

    type idm_2body
        type(idmrow_2body), dimension(:), allocatable:: matrix
    end type idm_2body

    type idm_3body
        type(idmrow_3body), dimension(:), allocatable:: matrix
    end type idm_3body

    type(idm_2body), dimension(9):: idmatrix_2body
    type(idm_3body), dimension(72):: idmatrix_3body
!----------------------------------------------------------------------------------------------
contains

!----------------------------------------------------------------------------------------------
    integer function planet_id(s)
! Get planet ID
! Given:
!   s - planet name IN CAPITALS
! Returns:
!   <integer> - ID
        character(*) s
        integer i

        select case (s)
        case ('MERCURY')
            i = 1
        case ('VENUS')
            i = 2
        case ('EARTHMOO')
            i = 3
        case ('MARS')
            i = 4
        case ('JUPITER')
            i = 5
        case ('SATURN')
            i = 6
        case ('URANUS')
            i = 7
        case ('NEPTUNE')
            i = 8
        case ('PLUTO')
            i = 9
        case default
            i = 0
            write (*, *) 'Warning! Detected incorrect planet name, be careful.'
        end select
        planet_id = i
    end function planet_id

!----------------------------------------------------------------------------------------------
    function planet_name(i) result(s)
! Get planet ID
! Given:
!   s - planet name IN CAPITALS
! Returns:
!   <integer> - ID
        character(8) s
        integer i

        select case (i)
        case (1)
            s = 'MERCURY'
        case (2)
            s = 'VENUS'
        case (3)
            s = 'EARTHMOO'
        case (4)
            s = 'MARS'
        case (5)
            s = 'JUPITER'
        case (6)
            s = 'SATURN'
        case (7)
            s = 'URANUS'
        case (8)
            s = 'NEPTUNE'
        case (9)
            s = 'PLUTO'
        case default
            write (*, *) 'Warning! Detected incorrect planet id, be careful.'
        end select
    end function planet_name

!----------------------------------------------------------------------------------------------
    integer function nod(a, b)
! Find gcd - greatest common divisor
! Given:
!   a,b - positive integer numbers
! Returns:
!   <integer> - gcd
        integer::a, b, c1, c2, c3, c4

        c1 = a; c2 = b
        do while (c1 /= c2)
            c3 = c1
            c4 = c2
            c1 = min(c3, c4)
            c2 = abs(c3 - c4)
        enddo
        nod = c1
    end function nod

!----------------------------------------------------------------------------------------------
end module global_parameters
