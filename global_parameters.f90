module global_parameters
    implicit none

! Global variables ----------------------------------------------------------------------------
    real(8), parameter:: pi = dacos(-1d0), deg2pi = dacos(-1d0)/18d1
    real(8), parameter:: twopi = pi*2
    real(8), parameter:: gp_k = 0.017202098795d0 ! or 1.720209895d−2 by IERS Conv. 2010

! 13.03.2017
! IMPORTANT PARAMETER: it is used for program version where we wanted to use only MERCURY integrator

    integer,parameter:: use_only_integration = 1

! Parent directory
    character(100), parameter::pwd="/home/ilya/Документы/resonances-fortran/"

    
    
!----------------------------------------------------------------------------------------------
! Parameters for libration module -------------------------------------------------------------
    real(8), parameter:: circulation_parameter = 1d3
    real(8), parameter:: libration_parameter = 2d4
    integer, parameter:: aei_header = 4
    integer, parameter:: bin_numrec = 480482
! Parameters for integrator module ------------------------------------------------------------
    integer, parameter:: kmax = 1000
    real(8), parameter:: ep = 2457600.5d0
    integer, parameter:: aei_numrec = 10001

! Parameters for id_matrices, resonance_finder etc. -------------------------------------------
    integer, parameter:: gp_max_order = 20
    real(8), parameter:: delta = 0d0


    real(8), dimension(0:9)::a_pl = (/0d0, &
                                      0.38709843d0, 0.72332102d0, 1.00000018d0, &
                                      1.52371243d0, 5.20248019d0, 9.54149883d0, &
                                      19.18797948d0, 30.06952752d0, 39.48686035d0/)
! Taken from http://ssd.jpl.nasa.gov/?planet_pos
! Link case for "Keplerian elements for 3000 BC to 3000 AD"

    real(8), dimension(0:9)::m_pl = (/0d0, &
                                      1.6601367952719304D-07, 2.4478383396645447D-06, 3.0404326462685257D-06, &
                                      3.2271514450538743D-07, 9.547919384243222D-04, 2.858859806661029D-04, &
                                      4.3662440433515637D-05, 5.151389020535497D-05, 7.407407407407407D-09/)
! Used: http://ssd.jpl.nasa.gov/?planet_phys_par

    real(8), dimension(0:9)::res_a_std_delta = (/0d0, &
                                                 1d-2, 2d-2, 3d-2, &
                                                 5d-2, 1d-1, 2d-1, &
                                                 5d-1, 5d-1, 2d0/)
! Used variations in planet semimajor axis over 100000 years of integration.
! I increased them twice because some resonances had not been found before that.

!----------------------------------------------------------------------------------------------
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

!----------------------------------------------------------------------------------------------
type argleaf
    ! Includes asteroid name and reference to the next object
    character(25)::name
    type(argleaf),pointer::next
end type argleaf

type arglist
    ! Includes references to the first and "current" argleaf objects and number of objects
    integer:: listlen
    type(argleaf),pointer::first
    type(argleaf),pointer::current
end type arglist

!----------------------------------------------------------------------------------------------
type orb_elem
    ! Includes asteroid name and its orbital elements
    character(25)::name
    real(8),dimension(6)::elem
end type orb_elem

type orb_elem_leaf
    ! Includes orb_elem item and reference to the next object
    type(orb_elem):: item
    type(orb_elem_leaf),pointer:: next
end type orb_elem_leaf

type orb_elem_list
    ! Includes references to the first and current orb_elem_leaf objects and number of objects
    integer:: listlen
    type(orb_elem_leaf),pointer:: first
    type(orb_elem_leaf),pointer:: current
end type orb_elem_list
!----------------------------------------------------------------------------------------------

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
            write (*, *) 'Error! Planet name must be one from the list:'
            write (*, *) 'MERCURY, VENUS, EARTHMOO, MARS, JUPITER, SATURN, URANUS, NEPTUNE, PLUTO'
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
            write (*, *) "Error! The planet ID must be between 1 and 9."
        end select
    end function planet_name

!----------------------------------------------------------------------------------------------
    integer function gcd(a, b)
! Find gcd - greatest common divisor
! Given:
!   a,b - positive integer numbers
! Returns:
!   <integer> - gcd
        integer:: a, b
        integer:: a1, b1, r

        a1 = a; b1 = b
        do while (b1 /= 0)
            r = mod(a1, b1)
            a1 = b1
            b1 = r
        enddo
        gcd = a1
    end function gcd

!----------------------------------------------------------------------------------------------
    real(8) function norm_ang(an)
! A function for adjusting the angle to (-PI;+PI)
! Given:
!   an - angle in radians
! Returns:
! <real(8)> - normalized angle
        real(8):: an
        real(8):: t,ds

        t=an;ds=dsign(1d0,an)
        do while(dabs(t)>pi)
            t=t-ds*pi*2d0
        enddo
        norm_ang=t
    end function norm_ang    

!----------------------------------------------------------------------------------------------
end module global_parameters
