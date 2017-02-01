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
