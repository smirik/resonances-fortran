module resonance_finder

    use global_parameters
    use resonance_finder_2body
    use resonance_finder_3body
    implicit none

contains

!----------------------------------------------------------------------------------------------

subroutine get_all_possible_resonances(mode, asteroid, delta)
! Finds possible resonances with any planets
! for a given semimajor axis within a given interval (-delta;+delta).
! Given:
!   mode - must be 2 or 3 (according to 2- or 3-body case)
!   asteroid - element of the asteroid list (including orbital information etc)
!   delta - semisize of interval
! Produces:
!   Fields of the asteroid element with lists of possible resonances

    integer:: mode
    type(orb_elem_leaf):: asteroid
    real(8):: delta

    if (mode == 3) then
        call get_all_possible_resonances_3body(asteroid, delta)
    elseif (mode == 2) then
        call get_all_possible_resonances_2body(asteroid, delta)
    else
        write(*, *) 'Error! Wrong mode value (must be 2 or 3).'
        return
    endif

end subroutine get_all_possible_resonances

!----------------------------------------------------------------------------------------------

end module resonance_finder
