module resonance_finder

    use global_parameters
    use id_matrix
    implicit none

contains

!----------------------------------------------------------------------------------------------
    subroutine get_all_possible_resonances_2body(asteroid, a, delta)
! Finds possible resonances with any planets
! for a given semimajor axis within a given interval (-delta;+delta)
! Given:
!   asteroid - asteroid name (e.g. 'example' )
!   a - semimajor axis of an asteroid
!   delta - semisize of interval
! Produces:
!   file with list of possible resonances
        character(*)::asteroid
        real(8)::a, delta
        integer pl_id, as

        as = 100
        open (unit=as, file=asteroid//'.rpin', status='replace')
        do pl_id = 1, 9
            call get_possible_resonances_2body(as, a, pl_id, delta)
        enddo
        close (as)
    end subroutine get_all_possible_resonances_2body

!----------------------------------------------------------------------------------------------
    subroutine get_possible_resonances_2body(as, a, pl_id, delta)
! Finds possible resonances with a given planet
! for a given semimajor axis within a given interval (-delta;+delta)
! Given:
!   as - unit descriptor for an asteroid file
!   a - semimajor axis of an asteroid
!   pl_id - planet ID
!   delta - semisize of interval
! Produces:
!   file records in the format:
!      PLANET_NAME RESONANCE_NUMBERS RESONANT_AXIS
        real(8):: a, delta, res_a, eps
        integer:: i, s, m1, m, p1, p, pl_id
        integer:: un, as
        character(8) pl_name

        pl_name = planet_name(pl_id)
        eps = delta
        if (delta == 0d0) eps = res_a_std_delta(pl_id)

        write (*, *) 'Run over idmatrix for ', pl_name, '...'
        select case (get_idmatrix_2body_status (pl_id))
        case (0)
            do i = 1, size(idmatrix_2body(pl_id)%matrix)
                res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                m1 = idmatrix_2body(pl_id)%matrix(i)%resonance(1)
                m = idmatrix_2body(pl_id)%matrix(i)%resonance(2)
                p1 = idmatrix_2body(pl_id)%matrix(i)%resonance(3)
                p = idmatrix_2body(pl_id)%matrix(i)%resonance(4)
                if (abs(res_a - a) <= eps) then
                    write (as, *) pl_name, m1, m, p1, p, res_a, abs(res_a - a)
                endif
            enddo
        case (-1)
            write (*, *) 'Not found idmatrix in memory - try to find corresponding file...'
            un = 8 + pl_id
            open (unit=un, file='id_matrix_'//trim(pl_name)//'.dat', action='read', iostat=s)
            if (s == 0) then
                do
                    read (un, *, iostat=s) m1, m, p1, p, res_a
                    if (s /= 0) exit
                    if (abs(res_a - a) <= eps) then
                        write (as, *) pl_name, m1, m, p1, p, res_a, abs(res_a - a)
                    endif
                enddo
                close (un)
            endif
        case default
            write (*, *) 'No founded id_matrix file!'
        end select
    end subroutine get_possible_resonances_2body

!----------------------------------------------------------------------------------------------
end module resonance_finder
