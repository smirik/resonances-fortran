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
        real(8)::a, eps
        real(8), optional::delta
        integer pl_id, as

        as = 100
        open (unit=as, file=trim(pwd)//'wd/'//asteroid//'.rpin', status='replace')
        do pl_id = 1, 9
            if (present(delta)) then
                eps = delta
            else
                eps = res_a_std_delta(pl_id)
            endif
            call get_possible_resonances_2body(as, a, pl_id, eps)
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
!   file records in the following format:
!      PLANET_NAME RESONANCE_NUMBERS RESONANT_AXIS
        real(8):: a, res_a, eps
        real(8), optional:: delta
        integer:: i, s, m1, m, p1, p, pl_id
        integer:: un, as
        character(8) pl_name

        pl_name = planet_name(pl_id)
        if (present(delta)) then
            eps = delta
        else
            eps = res_a_std_delta(pl_id)
        endif
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
                    write (as, '(a8,3x,4i4,f23.16)') pl_name, m1, m, p1, p, res_a!, abs(res_a - a), eps
                endif
            enddo
        case (-1)
            write (*, *) 'Cannot find idmatrix in memory - trying to find corresponding file...'
            call add_idmatrix_2body(pl_id)
            do i = 1, size(idmatrix_2body(pl_id)%matrix)
                res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                m1 = idmatrix_2body(pl_id)%matrix(i)%resonance(1)
                m = idmatrix_2body(pl_id)%matrix(i)%resonance(2)
                p1 = idmatrix_2body(pl_id)%matrix(i)%resonance(3)
                p = idmatrix_2body(pl_id)%matrix(i)%resonance(4)
                if (abs(res_a - a) <= eps) then
                    write (as, *) pl_name, m1, m, p1, p, res_a!, abs(res_a - a), eps
                endif
            enddo
        case default
            write (*, *) 'Idmatrix for ', pl_name, 'was not found!'
        end select
    end subroutine get_possible_resonances_2body

!----------------------------------------------------------------------------------------------
end module resonance_finder
