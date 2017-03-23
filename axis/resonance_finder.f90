module resonance_finder

    use global_parameters
    use id_matrix
    implicit none

contains

!----------------------------------------------------------------------------------------------
    subroutine get_all_possible_resonances(mode, asteroid, a, delta)
    ! Finds possible resonances with any planets
    ! for a given semimajor axis within a given interval (-delta;+delta).
    ! If no delta specified, the internal values will be used instead.
    ! Given:
    !   mode - must be 2 or 3 (according to 2- or 3-body case)
    !   asteroid - asteroid name (e.g. 'example' )
    !   a - semimajor axis of an asteroid
    !   delta - (OPTIONAL) semisize of interval
    ! Produces:
    !   file with the list of possible resonances
        character(*):: asteroid
        real(8):: a, eps
        real(8), optional:: delta
        integer:: pl_id, pl2_id, as, mode

        as = 100
        if (mode == 3) then
            open (unit=as, file=trim(pwd)//'wd/'//asteroid//'.rp3', status='replace')
        elseif (mode == 2) then
            open (unit=as, file=trim(pwd)//'wd/'//asteroid//'.rp2', status='replace')
        else
            write(*,*) 'Error! Wrong mode value (must be 2 or 3).'
            return
        endif
        do pl_id = 1, 9
            if (.not. present(delta) .or. delta == 0d0) then
                eps = res_a_std_delta(pl_id)
            else
                eps = delta
            endif
            if( mode == 3) then
                do pl2_id = pl_id + 1, 9
                    call get_possible_resonances(as, a, eps, pl_id, pl2_id)
                enddo
            else
                call get_possible_resonances(as, a, eps, pl_id)
            endif
        enddo
        close (as)
    end subroutine get_all_possible_resonances

!----------------------------------------------------------------------------------------------
    subroutine get_possible_resonances(as, a, eps, pl_id, pl2_id)
    ! Finds possible resonances with a given planet or two planets
    ! for a given semimajor axis within a given interval (-eps;+eps)
    ! Given:
    !   as - unit descriptor for an asteroid file
    !   a - semimajor axis of an asteroid
    !   pl_id - planet ID
    !   pl2_id - (OPTIONAL) second planet ID
    !   eps - semisize of interval
    ! Produces:
    !   file records in the following format:
    !      PLANET_NAME <PLANET2_NAME> RESONANCE_NUMBERS RESONANT_AXIS
        real(8):: a, res_a
        real(8):: eps
        integer:: i, s, p, pl_id
        integer, optional:: pl2_id
        integer:: as
        character(8) pl_name,pl2_name
        logical:: case_3body

        case_3body = present(pl2_id)
        pl_name = planet_name(pl_id)
        if (case_3body) pl2_name = planet_name(pl2_id)
        !write (*, *) 'Run over idmatrix for ', pl_name
        !if(case_3body) write (*, *) '............ & ',pl2_name
        if(case_3body) then
            s = get_idmatrix_status(pl_id, pl2_id)
        else
            s = get_idmatrix_status(pl_id)
        endif
        select case (s)
        case (0)
            if(case_3body) then
                p = idm_index(pl_id, pl2_id)
                do i = 1, size(idmatrix_3body(p)%matrix)
                    res_a = idmatrix_3body(p)%matrix(i)%res_a
                    if (abs(res_a - a) <= eps) then
                        write (as, '(a8,3x,a8,3x,6i4,f23.16)') pl_name, pl2_name, &
                            idmatrix_3body(p)%matrix(i)%resonance, res_a
                    endif
                enddo
            else
                do i = 1, size(idmatrix_2body(pl_id)%matrix)
                    res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                    if (abs(res_a - a) <= eps) then
                        write (as, '(a8,3x,4i4,f23.16)') pl_name, &
                        idmatrix_2body(pl_id)%matrix(i)%resonance, res_a
                    endif
                enddo
            endif
        case (-1)
            write (*, *) 'Cannot find idmatrix in memory - trying to find corresponding file.'
            if(case_3body) then
                call add_idmatrix(pl_id, pl2_id)
                p = idm_index(pl_id, pl2_id)
                do i = 1, size(idmatrix_3body(p)%matrix)
                    res_a = idmatrix_3body(p)%matrix(i)%res_a
                    if (abs(res_a - a) <= eps) then
                        write (as, '(a8,3x,a8,3x,6i4,f23.16)') pl_name, pl2_name, &
                            idmatrix_3body(p)%matrix(i)%resonance, res_a
                    endif
                enddo
            else
                call add_idmatrix(pl_id)
                do i = 1, size(idmatrix_2body(pl_id)%matrix)
                    res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                    if (abs(res_a - a) <= eps) then
                        write (as, '(a8,3x,4i4,f23.16)') pl_name, &
                            idmatrix_2body(pl_id)%matrix(i)%resonance, res_a
                    endif
                enddo
            endif
        case default
            if(case_3body) then
                write (*, *) 'Idmatrix for ', pl_name,' & ',pl2_name,' was not found!'
            else
                write (*, *) 'Idmatrix for ', pl_name,' was not found!'
            endif
        end select
    end subroutine get_possible_resonances

!----------------------------------------------------------------------------------------------
end module resonance_finder
