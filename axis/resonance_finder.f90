module resonance_finder

    use global_parameters
    use id_matrix
    implicit none

type rscr
    type(res_list_2body):: r2scr
    type(res_list_3body):: r3scr
end type rscr
contains

!----------------------------------------------------------------------------------------------
    subroutine get_all_possible_resonances(mode, asteroid, delta)
    ! Finds possible resonances with any planets
    ! for a given semimajor axis within a given interval (-delta;+delta).
    ! If no delta specified, the internal values will be used instead.
    ! Given:
    !   mode - must be 2 or 3 (according to 2- or 3-body case)
    !   asteroid - element of the asteroid list (including orbital information etc)
    !   delta - semisize of interval
    ! Produces:
    !   Fields of the asteroid element with lists of possible resonances

        type(orb_elem_leaf):: asteroid
        real(8):: a
        real(8):: delta
        integer:: pl_id, pl2_id, as, mode, a_count, j
        type(rscr):: r
        type(res_leaf_2body), pointer:: r2scr
        type(res_leaf_3body), pointer:: r3scr

        if (mode == 3) then
            r%r3scr%listlen = 0
            r%r3scr%first => null()
        elseif (mode == 2) then
            r%r2scr%listlen = 0
            r%r2scr%first => null()
        else
            write(*,*) 'Error! Wrong mode value (must be 2 or 3).'
            return
        endif

        ! Finding and creating resonance data
        do pl_id = 1, 8
            if(mode == 3) then
                do pl2_id = pl_id + 1, 8
                    call get_possible_resonances(asteroid, r, r2scr, r3scr, delta, pl_id, pl2_id)
                enddo
            else
                call get_possible_resonances(asteroid, r, r2scr, r3scr, delta, pl_id)
            endif
        enddo

        ! Fill resonance data for the asteroid element
        if (mode==3) then
            if (r%r3scr%listlen > 0) then
                allocate(asteroid%r3list(1:r%r3scr%listlen))
                r%r3scr%current=>r%r3scr%first
                do j=1,r%r3scr%listlen
                    asteroid%r3list(j) = r%r3scr%current
                    r%r3scr%current => r%r3scr%current%next
                enddo
                do a_count = 1, size(asteroid%a_list)
                    allocate(asteroid%a_list(a_count)%item3(1:r%r3scr%listlen))
                    do j=1,r%r3scr%listlen
                        asteroid%a_list(a_count)%item3(j) = asteroid%r3list(j)%clu(a_count)
                    enddo
                enddo
            endif
        else
            if (r%r2scr%listlen > 0) then
                allocate(asteroid%r2list(1:r%r2scr%listlen))
                r%r2scr%current=>r%r2scr%first
                do j=1,r%r2scr%listlen
                    asteroid%r2list(j) = r%r2scr%current
                    r%r2scr%current => r%r2scr%current%next
                enddo
                do a_count = 1, size(asteroid%a_list)
                    allocate(asteroid%a_list(a_count)%item2(1:r%r2scr%listlen))
                    do j=1,r%r2scr%listlen
                        asteroid%a_list(a_count)%item2(j) = asteroid%r2list(j)%clu(a_count)
                    enddo
                enddo
            endif
        endif

    end subroutine get_all_possible_resonances

!----------------------------------------------------------------------------------------------
    subroutine get_possible_resonances(as, r, r2scr, r3scr, eps, pl_id, pl2_id)
    ! Finds possible resonances with a given planet or two planets
    ! for a given semimajor axis within a given interval (-eps;+eps)
    ! Given:
    !   as - asteroid element
    !   r - scratch resonance collector
    !   r2scr - scratch single 2-body resonance element
    !   r3scr - scratch single 3-body resonance element
    !   pl_id - planet ID
    !   pl2_id - (OPTIONAL) second planet ID
    !   eps - semisize of interval
    ! Produces:
    !   file records in the following format:
    !      PLANET_NAME <PLANET2_NAME> RESONANCE_NUMBERS RESONANT_AXIS

        real(8):: a, res_a
        real(8):: eps
        integer:: i, s, p, pl_id, j
        integer, optional:: pl2_id
        type(orb_elem_leaf):: as
        character(8) pl_name,pl2_name
        logical:: case_3body
        type(rscr):: r
        type(res_leaf_2body),pointer:: r2scr
        type(res_leaf_3body),pointer:: r3scr

        case_3body = present(pl2_id)
        pl_name = planet_name(pl_id)
        if (case_3body) pl2_name = planet_name(pl2_id)

        if(case_3body) then
            s = get_idmatrix_status(pl_id, pl2_id)
        else
            s = get_idmatrix_status(pl_id)
        endif
        select case (s)
        case (0)
            ! Finding and appending resonances to scratch objects
            if(case_3body) then
                allocate(r3scr)
                r3scr%pl_name = pl_name
                r3scr%pl2_name = pl2_name
                allocate(r3scr%clu(1:size(as%a_list)))

                p = idm_index(pl_id, pl2_id)
                do i = 1, size(idmatrix_3body(p)%matrix)
                    res_a = idmatrix_3body(p)%matrix(i)%res_a
                    r3scr%clu = 0
                    do j = 1, size(as%a_list)
                        a=as%a_list(j)%a
                        if (abs(res_a - a) <= eps*res_a) then
                            r3scr%clu(j) = 1
                        endif
                    enddo
                    ! If a potential resonance is valid on at least one a-cluster, we add it
                    if (any(r3scr%clu == 1)) then
                        r3scr%res_a = idmatrix_3body(p)%matrix(i)%res_a
                        r3scr%res_num = idmatrix_3body(p)%matrix(i)%resonance
                        r3scr%next => r%r3scr%first
                        r%r3scr%first => r3scr
                        r%r3scr%listlen = r%r3scr%listlen + 1

                        r3scr => null()
                        allocate(r3scr)
                        r3scr%pl_name = pl_name
                        r3scr%pl2_name = pl2_name
                        allocate(r3scr%clu(1:size(as%a_list)))
                    endif
                enddo
                deallocate(r3scr%clu)
                deallocate(r3scr)

            else
                allocate(r2scr)
                r2scr%pl_name = pl_name
                allocate(r2scr%clu(1:size(as%a_list)))

                do i = 1, size(idmatrix_2body(pl_id)%matrix)
                    res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                    r2scr%clu = 0
                    do j = 1, size(as%a_list)
                        a=as%a_list(j)%a
                        if (abs(res_a - a) <= eps*res_a) then
                            r2scr%clu(j) = 1
                        endif
                    enddo
                    ! If a potential resonance is valid on at least one a-cluster, we add it
                    if (any(r2scr%clu == 1)) then
                        r2scr%res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                        r2scr%res_num = idmatrix_2body(pl_id)%matrix(i)%resonance
                        r2scr%next => r%r2scr%first
                        r%r2scr%first => r2scr
                        r%r2scr%listlen = r%r2scr%listlen + 1

                        r2scr => null()
                        allocate(r2scr)
                        r2scr%pl_name = pl_name
                        allocate(r2scr%clu(1:size(as%a_list)))
                    endif
                enddo
                deallocate(r2scr%clu)
                deallocate(r2scr)
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
