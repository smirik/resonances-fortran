module resonance_finder_2body

    use global_parameters
    use idmatrix
    implicit none

contains

!----------------------------------------------------------------------------------------------

subroutine get_all_possible_resonances_2body(asteroid, delta)
! Find possible resonances with each possible configuration of two planets
! for a given semimajor axis within a given interval (-delta; +delta).
! Given:
!   asteroid - element of the asteroid list (including orbital information etc)
!   delta - semisize of interval
! Produces:
!   Fields of the asteroid element with lists of possible resonances

    type(orb_elem_leaf):: asteroid
    real(8):: delta
    integer:: pl_id, a_count, j
    type(res_list_2body):: resonance_list
    !type(res_leaf_3body), pointer:: resonance_record

    ! Finding resonances for each planet configuration and collect them in resonance_list
    resonance_list%listlen = 0
    resonance_list%first => null()
    do pl_id = 1, 8
        call get_possible_resonances_2body(asteroid, resonance_list, delta, pl_id)
    enddo

    ! If resonance_list is not empty, we make a copy to the asteroid resonance list
    if (resonance_list%listlen > 0) then
        ! The asteroid resonance list is an array, not stack
        allocate(asteroid%r2list(1:resonance_list%listlen))
        ! Copying item by item
        resonance_list%current => resonance_list%first
        do j = 1, resonance_list%listlen
            asteroid%r2list(j) = resonance_list%current
            resonance_list%current => resonance_list%current%next
        enddo
        ! Initializing resonance links for a-clusters
        do a_count = 1, size(asteroid%a_list)
            allocate(asteroid%a_list(a_count)%item2(1:resonance_list%listlen))
            do j = 1, resonance_list%listlen
                ! 1 (for potential resonance) or 0
                asteroid%a_list(a_count)%item2(j) = asteroid%r2list(j)%clu(a_count)
            enddo
        enddo
    endif

end subroutine get_all_possible_resonances_2body

!----------------------------------------------------------------------------------------------

subroutine get_possible_resonances_2body(asteroid, resonance_list, eps, pl_id)
! Find possible resonances with one given planet
! for a given semimajor axis within a given interval (-eps;+eps)
! Given:
!   asteroid - asteroid element
!   resonance_list - scratch resonance collector
!   pl_id - planet ID
!   eps - semisize of interval
! Produces:
!   resonance stack of records in the following format:
!      PLANET_NAME, RESONANCE_NUMBERS, RESONANT_AXIS

    real(8):: res_a, eps
    integer:: i, j, pl_id
    type(orb_elem_leaf):: asteroid
    character(8) pl_name
    type(res_list_2body):: resonance_list
    type(res_leaf_2body), pointer:: resonance_record

    pl_name = planet_name(pl_id)
    if (get_idmatrix_status(pl_id) == 0) then
    ! Finding and appending resonances to scratch objects
        allocate(resonance_record)
        resonance_record%pl_name = pl_name
        allocate(resonance_record%clu(1:size(asteroid%a_list)))
        ! Run over id_matrix of given planets
        do i = 1, size(idmatrix_2body(pl_id)%matrix)
            res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
            ! At first no resonances is bounded with any a-clusters
            resonance_record%clu = 0
            ! Run over a-clusters
            do j = 1, size(asteroid%a_list)
                ! In case of match, we bound it with current a-cluster
                if (abs(res_a - asteroid%a_list(j)%a) <= eps * res_a) then
                    resonance_record%clu(j) = 1
                endif
            enddo
            ! If a potential resonance is valid on at least one a-cluster, we add it to a list
            if (any(resonance_record%clu == 1)) then
                resonance_record%res_a = idmatrix_2body(pl_id)%matrix(i)%res_a
                resonance_record%res_num = idmatrix_2body(pl_id)%matrix(i)%resonance
                resonance_record%next => resonance_list%first
                resonance_list%first => resonance_record
                resonance_list%listlen = resonance_list%listlen + 1
                ! Create the field for next possible resonance
                resonance_record => null()
                allocate(resonance_record)
                resonance_record%pl_name = pl_name
                allocate(resonance_record%clu(1:size(asteroid%a_list)))
            endif
        enddo
        deallocate(resonance_record%clu)
        deallocate(resonance_record)
    else
        write (*, *) 'Idmatrix for ', pl_name, ' was not found!'
        write (*, *) 'Resonances in this configuration will not be identified.'
    endif

end subroutine get_possible_resonances_2body

!----------------------------------------------------------------------------------------------

end module resonance_finder_2body
