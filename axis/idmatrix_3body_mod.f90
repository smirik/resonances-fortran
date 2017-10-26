module idmatrix_3body_mod

    use global_parameters
    use resonant_axis
    implicit none

    character(16), parameter:: idmatrix_3body_format = '(6i4,f23.16)'

contains

!----------------------------------------------------------------------------------------------

character(32) function idmatrix_name_3body(pl_name, pl2_name) result(s)
! Returns file name that corresponds to a given idmatrix (3-body case)

    character(8):: pl_name, pl2_name
    s = "id_matrix_" // trim(pl_name) // "_" // trim(pl2_name) // ".dat"

end function idmatrix_name_3body

!----------------------------------------------------------------------------------------------

subroutine validate_resonance_3body(un, m1, m2, m, n1, n2)
! Support subroutine
! Check whether the resonance has been already used or not
! If not, then add a record to a corresponding file

    integer, dimension(1:6):: resonance
    integer:: un, m1, m2, m
    real(8):: n1, n2

    ! Waste already observed cases
    if (gcd(m1, gcd(abs(m2), abs(m))) /= 1 ) then
        return
    endif
    ! Look for main subresonance
    resonance = (/ m1, m2, m, 0, 0, -m1 - m2 - m /)
    write (un, idmatrix_3body_format) resonance, count_axis_3body(resonance, n1, 0d0, n2, 0d0)

end subroutine validate_resonance_3body

!----------------------------------------------------------------------------------------------

subroutine build_idmatrix_3body(pl_name, pl2_name)
! Creates id. matrix (3-body case)
! Given:
!   pl_name - planet name IN CAPITALS
!   pl2_name - second planet name IN CAPITALS
! Produces:
!   file with idmatrix_3body data

    integer:: pl_id, pl2_id, un
    integer:: m1, m2, m
    character(8):: pl_name, pl2_name
    real(8):: n1, n2

    pl_id = planet_id(pl_name)
    pl2_id = planet_id(pl2_name)
    un = 8 + pl_id
    open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
        trim(idmatrix_name_3body(pl_name, pl2_name)), status = 'replace')
    n1 = n_from_a(a_pl(pl_id))
    n2 = n_from_a(a_pl(pl2_id))
    ! Run over possible resonance configurations
    do m1 = 1, gp_max_value_3body
        do m2 = -gp_max_value_3body, floor(-n1 / n2 * m1)
            do m = min(gp_max_value_3body, max(1, -gp_max_order_3body - m1 - m2)), &
                min(gp_max_value_3body, gp_max_order_3body - m1 - m2)
                call validate_resonance_3body(un, m1, m2, m, n1, n2)
            enddo
        enddo
        do m2 = ceiling(-n1 / n2 * m1), gp_max_value_3body
            if (m2 == 0) cycle
            do m = max(-gp_max_value_3body, min(-1, gp_max_order_3body - m1 - m2)), &
            max(-gp_max_value_3body, -m1 - m2 - gp_max_order_3body), -1
                call validate_resonance_3body(un, m1, m2, m, n1, n2)
            enddo
        enddo
    enddo
    close(un)

end subroutine build_idmatrix_3body

!----------------------------------------------------------------------------------------------

integer function get_idmatrix_3body_status(pl_id, pl2_id) result(s)
! Get information about idmatrix existance (3-body case)
! Given:
!   pl_id - Planet ID
!   pl2_id - second planet ID
! Returns:
!   0 - idmatrix exists and is in RAM
!  -1 - idmatrix exists only as a file
!  >0 - idmatrix does not exist

    character(8):: pl_name, pl2_name
    integer:: pl_id, pl2_id, un

    if (allocated(idmatrix_3body(idm_index(pl_id,pl2_id))%matrix)) then
        s = 0
        return
    else
        un = 8 + pl_id
        pl_name = planet_name(pl_id)
        pl2_name = planet_name(pl2_id)
        open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
            trim(idmatrix_name_3body(pl_name, pl2_name)), action = 'read', iostat = s)
        if (s == 0) then
            close (un)
            s = -1
            return
        endif
    endif

end function get_idmatrix_3body_status

!----------------------------------------------------------------------------------------------

subroutine add_idmatrix_3body(pl_id, pl2_id)
! Loads one id. matrix in RAM from a file (3-body case)
! Given:
!   pl_id - planet ID
!   pl2_id - second planet ID
! Produces:
!   idmatrix_3body<...>%matrix

    integer:: pl_id, pl2_id, s, l, un, i

    un = 8 + pl_id
    l = 0
    open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
        trim(idmatrix_name_3body(planet_name(pl_id), planet_name(pl2_id))), &
        action = 'read', iostat = s)
    if (s /= 0) then
        write (*, *) 'Cannot add idmatrix for ', planet_name(pl_id), &
            ' and ', planet_name(pl2_id), ' from file - this file does not exist.'
        return
    endif
    do
        read (un, '(a)', iostat = s)
        if (s /= 0) exit
        l = l + 1
    enddo
    rewind (un)
    s = idm_index(pl_id, pl2_id)
    allocate (idmatrix_3body(s)%matrix(1:l))
    do i = 1, l
        read (un, *) idmatrix_3body(s)%matrix(i)
    enddo
    close (un)

end subroutine add_idmatrix_3body

!----------------------------------------------------------------------------------------------

subroutine init_idmatrix_3body()
! Loads id. matrices in RAM from files (3-body case)
! Produces:
!   idmatrix_3body

    integer pl_id, pl2_id, s

    do pl_id = 1, 9
        do pl2_id = pl_id + 1, 9
            s = get_idmatrix_3body_status(pl_id, pl2_id)
            if (s > 0) &
                call build_idmatrix_3body(planet_name(pl_id), planet_name(pl2_id))
            if (s /= 0) &
                call add_idmatrix_3body(pl_id, pl2_id)
        enddo
    enddo

end subroutine init_idmatrix_3body

!----------------------------------------------------------------------------------------------

subroutine clear_idmatrix_3body()
! Frees the RAM from idmatrix (3-body case)

    integer:: pl_id, pl2_id, s

    do pl_id = 1, 9
        do pl2_id = pl_id + 1, 9
            s = idm_index(pl_id, pl2_id)
            if (allocated(idmatrix_3body(s)%matrix)) &
                deallocate (idmatrix_3body(s)%matrix)
        enddo
    enddo

end subroutine clear_idmatrix_3body

!----------------------------------------------------------------------------------------------

end module idmatrix_3body_mod
