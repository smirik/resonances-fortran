module idmatrix_2body_mod

    use global_parameters
    use resonant_axis
    implicit none

    character(16), parameter:: idmatrix_2body_format = '(4i4,f23.16)'

contains

!----------------------------------------------------------------------------------------------

character(32) function idmatrix_name_2body(pl_name) result(s)
! Returns file name that corresponds to a given idmatrix (2-body case)

    character(8):: pl_name
    s = "id_matrix_" // trim(pl_name) // ".dat"

end function idmatrix_name_2body

!----------------------------------------------------------------------------------------------

subroutine validate_resonance_2body(un, m1, m, pl_id)
! Support subroutine
! Check whether the resonance has been already used or not
! If not, then add a record to a corresponding file

    integer, dimension(1:4):: resonance
    integer:: un, m1, m, pl_id

    ! Waste already observed cases
    if (gcd(abs(m), m1) /= 1) then
        return
    endif
    ! Look for main subresonance
    resonance = (/ m1, m, 0, -m1 - m /)
    write (un, idmatrix_2body_format) resonance, count_axis_2body(resonance, a_pl(pl_id), m_pl(pl_id))

end subroutine validate_resonance_2body

!----------------------------------------------------------------------------------------------

subroutine build_idmatrix_2body(pl_name)
! Creates id. matrix (2-body case)
! Given:
!   pl_name - planet name IN CAPITALS
! Produces:
!   file with idmatrix_2body data

    integer:: pl_id, un
    integer:: m1, m
    character(8):: pl_name

    pl_id = planet_id(pl_name)
    un = 8 + pl_id
    open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
        trim(idmatrix_name_2body(pl_name)), status = 'replace')
    ! Run over possible resonance configurations
    do m1 = 1, gp_max_value_2body
        do m = -1, max(-gp_max_value_2body, -m1 - gp_max_order_2body), -1
            call validate_resonance_2body(un, m1, m, pl_id)
        enddo
    enddo
    close(un)

end subroutine build_idmatrix_2body

!----------------------------------------------------------------------------------------------

integer function get_idmatrix_2body_status(pl_id) result(s)
! Get information about idmatrix existance (2-body case)
! Given:
!   pl_id - Planet ID
! Returns:
!   0 - idmatrix exists and is in RAM
!  -1 - idmatrix exists only as a file
!  >0 - idmatrix does not exist

    character(8):: pl_name
    integer:: pl_id, un

    if (allocated(idmatrix_2body(pl_id)%matrix)) then
        s = 0
        return
    else
        un = 8 + pl_id
        pl_name = planet_name(pl_id)
        open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
            trim(idmatrix_name_2body(pl_name)), action = 'read', iostat = s)
        if (s == 0) then
            close (un)
            s = -1
            return
        endif
    endif

end function get_idmatrix_2body_status

!----------------------------------------------------------------------------------------------

subroutine add_idmatrix_2body(pl_id)
! Loads one id. matrix in RAM from a file (2-body case)
! Given:
!   pl_id - planet ID
! Produces:
!   idmatrix_2body<...>%matrix

    integer:: pl_id, s, l, un, i

    un = 8 + pl_id
    l = 0
    open (unit = un, file = trim(pwd) // trim(idmatrix_pwd) // &
        trim(idmatrix_name_2body(planet_name(pl_id))), action = 'read', iostat = s)
    if (s /= 0) then
        write (*, *) 'Cannot add idmatrix for ', planet_name(pl_id), &
            ' from file - this file does not exist.'
        return
    endif
    do
        read (un, '(a)', iostat = s)
        if (s /= 0) exit
        l = l + 1
    enddo
    rewind (un)
    allocate (idmatrix_2body(pl_id)%matrix(1:l))
    do i = 1, l
        read (un, *) idmatrix_2body(pl_id)%matrix(i)
    enddo
    close (un)

end subroutine add_idmatrix_2body

!----------------------------------------------------------------------------------------------

subroutine init_idmatrix_2body()
! Loads id. matrices in RAM from files (2-body case)
! Produces:
!   idmatrix_2body

    integer pl_id, s

    do pl_id = 1, 9
        s = get_idmatrix_2body_status(pl_id)
        if (s > 0) &
            call build_idmatrix_2body(planet_name(pl_id))
        if (s /= 0) &
            call add_idmatrix_2body(pl_id)
    enddo

end subroutine init_idmatrix_2body

!----------------------------------------------------------------------------------------------

subroutine clear_idmatrix_2body()
! Frees the RAM from idmatrix (2-body case)

    integer:: pl_id

    do pl_id = 1, 9
        if (allocated(idmatrix_2body(pl_id)%matrix)) &
            deallocate (idmatrix_2body(pl_id)%matrix)
    enddo

end subroutine clear_idmatrix_2body

!----------------------------------------------------------------------------------------------

end module idmatrix_2body_mod
