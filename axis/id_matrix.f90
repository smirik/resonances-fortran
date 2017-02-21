module id_matrix

    use global_parameters
    use resonant_axis
    implicit none

contains

!----------------------------------------------------------------------------------------------
    subroutine build_idmatrix_2body(pl_name, max_order)
! Creates id. matrix (twobody case)
! Given:
!   pl_name - planet name IN CAPITALS
!   max_order - maximum resonance order
! Produces:
!   file with idmatrix_2body
        integer:: pl_id, un
        integer, optional:: max_order
        integer:: m1, m, m_o
        character(8):: pl_name
        character(14):: co = '(4i4,f23.16)'
        integer, dimension(4):: resonance

        pl_id = planet_id(pl_name)
        un = 8 + pl_id
        open (unit=un, file=trim(pwd)//"axis/id_matrices_2body/id_matrix_"//trim(pl_name)//".dat", status='replace')
        if (present(max_order)) then
            m_o = max_order
        else
            m_o = gp_max_order
        endif
        ! Get id. matrix for a planet by a given maximum order
        do m1 = 1, m_o
            do m = -1, -m1 - m_o, -1
                ! Waste already observed cases
                if (gcd(abs(m), m1) /= 1) cycle
                ! Look for main subresonance
                resonance = (/m1, m, 0, -m1 - m/)
                write (un, co) resonance, &
                    count_axis_2body(resonance, a_pl(pl_id), m_pl(pl_id))
            enddo
        enddo
        close (un)
    end subroutine build_idmatrix_2body

!----------------------------------------------------------------------------------------------
    integer function get_idmatrix_2body_status(pl_id) result(s)
! Get information about idmatrix_2body existance
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
            open (unit=un, file=trim(pwd)//"axis/id_matrices_2body/id_matrix_"//trim(pl_name)//'.dat',&
                action='read', iostat=s)
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
!   idmatrix_2body(pl_id)%matrix
        integer:: pl_id, s, l, un, i
        type(idmrow_2body):: str

        un = 8 + pl_id
        l = 0
        open (unit=un, file=trim(pwd)//"axis/id_matrices_2body/id_matrix_"//trim(planet_name(pl_id))//'.dat',&
            action='read', iostat=s)
        if (s /= 0) then
            write (*, *) 'Cannot add idmatrix for', planet_name(pl_id),&
                'from file - this file does not exist.'
            return
        endif
        do
            read (un, *, iostat=s) str
            if (s /= 0) exit
            l = l + 1
        enddo
        rewind (un)

        allocate (idmatrix_2body(pl_id)%matrix(1:l))
        do i = 1, l
            read (un, *) idmatrix_2body(pl_id)%matrix(i)
        enddo
        close (un)
! TO DO - here was a weakness of the program with uncertainity of matrix length - need a cure
! This code was commented and another solution used instead
!         type(idmrow_2body), dimension(1000):: matrix
!         un = 8 + pl_id
!         l = 0
!         open (unit=un, file='id_matrix_'//trim(planet_name(pl_id))//'.dat', action='read', iostat=s)
!         if (s /= 0) then
!             write (*, *) 'Cannot add idmatrix for', planet_name(pl_id), 'from file. Sorry.'
!             return
!         endif
!         do
!             l = l + 1
!             read (un, *, iostat=s) matrix(l)%resonance, matrix(l)%res_a
!             if (s /= 0) exit
!         enddo
!         l = l - 1
!         close (un)
!         allocate (idmatrix_2body(pl_id)%matrix(1:l))
!         idmatrix_2body(pl_id)%matrix = matrix(1:l)
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

    subroutine clear_idmatrix_2body()
! Frees the RAM from idmatrix_2body
        integer pl_id

        do pl_id = 1, 9
            if (allocated(idmatrix_2body(pl_id)%matrix)) &
                deallocate (idmatrix_2body(pl_id)%matrix)
        enddo
    end subroutine clear_idmatrix_2body

!----------------------------------------------------------------------------------------------
    subroutine show_idmatrix_2body(pl_id)
! Shows idmatrix for a given planet on the screen if it exists in RAM
! Given:
!   pl_id - planet ID
        integer pl_id, i

        if (get_idmatrix_2body_status(pl_id) == 0) then

            write (*, *) "Showing idmatrix for ", planet_name(pl_id), ":"
            do i = 1, size(idmatrix_2body(pl_id)%matrix)
                write (*, *) planet_name(pl_id), &
                    idmatrix_2body(pl_id)%matrix(i)%resonance, &
                    idmatrix_2body(pl_id)%matrix(i)%res_a
            enddo
        else
            write (*, *) "Can't show idmatrix for ", planet_name(pl_id), &
                " - not found in RAM. Try to found it in file."
        endif
    end subroutine show_idmatrix_2body

!----------------------------------------------------------------------------------------------
end module id_matrix
