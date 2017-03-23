module id_matrix

    use global_parameters
    use resonant_axis
    implicit none

contains

!----------------------------------------------------------------------------------------------
    subroutine build_idmatrix(pl_name, pl2_name)
    ! Creates id. matrix
    ! Given:
    !   pl_name - planet name IN CAPITALS
    !   pl2_name - (OPTIONAL) second planet name IN CAPITALS
    ! Produces:
    !   file with idmatrix_2body/idmatrix_2body data
        integer:: pl_id, pl2_id, un
        integer:: max_order,max_value
        integer:: m1, m2, m
        character(8):: pl_name
        character(8),optional:: pl2_name
        character(14):: co
        integer, dimension(:),allocatable:: resonance
        logical:: case_3body
        real(8):: n1,n2

        case_3body = present(pl2_name)
        pl_id = planet_id(pl_name)
        un = 8 + pl_id
        ! Get id. matrix by a given maximum order
        if(case_3body) then
            pl2_id = planet_id(pl2_name)
            co = '(6i4,f23.16)'
            max_order=gp_max_order_3body
            max_value=gp_max_value_3body
            allocate(resonance(1:6))
            open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"&
            //trim(pl_name)//"_"//trim(pl2_name)//".dat", status='replace')
            n1=n_from_a(a_pl(pl_id))
            n2=n_from_a(a_pl(pl2_id))
            do m1 = 1, max_value
                do m2 = -max_value, floor(-n1/n2*m1)
                    do m=min(max_value,max(1,-max_order-m1-m2)),&
                    min(max_value,max_order-m1-m2)
                        if (gcd(m1, gcd(abs(m2), abs(m))) /= 1 ) cycle
                        resonance = (/m1, m2, m, 0, 0, -m1 - m2 - m/)
                        write (un, co) resonance, &
                            count_axis_3body(resonance,a_pl(pl_id),n1,0d0,n2,0d0)
                    enddo
                enddo
                do m2 = ceiling(-n1/n2*m1), max_value
                    if (m2 == 0) cycle
                    do m=max(-max_value,min(-1,max_order-m1-m2)),&
                    max(-max_value,-m1-m2-max_order),-1
                        if (gcd(m1, gcd(abs(m2), abs(m))) /= 1 ) cycle
                        resonance = (/m1, m2, m, 0, 0, -m1 - m2 - m/)
                        write (un, co) resonance, &
                            count_axis_3body(resonance,a_pl(pl_id),n1,0d0,n2,0d0)
                    enddo
                enddo
            enddo
        else
            co = '(4i4,f23.16)'
            max_order=gp_max_order_2body
            max_value=gp_max_value_2body
            allocate(resonance(1:4))
            open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"&
            //trim(pl_name)//".dat", status='replace')
            do m1 = 1, max_value
                do m = -1, max(-max_value, -m1 - max_order), -1
                    ! Waste already observed cases
                    if (gcd(abs(m), m1) /= 1) cycle
                    ! Look for main subresonance
                    resonance = (/m1, m, 0, -m1 - m/)
                    write (un, co) resonance, &
                        count_axis_2body(resonance, a_pl(pl_id), m_pl(pl_id))
                enddo
            enddo
        endif
        close (un)
        deallocate(resonance)
    end subroutine build_idmatrix

!----------------------------------------------------------------------------------------------
    integer function get_idmatrix_status(pl_id,pl2_id) result(s)
    ! Get information about idmatrix existance
    ! Given:
    !   pl_id - Planet ID
    !   pl2_id - (OPTIONAL) second planet ID
    ! Returns:
    !   0 - idmatrix exists and is in RAM
    !  -1 - idmatrix exists only as a file
    !  >0 - idmatrix does not exist
        character(8):: pl_name, pl2_name
        integer:: pl_id, un
        integer,optional:: pl2_id
        logical:: case_3body

        case_3body = present(pl2_id)
        if(case_3body) then
            if (allocated(idmatrix_3body(idm_index(pl_id,pl2_id))%matrix)) then
                s = 0
                return
            else
                un = 8 + pl_id
                pl_name = planet_name(pl_id)
                pl2_name = planet_name(pl2_id)
                open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"//&
                    trim(pl_name)//"_"//trim(pl2_name)//'.dat',&
                    action='read', iostat=s)
                if (s == 0) then
                    close (un)
                    s = -1
                    return
                endif
            endif
        else
            if (allocated(idmatrix_2body(pl_id)%matrix)) then
                s = 0
                return
            else
                un = 8 + pl_id
                pl_name = planet_name(pl_id)
                open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"//&
                    trim(pl_name)//'.dat',&
                    action='read', iostat=s)
                if (s == 0) then
                    close (un)
                    s = -1
                    return
                endif
            endif
        endif
    end function get_idmatrix_status

!----------------------------------------------------------------------------------------------
    subroutine add_idmatrix(pl_id,pl2_id)
    ! Loads one id. matrix in RAM from a file
    ! Given:
    !   pl_id - planet ID
    !   pl2_id - (OPTIONAL) second planet ID
    ! Produces:
    !   idmatrix_(2/3)body<...>%matrix
        integer:: pl_id, s, l, un, i
        integer,optional:: pl2_id
        type(idmrow_2body):: str
        logical:: case_3body

        case_3body = present(pl2_id)
        un = 8 + pl_id
        l = 0
        if(case_3body) then
            open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"//&
                trim(planet_name(pl_id))//"_"//trim(planet_name(pl2_id))//'.dat',&
                action='read', iostat=s)
        else
            open (unit=un, file=trim(pwd)//"id_matrices/id_matrix_"//&
                trim(planet_name(pl_id))//'.dat', action='read', iostat=s)
        endif
        if (s /= 0) then
            write (*, *) 'Cannot add idmatrix for ', planet_name(pl_id)
            if(case_3body) write(*,*) '...... and ', planet_name(pl2_id)
            write (*, *) 'from file - this file does not exist.'
            return
        endif
        do
            read (un, '(a)', iostat=s)! str
            if (s /= 0) exit
            l = l + 1
        enddo
        rewind (un)
        if(case_3body) then
            s=idm_index(pl_id,pl2_id)
            allocate (idmatrix_3body(s)%matrix(1:l))
            do i = 1, l
                read (un, *) idmatrix_3body(s)%matrix(i)
            enddo
        else
            allocate (idmatrix_2body(pl_id)%matrix(1:l))
            do i = 1, l
                read (un, *) idmatrix_2body(pl_id)%matrix(i)
            enddo
        endif
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
    end subroutine add_idmatrix

!----------------------------------------------------------------------------------------------
    subroutine init_idmatrix(mode)
    ! Loads id. matrices in RAM from files
    ! Given:
    !   mode - must be 2 or 3 (according to 2- or 3-body case)
    ! Produces:
    !   idmatrix_(2/3)body
        integer:: mode
        integer pl_id, pl2_id, s

        if (mode == 3) then
            do pl_id = 1, 9
                do pl2_id = pl_id + 1, 9
                    s = get_idmatrix_status(pl_id, pl2_id)
                    if (s > 0) &
                        call build_idmatrix(planet_name(pl_id), planet_name(pl2_id))
                    if (s /= 0) &
                        call add_idmatrix(pl_id, pl2_id)
                enddo
            enddo
        elseif (mode == 2) then
            do pl_id = 1, 9
                s = get_idmatrix_status(pl_id)
                if (s > 0) &
                    call build_idmatrix(planet_name(pl_id))
                if (s /= 0) &
                    call add_idmatrix(pl_id)
            enddo
        else
            write(*,*) 'Error! Wrong mode value (must be 2 or 3).'
        endif
    end subroutine init_idmatrix

!----------------------------------------------------------------------------------------------
    subroutine clear_idmatrix(mode)
    ! Frees the RAM from idmatrix
    ! Given:
    !   mode - must be 2 or 3 (according to 2- or 3-body case)
        integer:: pl_id, pl2_id, mode, s

        if (mode == 3) then
            do pl_id = 1, 9
                do pl2_id = pl_id + 1, 9
                    s = idm_index(pl_id, pl2_id)
                    if (allocated(idmatrix_3body(s)%matrix)) &
                        deallocate (idmatrix_3body(s)%matrix)
                enddo
            enddo
        elseif (mode == 2) then
            do pl_id = 1, 9
                if (allocated(idmatrix_2body(pl_id)%matrix)) &
                    deallocate (idmatrix_2body(pl_id)%matrix)
            enddo
        else
            write(*,*) 'Error! Wrong mode value (must be 2 or 3).'
        endif
    end subroutine clear_idmatrix

!----------------------------------------------------------------------------------------------
    subroutine show_idmatrix(pl_id,pl2_id)
    ! Shows idmatrix for a given planet on the screen if it exists in RAM
    ! (currently not used)
    ! Given:
    !   pl_id - planet ID
    !   pl2_id - (OPTIONAL) second planet ID
        integer pl_id, i,s
        integer,optional:: pl2_id

        if(present(pl2_id)) then
            if (get_idmatrix_status(pl_id, pl2_id) == 0) then
                write (*, *) "Showing idmatrix for ", planet_name(pl_id),&
                ' & ',planet_name(pl2_id),":"
                s = idm_index(pl_id, pl2_id)
                do i = 1, size(idmatrix_3body(s)%matrix)
                    write (*, *) planet_name(pl_id),' ',planet_name(pl2_id), &
                        idmatrix_3body(s)%matrix(i)%resonance, &
                        idmatrix_3body(s)%matrix(i)%res_a
                enddo
            else
                write (*, *) "Can't show idmatrix for ", planet_name(pl_id),' & ', &
                    planet_name(pl2_id)," - not found in RAM. Try to found it in file."
            endif
        else 
            if (get_idmatrix_status(pl_id) == 0) then
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
        endif
    end subroutine show_idmatrix

!----------------------------------------------------------------------------------------------
end module id_matrix
