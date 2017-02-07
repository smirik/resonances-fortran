module id_matrix

use global_parameters
implicit none


contains

!----------------------------------------------------------------------------------------------
integer function get_idmatrix_2body_status(pl_id)
! Get information about idmatrix_2body existance
! Given:
!   pl_id - Planet ID
! Returns:
!   0 - idmatrix exists and is in RAM
!  -1 - idmatrix exists only as a file
!  >0 - idmatrix does not exist
    character(8):: pl_name
    integer:: s
    integer:: pl_id,un

    if(allocated(idmatrix_2body(pl_id)%matrix)) then
        s=0
    else
        un=8+pl_id
        pl_name=planet_name(pl_id)
        open(unit=un,file='id_matrix_'//trim(pl_name)//'.dat',action='read',iostat=s)
        if(s==0) then
            close(un)
            s=-1
        endif
    endif
    get_idmatrix_2body_status=s
end function get_idmatrix_2body_status

!----------------------------------------------------------------------------------------------
subroutine add_idmatrix_2body(pl_id)
! Loads one id. matrix in RAM from a file (2-body case)
! Given:
!   pl_id - planet ID
! Produces:
!   idmatrix_2body(pl_id)%matrix
    integer:: pl_id,s,l,un
    type(idmrow_2body),dimension(1000):: matrix

    un=8+pl_id
    l=0
    open(unit=un,file='id_matrix_'//trim(planet_name(pl_id))//'.dat',action='read',iostat=s)
    if(s/=0) then
        write(*,*) 'Cannot add idmatrix for',planet_name(pl_id),'from file. Sorry.'
        return
    endif
    do
        l=l+1
        read(un,*,iostat=s) matrix(l)%resonance,matrix(l)%res_a
        if(s/=0) exit
    enddo
    l=l-1
    close(un)
    allocate(idmatrix_2body(pl_id)%matrix(1:l))
    idmatrix_2body(pl_id)%matrix = matrix(1:l)
end subroutine add_idmatrix_2body

!----------------------------------------------------------------------------------------------
subroutine init_idmatrix_2body()
! Loads id. matrices in RAM from files (2-body case)
! Produces:
!   idmatrix_2body
    integer pl_id

    do pl_id=1,9
        if(.not. allocated(idmatrix_2body(pl_id)%matrix)) &
            call add_idmatrix_2body(pl_id)
    enddo
end subroutine init_idmatrix_2body


subroutine clear_idmatrix_2body()
! Frees the RAM from idmatrix_2body
    integer pl_id

    do pl_id=1,9
        if(allocated(idmatrix_2body(pl_id)%matrix)) &
            deallocate(idmatrix_2body(pl_id)%matrix)
    enddo
end subroutine clear_idmatrix_2body

!----------------------------------------------------------------------------------------------
subroutine show_idmatrix_2body(pl_id)
! Shows idmatrix for a given planet on the screen if it exists in RAM
! Given:
!   pl_id - planet ID
    integer pl_id,i
    
    if (get_idmatrix_2body_status(pl_id)==0) then
    
        write(*,*) "Showing idmatrix for ",planet_name(pl_id),":"
        do i=1,size(idmatrix_2body(pl_id)%matrix)
            write(*,*) planet_name(pl_id),&
                idmatrix_2body(pl_id)%matrix(i)%resonance,&
                idmatrix_2body(pl_id)%matrix(i)%res_a
        enddo
    else
        write(*,*) "Can't show idmatrix for ",planet_name(pl_id),&
            " - not found in RAM. Try to found it in file."
    endif
end subroutine show_idmatrix_2body

!----------------------------------------------------------------------------------------------
end module id_matrix