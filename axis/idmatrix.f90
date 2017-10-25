module idmatrix

    use idmatrix_2body_mod
    use idmatrix_3body_mod
    implicit none

contains

!----------------------------------------------------------------------------------------------

subroutine build_idmatrix(pl_name, pl2_name)
! Creates id. matrix
! Given:
!   pl_name - planet name IN CAPITALS
!   pl2_name - (OPTIONAL) second planet name IN CAPITALS
! Produces:
!   file with idmatrix_2body/idmatrix_3body data

    character(8):: pl_name
    character(8), optional:: pl2_name

    if (present(pl2_name)) then
        call build_idmatrix_3body(pl_name, pl2_name)
    else
        call build_idmatrix_2body(pl_name)
    endif

end subroutine build_idmatrix

!----------------------------------------------------------------------------------------------

integer function get_idmatrix_status(pl_id, pl2_id) result(s)
! Get information about idmatrix existance
! Given:
!   pl_id - Planet ID
!   pl2_id - (OPTIONAL) second planet ID
! Returns:
!   0 - idmatrix exists and is in RAM
!  -1 - idmatrix exists only as a file
!  >0 - idmatrix does not exist

    integer:: pl_id
    integer, optional:: pl2_id

    if (present(pl2_id)) then
        s = get_idmatrix_3body_status(pl_id, pl2_id)
    else
        s = get_idmatrix_2body_status(pl_id)
    endif

end function get_idmatrix_status

!----------------------------------------------------------------------------------------------

subroutine add_idmatrix(pl_id, pl2_id)
! Loads one id. matrix in RAM from a file
! Given:
!   pl_id - planet ID
!   pl2_id - (OPTIONAL) second planet ID
! Produces:
!   idmatrix_(2/3)body<...>%matrix

    integer:: pl_id
    integer,optional:: pl2_id

    if (present(pl2_id)) then
        call add_idmatrix_3body(pl_id, pl2_id)
    else
        call add_idmatrix_2body(pl_id)
    endif

end subroutine add_idmatrix

!----------------------------------------------------------------------------------------------

subroutine init_idmatrix(mode)
! Loads id. matrices in RAM from files
! Given:
!   mode - must be 2 or 3 (according to 2- or 3-body case)
! Produces:
!   idmatrix_(2/3)body

    integer:: mode

    if (mode == 3) then
        call init_idmatrix_3body()
    elseif (mode == 2) then
        call init_idmatrix_2body()
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
        call clear_idmatrix_3body()
    elseif (mode == 2) then
        call clear_idmatrix_2body()
    else
        write(*,*) 'Error! Wrong mode value (must be 2 or 3).'
    endif

end subroutine clear_idmatrix

!----------------------------------------------------------------------------------------------

end module idmatrix
