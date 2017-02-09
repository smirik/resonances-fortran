program resonant_axis_calculator

    use global_parameters
    use resonant_axis
    use id_matrix
    use resonance_finder
    implicit none

    character(25)::co, asteroid
    character(8):: pl_name
    integer:: pl_id
    real(8):: a, delta

!---------------------------------------------
    write (*, *) 'Initiating idmatrix_2body...'
    call init_idmatrix_2body()
    write (*, *) 'Successfully done!'
!---------------------------------------------

! asteroid name
    call get_command_argument(1, asteroid)
! it's orbit semimajor axis
    call get_command_argument(2, co)
    read (co, *) a
! semi-interval delta (may be 0)
    call get_command_argument(3, co)
    read (co, *) delta
!--------------------------------------------------------

! Here is the place for playing
    if (delta > 0d0) then
        call get_all_possible_resonances_2body(trim(asteroid), a, delta)
    else
        write (*, *) 'Delta was not specified and will be chosen by internal methods'
        call get_all_possible_resonances_2body(trim(asteroid), a)
    endif
!--------------------------------------
    write (*, *) 'Clearing memory...'
    call clear_idmatrix_2body()
    write (*, *) 'Done. Ending program.'
!--------------------------------------

end
