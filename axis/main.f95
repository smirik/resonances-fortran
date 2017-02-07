program resonant_axis_calculator

use global_parameters
use resonant_axis
use id_matrix
use resonance_finder
implicit none

    character(25)::co,asteroid
    character(8):: pl_name
    integer:: pl_id
    real(8):: a,delta

!---------------------------------------------
    write(*,*) 'Initiating idmatrix_2body...'
    call init_idmatrix_2body()
    write(*,*) 'Successfully done!'
!---------------------------------------------


! asteroid name, it's semimajor axis and delta (may be 0)
    call get_command_argument(1,asteroid)

    call get_command_argument(2,co)
    read(co,*) a

    call get_command_argument(3,co)
    read(co,*) delta
!--------------------------------------------------------


! This part will be inmodulated in the future
    do pl_id=1,9
        select case(get_idmatrix_2body_status(pl_id))
            case(0)
                cycle
            case(-1)
                call add_idmatrix_2body(pl_id)
            case default
                pl_name=planet_name(pl_id)
                write(*,*) 'Id. matrix for ',pl_name,&
                    'is not neither in RAM nor in file and will be created now'
                call make_id_matrix_2body(pl_name,gp_max_order)
                call add_idmatrix_2body(pl_id)
        end select
    enddo
!--------------------------------------------

! Here is the place for playing

    call get_all_possible_resonances_2body(trim(asteroid),a,delta)


!--------------------------------------
    write(*,*) 'Clearing memory...'
    call clear_idmatrix_2body()
    write(*,*) 'Done. Ending program.'
!--------------------------------------

end
