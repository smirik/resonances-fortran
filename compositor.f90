program compositor

    use global_parameters, n => bin_numrec
    use resonant_axis
    use id_matrix
    use resonance_finder
    use integrator
    use librations
    implicit none

    character(25)::co, asteroid
    character(8):: pl_name
    integer:: pl_id,s
    real(8):: a

    integer:: lenarg
    type(orb_elem)::scr
    type(orb_elem),pointer::buffer
    type(orb_elem_list)::elementlist
    type(arglist)::astlist
    character(25):: sample,s1,s2
    integer:: i,i1,i2

if (just_id_matrices) then
    write (*, *) 'Initiating idmatrix_2body...'
    call init_idmatrix(2)
    write (*, *) 'Successfully done!'
    write (*, *) 'Initiating idmatrix_3body...'
    call init_idmatrix(3)
    write (*, *) 'Successfully done!'
    call clear_idmatrix(2)
    call clear_idmatrix(3)
    stop
endif
!----------------------------------------------------------------------------------------------
! At first we interpretate argument list

lenarg = command_argument_count()
if (lenarg < 1) stop('Error! No arguments found. Please follow the format described in documentation')
! If only one asteroid in input
if (lenarg == 1) then
    allocate(astlist%first)
    astlist%first%next => null()
    astlist%current=>astlist%first
    call get_command_argument(1,astlist%current%name)
    astlist%listlen=1
endif
! If "-list" or "-range" or neither keys are specified
if (lenarg > 1) then
    call get_command_argument(1,sample)
    select case (trim(sample))
    case('-list')
            allocate(astlist%first)
            astlist%first%next => null()
            astlist%current=>astlist%first
            call get_command_argument(2,astlist%first%name)
        do i=3,lenarg
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
            astlist%current%next => null()
            call get_command_argument(i,astlist%current%name)
        enddo
        astlist%listlen=lenarg-1
    case('-range')
        call get_command_argument(2,s1)
        call get_command_argument(3,s2)
        read(s1,*,iostat=s) i1
        read(s2,*,iostat=s) i2
        if(s/=0) stop('Error! Wrong range arguments. Please follow the format described in documentation')
        allocate(astlist%first)
        astlist%first%next => null()
        astlist%current=>astlist%first
        astlist%first%name=s1
        do i=i1+1,i2
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
            astlist%current%next => null()
            write(sample,'(i25)') i
            astlist%current%name=trim(adjustl(sample))
        enddo
        astlist%listlen=max(0,i2-i1+1)
    case default
            allocate(astlist%first)
            astlist%first%next => null()
            astlist%current=>astlist%first
            call get_command_argument(1,astlist%first%name)
        do i=2,lenarg
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
            astlist%current%next => null()
            call get_command_argument(i,astlist%current%name)
        enddo
        astlist%listlen=lenarg
    end select
endif

!----------------------------------------------------------------------------------------------
! Now try to find orbital information about asteroids in a list

open(unit=9,file='asteroids.bin',access='direct',recl=sizeof(scr),action='read')
write(*,*) 'astlist',astlist%listlen
astlist%current=>astlist%first
elementlist%listlen=0
do i=1,astlist%listlen
    if (find_asteroid(astlist%current%name,9,n,scr)==0) then
        elementlist%listlen=elementlist%listlen+1
        if (elementlist%listlen==1) then
            allocate(elementlist%current)
            elementlist%first=>elementlist%current
        else
            allocate(elementlist%current%next)
            elementlist%current=>elementlist%current%next
        endif
        elementlist%current%item=scr
        elementlist%current%next=>null()
        astlist%current=>astlist%current%next
    endif
enddo
write(*,*) 'Total number of found asteroids: ',elementlist%listlen
close(9)

!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------

! Now performing an integration of asteroids in list
call mercury_processing(elementlist)

!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------

! Now find all possible resonances for asteroids in list
!----------------------------------------------------------------------------------------------
if (.not. use_only_integration) then

! Previously we initialize id-matrices (2body case for now)
    if(mode_2body) then
        write (*, *) 'Initiating idmatrix_2body...'
        call init_idmatrix(2)
        write (*, *) 'Successfully done!'
    endif
    if(mode_3body) then
        write (*, *) 'Initiating idmatrix_3body...'
        call init_idmatrix(3)
        write (*, *) 'Successfully done!'
    endif
! And planet data (longitudes for building resonant phase
    write (*, *) 'Initiating .aei planet data...'
    call init_planet_data()
    write (*, *) 'Successfully done!'
!----------------------------------------------------------------------------------------------

! Finally, finding resonances
    if (delta <= 0d0)&
        write (*,*) 'Delta was not specified and will be chosen by internal methods'
    if(.not. just_id_matrices) then
    elementlist%current=>elementlist%first
    do i=1,elementlist%listlen
        if(mode_2body) call get_all_possible_resonances(2, &
            trim(elementlist%current%item%name),&
            elementlist%current%item%elem(1), delta)
        if(mode_3body) call get_all_possible_resonances(3, &
            trim(elementlist%current%item%name),&
            elementlist%current%item%elem(1), delta)
        elementlist%current=>elementlist%current%next
    enddo
!----------------------------------------------------------------------------------------------
! Now performing global classification of resonances for asteroids in list
    if(mode_2body) call global_libration_processing(2, elementlist)
    if(mode_3body) call global_libration_processing(3, elementlist)
    endif
!----------------------------------------------------------------------------------------------
! Now deallocate lists and others

    write (*, *) 'Clearing memory...'
    if(mode_2body) call clear_idmatrix(2)
    if(mode_3body) call clear_idmatrix(3)
    call clear_planet_data()

endif

write (*, *) 'Clearing element lists...'

elementlist%current=>elementlist%first
do i=1,elementlist%listlen
    elementlist%first=>elementlist%current%next
    deallocate(elementlist%current)
    elementlist%current=>elementlist%first
enddo

write (*, *) 'Clearing argument lists...'

astlist%current=>astlist%first
do i=1,astlist%listlen
    astlist%first=>astlist%current%next
    deallocate(astlist%current)
    astlist%current=>astlist%first
enddo

if(associated(astlist%current)) then
    write(*, *) 'Some unallocated argument items still in the memory. Please check the source code.'
    write(*,*) astlist%current%name
endif
if(associated(elementlist%current)) then
    write(*, *) 'Some unallocated element items still in the memory. Please check the source code.'
    write(*,*) elementlist%current%item
endif
    write (*, *) 'Done. Ending program.'
!--------------------------------------

end
