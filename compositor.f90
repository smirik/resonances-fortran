program compositor

    use global_parameters, n => bin_numrec
    use astdys_adapter
    use resonant_axis
    use idmatrix
    use mercury_adapter
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
    integer:: i,j,i1,i2

! Deriving a directory where program is running
call getcwd(pwd,s)
if(s>0) &
    stop ('Error! Cannot get path to working directory. Maybe it is too long (>255 symbols).')

! When we need only id_matrices, we do not need anything else
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

! Check the source file
n = check_source(trim(pwd) // trim(input_pwd) // trim(source_name))
if (n <= 0 .or. force_source_building) call source_builder(n, epoch)
! Interpretate argument list

lenarg = command_argument_count()
if (lenarg < 1) stop ('Error! No arguments found. Please follow the format described in documentation')
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
        if(s/=0) stop ('Error! Wrong range arguments. Please follow the format described in documentation')
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

write(*, *) 'Trying to find the orbital information for ', astlist%listlen, ' asteroids.'
open(unit = 9, file = trim(pwd) // trim(input_pwd) // trim(source_name), &
    access = 'direct', recl = sizeof(scr), action = 'read')
astlist%current=>astlist%first
elementlist%listlen=0
do i=1,astlist%listlen
    if (find_asteroid(astlist%current%name,9,scr)==0) then
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
    endif
    astlist%current=>astlist%current%next
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

! Now performing global classification of resonances for asteroids in list
    if(.not. just_id_matrices) then
        open(unit = 9, file = trim(pwd) // trim(input_pwd) // 'z_treshold.txt', action = 'read')
        ! Read treshold coefficients for periodograms
        do i=100,10001
            read(9,*,iostat=s) j,z_value(j)
        enddo
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
    if(allocated(elementlist%first%a_list)) then
        do j=1,size(elementlist%first%a_list)
            if(allocated(elementlist%first%a_list(j)%item2)) deallocate(elementlist%first%a_list(j)%item2)
            if(allocated(elementlist%first%a_list(j)%item3)) deallocate(elementlist%first%a_list(j)%item3)
        enddo
        deallocate(elementlist%first%a_list)
    endif
    if(allocated(elementlist%first%r2list)) then
        do j=1,size(elementlist%first%r2list)
            if(allocated(elementlist%first%r2list(j)%clu)) deallocate(elementlist%first%r2list(j)%clu)
        enddo
        deallocate(elementlist%first%r2list)
    endif
    if(allocated(elementlist%first%r3list)) then
        do j=1,size(elementlist%first%r3list)
            if(allocated(elementlist%first%r3list(j)%clu)) deallocate(elementlist%first%r3list(j)%clu)
        enddo
        deallocate(elementlist%first%r3list)
    endif
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
