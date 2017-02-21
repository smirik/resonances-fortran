program compositor

    use global_parameters
    use resonant_axis
    use id_matrix
    use resonance_finder
    use integrator
    implicit none

    character(25)::co, asteroid
    character(8):: pl_name
    integer:: pl_id,s
    real(8):: a
    real(8):: delta=0d0

    integer:: lenarg
    type(orb_elem)::scr
    type(orb_elem_list)::elementlist
    type(arglist)::astlist
    character(25):: sample,s1,s2
    integer:: i,n,i1,i2

!----------------------------------------------------------------------------------------------
! At first we initialize id-matrices (2body case for now)
    write (*, *) 'Initiating idmatrix_2body...'
    call init_idmatrix_2body()
    write (*, *) 'Successfully done!'
!----------------------------------------------------------------------------------------------
! Now we interpretate argument list
lenarg = command_argument_count()
if (lenarg < 1) stop('Error! No arguments found. Please follow the format described in documentation')
! If only one asteroid in input
if (lenarg == 1) then
    allocate(astlist%first)
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
            astlist%current=>astlist%first
            call get_command_argument(2,astlist%first%name)
        do i=3,lenarg
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
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
        astlist%current=>astlist%first
        astlist%first%name=s1
        do i=i1+1,i2
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
            write(sample,'(i25)') i
            astlist%current%name=trim(adjustl(sample))
        enddo
        astlist%listlen=max(0,i2-i1+1)
    case default
            allocate(astlist%first)
            astlist%current=>astlist%first
            call get_command_argument(1,astlist%first%name)
        do i=2,lenarg
            allocate(astlist%current%next)
            astlist%current=>astlist%current%next
            call get_command_argument(i,astlist%current%name)
        enddo
        astlist%listlen=lenarg
    end select
endif
!----------------------------------------------------------------------------------------------
! Now try to find information about asteroids in a list
open(unit=9,file='asteroids.bin',access='direct',recl=sizeof(scr),action='read')
n=480482
write(*,*) 'astlist',astlist%listlen
astlist%current=>astlist%first
elementlist%listlen=0
allocate(elementlist%current)
do i=1,astlist%listlen
    if (find_asteroid(astlist%current%name,9,n,scr)==0) then
        elementlist%listlen=elementlist%listlen+1
        if (elementlist%listlen==1) elementlist%first=>elementlist%current
        elementlist%current%item=scr
        allocate(elementlist%current%next)
        elementlist%current=>elementlist%current%next
        astlist%current=>astlist%current%next
    endif
enddo
deallocate(elementlist%current)
write(*,*) 'Total number of found asteroids: ',elementlist%listlen
close(9)

!----------------------------------------------------------------------------------------------
! Now performing an integration of asteroids in list
call mercury_processing(elementlist)
!----------------------------------------------------------------------------------------------
! Now find all possible resonances for asteroids in list
elementlist%current=>elementlist%first
do i=1,elementlist%listlen
    if (delta > 0d0) then
        call get_all_possible_resonances_2body(trim(elementlist%current%item%name),&
            elementlist%current%item%elem(1), delta)
    else
        write (*,*) 'Delta was not specified and will be chosen by internal methods'
        call get_all_possible_resonances_2body(trim(elementlist%current%item%name),&
            elementlist%current%item%elem(1))
    endif
    elementlist%current=>elementlist%current%next
enddo

call execute_command_line('cd librations;'//&
    "time ./lib `echo ../wd/*.rpin | sed 's/.rpin//g' | sed 's/...wd\///g'`",wait=.true.)



!----------------------------------------------------------------------------------------------
! Now deallocate lists

elementlist%current=>elementlist%first
do i=1,elementlist%listlen
    elementlist%first=>elementlist%current%next
    deallocate(elementlist%current)
    elementlist%current=>elementlist%first
enddo

astlist%current=>astlist%first
do i=1,astlist%listlen
    astlist%first=>astlist%current%next
    deallocate(astlist%current)
    astlist%current=>astlist%first
enddo

!--------------------------------------
    write (*, *) 'Clearing memory...'
    call clear_idmatrix_2body()
    write (*, *) 'Done. Ending program.'
!--------------------------------------


end
