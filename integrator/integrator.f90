module integrator

    use global_parameters
    implicit none

contains

!----------------------------------------------------------------------------------------------

integer function find_asteroid(sample,datafile,nrec,st) result(flag)
! Find orbital elements for a given asteroid.
! Given:
!   sample - asteroid name
!   datafile - unit descriptor of the source file (asteroids.bin by default)
!   nrec - number of records in the source file (480482 by this time)
! Returns:
!   <integer> - status (0 - if record is found, 1 - if record is not found)
!   st - the corresponding special type orb_elem object for a given asteroid
!       (if no records found, returns as was)

    integer:: datafile, nrec
    character(25):: sample
    type(orb_elem):: st, record_data
    integer left_record, right_record, middle_record

    write(*, *) 'Seeking record about ', sample
    flag = 1
    left_record = 1
    right_record = nrec
    ! Check if either first or last record is what we need
    read(datafile, rec = left_record) record_data
    if (record_data%name == sample) then
        middle_record = left_record
        flag = 0
    endif
    read(datafile, rec = right_record) record_data
    if (record_data%name == sample) then
        middle_record = right_record
        flag = 0
    endif

    if (flag /= 0) then
    ! Start pyramidal loop over records
        do while (right_record - left_record > 1)
            middle_record = (left_record + right_record) / 2
            read(datafile, rec = middle_record) record_data
            if (record_data%name == sample) then
                flag = 0
                exit
            else
                if (record_data%name > sample) then
                    right_record = middle_record
                else
                    left_record = middle_record
                endif
            endif
        enddo
    endif

    if (flag == 0) then
            write(*, *) 'Found record for ', record_data%name, 'at position ', middle_record
            st = record_data
    else
        write(*, *) '...Not found any record about ', record_data%name
    endif

end function find_asteroid

!----------------------------------------------------------------------------------------------

subroutine mercury_processing(element_list)
! Performs integration of asteroids and creates .aei files if needed
! Given:
!   element_list - special type list (see above) of asteroids and their orbital elements
! Produces:
!   .aei files in "aeibase" directory

    integer i,j,s,block_counter,flag
    type(orb_elem_list):: element_list

    write(*,*) '... Creating .aei files for a given subset...'
    element_list%current=>element_list%first
    block_counter=element_list%listlen / kmax
    do j=0,block_counter
        open(unit=8,file=trim(pwd)//'/mercury/small.in',status='replace')
        write(8,'(a)') ')O+_06 Small-body initial data  (WARNING: Do not delete this line!!)'
        write(8,'(a)') ") Lines beginning with `)' are ignored."
        write(8,'(a)') ')---------------------------------------------------------------------'
        write(8,'(a)') 'style (Cartesian, Asteroidal, Cometary) = Asteroidal'
        write(8,'(a)') ')--------------------------------------------d or D is not matter--0d0 is possible too--'
        flag=0
        open(unit=110,file=trim(pwd)//'/aei_planet/MERCURY.aei',action='read',iostat=flag)
        close(110)
        c2: do i=j*kmax+1,min((j+1)*kmax,element_list%listlen)
            open(unit=110,file=trim(pwd)//'/aeibase/'//trim(element_list%current%item%name)//'.aei',&
                action='read',iostat=s)
            if (s/=0) open(unit=110,file=trim(pwd)//'/aei_bank/'//trim(element_list%current%item%name)//&
            '.aei', action='read',iostat=s)
            if(s==0 .and. .not. force_aei_rebuilding) then
                write(*,*) 'Asteroid ',element_list%current%item%name," doesn't need an integration"
                close(110)
                element_list%current=>element_list%current%next
                cycle c2
            endif
            write(8,*) element_list%current%item%name,' ep=',ep
            write(8,*) element_list%current%item%elem(1),element_list%current%item%elem(2),&
                       element_list%current%item%elem(3),element_list%current%item%elem(5),&
                       element_list%current%item%elem(4),element_list%current%item%elem(6),&
                       ' 0d0 0d0 0d0'
            flag=flag+1
            element_list%current=>element_list%current%next
        enddo c2
        close(8)
        if(flag>0) then
            write(*,*) 'Starting integration of a ',j,' block of asteroids'
            call execute_command_line('cd '//trim(pwd)//'/mercury; '// &
                "./mercury6; ./element6",wait=.true.)
            call execute_command_line('cd '//trim(pwd)//'/mercury; '// &
                "mv MERCURY.aei ../aei_planet/; mv VENUS.aei ../aei_planet; "// &
                "mv EARTHMOO.aei ../aei_planet/; mv MARS.aei ../aei_planet; "// &
                "mv JUPITER.aei ../aei_planet/; mv SATURN.aei ../aei_planet; "// &
                "mv URANUS.aei ../aei_planet/; mv NEPTUNE.aei ../aei_planet; "// &
                "mv PLUTO.aei ../aei_planet/;",wait=.true.)
            call execute_command_line('cd '//trim(pwd)//'/mercury; '// &
                "mv *.aei ../aeibase/; mv info* ../ ; ./simple_clean.sh",wait=.true.)
        endif
    enddo
    write(*,*) '   .aei files created.'    

end subroutine mercury_processing

!----------------------------------------------------------------------------------------------

end module integrator
