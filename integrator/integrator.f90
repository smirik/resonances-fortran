module integrator

    use global_parameters
    implicit none

contains

!----------------------------------------------------------------------------------------------

integer function find_asteroid(ast_name, datafile, nrecords, ast_data) result(flag)
! Find orbital elements for a given asteroid.
! Given:
!   ast_name - asteroid name used to find the data
!   datafile - unit descriptor of the source file (asteroids.bin by default)
!   nrecords - number of records in the source file (480482 at present)
! Returns:
!   <integer> - status (0 - if record is found, 1 - if record is not found)
!   ast_data - the corresponding special type orb_elem object for a given asteroid
!       (if no records found, returns as was)

    integer:: datafile, nrecords
    character(25):: ast_name
    type(orb_elem):: ast_data, record_data
    integer left_record, right_record, middle_record

    write(*, *) 'Seeking record about ', ast_name
    flag = 1
    left_record = 1
    right_record = nrecords
    ! Check if either first or last record is what we need
    read(datafile, rec = left_record) record_data
    if (record_data%name == ast_name) then
        middle_record = left_record
        flag = 0
    endif
    read(datafile, rec = right_record) record_data
    if (record_data%name == ast_name) then
        middle_record = right_record
        flag = 0
    endif

    if (flag /= 0) then
    ! Start pyramidal loop over records
        do while (right_record - left_record > 1)
            middle_record = (left_record + right_record) / 2
            read(datafile, rec = middle_record) record_data
            if (record_data%name == ast_name) then
                flag = 0
                exit
            else
                if (record_data%name > ast_name) then
                    right_record = middle_record
                else
                    left_record = middle_record
                endif
            endif
        enddo
    endif

    if (flag == 0) then
            write(*, *) 'Found record for ', record_data%name, 'at position ', middle_record
            ast_data = record_data
    else
        write(*, *) '...Not found any record about ', ast_name
    endif

end function find_asteroid

!----------------------------------------------------------------------------------------------

subroutine small_in_header(small_in)
! Internal subroutine
! Writes the header for "small.in"

    integer:: small_in

    write(small_in, '(a)') ')O+_06 Small-body initial data  (WARNING: Do not delete this line!!)'
    write(small_in, '(a)') ") Lines beginning with `)' are ignored."
    write(small_in, '(a)') ')---------------------------------------------------------------------'
    write(small_in, '(a)') 'style (Cartesian, Asteroidal, Cometary) = Asteroidal'
    write(small_in, '(a)') ')--------------------------------------------d or D is not matter--0d0 is possible too--'

end subroutine small_in_header

!---------------------------------------------------------------------------------

logical function invalid_asteroid_aei(small_in, ast_item) result(l)
! Internal function
! Checks if current asteroid already has its .aei file

    type(orb_elem):: ast_item
    integer:: small_in, s

    open(unit = 110, file = trim(pwd) // trim(aeibase_pwd) // trim(ast_item%name) // '.aei', &
        action = 'read', iostat = s)
    if (s == 0 .and. .not. force_aei_rebuilding) then
        write(*, *) "Asteroid ", ast_item%name, " doesn't need an integration"
        close(110)
        l = .false.
    else
        write(small_in, *) ast_item%name, ' ep=', ep
        write(small_in, *) ast_item%elem(1), ast_item%elem(2), ast_item%elem(3), &
            ast_item%elem(5), ast_item%elem(4), ast_item%elem(6), ' 0d0 0d0 0d0'
        l = .true.
    endif

end function invalid_asteroid_aei
!---------------------------------------------------------------------------------

subroutine make_integration(j)
! Internal subroutine
! Performs integration of the asteroid block number "j"
! and places .aei files to the corresponding directories

    integer:: j, pl_id

    write(*,*) 'Starting integration of a ', j, ' block of asteroids'
        call chdir (trim(pwd) // trim(mercury_pwd))
        call execute_command_line("./mercury6; ./element6", wait = .true.)
        do pl_id = 1, 9
            call execute_command_line("mv " // trim(planet_name(pl_id)) // ".aei " // &
                trim(pwd) // trim(aeiplanet_pwd), wait = .true.)
        enddo
        ! Move new .aei files to a needed directory and clean the workplace
        call execute_command_line("mv *.aei " // trim(pwd) // trim(aeibase_pwd) // &
            "; ./simple_clean.sh", wait = .true.)
        call chdir(trim(pwd))

end subroutine make_integration

!---------------------------------------------------------------------------------

subroutine mercury_processing(element_list)
! Performs integration of asteroids by blocks and creates .aei files if needed
! Given:
!   element_list - special list type for asteroids and their orbital elements
! Produces:
!   .aei files for asteroids in "aeibase" directory
!   .aei files for planets in "aei_planet" directory

    type(orb_elem_list):: element_list
    integer i, j, s, block_counter, flag

    write(*,*) '... Creating .aei files for a given subset...'
    element_list%current => element_list%first
    block_counter = element_list%listlen / max_block_size
    do j = 0, block_counter
        open(unit = 8, file = trim(pwd) // trim(mercury_pwd) // 'small.in', status = 'replace')
        call small_in_header(8) 
        flag = 0
        ! Check that planet .aei files are available
        do i = 1, 8
            open(unit = 110, file = trim(pwd) // trim(aeiplanet_pwd) // trim(planet_name(i)), &
                action = 'read', iostat = s)
            if (s /= 0) then
                flag = flag + 1
            else
                close(110)
            endif
        enddo
        ! Checking asteroids in a current block
        do i = j * max_block_size + 1, min((j + 1) * max_block_size, element_list%listlen)
            if (invalid_asteroid_aei(8, element_list%current%item) ) flag = flag + 1
            element_list%current => element_list%current%next
        enddo
        close(8)
        ! Make the integration
        if(flag > 0) then
            call make_integration(j)
        endif
    enddo
    write(*, *) '.aei files created.'

end subroutine mercury_processing

!---------------------------------------------------------------------------------

end module integrator
