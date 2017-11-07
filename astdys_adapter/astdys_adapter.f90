module astdys_adapter

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

!---------------------------------------------------------------------------------

end module astdys_adapter
