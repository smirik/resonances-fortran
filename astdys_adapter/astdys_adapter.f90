module astdys_adapter

    use global_parameters, nrecords => bin_numrec
    implicit none

contains

!----------------------------------------------------------------------------------------------

integer function check_source(source) result (s)
! Checks whether the binary source file exists.
! (by default it is "asteroids.bin" from AstDys)
! If not, returns -1
! In other case, returns the number of records in it.

    character(*) source
    type(orb_elem):: scr
    integer:: rec_counter

    open(unit = 9, file = trim(source), &
    access = 'direct', recl = sizeof(scr), action = 'read', iostat = s)
    if (s /= 0) then
        s = -1
    else
        rec_counter = 1
        do
            read(9, rec = rec_counter, iostat = s) scr
            if (s /= 0) exit
            rec_counter = rec_counter + 1
        enddo
        close(9)
        s = rec_counter - 1
    endif

end function check_source

!----------------------------------------------------------------------------------------------

subroutine source_builder(rec_number, epoch)
! Builds the binary source file using previously received source
! (by default it is "allnum.cat" from AstDys)
! Produces:
!   rec_number - number of records in a new source

    type(orb_elem):: scr
    integer:: s, rec_counter
    integer, intent(out):: rec_number
    real(8), intent(out):: epoch

    write(*, *) 'Trying to use basic source file for building source file.'
    open(unit = 10, file = trim(pwd) // trim(input_pwd) // trim(basic_source_name), action = 'read', iostat = s)
    if (s /= 0) stop ('Error! Cannot find '// trim(basic_source_name) // 'for building the source file. Program terminated.')
    open(unit = 9, file = trim(pwd) // trim(input_pwd) // trim(source_name), &
        access = 'direct', recl = sizeof(scr), status = 'replace')

    ! Pass header of basic source file
    do
        read(10, *, iostat = s) scr%name
        if (scr%name == 'END_OF_HEADER') then
            read(10, '(a)')
            exit
        endif
    enddo

    rec_counter = 0
    do
        read(10, *, iostat = s) scr%name, epoch, scr%elem
        if (s /= 0) exit
        rec_counter = rec_counter + 1
        scr%name = adjustl(scr%name)
        write(9, rec = rec_counter) scr
    enddo

    close(9)
    close(10)
    rec_number = rec_counter
    if (epoch < 2400000.0d0) epoch = epoch + 2400000.0d0

end subroutine source_builder
!----------------------------------------------------------------------------------------------

logical function greater(a, b, method)
! ">" logical operator for strings to be compared as "strings" (lexicographically)
! or as "numbers" (numerically)
! Given:
!   a, b - strings
!   method - must be 'lex' or 'num'
! Returns:
!   "true" or "false"

    character(*):: a, b, method
    integer:: i1, i2

    select case (method)
    case ('num')
        read(a, *) i1
        read(b, *) i2
        greater = (i1 > i2)
    case default
        greater = (a > b)
    end select

end function greater

!----------------------------------------------------------------------------------------------

integer function find_asteroid(ast_name, datafile, ast_data) result(flag)
! Find orbital elements for a given asteroid.
! Given:
!   ast_name - asteroid name used to find the data
!   datafile - unit descriptor of the source file (asteroids.bin by default)
! Returns:
!   <integer> - status (0 - if record is found, 1 - if record is not found)
!   ast_data - the corresponding special type orb_elem object for a given asteroid
!       (if no records found, returns as was)

    integer:: datafile
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
                if (greater(record_data%name, ast_name, search_method)) then
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
