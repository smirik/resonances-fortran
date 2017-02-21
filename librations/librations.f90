module librations

use global_parameters, reclen => aei_numrec
implicit none

integer,dimension(9):: planet_stat
real(8),dimension(:,:),allocatable:: arg_l,arg_m
real(8),dimension(:),allocatable:: time,ctime,ph
integer,dimension(4):: res_num

contains

subroutine init_planet_data()
! Check number of records in .aei files
    real(8) t
    integer i,j,s,pl_id
    
    allocate(arg_m(1:10,1:reclen),arg_l(1:10,1:reclen))
    do pl_id=1,9
        i=100+pl_id
        open(unit=i,file=trim(pwd)//'aeibase/'//trim(planet_name(pl_id))//'.aei',action='read',iostat=s)
        if (s/=0) then
            write(*,*) 'Error! ',planet_name(pl_id),'.aei cannot be opened.'
            write(*,*) 'This planet will be ignored in this session'
            planet_stat(pl_id)=1
            cycle
        endif
        if (count_aei_file_records(i) /= reclen) then
            write(*,*) 'Warning! Unexpectedly different file length of ',&
                        planet_name(pl_id),'.aei'
        endif
    
        do j=1,aei_header
            read(i,'(a)')! Pass header
        enddo
        do j=1,reclen
            read(i,*,iostat=s) t,arg_l(pl_id,j),arg_m(pl_id,j)
        enddo
        close(i)
        planet_stat(pl_id)=0
    enddo
end subroutine init_planet_data


subroutine global_libration_processing_2body(astlist)

type(orb_elem_list):: astlist
integer :: s,pl_id,i,j,cac
character(30)::ast_rpin,ast_aei,ast_name
character(8):: pl_name

allocate(ph(1:reclen),ctime(1:reclen),time(1:reclen))
astlist%current=>astlist%first
! Run over .rpin list
do i=1,astlist%listlen
    ast_name=astlist%current%item%name
    ast_rpin=trim(ast_name)//'.rpin'
    ast_aei=trim(ast_name)//'.aei'
    write(*,*) "Phase building and classification for ",ast_name
    open(unit=111,file=trim(pwd)//'wd/'//trim(ast_rpin),action='read',iostat=s)
    ! If .rpin cannot be opened
    if (s /= 0) then
        write(*,*) 'Error! ',ast_rpin,' cannot be opened. This asteroid will be passed.'
        astlist%current=>astlist%current%next
        cycle
    endif
    ! Open .aei for asteroid
    open(unit=110,file=trim(pwd)//'aeibase/'//trim(ast_aei),action='read',iostat=s)
    ! If .aei cannot be opened
    if (s /= 0) then
        write(*,*) 'Error! ',ast_aei,' cannot be opened. This asteroid will be passed.'
        astlist%current=>astlist%current%next
        cycle
    endif
    ! If .aei has another number of records
    if (count_aei_file_records(110) /= reclen) then
        write(*,*) 'Warning! Unexpectedly different file length of ',ast_aei
    endif
    
    do j=1,aei_header
        read(110,'(a)')! Pass header
    enddo
    ! Load information
    do j=1,reclen
        read(110,*,iostat=s) time(j),arg_l(10,j),arg_m(10,j)
    enddo
    close(110)

    open(unit=112,file=trim(pwd)//'wd/'//trim(ast_name)//'.phout',status='replace')
    open(unit=113,file=trim(pwd)//'wd/'//trim(ast_name)//'.circ',status='replace')
    open(unit=114,file=trim(pwd)//'wd/'//trim(ast_name)//'.rpout',status='replace')
    ! Run over resonances list for asteroid
    runover: do
        read(111,'(a8,3x,4i4)',iostat=s) pl_name,res_num
        if(s/=0) exit runover
        pl_id=planet_id(pl_name)
        
        if (planet_stat(pl_id)>0) then
            write(*,*) 'Planet ',pl_name,' is inaccessible and will be passed.'
            cycle
        endif
        write(112,*) 'RESONANCE ',pl_name,res_num
        call resph_2body(pl_id,res_num)
        call classifier_2body(pl_id,res_num)
    enddo runover
    close(111)
    close(112)
    close(113)
    close(114)
    astlist%current=>astlist%current%next
enddo
deallocate(ph,ctime,time)

end subroutine global_libration_processing_2body


subroutine clear_planet_data()
    deallocate(arg_m,arg_l)
end subroutine clear_planet_data

!------------------------------------------------------------------------

! Calculating phases
subroutine resph_2body(pl_id,res_num)

integer,dimension(1:4):: res_num
integer:: pl_id
! Locals
integer j,s

! Run over asteroid.aei file
do j=1,reclen
    ph(j)=(res_num(1)*(arg_m(pl_id,j)+arg_l(pl_id,j))+&
          res_num(2)*(arg_m(10,j)+arg_l(10,j))+&
          !res_num(3)*arg_l(pl_id,j)+&
          res_num(4)*arg_l(10,j))*deg2pi
    ph(j)=norm_ang(ph(j))
    ! Write out to .phout file
    write(112,*) time(j),ph(j)
enddo
write(112,*)
write(112,*)

end subroutine resph_2body

!------------------------------------------------------------------------

! Count circulations
subroutine classifier_2body(pl_id,res_num)

integer,dimension(1:4):: res_num
integer:: pl_id
! Locals
real(8) ttime,arg,tlast
real(8),dimension(-1:1):: a,t
integer f,s
real(8) d,dlast,dd

integer circus,k,verdict
real(8) circulation_time,slow_circulation_time

! Initialization
f=0
 circus=0
 circulation_time=0d0
 slow_circulation_time=0d0

t(f)=time(1);a(f)=ph(1)
tlast=time(1)
dlast=0d0

do k=2,reclen

ttime=time(k);arg=ph(k)

d=norm_ang(arg-a(f))!    Normalize the distance from origin
dd=norm_ang(d-dlast)!    Normalize distance from last point

if (dabs(dlast+dd)<=pi) then! If it is slightly different from the last one
    dlast=d
    !cycle!                 Then just update dlast and pass this step
else!                 Or... it can be more interesting
    f=f+sign(1d0,dd)
    
    select case (abs(f))
    case(0)!                Suspension cancelled
        a(f)=arg;t(f)=ttime! Set back to main origin (not exactly)
        dlast=0d0
    
    case(1)!                Just a suspension
        a(f)=arg;t(f)=ttime! Update the current origin
        dlast=0d0
    
    case(2)!                   Suspension was correct
        f=0!                   Reset the status
        circus=circus+1!                       Increase the number of circulations
        ctime(circus)=ttime!    Register this moment
        circulation_time=circulation_time+(ttime-tlast)
        if(ttime-tlast > circulation_parameter) &! In case of slow circulation
            slow_circulation_time=slow_circulation_time+(ttime-tlast)
        tlast=ttime
        a(f)=arg; t(f)=ttime!                       Reset the current origin
        dlast=0d0
    end select
endif

enddo

if(circus>0) then! Write out to .circ if necessary
    write(113,*) planet_name(pl_id),res_num
    do k=1,circus
        write(113,*) ctime(k)
    enddo
    write(113,*)
endif

! Classifier in work
if(circus==0) then
    verdict=1!                Means pure libration
else
    if (slow_circulation_time>libration_parameter) then
        verdict=0!            Means transient libration
    else
        verdict=-1!           Means circulation
    endif
endif

! Update .rpout list
write(114,*) verdict, planet_name(pl_id),res_num,circus,circulation_time,&
    circulation_time/(time(reclen)-time(1)),slow_circulation_time,&
    slow_circulation_time/(time(reclen)-time(1))! For common file

end subroutine classifier_2body


integer function count_aei_file_records(f) result(plen)
    integer f,j,s
    rewind(f)
    plen=0
    do j=1,aei_header
        read(f,'(a)')! Pass header
    enddo
    do
        read(f,*,iostat=s)
        if(s/=0) exit
        plen=plen+1
    enddo
    rewind(f)
end function count_aei_file_records


end module librations