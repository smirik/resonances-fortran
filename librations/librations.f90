module librations

use global_parameters, reclen => aei_numrec
implicit none

! Work arrays ---------------------------------------------------------------------------------
integer,dimension(9):: planet_stat
real(8),dimension(:,:),allocatable:: arg_l, arg_m
real(8),dimension(:),allocatable:: time, ctime, ph,axis
integer,dimension(:),allocatable:: res_num

! Used for periodograms -----------------------------------------------------------------------
integer,parameter:: ln = ceiling(dlog(dble(reclen))/dlog(2d0))+1
integer,parameter:: n2 = 2 ** ln
complex(8),dimension(:),allocatable:: ph_comp, ph_per, axis_per

!----------------------------------------------------------------------------------------------
! Special types for classificator
type classifier_verdict
    integer:: verdict
    integer:: circus
    real(8):: circulation_time
    real(8):: circulation_ratio
    real(8):: slow_circulation_time
    real(8):: slow_circulation_ratio
end type classifier_verdict

type cl_2body
    character(9):: pl_name
    integer,dimension(4):: res_num
    type(classifier_verdict):: verdict
end type cl_2body

type cl_3body
    character(9):: pl_name
    character(9):: pl2_name
    integer,dimension(6):: res_num
    type(classifier_verdict):: verdict
end type cl_3body

!----------------------------------------------------------------------------------------------
contains

!----------------------------------------------------------------------------------------------
subroutine global_libration_processing(mode, astlist)
! Performs global phase building and resonance classification upon given asteroids
! Given:
!   mode - must be 2 or 3 (according to 2- or 3-body case)
!   astlist - special type list of asteroids with names, semimajor axes etc.
! Produces:
!   files of classified resonances for each asteroid
!   (some keys in global parameters allow also create metadata and draw graphics)
type(orb_elem_list):: astlist
integer:: s, pl_id, pl2_id, i, j, cac, mode
character(30):: ast_rpin,ast_aei,ast_name,res_index,axis_treshold,cross_treshold,ph_treshold
character(50):: res_name
character(8):: pl_name, pl2_name, script_name
character(1):: cmode
real(8):: max_ph_per, max_axis_per, mean_axis
real(8):: axis_disp, ph_disp! Std deviations (estimated)
complex(8):: avg
type(cl_2body):: v2
type(cl_3body):: v3
if (mode==3) then
    allocate(res_num(1:6))
    script_name='plot3.gp'
elseif (mode==2) then
    allocate(res_num(1:4))
    script_name='plot2.gp'
else
    write(*,*) 'Error! Wrong mode value (must be 2 or 3)'
    return
endif
write(cmode,'(i1)') mode

allocate(ph(1:reclen),axis(1:reclen),ctime(1:reclen),time(1:reclen))
allocate(ph_per(0:n2-1),axis_per(0:n2-1))
allocate(ph_comp(1:reclen))

astlist%current=>astlist%first
! Run over asteroid list
do i=1,astlist%listlen
    ast_name=astlist%current%item%name
    ast_rpin=trim(ast_name)//'.rp'//cmode
    ast_aei=trim(ast_name)//'.aei'
    write(*,*) "Phase building and classification for ",ast_name,'('//cmode//'-body case)'
    open(unit=111,file=trim(pwd)//'wd/'//trim(ast_rpin),action='read',iostat=s)
    ! If resonances cannot be read
    if (s /= 0) then
        write(*,*) 'Error! ',ast_rpin,' cannot be opened. This case will be passed.'
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
    ! If .aei has another number of records, it will still work
    if (count_aei_file_records(110) /= reclen) then
        write(*,*) 'Warning! Unexpectedly different file length of ',ast_aei
    endif
    ! Pass header
    do j=1,aei_header
        read(110,'(a)')
    enddo
    ! Load information
    do j=1,reclen
        read(110,*,iostat=s) time(j),arg_l(10,j),arg_m(10,j),axis(j)
    enddo
    close(110)

    !axis=smoother(axis)
    mean_axis=sum(axis(:)/reclen)
    axis=axis-mean_axis
    axis_disp= sum(axis(:)**2)/dble(reclen-1)
    write(axis_treshold,'(f30.16)') axis_disp**2*55.0d0/reclen**2
    axis_per(0:reclen-1)=dcmplx(axis(1:reclen),0d0)
    axis_per(reclen:n2-1)=(0d0,0d0)
    call fft(1,axis_per)
    max_axis_per=maxval(cdabs(axis_per(:)))**2/(reclen**2)

    open(unit=114,file=trim(pwd)//'wd/'//trim(ast_name)//'.rpout'//cmode,status='replace')
    if(allow_writing_metadata) then
        open(unit=112,file=trim(pwd)//'wd/'//trim(ast_name)//'.phout'//cmode,status='replace')
        close(112)
        open(unit=115,file=trim(pwd)//'wd/'//trim(ast_name)//'.smooth'//cmode,status='replace')
        close(115)
        open(unit=116,file=trim(pwd)//'wd/'//trim(ast_name)//'.per'//cmode,status='replace')
        close(116)
        open(unit=117,file=trim(pwd)//'wd/'//trim(ast_name)//'.circ'//cmode,status='replace')
        close(117)
    endif
    ! Run over resonances list for asteroid
    cac=0
    runover: do
        if(allow_writing_metadata) then
            open(unit=112,file=trim(pwd)//'wd/'//trim(ast_name)//'.phout'//cmode,&
                action='write',position='append')
            open(unit=115,file=trim(pwd)//'wd/'//trim(ast_name)//'.smooth'//cmode,&
                action='write',position='append')
            open(unit=116,file=trim(pwd)//'wd/'//trim(ast_name)//'.per'//cmode,&
                action='write',position='append')
            open(unit=117,file=trim(pwd)//'wd/'//trim(ast_name)//'.circ'//cmode,&
                action='write',position='append')
        endif
        if(mode==3) then
            read(111,'(a8,3x,a8,3x,6i4)',iostat=s) pl_name,pl2_name,res_num
        else
            read(111,'(a8,3x,4i4)',iostat=s) pl_name,res_num
        endif
        if(s/=0) exit runover
!       write(*,*) "resonance index ",cac
        pl_id=planet_id(pl_name)
        if (planet_stat(pl_id)>0) then
            write(*,*) 'Planet ',pl_name,' is inaccessible and will be passed.'
            cycle
        endif
        if(mode==3) then
            pl2_id=planet_id(pl2_name)
            if (planet_stat(pl2_id)>0) then
                write(*,*) 'Second planet ',pl2_name,' is inaccessible and will be passed.'
                cycle
            endif
        endif
        if(mode==3) then
            call resph(res_num,pl_id,pl2_id)
        else
            call resph(res_num,pl_id)
        endif

        ph_comp(:)=dcmplx(cos(ph(:)),sin(ph(:)))
        avg=dcmplx( sum(dreal(ph_comp(:))/reclen),sum(aimag(ph_comp(:))/reclen) )
        ph_disp= sum(cdabs(ph_comp(0:reclen-1)-avg)**2)/dble(reclen-1)

        ph_per(0:reclen-1)=ph_comp(1:reclen)
        ph_per(reclen:n2-1)=(0d0,0d0)
        call fft(1,ph_per)
        max_ph_per=maxval(cdabs(ph_per(:)))**2/(reclen**2)
        
        if(mode==3) then
            v3%pl_name=pl_name
            v3%pl2_name=pl2_name
            v3%res_num=res_num
            v3%verdict=new_classifier(res_num,pl_id,pl2_id)
            write(114,*) v3
        else
            v2%pl_name=pl_name
            v2%res_num=res_num
            v2%verdict=new_classifier(res_num,pl_id)
            write(114,*) v2
        endif
        if(allow_writing_metadata) then
            ! Current data rows
            if(mode==3) then
                write(112,*) 'RESONANCE ',pl_name,' ',pl2_name,res_num
            else
                write(112,*) 'RESONANCE ',pl_name,res_num
            endif
            do j=1,reclen
                write(115,*) time(j),dreal(ph_comp(j)),aimag(ph_comp(j)),&
                axis(j)+mean_axis,ctime(j)
                write(112,*) time(j),ph(j)
            enddo
            write(115,*)
            write(115,*)
            write(112,*)
            write(112,*)
            do j=1,n2/2
                write(116,*) 1d0/dble(n2)/1d1*(j-1),&!                      1
                    cdabs(ph_per(j-1))**2/(reclen**2),&!                    2
                    cdabs(axis_per(j-1))**2/(reclen**2),&!                  3
                    cdabs(ph_per(j-1))**2/(reclen**2)/max_ph_per,&!         4
                    cdabs(axis_per(j-1))**2/(reclen**2)/max_axis_per,&!     5
                    cdabs(conjg(ph_per(j-1))*axis_per(j-1))**2/(reclen**2)/(reclen**2)! 6
            enddo
            write(116,*)
            write(116,*)        
            close(112)
            close(115)
            close(116)
            close(117)
        endif
        if (allow_plotting) then
            if((mode==3 .and. v3%verdict%verdict >=0) .or. &
                (mode==2 .and. v2%verdict%verdict >=0)) then
                write(*,*) 'Got some interesting...'
                write(res_index,'(i25)') cac
                if(mode==3) then
                    write(res_name,'(i2,2a9,6i5)') v3%verdict%verdict,pl_name,pl2_name,res_num
                else
                    write(res_name,'(i2,a9,4i5)') v2%verdict%verdict,pl_name,res_num
                endif
                write(ph_treshold,'(f30.16)') ph_disp**2*55.0d0/reclen**2
                write(cross_treshold,'(f30.16)') axis_disp*ph_disp*55.0d0/reclen**2
                write(*,*) 'Plotting '//script_name//':'//res_index//&
                    ' diagram for '//trim(ast_name)
                call execute_command_line('cd '//trim(pwd)//&
                    'wd; gnuplot -e "name='//trim(ast_name)//&
                    ';i='//res_index//&
                    ';resonance=\"'//res_name//&
                    '\";ax_t='//axis_treshold//&
                    ';ph_t='//ph_treshold//&
                    ';cr_t='//cross_treshold//&
                    '" '//script_name, wait=.true.)
            endif
        endif
        cac=cac+1
    enddo runover

    close(114)
    close(111)
    if(allow_writing_metadata .and. dispose_metadata) &
        call execute_command_line('cd '//trim(pwd)//'wd; rm -f '//&
            trim(ast_name)//'.circ'//cmode//' '//&
            trim(ast_name)//'.per'//cmode//' '//&
            trim(ast_name)//'.rp'//cmode//' '//&
            trim(ast_name)//'.phout'//cmode//' '//&
            trim(ast_name)//'.smooth'//cmode, wait=.false.)
    astlist%current=>astlist%current%next
enddo
deallocate(ph,ctime,time,axis)
deallocate(ph_per,axis_per)
deallocate(ph_comp)
deallocate(res_num)
end subroutine global_libration_processing

!----------------------------------------------------------------------------------------------
subroutine resph(res_num, pl_id, pl2_id)
! Calculating resonant phase angle
! Given:
!   res_num - resonance numbers
!   pl_id - planet ID
!   pl2_id - (OPTIONAL) second planet ID
! Produces:
!   ph - array with lesonant phase over all period
    integer,dimension(:):: res_num
    integer:: pl_id
    integer,optional:: pl2_id
    integer j,s

    if(present(pl2_id)) then
        forall(j=1:reclen) &
            ph(j)=norm_ang( (res_num(1)*(arg_m(pl_id,j)+arg_l(pl_id,j))+&
                res_num(2)*(arg_m(pl2_id,j)+arg_l(pl2_id,j))+&
                res_num(3)*(arg_m(10,j)+arg_l(10,j))+&
                res_num(6)*arg_l(10,j))*deg2pi)
    else
        forall(j=1:reclen) &
            ph(j)=norm_ang((res_num(1)*(arg_m(pl_id,j)+arg_l(pl_id,j))+&
                res_num(2)*(arg_m(10,j)+arg_l(10,j))+&
                res_num(4)*arg_l(10,j))*deg2pi)
    endif
end subroutine resph

!----------------------------------------------------------------------------------------------
type(classifier_verdict) function new_classifier(res_num, pl_id, pl2_id) result(v)
! Current version of libration classifier
! Given:
!   res_num - resonance numbers
!   pl_id - planet ID
!   pl2_id - (OPTIONAL) second planet ID
!   ph - (IMPLICIT) - array of phase angles
! Returns:
!   v - special type object, including:
!       verdict
!       number of all detected circulations
!       total time of all circulations and it's ratio
!       total time of slow circulations and transient librations and it's ratio
    integer,dimension(:):: res_num
    integer:: pl_id
    integer, optional:: pl2_id
    logical:: case_3body
    real(8) ttime,tlast,d,dd,r2,sum_r2,r2_t
    complex(8) z1,z2
    integer k,klast,i

 case_3body=present(pl2_id)
! Initialization
v%circus=0
v%circulation_time=0d0
v%slow_circulation_time=0d0

klast=1
tlast=time(1)
z1=ph_comp(1)
dd=0d0
sum_r2=0d0
 ctime(klast)=0d0

if(case_3body) then
    r2_t = r2_treshold_3body
    if(allow_writing_metadata) write(117,*) 0,0,0,0,0,0,0,0,0,&
        planet_name(pl_id),' ',planet_name(pl2_id), res_num
else
    r2_t = r2_treshold_2body
    if(allow_writing_metadata) write(117,*) 0,0,0,0,0,0,0,0,0,&
        planet_name(pl_id),res_num
endif

do k=2,reclen
    ttime=time(k)
    z2=ph_comp(k)
    d=datan2(dreal(z1)*aimag(z2)-aimag(z1)*dreal(z2),dreal(z1)*dreal(z2)+aimag(z1)*aimag(z2))
    dd=dd+d
    ctime(k)=dd
    if (dabs(dd)<twopi) then
        z1=ph_comp(k)
    else
        v%circus=v%circus+1! Increase the number of circulations
        v%circulation_time=v%circulation_time+(ttime-tlast)
        ctime(klast:k)=ctime(klast:k)-dd/(k-klast)*(/ (i-1, i=1,k-klast+1) /)
        if(ttime-tlast > circulation_parameter) then! In case of slow circulation
            r2=dsqrt( sum(ctime(klast:k)**2)/(k-klast+1) )
            if (r2*(ttime-tlast)>r2_t) then
                v%slow_circulation_time=v%slow_circulation_time+(ttime-tlast)
                sum_r2 = sum_r2+r2*(ttime-tlast)
            endif
        else
            r2=0d0
        endif
        if(allow_writing_metadata) write(117,*) k, ttime,ttime-tlast,r2,r2*(ttime-tlast)
        tlast=ttime
        dd=0d0
        z1=ph_comp(k)
        klast=k
        ctime(k)=0d0
    endif
enddo
! Processing the "tail" (maybe here is libration too)
if (klast/=reclen .and. dabs(dd)>pi/4d0) then
    ctime(klast:reclen)=ctime(klast:reclen)- &
        dd/(reclen-klast)*(/ (i-1, i=1,reclen-klast+1) /)
    if(ttime-tlast > circulation_parameter) then! In case of slow circulation
        r2=dsqrt( sum(ctime(klast:reclen)**2)/(reclen-klast+1) )
        if (r2*(ttime-tlast)>r2_t) then
            v%slow_circulation_time=v%slow_circulation_time+(ttime-tlast)
            sum_r2 = sum_r2+r2*(ttime-tlast)
        endif
    else
        r2=0d0
    endif
endif
if(allow_writing_metadata) then
    write(117,*)
    write(117,*)
endif
! Classifier in work
if(v%circus==0) then
    v%verdict=1!                Means pure libration
else
    if ( sum_r2 >= sum_r2_treshold .or. &
    (v%slow_circulation_time>libration_parameter .or. &
    v%circulation_time-v%slow_circulation_time<1d5-libration_parameter) ) then
        v%verdict=0!            Means transient libration
    else
        v%verdict=-1!           Means circulation
    endif
endif
v%circulation_ratio=v%circulation_time/(time(reclen)-time(1))
v%slow_circulation_ratio=v%slow_circulation_time/(time(reclen)-time(1))

end function new_classifier

!------------------------------------------------------------------------
! Fast Fourier Transform
! Do not try to check it. Really.
! This is the most correct and precise code in the world. No jokes.
subroutine fft(f,x)
complex(8),dimension(0:n2-1)::x,xx
integer i,j,m,k,v,ni,jm,f
do m=1,ln
 do k=0,2**(ln-m)-1
  do v=0,2**(m-1)-1
   j=k*2**m+v;jm=2**(m-1)+j
   i=k*2**(m-1)+v;ni=2**(ln-1)+i
   xx(j)=x(i)+x(ni)
   xx(jm)=(x(i)-x(ni))*exp(dcmplx(0d0,-2d0*pi*dble(k*f)/dble(2**(ln-m+1))))
  enddo; enddo
 x(:)=xx(:); enddo
 if (f==-1) x=x/n2
end subroutine fft

!----------------------------------------------------------------------------------------------
subroutine init_planet_data()
! Loads planet data from .aei files into RAM
    real(8) t
    integer i,j,s,pl_id

    allocate(arg_m(1:10,1:reclen),arg_l(1:10,1:reclen))
    do pl_id=1,9
        i=100+pl_id
        open(unit=i,file=trim(pwd)//'aeibase/'//trim(planet_name(pl_id))//'.aei',&
        action='read',iostat=s)
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

!----------------------------------------------------------------------------------------------
subroutine clear_planet_data()
! Clears memory from loaded planet data
    deallocate(arg_m,arg_l)
end subroutine clear_planet_data

!----------------------------------------------------------------------------------------------
integer function count_aei_file_records(f) result(plen)
! Count number of records in .aei file (current standard is 10001)
! Given:
!   f - unit descriptor for .aei file
! Returns:
!   <integer> - number of records
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

!----------------------------------------------------------------------------------------------
function smoother(arg_array)
! Do smoothing
! (currently not used, but may be)
! Given:
!   arg_array - array to be smoothed
! Returns:
!   smoothed array of the same size
    real(8),dimension(:):: arg_array
    real(8),dimension(1:size(arg_array))::smoother
    !real(8):: lambda
    integer::i,lambda
    !smoother(1)=arg_array(1)
    do i=1,size(arg_array)
        !lambda=min(1d1,dble(i))
        lambda=max(i-0,1)
        !smoother(i)=((lambda-1d0)*smoother(i-1)+arg_array(i))/lambda
        smoother(i)=sum(arg_array(lambda:i))/dble(i+1-lambda)
    enddo
end function smoother

!----------------------------------------------------------------------------------------------
end module librations