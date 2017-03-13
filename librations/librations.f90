module librations

use global_parameters, reclen => aei_numrec
implicit none

integer,dimension(9):: planet_stat
real(8),dimension(:,:),allocatable:: arg_l,arg_m
real(8),dimension(:),allocatable:: time,ctime,ph,axis
integer,dimension(4):: res_num


integer:: ln=ceiling(dlog(dble(reclen))/dlog(2d0))+1
complex(8),dimension(:),allocatable:: ph_per,axis_per


integer:: n2
integer:: nstar! For correlograms
complex(8),dimension(:),allocatable:: ph_comp

type cl_2body
    integer::verdict
    character(8)::pl_name
    integer,dimension(4)::res_num
    integer:: circus
    real(8)::circulation_time
    real(8)::circulation_ratio
    real(8)slow_circulation_time
    real(8)slow_circulation_ratio
end type cl_2body


contains
!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------

subroutine global_libration_processing_2body(astlist)

type(orb_elem_list):: astlist
integer :: s,pl_id,i,j,cac
character(30)::ast_rpin,ast_aei,ast_name,res_index,axis_treshold,cross_treshold,ph_treshold
character(8):: pl_name
real(8):: max_ph_per,max_axis_per
complex(8):: avg
real(8):: axis_disp,ph_disp! Std deviarions (estimated)
n2=2**ln
nstar=reclen*4/10

allocate(ph(1:reclen),axis(1:reclen),ctime(1:reclen),time(1:reclen))
allocate(ph_per(0:n2-1),axis_per(0:n2-1))
allocate(ph_comp(1:reclen))

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
        read(110,*,iostat=s) time(j),arg_l(10,j),arg_m(10,j),axis(j)
    enddo
    close(110)

    !axis=smoother(axis)
    max_axis_per=sum(axis(:)/reclen)
    axis=axis-max_axis_per
    max_axis_per=maxval(dabs(axis(:)))
    !axis=axis/max_axis_per
    axis_disp= sum(axis(:)**2)/dble(reclen-1)
    write(axis_treshold,'(f30.16)') axis_disp**2*55.0d0/reclen**2
    
    axis_per(0:reclen-1)=dcmplx(axis(1:reclen),0d0)
    axis_per(reclen:n2-1)=(0d0,0d0)
    call fft(1,axis_per)
    max_axis_per=maxval(cdabs(axis_per(:)))**2/(reclen**2)
    
    open(unit=112,file=trim(pwd)//'wd/'//trim(ast_name)//'.phout',status='replace')
    close(112)
    open(unit=113,file=trim(pwd)//'wd/'//trim(ast_name)//'.circ',status='replace')
    open(unit=114,file=trim(pwd)//'wd/'//trim(ast_name)//'.rpout',status='replace')
    open(unit=115,file=trim(pwd)//'wd/'//trim(ast_name)//'.smooth',status='replace')
    close(115)
    open(unit=116,file=trim(pwd)//'wd/'//trim(ast_name)//'.per',status='replace')
    close(116)
    ! Run over resonances list for asteroid
    cac=0
    runover: do
        open(unit=112,file=trim(pwd)//'wd/'//trim(ast_name)//'.phout',&
        action='write',position='append')
        open(unit=115,file=trim(pwd)//'wd/'//trim(ast_name)//'.smooth',&
        action='write',position='append')
        open(unit=116,file=trim(pwd)//'wd/'//trim(ast_name)//'.per',&
        action='write',position='append')
        
        read(111,'(a8,3x,4i4)',iostat=s) pl_name,res_num
        if(s/=0) exit runover
        write(*,*) "resonance index ",cac

        pl_id=planet_id(pl_name)
        
        if (planet_stat(pl_id)>0) then
            write(*,*) 'Planet ',pl_name,' is inaccessible and will be passed.'
            cycle
        endif
        write(112,*) 'RESONANCE ',pl_name,res_num
        call resph_2body(pl_id,res_num)
        
        ph_comp(:)=dcmplx(cos(ph(:)),sin(ph(:)))
        avg=dcmplx( sum(dreal(ph_comp(:))/reclen),sum(aimag(ph_comp(:))/reclen) )
        ph_disp= sum(cdabs(ph_comp(0:reclen-1)-avg)**2)/dble(reclen-1)

        ph_per(0:reclen-1)=ph_comp(1:reclen)
        ph_per(reclen:n2-1)=(0d0,0d0)
        call fft(1,ph_per)
        max_ph_per=maxval(cdabs(ph_per(:)))**2/(reclen**2)
        
        ! Current data rows
        do j=1,reclen
            write(115,*) time(j),dreal(ph_comp(j)),aimag(ph_comp(j)),axis(j)
        enddo
        write(115,*)
        write(115,*)

        
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
        write(114,*) cdabs(avg),ph_disp,verd_avg(cdabs(avg)),&
        classifier_2body(pl_id,res_num)
        
        close(112)
        close(115)
        close(116)
        write(res_index,'(i25)') cac
        write(ph_treshold,'(f30.16)') ph_disp**2*55.0d0/reclen**2
        write(cross_treshold,'(f30.16)') axis_disp*ph_disp*55.0d0/reclen**2
        write(*,*) 'Plotting '//res_index//' diagram for '//trim(ast_name)
        call execute_command_line('cd '//trim(pwd)//'wd; gnuplot -e "name='//trim(ast_name)//&
        ';i='//res_index//&
        ';ax_t='//axis_treshold//&
        ';ph_t='//ph_treshold//&
        ';cr_t='//cross_treshold//&
        '" script.gp', wait=.true.)
        cac=cac+1
    enddo runover
    
    close(111)
    close(112)
    close(113)
    close(114)
!    write(res_index,'(i25)') cac-1
!    write(*,*) 'Plotting '//res_index//' diagrams for '//trim(ast_name)
!    call execute_command_line('cd '//trim(pwd)//'wd; gnuplot -e "imax='//res_index//&
!    ';name='//trim(ast_name)//'" script.gp', wait=.true.)
    
    astlist%current=>astlist%current%next
enddo
deallocate(ph,ctime,time,axis)
deallocate(ph_per,axis_per)
deallocate(ph_comp)

end subroutine global_libration_processing_2body

integer function verd_avg(x)
    real(8) x
    if (x>=1d-2) then
        verd_avg=1
    else
        verd_avg=0
    endif
end function verd_avg

!----------------------------------------------------------------------------------------------

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

!----------------------------------------------------------------------------------------------

! Do smoothing
function smoother(arg_array)
    real(8),dimension(:):: arg_array
    real(8),dimension(1:size(arg_array))::smoother
!    real(8):: lambda
    integer::i,lambda
!    smoother(1)=arg_array(1)
    do i=1,size(arg_array)
!        lambda=min(1d1,dble(i))
        lambda=max(i-0,1)
!        smoother(i)=((lambda-1d0)*smoother(i-1)+arg_array(i))/lambda
        smoother(i)=sum(arg_array(lambda:i))/dble(i+1-lambda)
    enddo
end function smoother

!------------------------------------------------------------------------

! Sinusoization of array (for phases)
function array_sin(arg_array)
    real(8),dimension(:):: arg_array
    real(8),dimension(1:size(arg_array))::array_sin
    integer::i
    do i=1,size(arg_array)
        array_sin(i)=dsin(arg_array(i))
    enddo
end function array_sin

!------------------------------------------------------------------------

! Fast Fourier Transition
! Do not try to check it. Really.
! Trust me, this is the most correct and precise code in the world. No jokes.
subroutine fft(f,x)
complex(8),dimension(0:n2-1)::x,xx
integer i,j,m,k,v,ni,jm,f
do m=1,ln
 do k=0,2**(ln-m)-1
  do v=0,2**(m-1)-1
   j=k*2**m+v;jm=2**(m-1)+j
   i=k*2**(m-1)+v;ni=2**(ln-1)+i
   xx(j)=x(i)+x(ni)
   xx(jm)=(x(i)-x(ni))*exp(dcmplx( 0d0,-2d0*pi*dble(k*f)/dble(2**(ln-m+1)) ))
  enddo; enddo
 x(:)=xx(:); enddo
 if (f==-1) x=x/n2
end subroutine fft
!------------------------------------------------------------------------

! Count circulations
type(cl_2body) function classifier_2body(pl_id,res_num) result(v_2body)

integer,dimension(1:4):: res_num
integer:: pl_id
! Locals
real(8) ttime,arg,tlast
real(8),dimension(-1:1):: a,t
integer f,s
real(8) d,dlast,dd

integer k
!integer circus,k,verdict
!real(8) circulation_time,slow_circulation_time

! Initialization
f=0
 v_2body%circus=0
 v_2body%circulation_time=0d0
 v_2body%slow_circulation_time=0d0

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
        v_2body%circus=v_2body%circus+1!                  Increase the number of circulations
        ctime(v_2body%circus)=ttime!    Register this moment
        v_2body%circulation_time=v_2body%circulation_time+(ttime-tlast)
        if(ttime-tlast > circulation_parameter) &! In case of slow circulation
            v_2body%slow_circulation_time=v_2body%slow_circulation_time+(ttime-tlast)
        tlast=ttime
        a(f)=arg; t(f)=ttime!                       Reset the current origin
        dlast=0d0
    end select
endif

enddo

if(v_2body%circus>0) then! Write out to .circ if necessary
    write(113,*) planet_name(pl_id),res_num
    do k=1,v_2body%circus
        write(113,*) ctime(k)
    enddo
    write(113,*)
endif

! Classifier in work
if(v_2body%circus==0) then
    v_2body%verdict=1!                Means pure libration
else
    if (v_2body%slow_circulation_time>libration_parameter) then
        v_2body%verdict=0!            Means transient libration
    else
        v_2body%verdict=-1!           Means circulation
    endif
endif

v_2body%pl_name=planet_name(pl_id)
v_2body%res_num=res_num
v_2body%circulation_ratio=v_2body%circulation_time/(time(reclen)-time(1))
v_2body%slow_circulation_ratio=v_2body%slow_circulation_time/(time(reclen)-time(1))


! Update .rpout list

!write(114,*) verdict, planet_name(pl_id),res_num,circus,circulation_time,&
!    circulation_time/(time(reclen)-time(1)),slow_circulation_time,&
!    slow_circulation_time/(time(reclen)-time(1))! For common file

end function classifier_2body


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


subroutine init_planet_data()
! Check number of records in .aei files
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

subroutine clear_planet_data()
    deallocate(arg_m,arg_l)
end subroutine clear_planet_data

end module librations