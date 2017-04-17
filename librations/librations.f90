module librations

use global_parameters, reclen => aei_numrec
use librations_support
implicit none

! Work arrays ---------------------------------------------------------------------------------
real(8),dimension(:),allocatable:: time, ctime, ph, axis
integer,dimension(:),allocatable:: res_num

! Used for periodograms -----------------------------------------------------------------------
complex(8),dimension(:),allocatable:: ph_comp,ph_f,ph_f_smooth,ph_comp_smooth
complex(8),dimension(:),allocatable:: axis_f,axis_f_smooth,axis_smooth
real(8),dimension(:),allocatable:: ph_per,axis_per,cross_per,ph_per_smooth,axis_per_smooth
real(8):: axis_disp, ph_disp! Std deviations (estimated)
!----------------------------------------------------------------------------------------------
! Special types for classificator
type classifier_verdict
    integer:: verdict
    integer:: acknowledged
    integer:: circus
    integer:: libration
    real(8):: circulation_time
    real(8):: circulation_ratio
    real(8):: libration_time
    real(8):: libration_ratio
    real(8):: mean_libration_time
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
character(52):: res_name
character(8):: pl_name, pl2_name, script_name
character(1):: cmode
real(8):: max_ph_per, max_axis_per, mean_axis
complex(8):: avg
complex(8),dimension(:),allocatable:: filter1

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

allocate(ph(1:reclen),ph_comp(1:reclen),ph_f(0:n2-1),ph_f_smooth(0:n2-1),&
    ph_per(0:n2-1),ph_per_smooth(0:n2-1),ph_comp_smooth(0:n2-1))
allocate(axis(1:reclen),axis_f(0:n2-1),axis_f_smooth(0:n2-1),&
    axis_per(0:n2-1),axis_per_smooth(0:n2-1),axis_smooth(0:n2-1))
allocate(ctime(1:reclen),time(1:reclen),cross_per(0:n2-1))
allocate(filter1(0:n2-1))

call get_filter(1d1,0.024d0,80,filter1)!

open(unit=1000,file=trim(pwd)//'/wd/current_result_'//cmode//'.txt',status='replace')
astlist%current=>astlist%first
! Run over asteroid list
do i=1,astlist%listlen
    ast_name=astlist%current%item%name
    ast_rpin=trim(ast_name)//'.rp'//cmode
    ast_aei=trim(ast_name)//'.aei'
    write(*,*) "Phase building and classification for ",ast_name,'('//cmode//'-body case)'
    open(unit=111,file=trim(pwd)//'/wd/'//trim(ast_rpin),action='read',iostat=s)
    ! If resonances cannot be read
    if (s /= 0) then
        write(*,*) 'Error! ',ast_rpin,' cannot be opened. This case will be passed.'
        astlist%current=>astlist%current%next
        cycle
    endif
    ! Open .aei for asteroid
    open(unit=110,file=trim(pwd)//'/aeibase/'//trim(ast_aei),action='read',iostat=s)
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
    
    ! Centering axis oscillations
    mean_axis=sum(axis(:)/reclen)
    axis=axis-mean_axis
    axis_f(0:reclen-1)=dcmplx(axis(1:reclen),0d0)
    axis_f(reclen:n2-1)=(0d0,0d0)
    call fft(1,axis_f)
    axis_per(:)=(cdabs(axis_f(:))/reclen)**2
    max_axis_per=maxval(axis_per)
    ! Make smoothed axis oscillations
    axis_f_smooth(:)=axis_f(:)*filter1(:)
    axis_per_smooth(:)=(cdabs(axis_f_smooth(:))/reclen)**2
    axis_smooth(:)=axis_f_smooth(:)
    call fft(-1,axis_smooth)
    axis_disp= sum((axis(1:reclen)-dreal(axis_smooth(0:reclen-1)))**2)/dble(reclen-1)
    write(axis_treshold,'(f30.16)') axis_disp*13.11d0/reclen!

    open(unit=114,file=trim(pwd)//'/wd/'//trim(ast_name)//'.rpout'//cmode,status='replace')
    if(allow_writing_metadata) then
        open(unit=112,file=trim(pwd)//'/wd/'//trim(ast_name)//'.phout'//cmode,status='replace')
        close(112)
        open(unit=115,file=trim(pwd)//'/wd/'//trim(ast_name)//'.smooth'//cmode,status='replace')
        close(115)
        open(unit=116,file=trim(pwd)//'/wd/'//trim(ast_name)//'.per'//cmode,status='replace')
        close(116)
        open(unit=117,file=trim(pwd)//'/wd/'//trim(ast_name)//'.circ'//cmode,status='replace')
        close(117)
    endif
    ! Run over resonances list for asteroid ---------------------------------------------------
    cac=0
    runover: do
        if(allow_writing_metadata) then
            open(unit=112,file=trim(pwd)//'/wd/'//trim(ast_name)//'.phout'//cmode,&
                action='write',position='append')
            open(unit=115,file=trim(pwd)//'/wd/'//trim(ast_name)//'.smooth'//cmode,&
                action='write',position='append')
            open(unit=116,file=trim(pwd)//'/wd/'//trim(ast_name)//'.per'//cmode,&
                action='write',position='append')
            open(unit=117,file=trim(pwd)//'/wd/'//trim(ast_name)//'.circ'//cmode,&
                action='write',position='append')
        endif
        if(mode==3) then
            read(111,'(a8,3x,a8,3x,6i4)',iostat=s) pl_name,pl2_name,res_num
        else
            read(111,'(a8,3x,4i4)',iostat=s) pl_name,res_num
        endif
        if(s/=0) exit runover
        pl_id=planet_id(pl_name)
        if (planet_stat(pl_id)>0) then
            write(*,*) 'Planet ',pl_name,' is inaccessible and will be passed.'
            cycle runover
        endif
        if(mode==3) then
            pl2_id=planet_id(pl2_name)
            if (planet_stat(pl2_id)>0) then
                write(*,*) 'Second planet ',pl2_name,' is inaccessible and will be passed.'
                cycle runover
            endif
        endif

        ! Calculating phase
        if(mode==3) then
            call resph(res_num,pl_id,pl2_id)
        else
            call resph(res_num,pl_id)
        endif
        ph_comp(:)=dcmplx(cos(ph(:)),sin(ph(:)))
        ph_f(0:reclen-1)=ph_comp(1:reclen)
        ph_f(reclen:n2-1)=(0d0,0d0)
        call fft(1,ph_f)
        ph_per(:)=(cdabs(ph_f(:))/reclen)**2
        max_ph_per=maxval(ph_per)
        ! Make smoothed phase
        ph_f_smooth(:)=ph_f(:)*filter1(:)
        ph_per_smooth(:)=(cdabs(ph_f_smooth(:))/reclen)**2
        ph_comp_smooth(:)=ph_f_smooth(:)
        call fft(-1,ph_comp_smooth)
        ph_disp= sum(cdabs(ph_comp(1:reclen)-ph_comp_smooth(0:reclen-1))**2)/dble(reclen-1)
        ! Cross-periodogram
        cross_per(:)=(cdabs(ph_f(:))/reclen)*(cdabs(axis_f(:))/reclen)

        !Make classification
        if(mode==3) then
            v3%pl_name=pl_name
            v3%pl2_name=pl2_name
            v3%res_num=res_num
            v3%verdict=new_classifier(res_num,pl_id,pl2_id)
            write(114,*) v3%verdict%verdict,v3%verdict%acknowledged,&
                v3%pl_name,v3%pl2_name,v3%res_num
            if(v3%verdict%verdict>=0) &
                write(1000,*) ast_name,' ',v3%verdict%verdict,v3%verdict%acknowledged,&
                v3%pl_name,v3%pl2_name,v3%res_num
        else
            v2%pl_name=pl_name
            v2%res_num=res_num
            v2%verdict=new_classifier(res_num,pl_id)
            write(114,*) v2%verdict%verdict,v2%verdict%acknowledged,&
                v2%pl_name,v2%res_num
            if(v2%verdict%verdict>=0) &
                write(1000,*) ast_name,' ',v2%verdict%verdict,v2%verdict%acknowledged,&
                v2%pl_name,v2%res_num
        endif
        if(allow_writing_metadata) then
            ! Current data rows
            if(mode==3) then
                write(112,*) 'RESONANCE ',pl_name,' ',pl2_name,res_num
            else
                write(112,*) 'RESONANCE ',pl_name,res_num
            endif
            do j=1,reclen
                write(115,*) time(j),dreal(ph_comp(j)),aimag(ph_comp(j)),&!         1,2,3
                    dreal(ph_comp_smooth(j-1))/cdabs(ph_comp_smooth(j-1)),&!        4
                    aimag(ph_comp_smooth(j-1))/cdabs(ph_comp_smooth(j-1)),&!        5
                    axis(j)+mean_axis,dreal(axis_smooth(j-1))+mean_axis,ctime(j)!   6,7,8
                write(112,*) time(j),ph(j),&!                                       1,2
                    datan2(aimag(ph_comp_smooth(j-1)),dreal(ph_comp_smooth(j-1)))!  3
            enddo
            write(115,*)
            write(115,*)
            write(112,*)
            write(112,*)
            do j=0,n2/2-1
                write(116,*) 1d0/dble(n2)/1d1*j,&!      1
                    ph_per(j),&!                        2
                    ph_per_smooth(j),&!                 3
                    axis_per(j),&!                      4
                    axis_per_smooth(j),&!               5
                    ph_per(j)/max_ph_per,&!             6
                    axis_per(j)/max_axis_per,&!         7
                    cross_per(j)!                       8
            enddo
            write(116,*)
            write(116,*)        
            close(112)
            close(115)
            close(116)
            close(117)
        endif
        if (allow_plotting) then
            if((.not. plot_all .and. ((mode==3 .and. v3%verdict%verdict>=0) .or. &
                (mode==2 .and. v2%verdict%verdict>=0))) .or. plot_all) then
                write(*,*) 'Got some interesting...'
                write(res_index,'(i25)') cac
                if(mode==3) then
                    write(res_name,'(2i2,2a9,6i5)') v3%verdict%verdict,&
                        v3%verdict%acknowledged,pl_name,pl2_name,res_num
                else
                    write(res_name,'(2i2,a9,4i5)') v2%verdict%verdict,&
                        v2%verdict%acknowledged,pl_name,res_num
                endif
                write(ph_treshold,'(f30.16)') ph_disp*13.11d0/reclen
                write(cross_treshold,'(f30.16)') dsqrt(axis_disp*ph_disp)/reclen*55.0d0
                write(*,*) 'Plotting '//script_name//':'//res_index//&
                    ' diagram for '//trim(ast_name)
                call execute_command_line('cd '//trim(pwd)//&
                    '/wd; gnuplot -e "name='//trim(ast_name)//&
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

    ! Make cleaning after current asteroid
    close(114)
    close(111)
    if(dispose_metadata) &
        call execute_command_line('cd '//trim(pwd)//'/wd; rm -f '//&
            trim(ast_name)//'.circ'//cmode//' '//&
            trim(ast_name)//'.per'//cmode//' '//&
            trim(ast_name)//'.rp'//cmode//' '//&
            trim(ast_name)//'.phout'//cmode//' '//&
            trim(ast_name)//'.smooth'//cmode, wait=.false.)
    astlist%current=>astlist%current%next
enddo

close(1000)
deallocate(ph,ph_comp,ph_f,ph_f_smooth,ph_comp_smooth)
deallocate(axis,axis_f,axis_f_smooth,axis_smooth)
deallocate(axis_per,axis_per_smooth,ph_per,ph_per_smooth)
deallocate(ctime,time,cross_per)
deallocate(filter1)
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
!   (IMPLICIT) - array of phase angles and it's periodogram
!                and smoothed phase and it's periodogram
! Returns:
!   v - special type object, including:
!       verdict
!       status of acknowledged resonance
!       number of all detected circulations
!       total time of circulations and it's ratio
!       total time of transient librations and it's ratio
!       mean libration time (currently not used)
    integer,dimension(:):: res_num
    integer:: pl_id
    integer, optional:: pl2_id
    logical:: case_3body
    real(8):: ttime,tlast
    real(8):: d,ds,dd,dds,r2,sum_r2,r2_t
    complex(8) z1,z2,z1s,z2s
    integer k,klast,i

    case_3body=present(pl2_id)
    ! Initialization
    v%circus=0
    v%libration=0
    v%circulation_time=0d0
    v%libration_time=0d0
    klast=1
    tlast=time(1)
    z1=ph_comp(1); z1s=ph_comp_smooth(1)
    dd=0d0; dds=0
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

! Run over phase
do k=2,reclen
    ttime=time(k)
    z2=ph_comp(k); z2s=ph_comp_smooth(k)
    d=datan2(dreal(z1)*aimag(z2)-aimag(z1)*dreal(z2),dreal(z1)*dreal(z2)+aimag(z1)*aimag(z2))
    ds=datan2(dreal(z1s)*aimag(z2s)-aimag(z1s)*dreal(z2s),dreal(z1s)*dreal(z2s)+aimag(z1s)*aimag(z2s))
    dd=dd+d; dds=dds+ds
    ctime(k)=dds
    if (dabs(dd)<twopi) then
        z1=ph_comp(k); z1s=ph_comp_smooth(k)
    else
        ctime(klast:k)=ctime(klast:k)-dds/(k-klast)*(/ (i-1, i=1,k-klast+1) /)
        if(ttime-tlast > circulation_parameter) then! In case of slow circulation (or not?)
            r2=dsqrt( sum(ctime(klast:k)**2)/(k-klast+1) )
            r2=r2*dlog(ttime-tlast)/dlog(circulation_parameter)
            if (r2>=r2_t .or. ttime-tlast>30d3) then ! If deviation from straight line is big
                v%libration=v%libration+1
                v%libration_time=v%libration_time+(ttime-tlast)
                sum_r2 = sum_r2+r2
            else
                v%circus=v%circus+1
                v%circulation_time=v%circulation_time+(ttime-tlast)
            endif
        else
            v%circus=v%circus+1
            v%circulation_time=v%circulation_time+(ttime-tlast)
            r2=0d0
        endif
        if(allow_writing_metadata) write(117,*) k, ttime,ttime-tlast,r2,r2
        tlast=ttime
        dd=0d0; dds=0d0
        z1=ph_comp(k); z1s=ph_comp_smooth(k)
        klast=k
        ctime(k)=0d0
    endif
enddo

    ! Processing the "tail" (maybe here is libration too)
    if (klast > 1 .and. klast<reclen .and. &
        ( ttime-tlast>circulation_parameter .or. dabs(dd)>pi/4d0 ) ) then
        ctime(klast:reclen)=ctime(klast:reclen)- &
            dds/(reclen-klast)*(/ (i-1, i=1,reclen-klast+1) /)
        if(ttime-tlast > circulation_parameter) then
            r2=dsqrt( sum(ctime(klast:reclen)**2)/(reclen-klast+1) )
            r2=r2*dlog(ttime-tlast)/dlog(circulation_parameter)
            if (r2>=r2_t .or. ttime-tlast>30d3) then ! If deviation from straight line is big
                v%libration=v%libration+1
                v%libration_time=v%libration_time+(ttime-tlast)
                sum_r2 = sum_r2+r2
            endif
        else
            v%circus=v%circus+1
            v%circulation_time=v%circulation_time+(ttime-tlast)
            r2=0d0
        endif
        if(allow_writing_metadata) write(117,*) k, ttime,ttime-tlast,r2,r2
    endif
    v%mean_libration_time=v%libration_time/max(v%libration,1)
    if(allow_writing_metadata) then
        write(117,*)
        write(117,*)
    endif

    ! Classifier's verdict
    if(v%circus==0 .and. v%libration<=1) then
        v%verdict=1!                Means pure libration
    else
        if ( sum_r2 >= sum_r2_treshold .or. &
        (v%libration_time>libration_parameter )) then
            v%verdict=0!            Means transient libration
        else
            v%verdict=-1!           Means circulation
        endif
    endif
    ! Status of acknowledged (by using cross-periogram and Shooster treshold)
    if(maxval(cross_per(0:n2/5-1))>=dsqrt(axis_disp*ph_disp)/reclen*55.0d0) then
        v%acknowledged = 1
    else
        v%acknowledged = 0
    endif
    v%circulation_ratio=v%circulation_time/(time(reclen)-time(1))
    v%libration_ratio=v%libration_time/(time(reclen)-time(1))
end function new_classifier

!----------------------------------------------------------------------------------------------
end module librations