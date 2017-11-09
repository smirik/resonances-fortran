module librations

use global_parameters, reclen => aei_numrec
use librations_support
use resonance_finder
use simp_res_str
implicit none

! Work arrays ---------------------------------------------------------------------------------
real(8),dimension(:),allocatable:: time, ctime, ph, axis, excentr, incl, argp
integer,dimension(:),allocatable:: res_num
! Used for periodograms -----------------------------------------------------------------------
complex(8),dimension(:),allocatable:: ph_comp,ph_f,ph_f_smooth,ph_comp_smooth
complex(8),dimension(:),allocatable:: axis_f,axis_f_smooth,axis_smooth
real(8),dimension(:),allocatable:: ph_per,axis_per,cross_per,ph_per_smooth,axis_per_smooth
! Std deviations (estimated) ------------------------------------------------------------------
real(8):: axis_disp, axis_smooth_disp, ph_disp
!----------------------------------------------------------------------------------------------
! Special type for classificator
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
    real(8):: mean_circulation_time
    real(8):: mode_time
end type classifier_verdict

!----------------------------------------------------------------------------------------------
contains

!----------------------------------------------------------------------------------------------
subroutine global_libration_processing(mode, astlist)
! Performs global phase building and resonance classification upon given asteroids
! Given:
!   mode - must be 2 or 3 (according to 2- or 3-body case)
!   astlist - special type list of asteroids with names, semimajor axes etc.
! Produces:
!   file with sets of classified resonances for each asteroid
!   (some keys in global parameters allow also create metadata and draw graphics)

type(orb_elem_list):: astlist
integer:: s, pl_id, pl2_id, i, j, mode, problem_status
character(30):: ast_rpin,ast_aei,ast_name,res_index,axis_treshold,cross_treshold,ph_treshold
character(52):: res_name
character(8):: pl_name, pl2_name, script_name
character(1):: cmode
real(8):: max_ph_per, max_axis_per, mean_axis, mean_axis_smooth
complex(8):: avg
complex(8),dimension(:),allocatable:: filter1

integer:: a_count,rescounter,resmax,leader
real(8):: strength, max_strength
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
    ph_comp_smooth(0:n2-1))
allocate(axis(1:reclen),axis_f(0:n2-1),axis_f_smooth(0:n2-1),&
    axis_smooth(0:n2-1))
allocate(excentr(1:reclen),incl(1:reclen),argp(1:reclen))
allocate(ctime(1:reclen),time(1:reclen))
allocate(filter1(0:n2-1))

call get_filter(1d1,0.024d0,80,filter1)!

open(unit=1000,file=trim(pwd)//trim(results_pwd)// &
    'result_'//cmode//'body.txt',status='replace')

astlist%current=>astlist%first
! Run over asteroid list
do i=1,astlist%listlen
    problem_status = 0
    ast_name=astlist%current%item%name
    ast_rpin=trim(ast_name)//'.rp'//cmode
    ast_aei=trim(ast_name)//'.aei'

    write(*,*) "Phase building and classification for ",ast_name,'('//cmode//'-body case)'
    if (mode==3) then
        if ( allocated(astlist%current%a_list) ) then
            ! TODO - need to check all possible a-clusters, not only the first
            if (allocated(astlist%current%a_list(1)%item2)) then
                if( any(astlist%current%a_list(1)%item2 >= 0) ) then
                    write(*,*) 'Error! ',ast_name,' has already in 2-body resonance. ',&
                        'This case will be passed.'
                    astlist%current=>astlist%current%next
                    cycle
                endif
            endif
        endif
    endif
    write(*,*) '...'
    ! Open .aei for asteroid
    open(unit = 110, file = trim(pwd) // trim(aeibase_pwd) // trim(ast_aei), action = 'read', iostat = s)
    ! If .aei cannot be opened
    if (s /= 0) then
        write(*,*) 'Error! ',ast_aei,' cannot be opened. This asteroid will be passed.'
        astlist%current=>astlist%current%next
        cycle
    endif
    ! If .aei has another number of records, it will still work
    if (count_aei_file_records(110) /= reclen) then
        write(*,*) 'Warning! Unexpectedly different file length of ',ast_aei
        problem_status = 10*problem_status+2
    endif
    ! Pass header
    do j=1,aei_header
        read(110,'(a)')
    enddo
    ! Load information
    do j=1,reclen
        read(110,*,iostat=s) time(j),arg_l(10,j),arg_m(10,j),axis(j),excentr(j),incl(j),argp(j)
    enddo
    close(110)

    ! Make smoothed axis oscillations
    mean_axis=sum(axis(:)/reclen)
    axis=axis-mean_axis
    axis_f(0:reclen-1)=dcmplx(axis(1:reclen),0d0)
    axis_f(reclen:n2-1)=(0d0,0d0)
    call fft(1,axis_f)
    axis_f_smooth(:)=axis_f(:)*filter1(:)
    axis_smooth(:)=axis_f_smooth(:)
    call fft(-1,axis_smooth)
    ! Get pseudo-synthetic proper semiaxis
    mean_axis_smooth = sum(dreal(axis_smooth(0:reclen-1))/reclen)
    axis_smooth_disp = sum((dreal(axis_smooth(0:reclen-1))-mean_axis_smooth)**2)/dble(reclen-1)
    if (axis_smooth_disp/sum(axis(1:reclen)**2*dble(reclen-1)) > 1d-2) then
        problem_status = problem_status*10+1
        write(*,*) 'Warning! Asteroid ',ast_name,' semiaxis has too large perturbations'
    endif
    if (axis_smooth_disp/(mean_axis_smooth+mean_axis) > 1d-1) then
        problem_status = problem_status*10+3
        write(*,*) 'Warning! Asteroid ',ast_name,' semiaxis is not stable and proper value might be bad'
        write(*,*)  mean_axis_smooth+mean_axis, axis_smooth_disp
    endif
    if (problem_status <= 1) then
        mean_axis_smooth = mean_axis_smooth+mean_axis
    else
        mean_axis_smooth = sum(axis(1:reclen/10)/(reclen/10)) + mean_axis
    endif

! FUTURE MODIFICATION------------------------------------------------------------
! In future versions we should not check librations by the way "circ by circ"
! but using the local clusters characterized by pseudo-stable semiaxis behavior...
!
!    call get_a_clusters(time,dreal(axis_smooth),astlist%current,maxval(axis),minval(axis))
    if ( .not. allocated(astlist%current%a_list) ) then
        allocate(astlist%current%a_list(1:1))
        astlist%current%a_list(1)%a = mean_axis_smooth
        astlist%current%a_list(1)%min_a = mean_axis_smooth
        astlist%current%a_list(1)%max_a = mean_axis_smooth
        astlist%current%a_list(1)%e = sum(excentr(:))/reclen
        astlist%current%a_list(1)%incl = sum(incl(:))/reclen*deg2rad
        astlist%current%a_list(1)%argp = sum(argp(:))/reclen*deg2rad
        astlist%current%a_list(1)%start_i = 1000
        astlist%current%a_list(1)%end_i = 9001
    endif
!--------------------------------------------------------------------------------
    ! Finding resonances
    write(*,*) "Finding resonances for ",ast_name,'('//cmode//'-body case), proper axis is: ',&
        mean_axis_smooth
    if (mean_axis_smooth<=1d1) then
        delta=1d-2
    else
        delta=1d-1
    endif
    call get_all_possible_resonances(mode,astlist%current,delta)
    if (mode==3) then
        if (.not. allocated(astlist%current%r3list)) then
            write(*,*) 'Error! ',ast_name,' has not founded resonances of this type. ',&
                'This case will be passed.'
            astlist%current=>astlist%current%next
            cycle
        endif
        resmax= size(astlist%current%r3list)
    else
        if (.not. allocated(astlist%current%r2list)) then
            write(*,*) 'Error! ',ast_name,' has not founded resonances of this type. ',&
                'This case will be passed.'
            astlist%current=>astlist%current%next
            cycle
        endif
        resmax= size(astlist%current%r2list)
    endif

    ! Run over resonances list for asteroid ---------------------------------------------------
    runover: do rescounter = 1, resmax
        if(mode==3) then
            pl_name = astlist%current%r3list(rescounter)%pl_name
            pl2_name = astlist%current%r3list(rescounter)%pl2_name
            res_num = astlist%current%r3list(rescounter)%res_num
        else
            pl_name = astlist%current%r2list(rescounter)%pl_name
            res_num = astlist%current%r2list(rescounter)%res_num
        endif
        pl_id=planet_id(pl_name)
        if (planet_stat(pl_id)>0) then
            write(*,*) 'Planet ',pl_name,' is inaccessible and the case will be passed.'
            cycle runover
        endif
        if(mode==3) then
            pl2_id=planet_id(pl2_name)
            if (planet_stat(pl2_id)>0) then
                write(*,*) 'Second planet ',pl2_name,' is inaccessible and the case will be passed.'
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
        ! Make smoothed phase
        ph_f_smooth(:)=ph_f(:)*filter1(:)
        ph_comp_smooth(:)=ph_f_smooth(:)
        call fft(-1,ph_comp_smooth)

        !Make classification (there were other instructions so I left this IF-block)
        if(mode==3) then
            call classifier(mode, astlist%current, rescounter)
        else
            call classifier(mode, astlist%current, rescounter)
        endif
    enddo runover

! Now we will leave only strong resonances (in case of 3-body resonances)
    if (mode==3) then
        ! Run over a-clusters
        do a_count = 1,size(astlist%current%a_list)
            leader=0
            max_strength = 0d0
            ! Run over identified resonances for a-cluster
            do rescounter = 1, resmax
                if (astlist%current%a_list(a_count)%item3(rescounter)>=0) then
                    strength = res_str(&
                        astlist%current%a_list(a_count)%a,&
                        planet_id(astlist%current%r3list(rescounter)%pl_name),&
                        planet_id(astlist%current%r3list(rescounter)%pl2_name),&
                        astlist%current%r3list(rescounter)%res_num,&
                        astlist%current%a_list(a_count)%argp,&
                        astlist%current%a_list(a_count)%incl,&
                        astlist%current%a_list(a_count)%e)
                    if (strength > max_strength) then
                        if (leader>0) then
                            astlist%current%r3list(leader)%clu(a_count) = -1
                            astlist%current%a_list(a_count)%item3(leader) = -1
                        endif
                        leader = rescounter
                        max_strength = strength
                    else
                        astlist%current%r3list(rescounter)%clu(a_count) = -1
                        astlist%current%a_list(a_count)%item3(rescounter) = -1
                    endif
                endif
            enddo
            astlist%current%a_list(a_count)%res3leader = leader
        enddo
        ! TODO - problem with several "resmax" for sevearl a-clusters?
        do rescounter = 1, resmax
            run3: do a_count = 1,size(astlist%current%a_list)
            if (astlist%current%r3list(rescounter)%clu(a_count) >=0) then
                write(1000,*) ast_name,' ',astlist%current%a_list(a_count)%a,&
                    problem_status,&
                    astlist%current%r3list(rescounter)%clu(a_count),&
                    astlist%current%r3list(rescounter)%pl_name,&
                    astlist%current%r3list(rescounter)%pl2_name,&
                    astlist%current%r3list(rescounter)%res_num
                exit run3
            endif
            enddo run3
        enddo
    else
        do rescounter = 1, resmax
            do a_count = 1,size(astlist%current%a_list)
            if (astlist%current%r2list(rescounter)%clu(a_count) >=0) then
                write(1000,*) ast_name,' ',astlist%current%a_list(a_count)%a,&
                    problem_status,&
                    astlist%current%r2list(rescounter)%clu(a_count),&
                    astlist%current%r2list(rescounter)%pl_name,&
                    astlist%current%r2list(rescounter)%res_num
            endif
            enddo
        enddo
    endif

    astlist%current=>astlist%current%next
enddo

close(1000)
deallocate(ph,ph_comp,ph_f,ph_f_smooth,ph_comp_smooth)
deallocate(axis,axis_f,axis_f_smooth,axis_smooth)
deallocate(ctime,time)
deallocate(filter1)
deallocate(res_num)
deallocate(excentr,incl,argp)
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
                res_num(6)*arg_l(10,j))*deg2rad)
    else
        forall(j=1:reclen) &
            ph(j)=norm_ang((res_num(1)*(arg_m(pl_id,j)+arg_l(pl_id,j))+&
                res_num(2)*(arg_m(10,j)+arg_l(10,j))+&
                res_num(4)*arg_l(10,j))*deg2rad)
    endif
end subroutine resph

!----------------------------------------------------------------------------------------------
subroutine classifier(mode, asteroid, rescounter)

! Current version of libration classifier
! Given:
!   mode - 2 or 3
!   asteroid - asteroid lis element
!   rescounter - position index of potential resonance in used mode
! Produces:
!   Upadting status of the resonance by a classification verdict
    type(orb_elem_leaf):: asteroid
    integer:: mode, rescounter, a_count
    type(classifier_verdict):: v
    integer:: pl_id,pl2_id
    logical:: case_3body
    real(8):: ttime,tlast,mode_time
    real(8):: d,ds,dd,dds,r2,sum_r2,r2_t
    complex(8) z1,z2,z1s,z2s
    integer k,klast,i,mode_n,start_i,end_i

    case_3body = (mode == 3)
do a_count = 1,size(asteroid%a_list)
    if (case_3body) then
        if (asteroid%r3list(rescounter)%clu(a_count) < 1) cycle
        pl_id = planet_id(asteroid%r3list(rescounter)%pl_name)
        pl2_id = planet_id(asteroid%r3list(rescounter)%pl2_name)
    else
        if (asteroid%r2list(rescounter)%clu(a_count) < 1) cycle
        pl_id = planet_id(asteroid%r2list(rescounter)%pl_name)
    endif
    start_i = asteroid%a_list(a_count)%start_i
    end_i = asteroid%a_list(a_count)%end_i
    ! Initialization
    v%circus=0
    v%mean_circulation_time = 1d5
    v%libration=0
    v%circulation_time=0d0
    v%libration_time=0d0
    klast = start_i
    tlast = time(klast)
    z1 = ph_comp(klast); z1s = ph_comp_smooth(klast)
    dd=0d0; dds=0
    sum_r2=0d0
    ctime(klast)=0d0

    if(case_3body) then
        r2_t = r2_treshold_3body
    else
        r2_t = r2_treshold_2body
    endif

    mode_time=max(asteroid%a_list(a_count)%a,a_pl(pl_id))
    if(case_3body) mode_time=max(mode_time,a_pl(pl2_id))
    mode_time=n_from_a(mode_time)/twopi*365.25d0/10d0

    ! Run over phase ------------------------------------------------------------------------------
    do k = start_i,end_i
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
                v%acknowledged = check_corr(ph_comp(klast:k),ph_comp_smooth(klast:k), &
                    dcmplx(axis(klast:k),0d0),axis_smooth(klast:k),mode_time)
                if (v%acknowledged == 1) then
                    v%libration=v%libration+1
                    v%libration_time=v%libration_time+(ttime-tlast)
                    sum_r2 = sum_r2+r2
                else
                    v%circus=v%circus+1
                endif
            else
                v%circus=v%circus+1
                v%mean_circulation_time = v%mean_circulation_time*(v%circus-1)/v%circus + &
                    (ttime-tlast)/v%circus
                v%circulation_time=v%circulation_time+(ttime-tlast)
            endif
        else
            v%circus=v%circus+1
            v%mean_circulation_time = v%mean_circulation_time*(v%circus-1)/v%circus + &
                (ttime-tlast)/v%circus
            v%circulation_time=v%circulation_time+(ttime-tlast)
            r2=0d0
        endif
        tlast=ttime
        dd=0d0; dds=0d0
        z1=ph_comp(k); z1s=ph_comp_smooth(k)
        klast=k
        ctime(k)=0d0
    endif
    enddo
!----------------------------------------------------------------------------------------------
    ! Processing the "tail" (maybe here is libration too)
    if (klast<end_i .and. ttime-tlast>circulation_parameter) then
        ctime(klast:end_i)=ctime(klast:end_i)- sign(twopi,dds)*(ttime-tlast)/v%mean_circulation_time/ &
            (end_i-klast)*(/ (i-1, i=1,end_i-klast+1) /)
        if(ttime-tlast > circulation_parameter) then
            r2=dsqrt( sum(ctime(klast:end_i)**2)/(end_i-klast+1) )
            r2=r2*dlog(ttime-tlast)/dlog(circulation_parameter)
            if (r2>=r2_t .or. ttime-tlast>30d3) then ! If deviation from straight line is big
                v%acknowledged = check_corr(ph_comp(klast:end_i),ph_comp_smooth(klast:end_i), &
                    dcmplx(axis(klast:end_i),0d0),axis_smooth(klast:end_i),mode_time)
                if (v%acknowledged == 1) then
                    v%libration=v%libration+1
                    v%libration_time=v%libration_time+(ttime-tlast)
                    sum_r2 = sum_r2+r2
                else
                    v%circus = v%circus + 1
                endif
            endif
        endif
    endif
    v%mean_libration_time=v%libration_time/max(v%libration,1)

    ! Classifier's verdict
    if(v%circus==0 .and. v%libration==1) then
        v%verdict=1!                Means pure libration
    else
        if ( sum_r2 >= sum_r2_treshold .or. &
        (v%libration_time>libration_parameter )) then
            v%verdict=0!            Means transient libration
        else
            v%verdict=-1!           Means circulation
        endif
    endif

    if(case_3body) then
        asteroid%r3list(rescounter)%clu(a_count) = v%verdict
        asteroid%a_list(a_count)%item3(rescounter) = v%verdict
    else
        asteroid%r2list(rescounter)%clu(a_count) = v%verdict
        asteroid%a_list(a_count)%item2(rescounter) = v%verdict
    endif
enddo
end subroutine classifier

!----------------------------------------------------------------------------------------------
end module librations

