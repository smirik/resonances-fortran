program main
implicit none

integer:: omp_get_num_procs
integer i,f,flag,nt
character(30):: a1,a2
character(10):: dir
integer s1,s2,n1,n2,m1,m2
call get_command_argument(1,a1)
call get_command_argument(2,a2)
read(a1,*) m1
read(a2,*) m2
! Make clones
!nt=max(omp_get_num_procs()/2, 4)
nt=omp_get_num_procs()
call execute_command_line('mkdir -p ./result_bank ; mkdir -p ./aei_bank',wait=.false.)
do i=1,nt
    write(dir,'(i1)') i
    open(unit=500+i,file='log'//trim(dir)//'.txt',status='replace')
    close(500+i)
    write(*,*) 'Producing clone ',i
    call execute_command_line('mkdir -p ./test'//trim(dir)//&
        '; cp -r ./axis/ ./integrator ./librations ./mercury ./test'//trim(dir)//&
        '; cp ./asteroids.bin ./*.f90 ./*.sh ./*.fun ./Makefile ./test'//trim(dir),&
        wait=.true.)
    write(*,*) 'Clone ',i,' created.'
    call execute_command_line('cd test'//trim(dir)//'/ ; make',wait=.true.,exitstat=f)
    if(f>1) stop ('Error! One clone failed to appear. Please check something.')
enddo

!Launch the program with blocks of 1000 asteroids
do 

    n1=m1;n2=min(m1+999,m2)
    if (n2-n1+1>=100) then
!$OMP parallel
!$OMP do private(i,dir,s1,s2,a1,a2,flag)
    do i=1,nt
        s1=n1+(n2-n1+1)*(i-1)/nt
        s2=n1+(n2-n1+1)*i/nt-1
        write(a1,'(i8)') s1
        write(a2,'(i8)') s2
        write(dir,'(i1)') i

        call execute_command_line('cd test'//trim(dir)//'/ ; ./comp.x -range '//&
            trim(a1)//' '//trim(a2),wait=.true.,exitstat=f)
        flag=f
        open(unit=500+i,file='log'//trim(dir)//'.txt',action='write',position='append')
        if(flag>0) then
            write(500+i,*) '!!!!! Range ',trim(a1),' ',trim(a2),&
                ' has some troubles and needs repeating.'
            call execute_command_line('cd ./test'//trim(dir)//&
                ' ; rm -f ./wd/* ./aeibase/* ./aei_planet/* ',wait=.true.)
        else
            write(500+i,*) 'Range ',trim(a1),' ',trim(a2),' successfully done.'
            call execute_command_line('mv ./test'//trim(dir)//&
                '/wd/current_result_2.txt ./result_bank/result_2_'//trim(adjustl(a1))//'_'//&
                trim(adjustl(a2))//'.txt',wait=.true.)
            call execute_command_line('mv ./test'//trim(dir)//&
                '/wd/current_result_3.txt ./result_bank/result_3_'//trim(adjustl(a1))//'_'//&
                trim(adjustl(a2))//'.txt',wait=.true.)
            call execute_command_line('mv ./test'//trim(dir)//&
                '/aeibase/*.aei ./aei_bank/',wait=.true.)
        endif
        close(500+i)
    enddo
!$OMP end do nowait
!$OMP end parallel
    else
        write(a1,'(i8)') n1
        write(a2,'(i8)') n2
        
        call execute_command_line('cd test1/ ; ./comp.x -range '//&
            trim(a1)//' '//trim(a2),wait=.true.,exitstat=f)
        flag=f
        open(unit=501,file='log1.txt',action='write',position='append')
        if(flag>0) then
            write(501,*) '!!!!! Range ',trim(a1),' ',trim(a2),&
                ' has some troubles and needs repeating.'
           write(*,*) 'Clone ',i,' created'
     call execute_command_line('cd ./test1 '//&
                '; rm -f ./wd/* ./aeibase/* ./aei_planet/* ',wait=.true.)
        else
            write(501,*) 'Range ',trim(a1),' ',trim(a2),' successfully done.'
            call execute_command_line('mv ./test1/wd/current_result_2.txt ./result_bank/result_2_'//&
                trim(adjustl(a1))//'_'//trim(adjustl(a2))//'.txt',wait=.true.)
            call execute_command_line('mv ./test1/wd/current_result_3.txt ./result_bank/result_3_'//&
                trim(adjustl(a1))//'_'//trim(adjustl(a2))//'.txt',wait=.true.)
            call execute_command_line('mv ./test1/aeibase/*.aei ./aei_bank/',wait=.true.)
        endif
        close(501)
    endif

    m1=n2
    if(m1==m2) exit
enddo

do i=1,nt
    write(dir,'(i1)') i
    write(*,*) 'Deleting clone ',i
    call execute_command_line('rm -rf ./test'//trim(dir),wait=.true.)
    write(*,*) 'Clone ',i,' deleted.'
enddo



write(*,*) 'Done'

end