module librations_support

use global_parameters, reclen => aei_numrec
implicit none

integer,dimension(9):: planet_stat
real(8),dimension(:,:),allocatable:: arg_l, arg_m
integer,parameter:: ln = ceiling(dlog(dble(reclen))/dlog(2d0))+1
integer,parameter:: n2 = 2 ** ln

contains

!----------------------------------------------------------------------------------------------
pure function mbessel0(t)
    real(8),intent(in):: t
    real(8) r
    integer k,i
    real(8) mbessel0
    k=0
    mbessel0=0
    do
        r=1
        do i=1,k
            r=r*(t/i/2d0)
        enddo
        r=r*r
        if(dabs(r)<1d-13) exit
        mbessel0=mbessel0+r
        k=k+1
    enddo
end function mbessel0

!----------------------------------------------------------------------------------------------
subroutine get_filter(b,x0,m,filter)
    real(8) b,x0
    integer,intent(in):: m
    real(8),dimension(-m:m):: get_f
    complex(8),dimension(0:n2-1):: filter
    integer i
    forall (i=-m:-1) get_f(i) = dsin(x0*pi*2*i)/(pi*i)* &
        mbessel0(b*dsqrt(1d0-(dble(i)/dble(m))**2))
    forall (i=1:m) get_f(i) = dsin(x0*pi*2*i)/(pi*i)* &
        mbessel0(b*dsqrt(1d0-(dble(i)/dble(m))**2))
    get_f(0) = x0*2*mbessel0(b)
    get_f=get_f/sum(get_f)
    filter = (0d0,0d0)
    filter(0:m)=dcmplx(get_f(0:m),0d0)
    filter(n2-m:n2-1)=dcmplx(get_f(-m:-1),0d0)
    call fft(1,filter)
end subroutine get_filter

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
        open(unit=i,file=trim(pwd)//'/aei_planet/'//trim(planet_name(pl_id))//'.aei',&
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
end module librations_support