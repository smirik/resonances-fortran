program resonant_axis_calculator

use global_parameters
use resonant_axis
implicit none
    integer :: i,j,k,s
    integer:: m1,m
    integer, dimension(4) :: resonance
    character(25)::co
    integer:: planet_number

    call get_command_argument(1,co)
    planet_number=planet_id(trim(co))
    open(unit=8,file="id_matrix_"//trim(co)//".dat",status='replace')
    co='(4i4,f23.16)'

    do m1=1,max_order
        do m=-1,-m1-max_order,-1
            if ( nod(abs(m),m1) /= 1) cycle
            resonance=(/ m1,m,0,-m1-m /)
            write(8,co) resonance,&
                count_axis_2body(resonance,a_pl(planet_number),m_pl(planet_number))
        enddo
    enddo
    close(8)
    
contains

integer function nod(a,b)
    integer::a,b,c1,c2,c3,c4
    c1=a;c2=b
    do while(c1 /= c2)
        c3=c1
        c4=c2
        c1=min(c3,c4)
        c2=abs(c3-c4)
    enddo
    nod=c1
end function nod

end
