module simp_res_str

use global_parameters
implicit none

real(8), parameter:: onet = 1d0/3d0, twot = 2d0/3d0

contains

real(8) function res_str(a0,pl1_id,pl2_id,res,arg,inc0,ex0)
! Calculating resonance strength of a 3-body resonance
! See description at (Gallardo, 2014)
! Given:
!   a0 - must be 2 or 3 (according to 2- or 3-body case)
!   pl1_id - first planet ID
!   pl2_id - second planet ID
!   res - integer array of six resonance numbers
!   arg - argument of pericenter for asteroid
!   inc0 - orbit inclination for asteroid
!   ex0 - orbit excentricity for asteroid
! Returns:
!   real value of resonance strength in units of "Veritas 5J-2S-2"

    ! NUMBER OF EVALUATIONS OF RHO(SIGMA) BETWEEN O AND 360 (FOR MORE PRECISTION USE MORE POINTS)
    ! STEPS OF THE NUMERICAL INTEGRATION (FOR MORE PRECITION USE MORE POINTS)
    integer,parameter:: jmax=36, n=180

    real(8),dimension(1:jmax):: rp
    integer,dimension(1:6):: res
    real(8) a0,arg,inc0,ex0
    integer pl1_id,pl2_id
    real(8) a1,a2
    integer iter
    integer j,j0,j1,j2
    real(8) rtot,teta,dtsq2,directa,indirecta
    real(8) aex0
    real(8) dam0,m0,l0,p0,lam0,fi0,r0,x0,y0,z0,lam00
    real(8) dam1,m1,l1,p1,lam1,fi1,r1,x1,y1,z1
    real(8) dam2,m2,l2,p2,lam2,fi2,r2,x2,y2,z2
    real(8) d01,d02,d12,r1r0,r2r0
    real(8) dr01dx0,dr01dy0,dr01dz0,dr01dx1,dr01dy1
    real(8) dr02dx0,dr02dy0,dr02dz0,dr02dx2,dr02dy2
    real(8) dr21dx2,dr21dy2,dr12dx1,dr12dy1

    a1 = a_pl(pl1_id)
    a2 = a_pl(pl2_id)
    ! Calculate integrals by several cases of Sigma
    do j = 1,jmax
        teta = dble(j-1)/dble(jmax)*360d0*deg2rad
        rtot = 0d0
        dam1 = twopi/dble(n)
        do j1 = 0,n-1
            m1 = dam1*j1
            l1 = 0d0; p1 = 0d0
            lam1 = m1+p1
            fi1 = m1
            r1 = a1
            x1 = r1*dcos(p1+fi1); y1 = r1*dsin(p1+fi1); z1 = 0d0
            dam2 = twopi/dble(n)
            do j2 = 0,n-1
                m2 = dam2*j2
                l2 = 0d0; p2 = 0d0
                lam2 = m2+p2
                fi2 = m2
                r2 = a2
                x2 = r2*dcos(p2+fi2); y2 = r2*dsin(p2+fi2); z2 = 0d0
                d12 = dsqrt((x2-x1)**2+(y2-y1)**2)
                dr21dx2 = (x1-x2)/d12**3 - x1/r1**3
                dr21dy2 = (y1-y2)/d12**3 - y1/r1**3
                dr12dx1 = (x2-x1)/d12**3 - x2/r2**3
                dr12dy1 = (y2-y1)/d12**3 - y2/r2**3
                lam00 = (teta - res(6)*arg - res(1)*lam1 - res(2)*lam2) / res(3)
                lam00 = norm_ang(lam00)
                if (lam00 < 0d0) lam00 = lam00 + twopi
                do j0=0,abs(res(3))-1
                    lam0 = lam00 + twopi/abs(res(3))*j0
                    l0 = 0d0; p0 = arg
                    m0 = norm_ang(lam0 - p0)
                    if(m0 < 0d0) m0=m0+twopi
                    call solkep(ex0,m0,aex0,iter)
                    ! true anomaly
                    ! TODO: update the expression below?
                    fi0 = dacos((dcos(aex0)-ex0)/(1d0-ex0*dcos(aex0)))
                    if(m0 > pi) fi0 = twopi - fi0
                    r0 = a0*(1d0-ex0*dcos(aex0))
                    x0 = r0*(dcos(l0)*dcos(p0-l0+fi0)-dsin(l0)*dsin(p0-l0+fi0)*dcos(inc0))
                    y0 = r0*(dsin(l0)*dcos(p0-l0+fi0)+dcos(l0)*dsin(p0-l0+fi0)*dcos(inc0))
                    z0 = r0*dsin(p0-l0+fi0)*dsin(inc0)
                    d01 = dsqrt((x1-x0)**2+(y1-y0)**2+(z1-z0)**2)
                    d02 = dsqrt((x2-x0)**2+(y2-y0)**2+(z2-z0)**2)
                    r1r0 = x1*x0+y1*y0; r2r0 = x2*x0+y2*y0
                    dr01dx1 = (x0-x1)/d01**3 - x0/r1**3 + 3d0*x1*r1r0/r1**5
                    dr01dy1 = (y0-y1)/d01**3 - y0/r1**3 + 3d0*y1*r1r0/r1**5
                    dr01dx0 = (x1-x0)/d01**3 - x1/r1**3
                    dr01dy0 = (y1-y0)/d01**3 - y1/r1**3
                    dr01dz0 = (z1-z0)/d01**3
                    dr02dx2 = (x0-x2)/d02**3 - x0/r2**3 + 3d0*x2*r2r0/r2**5
                    dr02dy2 = (y0-y2)/d02**3 - y0/r2**3 + 3d0*y2*r2r0/r2**5
                    dr02dx0 = (x2-x0)/d02**3 - x2/r2**3
                    dr02dy0 = (y2-y0)/d02**3 - y2/r2**3
                    dr02dz0 = (z2-z0)/d02**3

                    directa = 2d0*(dr02dx0*dr01dx0 + dr02dy0*dr01dy0 + dr02dz0*dr01dz0)
                    indirecta = dr02dx2*dr21dx2 + dr02dy2*dr21dy2 + &
                        dr01dx1*dr12dx1 + dr01dy1*dr12dy1
                    rtot = rtot + indirecta + directa
                enddo
            enddo
        enddo
        rtot = m_pl(pl1_id)*m_pl(pl2_id)*rtot/abs(res(3))/dble(n)**2
        dtsq2 = 0.5d0*a0*a1*a2*(2d0*pi/gp_k)**2
        rp(j)=rtot*dtsq2
    enddo
    res_str = (maxval(rp)-minval(rp))/2.d0/0.5358D-04
end function res_str

subroutine solkep(ex,m,e,iter)
! Solving Kepler equation
! Given:
!   ex - excentricity (must be < 1)
!   m - mean anomaly in radians
! Returns:
!   e - excentric anomaly in radians
    real(8) m, mk, ex, e
    integer iter, ndic
    real(8) tole,dex
    logical flag
    real(8) e0,se,ce,es,ec,u,xpri,xseg
    real(8) de,de0,em0,e1,em1

    tole = 1d-11
    m = dmod(m,twopi)
    e = m
    iter = 0
    flag = .false.
    do
        e0 = e
        se = dsin(e0); ce = dcos(e0)
        es = ex*se; ec = 1d0 - ex*ce

        mk = e0 - es
        u = (mk - m)/ec
        xpri = e0 - u; xseg = e0 - u/(1d0 - u*es)
        e = (xpri + xseg)/2d0
        dex = dabs(e - e0)
        iter = iter + 1
        if (iter > 20) then
            write(*,*) 'Troubles in solving of Kepler equation...'
            flag = .true.
            exit
        endif
        if (dex <= tole) exit
    enddo

    if (flag) then
        ndic=0
        e0 = -twopi
        de0 = twopi/10d0
        qq5: do
            de = de0/(10d0**ndic)
            se = dsin(e0); ce = dcos(e0)
            es = ex*se; em0 = e0 - es - m
            iter = 0
            qq6: do
                e1=e0+de
                iter = iter + 1
                if (iter > 100) then
                    write(*,*) 'Error in solving of Kepler equation!'
                exit qq5
                endif
                se = dsin(e1); ce = dcos(e1)
                es = ex*se; em1 = e1-es-m
                if (em1*em0 > 0d0) then
                    e0=e1; em0=em1
                    cycle qq6
                else
                    ndic = ndic + 1
                    if (ndic == 3) then
                        e = e1
                        exit qq5
                    endif
                    cycle qq5
                endif
            enddo qq6
        enddo qq5
    endif
end subroutine solkep

end module simp_res_str
