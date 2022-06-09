!*****************************************************************************************

    subroutine spline

    use splines
    use system
    use results
    implicit none

!    integer(ip),parameter :: nx = dimx0     !! number of points in x
!    integer(ip),parameter :: ny = dimy0     !! number of points in y
!    integer(ip),parameter :: nz = dimz0     !! number of points in z

    integer(ip),parameter :: iknot = 0  !! automatically select the knots

    real(wp) :: x(dimx0),y(dimy0),z(dimz0)

    real(wp),dimension(ky,kz)                    :: w1_3d
    real(wp),dimension(kz)                       :: w2_3d
    real(wp),dimension(3*max(kx,ky,kz))          :: w3_3d

    real(wp) :: tol
    real(wp) :: val,tru,err,errmax
    logical :: fail
    integer(ip) :: i,j,k,idx,idy,idz
    integer(ip) :: iflag
    character(len=:),allocatable :: msg    !! status message associated with `flag`

    integer,dimension(*),parameter :: iflags = [   -1_ip, &
                                                   -2_ip, &
                                                    0_ip, &
                                                    1_ip, &
                                                    2_ip, &
                                                    3_ip, &
                                                    4_ip, &
                                                    5_ip, &
                                                    6_ip, &
                                                    7_ip, &
                                                    8_ip, &
                                                    9_ip, &
                                                    10_ip, &
                                                    11_ip, &
                                                    12_ip, &
                                                    13_ip, &
                                                    14_ip, &
                                                    15_ip, &
                                                    16_ip, &
                                                    17_ip, &
                                                    18_ip, &
                                                    19_ip, &
                                                    20_ip, &
                                                    21_ip, &
                                                    22_ip, &
                                                    23_ip, &
                                                    24_ip, &
                                                    25_ip, &
                                                    26_ip, &
                                                    700_ip, &
                                                    701_ip, &
                                                    702_ip, &
                                                    703_ip, &
                                                    704_ip, &
                                                    705_ip, &
                                                    706_ip, &
                                                    707_ip, &
                                                    708_ip, &
                                                    709_ip, &
                                                    710_ip, &
                                                    711_ip, &
                                                    712_ip, &
                                                    713_ip, &
                                                    714_ip, &
                                                    715_ip, &
                                                    716_ip, &
                                                    717_ip, &
                                                    800_ip, &
                                                    801_ip, &
                                                    802_ip, &
                                                    803_ip, &
                                                    804_ip, &
                                                    805_ip, &
                                                    806_ip, &
                                                    100_ip, &
                                                    101_ip, &
                                                    102_ip, &
                                                    103_ip, &
                                                    104_ip, &
                                                    201_ip, &
                                                    202_ip, &
                                                    203_ip, &
                                                    204_ip, &
                                                    301_ip, &
                                                    401_ip, &
                                                    402_ip, &
                                                    403_ip, &
                                                    404_ip, &
                                                    405_ip, &
                                                    406_ip, &
                                                    501_ip, &
                                                    502_ip, &
                                                    503_ip, &
                                                    504_ip, &
                                                    505_ip, &
                                                    506_ip, &
                                                    601_ip, &
                                                    602_ip, &
                                                    603_ip, &
                                                    604_ip, &
                                                    605_ip, &
                                                    606_ip, &
                                                    901_ip, &
                                                    902_ip, &
                                                    903_ip, &
                                                    1001_ip, &
                                                    1002_ip, &
                                                    1003_ip, &
                                                    1004_ip, &
                                                    1005_ip, &
                                                    1101_ip, &
                                                    1102_ip, &
                                                    2001_ip, &
                                                    2002_ip, &
                                                    2003_ip, &
                                                    2004_ip, &
                                                    2005_ip, &
                                                    2006_ip, &
                                                    2007_ip, &
                                                    3001_ip, &
                                                    3002_ip, &
                                                    3003_ip, &
                                                    -999999_ip ] ! an unknown code


! allocate arrays

allocate(txa(dimx0+kx))
allocate(tya(dimx0+ky))
allocate(tza(dimx0+kz))

allocate(txp(dimx0+kx))
allocate(typ(dimx0+ky))
allocate(tzp(dimx0+kz))

ALLOCATE (avpol_coef(dimx0,dimy0,dimz0))
ALLOCATE (volprot_coef(dimx0,dimy0,dimz0))

    fail = .false.
    tol = 100 * epsilon(1.0_wp)
    idx = 0
    idy = 0
    idz = 0

     do i=1,dimx0
        x(i) = (real(i-1,wp)/real(dimx0-1,wp)-0.5)*real(dimx0,wp)*delta
     end do
     do j=1,dimy0
        y(j) = (real(j-1,wp)/real(dimy0-1,wp)-0.5)*real(dimy0,wp)*delta
     end do
     do k=1,dimz0
        z(k) = (real(k-1,wp)/real(dimz0-1,wp)-0.5)*real(dimz0,wp)*delta
     end do

    ! initialize
    call db3ink(x,dimx0,y,dimy0,z,dimz0,avpol0,kx,ky,kz,iknot,txa,tya,tza,avpol_coef,iflag)
    call db3ink(x,dimx0,y,dimy0,z,dimz0,volprot0,kx,ky,kz,iknot,txp,typ,tzp,volprot_coef,iflag)

    if (iflag/=0) then
        write(*,*) 'Error initializing D spline: '//get_status_message(iflag)
    end if

end subroutine


subroutine spline_test


use system
use splines
use results
use ellipsoid

implicit none
integer i,j,k
real(wp) :: x(dimx),y(dimy),z(dimz)
real(wp) v(3), vr(3)
integer(ip) :: idxa,idya,idza
integer(ip) :: idxp,idyp,idzp
real(wp) :: val,tru,err,errmax
integer(ip) :: inbvxa,inbvya,inbvza
integer(ip) :: inbvxp,inbvyp,inbvzp

real(wp),dimension(ky,kz)                    :: w1_3d
real(wp),dimension(kz)                       :: w2_3d
real(wp),dimension(3*max(kx,ky,kz))          :: w3_3d
integer cccc

integer(ip) :: iflag
integer(ip) :: iloya,iloza
integer(ip) :: iloyp,ilozp

idxa = 0
idya = 0
idza = 0

idxp = 0
idyp = 0
idzp = 0



!have to set these before the first evaluate call:
inbvxa = 1
inbvya = 1
inbvza = 1
iloya  = 1
iloza  = 1

inbvxp = 1
inbvyp = 1
inbvzp = 1
iloyp  = 1
ilozp  = 1


! points on final grid, centered in (0,0,0)

     do i=1,dimx
        x(i) = (real(i-1,wp)/real(dimx-1,wp)-0.5)*real(dimx,wp)*delta
     end do
     do j=1,dimy
        y(j) = (real(j-1,wp)/real(dimy-1,wp)-0.5)*real(dimy,wp)*delta
     end do
     do k=1,dimz
        z(k) = (real(k-1,wp)/real(dimz-1,wp)-0.5)*real(dimz,wp)*delta
     end do

! test spline

avpol = 0.0
volprot = 0.0

do i = 1, dimx
 do j = 1, dimy
  do k = 1, dimz

! rotate point
   v(1) = x(i)
   v(2) = y(j)
   v(3) = z(k)
  
   vr(:) = MATMUL(rotmatCO(:,:,1,1),v(:))
!   vr(:) = -v(:) ! MATMUL(rotmatCO(:,:,2,1),v(:))

  call db3val(vr(1),vr(2),vr(3),idxa,idya,idza,&
  txa,tya,tza,dimx0,dimy0,dimz0,kx,ky,kz,avpol_coef,val,iflag,&
  inbvxa,inbvya,inbvza,iloya,iloza,w1_3d,w2_3d,w3_3d)

  avpol(i,j,k) = val

  call db3val(vr(1),vr(2),vr(3),idxp,idyp,idzp,&
  txp,typ,tzp,dimx0,dimy0,dimz0,kx,ky,kz,volprot_coef,val,iflag,&
  inbvxp,inbvyp,inbvzp,iloyp,ilozp,w1_3d,w2_3d,w3_3d)

  volprot(i,j,k) = val

  enddo
 enddo
enddo

vtkflag = 1
cccc = 1
call savedata(cccc)

stop

end subroutine
