subroutine fixdata

use system
use results
use ellipsoid

implicit none
integer i
integer ix,iy,iz
integer jx,jy,jz
real*8 v(3), vr(3)
integer iix, iiy, iiz
integer subs
real*8 rotimatCO(3,3,2)
real*8 a(3,3), c(3,3)
integer n

avpol = 0.0
volprot= 0.0
avpoli = 0.0
volproti = 0.0

subs = 10

do i = 1, NNN

do ix = 1, dimx0
 do iy = 1, dimy0
  do iz = 1, dimz0

  do iix = 1, subs
  do iiy = 1, subs
  do iiz = 1, subs

! the particle is at float(dimx0)*delta/2.0
! the cell is at (float(ix)-0.5)*delta
!displace it by Rell 
!  v(1) = (float(ix))*delta + (float(iix)-0.5)/float(subs)*delta - float(dimx0)*delta/2.0
!  v(2) = (float(iy))*delta + (float(iiy)-0.5)/float(subs)*delta  - float(dimy0)*delta/2.0
!  v(3) = (float(iz))*delta + float(iiz-1)/float(subs-1)*delta - float(dimz0)*delta/2.0


  v(1) = (float(ix)-1.)*delta + (float(iix)-0.5)/float(subs)*delta - float(dimx0)*delta/2.0
  v(2) = (float(iy)-1.)*delta + (float(iiy)-0.5)/float(subs)*delta  - float(dimy0)*delta/2.0
  v(3) = (float(iz)-1.)*delta + float(iiz-1)/float(subs-1)*delta - float(dimz0)*delta/2.0



!  v(1) = (float(ix)-0.5)*delta + (float(iix)-0.5)/float(subs)*delta - float(dimx0)*delta/2.0
!  v(2) = (float(iy)-0.5)*delta + (float(iiy)-0.5)/float(subs)*delta  - float(dimy0)*delta/2.0
!  v(3) = (float(iz))*delta + float(iiz-1)/float(subs-1)*delta - float(dimz0)*delta/2.0

  vr = MATMUL(rotmatCO(:,:,i),v)

  vr(1) = vr(1) + Rellf(1,i)*float(dimx)*delta
  vr(2) = vr(2) + Rellf(2,i)*float(dimy)*delta
  vr(3) = vr(3) + Rellf(3,i)*float(dimz)*delta

  jx = int((vr(1))/delta)+1
  jy = int((vr(2))/delta)+1
  jz = int((vr(3))/delta)+1


  if((jx.ge.1).and.(jx.le.dimx)) then
   if((jy.ge.1).and.(jy.le.dimy)) then
    if((jz.ge.1).and.(jz.le.dimz)) then

     avpol(jx,jy,jz) = avpol(jx,jy,jz)+avpol0(ix,iy,iz)/float(subs**3)
     avpoli(jx,jy,jz,i) = avpol0(ix,iy,iz)/float(subs**3)

     volprot(jx,jy,jz) = volprot(jx,jy,jz)+volprot0(ix,iy,iz)/float(subs**3)
     volproti(jx,jy,jz,i) = volprot0(ix,iy,iz)/float(subs**3)

    endif
   endif
  endif

  enddo
  enddo
  enddo

  enddo
 enddo
enddo

enddo


end
