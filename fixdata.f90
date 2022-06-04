subroutine fixdata

use system
use results
use ellipsoid

implicit none
integer i
integer ix,iy,iz
integer jx,jy,jz
real*8  px, py, pz

avpol = 0.0
volprot= 0.0
avpoli = 0.0
volproti = 0.0


do i = 1, NNN

do ix = 1, dimx0
 do iy = 1, dimy0
  do iz = 1, dimz0

! the particle is at float(dimx0)*delta/2.0
! the cell is at (float(ix)-0.5)*delta
!displace it by Rell 

  px = (float(ix)-0.5)*delta - float(dimx0)*delta/2.0 + Rellf(1,i)*float(dimx)*delta
  jx = int(px/delta)+1

  py = (float(iy)-0.5)*delta - float(dimy0)*delta/2.0 + Rellf(2,i)*float(dimy)*delta
  jy = int(py/delta)+1

  pz = (float(iz)-0.5)*delta - float(dimz0)*delta/2.0 + Rellf(3,i)*float(dimz)*delta
  jz = int(pz/delta)+1 

 
  if((jx.ge.1).and.(jx.le.dimx)) then
   if((jy.ge.1).and.(jy.le.dimy)) then
    if((jz.ge.1).and.(jz.le.dimz)) then

     avpol(jx,jy,jz) = avpol(jx,jy,jz)+avpol0(ix,iy,iz)
     avpoli(jx,jy,jz,i) = avpol0(ix,iy,iz)

     volprot(jx,jy,jz) = volprot(jx,jy,jz)+volprot0(ix,iy,iz)
     volproti(jx,jy,jz,i) = volprot0(ix,iy,iz)

    endif
   endif
  endif

  enddo
 enddo
enddo

enddo


end
