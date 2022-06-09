subroutine fixdata(jj)


use splines
use system
use results
use ellipsoid
use limits

implicit none

real(wp) avpol_p1, avpol_p2, volprot_p1, volprot_p2
real(wp) dist
real(wp) pos_part1(3)

integer i, jj
integer ix,iy,iz
integer jx,jy,jz
integer iix, iiy, iiz
integer subs
real(wp) rotimatCO(3,3,2)
real(wp) rotd(3,3), roti(3,3) ! matrixes for transformation to a frame where part 1 is not rotated
real(wp) a(3,3), c(3,3)
integer, parameter :: maxn = 3 ! order of integrals to calculate
integer n

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

real(wp) AA(maxn,maxn), BB(maxn,maxn)
character*20 filenameA, filenameB

integer NN, MM

! Integrals 

AA = 0.0
BB = 0.0

filenameA = "A."//TRIM(title_data(jj))//".dat"
filenameB = "B."//TRIM(title_data(jj))//".dat"

open(file=filenameA,unit=10)
open(file=filenameB,unit=20)


! constants for spline

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

! positions

Rell(1,:) = Rellf(1,:)*real(dimx,wp)*delta
Rell(2,:) = Rellf(2,:)*real(dimy,wp)*delta
Rell(3,:) = Rellf(3,:)*real(dimz,wp)*delta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! prepare rotation matrixes
! particle 1 is not rotated (same as input), particle 2 is rotated)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Generate inverse matrixes
n = 3
do i = 1,2
a(:,:) = rotmatCO(:,:,i,jj)
call inverse(a,c,n)
rotimatCO(:,:,i) = c(:,:)
enddo

! rotate the position vector for particle 2 (vector from part 1 to part 2)
v(:) = Rell(:,2) - Rell(:,1)
vr = MATMUL(rotimatCO(:,:,1),v)
Rell(1,1)=0.0
Rell(2,1)=0.0
Rell(3,1)=0.0
Rell(:,2) = vr(:) + Rell(:,1) ! now Rell(:,2) is the center of part 2 a in frame where part 1 is not rotated
                              ! and particle 1 is at (0,0,0)

! rotate matrix part 1 and 2, so now part 1 is not rotated and part 2 is rotated
rotd(:,:) = MATMUL(rotimatCO(:,:,1),rotmatCO(:,:,2,jj)) ! rotmatCO transforms a vector of part 2 from non-rotated space into rotated space


! for particle 1, rotmaCO = Id
! Generate inverse matrix
a(:,:) = rotd(:,:)
call inverse(a,c,n)
roti(:,:) = c(:,:)

! position of particle 1 with respect to the center of particle 2
v(:) = Rell(:,1) - Rell(:,2)
vr(:) = MATMUL(roti(:,:),v(:))
pos_part1(:) = vr(:) ! position of particle 1 with respect to the center of particle 2 in a reference frame where part 2 is not rotated


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Calculation of integrals
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! loop over list of sites of particle 2 (rotated one)

do i = 1, Nlookup ! loop over list of sites of particle 2 (rotated one)

v(:) = tabler(:,i)-pos_part1(:) ! tabler = position of a point of particle 2 in a frame where part 2 is at (0,0,0) and is not rotated
                                ! pos_part1 is the center of particle 1 in a frame where part 2 is at (0,0,0) and is not rotated

dist = norm2(v)         

if(dist.lt.rcutoff) then ! only considers points of particle 2 that are at a distance smaller than rcutoff from the center of particle 1

  v(:) = tabler(:,i) ! position of a point of particle 2 in a frame where part 2 is at (0,0,0) and is not rotated
  vr(:) = MATMUL(rotd(:,:),v(:)) ! position of a point of particle 2 in a frame where part 2 is at (0,0,0) and part 1 is not rotated
  vr(:) = vr(:) + Rell(:,2) ! position of a point of particle 2 in a frame where part 1 is at (0,0,0) and part 1 is not rotated

  call db3val(vr(1),vr(2),vr(3),idxa,idya,idza,&
  txa,tya,tza,dimx0,dimy0,dimz0,kx,ky,kz,avpol_coef,val,iflag,&
  inbvxa,inbvya,inbvza,iloya,iloza,w1_3d,w2_3d,w3_3d)

  avpol_p1 = val ! polymer density of particle 1 at the position

  call db3val(vr(1),vr(2),vr(3),idxp,idyp,idzp,&
  txp,typ,tzp,dimx0,dimy0,dimz0,kx,ky,kz,volprot_coef,val,iflag,&
  inbvxp,inbvyp,inbvzp,iloyp,ilozp,w1_3d,w2_3d,w3_3d)

  volprot_p1 = val ! particle volume fraction of particle 1 at the position 

  ix = table(1,i)
  iy = table(2,i)
  iz = table(3,i)


  avpol_p2 = avpol0(ix,iy,iz) ! polymer density of particle 2 at the position
  volprot_p2 = volprot(ix,iy,iz) ! particle volume fraction of particle 2 at the position

!!!!!!!!!!!!!!!!!! ADD TO INTEGRALS !!!!!!!!!!!!!!!!!!!!!!!!!!

! Polymer-polymer
do MM = 1, maxn
 do NN = MM, maxn
  AA(MM,NN) = AA(MM,NN) + (avpol_p1**NN)*(avpol_p2**MM)
  AA(MM,NN) = AA(MM,NN) + (avpol_p1**MM)*(avpol_p2**NN)
 enddo
enddo

! Polymer-particle
do MM = 1, maxn
 do NN = 1, maxn
  BB(MM,NN) = BB(MM,NN) + (volprot_p1**NN)*(avpol_p2**MM)
  BB(MM,NN) = BB(MM,NN) + (volprot_p2**NN)*(avpol_p1**MM)
 enddo
enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

endif
enddo ! Nlookup


! Write integrals to disk

! Polymer-polymer
do MM = 1, maxn
 do NN = MM, maxn
  write(10, *)MM, NN, AA(MM,NN)
 enddo
enddo

! Polymer-particle
do MM = 1, maxn
 do NN = 1, maxn
  write(20, *)MM, NN, BB(MM,NN)
 enddo
enddo

close(10)
close(20)

end

