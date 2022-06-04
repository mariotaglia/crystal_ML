subroutine integral


use system
use results
use integrals


implicit none
real*8 A, B
integer M, N
real*8 integratepolpol, integratepolpart

open(file='A.dat',unit=10)
open(file='B.dat',unit=20)

! Polymer-polymer
do M = 1, maxn
 do N = M, maxn
  A = integratepolpol(M,N)
  write(10, *)M, N, A
 enddo
enddo

! Polymer-particle
do M = 1, maxn
 do N = 1, maxn
  B = integratepolpart(M,N)
  write(20, *)M, N, B
 enddo
enddo

close(10)
close(20)



end

double precision function integratepolpol(M,N)
use system
use results
implicit none
integer M,N
integer ix, iy ,iz
real*8 temp

temp = 0.0

do ix = 1, dimx
 do iy = 1, dimy
  do iz = 1, dimz

  temp = temp + (avpoli(ix,iy,iz,1)**M)*(avpoli(ix,iy,iz,2)**N) 
  temp = temp + (avpoli(ix,iy,iz,2)**M)*(avpoli(ix,iy,iz,1)**N) 

  enddo
 enddo
enddo

integratepolpol = temp/vol
end function


double precision function integratepolpart(M,N)
use system
use results
implicit none
integer M,N
integer ix, iy ,iz
real*8 temp

temp = 0.0

do ix = 1, dimx
 do iy = 1, dimy
  do iz = 1, dimz

  temp = temp + (volproti(ix,iy,iz,1)**M)*(avpoli(ix,iy,iz,2)**N) 
  temp = temp + (volproti(ix,iy,iz,2)**M)*(avpoli(ix,iy,iz,1)**N) 

  enddo
 enddo
enddo

integratepolpart = temp/vol
end function




