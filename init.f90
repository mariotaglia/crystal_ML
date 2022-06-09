subroutine loaddata
use results
use bspline_kinds_module, only: wp, ip
implicit none
integer ix,iy,iz
open (unit=8, file='propol.out', form='unformatted')
read(8)avpol0d
read(8)volprot0d
close(8)
do ix = 1, dimx0
do iy = 1, dimy0
do iz = 1, dimz0
volprot0(ix,iy,iz) = real(volprot0d(ix,iy,iz),wp)
avpol0(ix,iy,iz) = real(avpol0d(ix,iy,iz),wp)
enddo
enddo
enddo
end



subroutine savedata(cccc)
use bspline_kinds_module, only: wp, ip
use system
use results
use ellipsoid

implicit none
integer cccc
character*20 filename
character*5  title
real(wp) temp(dimx,dimy,dimz)
real(wp) sumpol
integer ix,iy,iz, im

! Polimero, todo
  title = 'avpol'
  call savetodisk(avpol, title, cccc)

! Polimer, 1
  title = 'avpo1'
  temp(:,:,:) =  avpoli(:,:,:,1)
  call savetodisk(temp, title, cccc)

! Polimer, 2
  title = 'avpo2'
  temp(:,:,:) =  avpoli(:,:,:,2)
  call savetodisk(temp, title, cccc)

! Particula, todo
  title = 'avpro'
  call savetodisk(volprot, title, cccc)

! Particula, 1
  title = 'avpr1'
  temp(:,:,:) =  volproti(:,:,:,1)
  call savetodisk(temp, title, cccc)

! Particula, 2
  title = 'avpr2'
  temp(:,:,:) =  volproti(:,:,:,2)
  call savetodisk(temp, title, cccc)

end
