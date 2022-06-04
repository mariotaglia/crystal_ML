subroutine loaddata
use results
implicit none
open (unit=8, file='propol.out', form='unformatted')
read(8)avpol0
read(8)volprot0
close(8)
end



subroutine savedata(cccc)
use system
use results
use ellipsoid

implicit none
integer cccc
character*20 filename
character*5  title
real*8 temp(dimx,dimy,dimz)
real*8 sumpol
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
  temp(:,:,:) =  volproti(:,:,:,1)
  call savetodisk(temp, title, cccc)

end
