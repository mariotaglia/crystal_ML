subroutine allocation

use ellipsoid
use system
use results
implicit none


! results
ALLOCATE (avpol(dimx, dimy, dimz))
ALLOCATE (avpoli(dimx, dimy, dimz, NNN))
ALLOCATE (avpol0(dimx0, dimy0, dimz0))

! ematrix
ALLOCATE (volprot(dimx,dimy,dimz))
ALLOCATE (volproti(dimx,dimy,dimz,NNN))
ALLOCATE (volprot0(dimx0,dimy0,dimz0))
end subroutine
