subroutine allocation

use ellipsoid
use system
use results
use limits 
implicit none


! results
ALLOCATE (avpol(dimx, dimy, dimz))
ALLOCATE (avpoli(dimx, dimy, dimz, NNN))
ALLOCATE (avpol0(dimx0, dimy0, dimz0))
ALLOCATE (avpol0d(dimx0, dimy0, dimz0))

ALLOCATE (volprot(dimx,dimy,dimz))
ALLOCATE (volproti(dimx,dimy,dimz,NNN))
ALLOCATE (volprot0(dimx0,dimy0,dimz0))
ALLOCATE (volprot0d(dimx0,dimy0,dimz0))

!limits
ALLOCATE (table(3,dimx0*dimy0*dimz0))
ALLOCATE (tabler(3,dimx0*dimy0*dimz0))


end subroutine
