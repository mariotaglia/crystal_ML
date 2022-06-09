module splines

use bspline_module
use bspline_kinds_module, only: wp, ip
real(wp), allocatable :: avpol_coef(:,:,:)
real(wp), allocatable :: volprot_coef(:,:,:)
real(wp), allocatable :: txa(:),tya(:),tza(:)
real(wp), allocatable :: txp(:),typ(:),tzp(:)

integer(ip),parameter :: kx = 2     !! order in x
integer(ip),parameter :: ky = 2     !! order in y
integer(ip),parameter :: kz = 2     !! order in z
endmodule

module limits
use bspline_kinds_module, only: wp, ip
real(wp) phicutoff
real(wp) rcutoff
integer Nlookup
integer, allocatable :: table(:,:)
real(wp), allocatable :: tabler(:,:)
endmodule

module system 
use bspline_kinds_module, only: wp, ip
real(wp) delta
real(wp) vol
integer  dimx, dimx0
integer  dimy, dimy0
integer  dimz, dimz0
integer vtkflag
integer Ndata
character*10, allocatable :: title_data(:)
endmodule


module results
use bspline_kinds_module, only: wp, ip
use system
real(wp), allocatable :: avpol(:,:,:) ! avpol ix iy iz im
real(wp), allocatable :: avpoli(:,:,:,:) ! avpol ix iy iz im
real(wp), allocatable :: avpol0(:,:,:) ! avpol ix iy iz im
real*8, allocatable :: avpol0d(:,:,:) ! avpol ix iy iz im
real(wp), allocatable :: volprot(:,:,:) ! avpol ix iy iz im
real(wp), allocatable :: volproti(:,:,:,:) ! avpol ix iy iz im
real(wp), allocatable :: volprot0(:,:,:) ! avpol ix iy iz im
real*8, allocatable :: volprot0d(:,:,:) ! avpol ix iy iz im
endmodule


module ellipsoid
use bspline_kinds_module, only: wp, ip
integer NNN
real(wp), allocatable :: rotmatrix(:,:,:)
real(wp), allocatable :: Rell(:,:)
real(wp), allocatable :: Rellf(:,:)
real(wp), allocatable :: rotmatCO(:,:,:,:)

end module

module integrals
integer maxn ! maximum exponent used to calculate integrals
end module
