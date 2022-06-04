module system 
real*8 delta
real*8 vol
integer  dimx, dimx0
integer  dimy, dimy0
integer  dimz, dimz0
integer vtkflag
endmodule


module results
use system
real*8, allocatable :: avpol(:,:,:) ! avpol ix iy iz im
real*8, allocatable :: avpoli(:,:,:,:) ! avpol ix iy iz im
real*8, allocatable :: avpol0(:,:,:) ! avpol ix iy iz im
real*8, allocatable :: volprot(:,:,:) ! avpol ix iy iz im
real*8, allocatable :: volproti(:,:,:,:) ! avpol ix iy iz im
real*8, allocatable :: volprot0(:,:,:) ! avpol ix iy iz im
endmodule


module ellipsoid
integer NNN
real*8, allocatable :: rotmatrix(:,:,:)
real*8, allocatable :: Rell(:,:)
real*8, allocatable :: Rellf(:,:)
real*8, allocatable :: rotmatCO(:,:,:)

end module

module integrals
integer maxn ! maximum exponent used to calculate integrals
end module
