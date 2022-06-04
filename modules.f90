module system 
real*8 delta
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

module COrotMod
real*8, allocatable :: klocta1(:)
real*8, allocatable :: klocta2(:)
real*8, allocatable :: klocta3(:)
real*8, allocatable :: klocta4(:)
real*8, allocatable :: klocta1b(:)
real*8, allocatable :: klocta2b(:)
real*8, allocatable :: klocta3b(:)
real*8, allocatable :: klocta4b(:)
real*8, allocatable :: klcubex1(:)
real*8, allocatable :: klcubex2(:)
real*8, allocatable :: klcubey1(:)
real*8, allocatable :: klcubey2(:)
real*8, allocatable :: klcubez1(:)
real*8, allocatable :: klcubez2(:)
real*8, allocatable :: plane1(:,:)
real*8, allocatable :: plane2(:,:)
real*8, allocatable :: plane3(:,:)
real*8, allocatable :: plane4(:,:)
real*8, allocatable :: plane5(:,:)
real*8, allocatable :: plane6(:,:)
real*8, allocatable :: plane7(:,:)
end module


