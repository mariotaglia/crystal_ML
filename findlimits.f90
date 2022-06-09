subroutine findlimits
use bspline_kinds_module, only: wp, ip
use ellipsoid
use system
use limits
use results
use splines
implicit none

real(wp) :: dists(dimx0*dimy0*dimz0)
real(wp) center(3), v(3)
integer i, ix, iy, iz

  center(1) = real(dimx0,wp)*delta/2.0
  center(2) = real(dimy0,wp)*delta/2.0
  center(3) = real(dimz0,wp)*delta/2.0

! generate list of distances to the center
i = 0
do ix = 1, dimx0
 do iy = 1, dimy0
  do iz = 1, dimz0

  i = i + 1

  v(1) = (real(ix,wp)-1.)*delta - center(1)
  v(2) = (real(iy,wp)-1.)*delta - center(2)
  v(3) = (real(iz,wp)-1.)*delta - center(3)

  dists(i) = norm2(v) 
  table(1,i) = ix
  table(2,i) = iy
  table(3,i) = iz
  
  enddo
 enddo
enddo

! sort list
call bubblesort(dimx0*dimy0*dimz0, dists, table)

! find farthest element with phi > phicutoff

do i = dimx0*dimy0*dimz0, 1, -1

  ix = table(1,i)
  iy = table(2,i)
  iz = table(3,i)

  if (avpol0(ix,iy,iz).gt.phicutoff) then
    Nlookup = i
    rcutoff = dists(i)
    print*, 'Nlookup, rcutoff, avpol0(ix,iy,iz), phicutoff'
    print*, Nlookup, rcutoff, avpol0(ix,iy,iz), phicutoff
    exit
  endif
enddo     

do i = 1, Nlookup
    ix = table(1, i) 
    iy = table(2, i)
    iz = table(3, i)

    tabler(1, i) = (real(ix-1,wp)/real(dimx0-1,wp)-0.5)*real(dimx0,wp)*delta  ! Contains x,y,z positions of cells 
    tabler(2, i) = (real(iy-1,wp)/real(dimy0-1,wp)-0.5)*real(dimy0,wp)*delta 
    tabler(3, i) = (real(iz-1,wp)/real(dimz0-1,wp)-0.5)*real(dimz0,wp)*delta 
enddo
end subroutine


subroutine bubblesort(n,dists,table)
   use bspline_kinds_module, only: wp, ip
   implicit none
   integer, intent(in) :: n
   real(wp), intent(in out), dimension(n) :: dists
   integer, intent(in out), dimension(3,n) :: table
   integer :: i, j, tempi(3)
   real(wp) tempr
   logical :: swapped

    do j = n-1, 1, -1
      swapped = .false.
      do i = 1, j
         if (dists(i).gt.dists(i+1)) then
            tempr = dists(i)
            dists(i) = dists(i+1)
            dists(i+1) = tempr
  
            tempi(:) = table(:,i)
            table(:,i) = table(:,i+1)
            table(:,i+1) = tempi(:)

            swapped = .true.
         end if
      end do
      if (.not. swapped) exit
   end do
end subroutine bubblesort

