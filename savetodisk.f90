subroutine savetodisk(array, title, counter)

use system
implicit none

integer ix, iy, iz, jx, jy, jz
real*8 array(dimx, dimy, dimz)
real*8 arraytemp(dimx, dimy, dimz)
real*8 arrayz(dimz)
integer counter
character*5 title
character*6 titlez
character*21 filename, tempc
real*4 singlepres
real*8 x(3), v(3)

!-----  coordenadas -------------------------
! Variables

arraytemp = 0
do iz = 1, dimz
  arraytemp(:,:,iz) = array(:,:,iz)
enddo
 
array = -1000
do iz = 1, dimz
  array(:,:,iz) = arraytemp(:,:,iz)
enddo

! Integra y graba promedios en z

do iz=1,dimz
         arrayz(iz) = 0.0
      do ix=1,dimx
         do iy=1, dimy
           arrayz(iz)=arrayz(iz)+array(ix,iy,iz)
         enddo
      enddo
enddo

! Archivo paraview

if(vtkflag.eq.1) then

      write(filename,'(A5, A1,I3.3, A4)')title,'.', counter, '.vtk'
      open (unit=45, file=filename)
      write(45,'(A)')'# vtk DataFile Version 2.0'
      write(45,'(A)')title
      write(45,'(A)')'ASCII'
      write(45,'(A)')'DATASET STRUCTURED_GRID '
      write(45,'(A, I5, A1, I5, A1, I5)')'DIMENSIONS', dimz+1, ' ', dimy+1, ' ',dimx+1
      write(45,'(A, I8, A)')'POINTS ',(dimx+1)*(dimy+1)*(dimz+1),' float'
      do ix = 0, dimx
        do iy = 0, dimy
          do iz = 0, dimz

          x(1) = ix*delta ! transformed coordinates
          x(2) = iy*delta
          x(3) = iz*delta

            write(45, *) x(1),'   ',  x(2), '   ', x(3) ! Cartesian coordinates 
          enddo
        enddo
      enddo

      write(45,'(A, I8)')'CELL_DATA ', dimx*dimy*dimz
      tempc = 'SCALARS ' // title // ' float 1'
      write(45,'(A)')tempc
      write(45,'(A)')'LOOKUP_TABLE default'

       do ix = 1, dimx
        do iy = 1, dimy
          do iz = 1, dimz

            singlepres = array(ix, iy, iz) ! Lo necesito en single presicion
 
            write(45,*)singlepres
          enddo
        enddo
      enddo
      close (45)

endif
return
end


