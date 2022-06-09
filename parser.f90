
subroutine readinput
use limits
use integrals
use ellipsoid
use system
use results
implicit none

! Input related variables
character (len=100)  buffer,label
integer pos
integer, parameter :: fh = 15
integer ios
integer line, linemax
integer i, j, jj
character(len=50) :: filename = 'DEFINITIONSML.txt'
character basura
integer ndi
real(wp) ndr
integer stdout
 
! not defined variables, change if any variable can take the value

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Control file variables

stdout = 6
line = 0
ios = 0

open(fh, file=filename)

write(stdout,*) 'parser:', 'Reading parameters from ', filename

! ios is negative  if an end of record condition is encountered or if
! an endfile condition was detected.  It is positive  if an error was
! detected.  ios is zero otherwise.

do while (ios == 0)

 read(fh, '(A)', iostat=ios) buffer
 if (ios == 0) then
 line = line + 1

! Find the first instance of whitespace.  Split label and data.

 pos = scan(buffer, ' ')

 label = buffer(1:pos)
 buffer = buffer(pos+1:)


 select case (label)

 case ('vtkflag')
   read(buffer, *, iostat=ios) vtkflag
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('phicutoff')
   read(buffer, *, iostat=ios) phicutoff
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimx')
   read(buffer, *, iostat=ios) dimx
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimx0')
   read(buffer, *, iostat=ios) dimx0
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('delta')
   read(buffer, *, iostat=ios) delta
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimy')
   read(buffer, *, iostat=ios) dimy
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimy0')
   read(buffer, *, iostat=ios) dimy0
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimz')
   read(buffer, *, iostat=ios) dimz
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('dimz0')
   read(buffer, *, iostat=ios) dimz0
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)

 case ('maxn')
   read(buffer, *, iostat=ios) maxn
   write(stdout,*) 'parser:','Set ',trim(label),' = ',trim(buffer)



case('particles')    
   NNN = 2 ! only two particles 
 
     read(fh, *) basura
     read(fh, *) Ndata
     write(stdout,*)'Number of cases', Ndata

   call allocateellCO

     read(fh, *) basura
     do j = 1, NNN
        read(fh, *) Rellf(1,j), Rellf(2,j), Rellf(3,j)
     write(stdout,*) 'parser:','Set particle',j,'pos to',  Rellf(1,j), Rellf(2,j), Rellf(3,j)
    enddo

     do jj = 1, Ndata
     read(fh, *) basura
     do j = 1, NNN
      read(fh, *) rotmatCO(1,1,j,jj), rotmatCO(1,2,j,jj), rotmatCO(1,3,j,jj)
      read(fh, *) rotmatCO(2,1,j,jj), rotmatCO(2,2,j,jj), rotmatCO(2,3,j,jj)
      read(fh, *) rotmatCO(3,1,j,jj), rotmatCO(3,2,j,jj), rotmatCO(3,3,j,jj)
     enddo
     read(fh, *) title_data(jj)
     enddo
     

!    call COrotation

endselect

endif

enddo

vol = float(dimx*dimy*dimz)*(delta**3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine
