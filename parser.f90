
subroutine readinput

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
integer i, j
character(len=50) :: filename = 'DEFINITIONS.txt'
character basura
integer ndi
real*8 ndr
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


case('particles')    
   NNN = 2 ! only two particles 
 
   call allocateellCO

     read(fh, *) basura
     do j = 1, NNN
        read(fh, *) Rellf(1,j), Rellf(2,j), Rellf(3,j)
     write(stdout,*) 'parser:','Set particle',j,'pos to',  Rellf(1,j), Rellf(2,j), Rellf(3,j)
     enddo

     read(fh, *) basura
     do j = 1, NNN
      read(fh, *) rotmatCO(1,1,j), rotmatCO(1,2,j), rotmatCO(1,3,j)
      read(fh, *) rotmatCO(2,1,j), rotmatCO(2,2,j), rotmatCO(2,3,j)
      read(fh, *) rotmatCO(3,1,j), rotmatCO(3,2,j), rotmatCO(3,3,j)
     enddo

!    call COrotation

endselect

endif

enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine
