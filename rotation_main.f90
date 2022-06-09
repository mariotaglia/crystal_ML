use system
implicit none
integer cccc, jj

call readinput ! read input parameters from DEFINITIONS.txt
call allocation ! allocate arrays
call loaddata   ! loads data from single particle
call spline     ! build spline

! call spline_test ! test spline
print*,'Spline ready'
call findlimits ! detemines the cut-off distance from the particle center based on the cutoff density and makes a lists of sites
print*,'Limits ready'

do jj = 1, Ndata
call fixdata(jj)
print*, 'Case ', jj, ' ok'
enddo

!cccc = 1
!call savedata(cccc)

end
