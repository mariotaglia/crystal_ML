implicit none
integer cccc

call readinput ! read input parameters from DEFINITIONS.txt
call allocation ! allocate arrays
call loaddata   ! loads data from single particle

call fixdata    ! generates new arrays based on previous ones 

cccc = 1

call savedata(cccc)

call integral


end
