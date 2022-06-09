TARGET = rotation_ML

SRC = bspline_kinds_module.f90 bspline_module.f90 bspline_sub_module.f90 bspline_oo_module.f90 modules.f90 parser.f90 rotation_main.f90 allocateellCO.f90 allocation.f90 init.f90 fixdata.f90 savetodisk.f90 inverse.f90 findlimits.f90 spline.f90

HOST=$(shell hostname)
$(info HOST is ${HOST})

#LFLAGS = -L/./libbspline-fortran.so


# some definitions
SHELL = /bin/bash
FFLAGS= -O3 #-fbacktrace -fbounds-check # -O3

GIT_VERSION := $(shell git describe --abbrev=6 --dirty --always --tags)
GFLAGS=-cpp -D_VERSION=\"$(GIT_VERSION)\"

FF = mpif77 #${F90}
VER = ~/bin/crystal

all:	$(TARGET)

$(TARGET): $(SRC:.f90=.o)
	$(FF) -o $(TARGET) $(SRC:.f90=.o) $(LFLAGS) $(GFLAGS)
	cp $(TARGET) $(VER)

$(SRC:.f90=.o): $(SRC)
	${FF} -c ${FFLAGS}  $(SRC) $(LFLAGS) $(GFLAGS)

install: all
	cp $(TARGET) $(VER)

clean:	
	@rm -f $(SRC:.f90=.o) $(SRC:.f90=.d) $(TARGET) *~

realclean: clean
	@rm -f .depend

depend dep:
	@$(FF)  $(CFLAGS) -MM $(SRC) > .depend 

ifeq (.depend, $(wildcard .depend))
include .depend
endif

















































