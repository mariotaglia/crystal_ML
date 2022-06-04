TARGET = rotation_ML

SRC = modules.f90 parser.f90 rotation_main.f90 allocateellCO.f90 allocation.f90 init.f90 fixdata.f90 savetodisk.f90 inverse.f90

HOST=$(shell hostname)
$(info HOST is ${HOST})

#LFLAGS = -lm /usr/lib/x86_64-linux-gnu/librt.so  -L/usr/local/lib  -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial ${LIBS} -Wl,-rpath,/usr/local/lib


# some definitions
SHELL = /bin/bash
FFLAGS= -fbacktrace -fbounds-check # -O3

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

















































