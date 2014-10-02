###############################################################################
#                                                                             #
# Makefile for glm                                                            #
#                                                                             #
#  Part of GLM (General Lake Model)                                           #
#                                                                             #
#  Developed by :                                                             #
#      AquaticEcoDynamics (AED) Group                                         #
#      School of Earth & Environment                                          #
#      The University of Western Australia                                    #
#                                                                             #
#      http://aed.see.uwa.edu.au/                                             #
#                                                                             #
#  Copyright 2013, 2014 -  The University of Western Australia                #
#                                                                             #
#   GLM is free software: you can redistribute it and/or modify               #
#   it under the terms of the GNU General Public License as published by      #
#   the Free Software Foundation, either version 3 of the License, or         #
#   (at your option) any later version.                                       #
#                                                                             #
#   GLM is distributed in the hope that it will be useful,                    #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU General Public License for more details.                              #
#                                                                             #
#   You should have received a copy of the GNU General Public License         #
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                             #
###############################################################################
#                                                                             #
# Define FABMDIR and FABM if using fabm (actually, now default)               #
# Define COMPILATION_MODE as debug if you want debugging on                   #
#                                                                             #
###############################################################################

#ifeq ($(OSTYPE),’darwin13’)
#  WITH_PLOTS=false
#  WITH_XPLOTS=false
#else
#  WITH_PLOTS=true
#  WITH_XPLOTS=true
#endif
WITH_PLOTS=false
WITH_XPLOTS=false

UTILDIR=../../libutil

DEFINES=
FINCLUDES=
CINCLUDES=-I$(UTILDIR)/include
CINCLUDES+=-I/opt/local/include
LIBS=-L$(UTILDIR) -lutil
ifeq ($(WITH_PLOTS),true)
  DEFINES+=-DPLOTS
  ifeq ($(WITH_XPLOTS),true)
    DEFINES+=-DXPLOTS
  endif
endif
ifeq ($(SINGLE),true)
  DEFINES += -DSINGLE=1
endif

ifeq ($(OSTYPE),Darwin)
  FINCLUDES+=-I/Users/hydro/devel/include
  CINCLUDES+=-I/Users/hydro/devel/include
  LIBS+=-L/Users/hydro/devel/lib
endif

ifeq ($(FORTRAN_COMPILER),IFORT11)
  F90=ifort
endif
ifeq ($(FORTRAN_COMPILER),IFORT12)
  F90=ifort
endif
ifeq ($(FORTRAN_COMPILER),IFORT15)
  F90=ifort
endif
ifeq ($(FORTRAN_COMPILER),OPEN64)
  F90=open95
endif
ifeq ($(FORTRAN_COMPILER),)
  FORTRAN_COMPILER=GFORTRAN
endif
ifeq ($(COMPILATION_MODE),)
  COMPILATION_MODE=production
endif

ifeq ($(FABM),true)
  ifeq ($(FABMDIR),)
    FABMDIR=../../fabm-git
  endif
  DEFINES+=-DFABM
  FINCLUDES+=-I$(FABMDIR)/include -I$(FABMDIR)/src/drivers/glm -I$(FABMDIR)/modules/glm/$(FORTRAN_COMPILER)
  ifeq ($(COMPILATION_MODE),production)
    FABMLIB=fabm_prod
    WITH_CHECKS=false
  endif
  ifeq ($(COMPILATION_MODE),debug)
    FABMLIB=fabm_debug
    WITH_CHECKS=true
  endif
  LIBS+=-L$(FABMDIR)/lib/glm/$(FORTRAN_COMPILER) -l$(FABMLIB)
endif


# Select specific compiler bits
ifeq ($(F90),ifort)
  FINCLUDES+=-I/opt/intel/include
  DEBUG_FFLAGS=-g -traceback
  OPT_FFLAGS=-O3
  FFLAGS=-warn all -i-static -mp1 -stand f03 $(DEFINES) $(FINCLUDES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-check
  endif
  #FFLAGS+=-real-size 64
  LIBS+=-lifcore -lsvml
  LIBS+=-limf -lintlc
  LIBS+=-L/opt/intel/lib
  ifeq ($(OSTYPE),Linux)
    LIBS+=-Wl,-rpath=/opt/intel/lib
  endif
else ifeq ($(F90),open95)
  FINCLUDES+=-I/opt/open64/include
  DEBUG_FFLAGS=-g -fbacktrace
  OPT_FFLAGS=-O3
  FFLAGS=-Wall -O3 -freeform $(DEFINES) $(FINCLUDES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-fcheck=all
  endif
  LIBS+=-L/opt/open64/lib -Wl,-rpath=/opt/open64/lib
else
  F90=gfortran
  FINCLUDES+=-I/usr/include
  DEBUG_FFLAGS=-g -fbacktrace
  OPT_FFLAGS=-O3
  FFLAGS=-Wall -ffree-line-length-none -std=f2003 $(DEFINES) $(FINCLUDES) -fall-intrinsics
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-fcheck=all
  endif
  FFLAGS+=-fdefault-real-8 -fdefault-double-8
  LIBS+=-lgfortran
endif

ifeq ($(DEBUG),true)
  DEBUG_CFLAGS=-g -fbounds-check
  OPT_CFLAGS=
  OPT_FFLAGS=
else
  DEBUG_FFLAGS=
  DEBUG_CFLAGS=
  # OPT_CFLAGS=-O4 -Ofast -frounding-math
  OPT_CFLAGS=-O3
  # OPT_CFLAGS=
  # OPT_FFLAGS=
endif

LIBS+=-lnetcdf #-lnetcdff
LIBS+=-L/opt/local/lib
ifeq ($(WITH_PLOTS),true)
  LIBS+=-L$(PLOTDIR) -lplot -lgd -lpng -ljpeg -lm
  ifeq ($(WITH_XPLOTS),true)
    LIBS+=-lX11
  endif
endif

ifeq ($(WITH_PLOTS),false)
  CFLAGS=-Wall -I$(UTILDIR) $(CINCLUDES) $(DEFINES) $(DEBUG_CFLAGS) $(OPT_CFLAGS)
else
  CFLAGS=-Wall -I$(UTILDIR) -I$(PLOTDIR) $(CINCLUDES) $(DEFINES) $(DEBUG_CFLAGS) $(OPT_CFLAGS)
endif
FFLAGS+=$(DEBUG_FFLAGS) $(OPT_FFLAGS)

FORT_OBJS=ode_solvers.o \
          glm_types.o   \
          glm_fabm.o

OBJS=glm_globals.o \
     glm_util.o \
     glm_csv.o \
     glm_mobl.o \
     $(FORT_OBJS) \
     glm_mixu.o \
     glm_layers.o \
     glm_surface.o \
     glm_input.o \
     glm_plot.o \
     glm_output.o \
     glm_ncdf.o \
     glm_lnum.o \
     glm_init.o \
     glm_flow.o \
     glm_mixer.o \
     glm_deep.o \
     glm_model.o \
     glm_main.o

all: glm

glm: $(OBJS)
	$(CC) -o glm $(OBJS) $(LIBS)

clean:
	@touch 1.o 1.mod 1.t 1__genmod.f90 glm
	@/bin/rm *.mod *.o *.t *__genmod.f90 glm
	@echo Made clean

distclean: clean

%.o: %.F90 glm.h
	$(F90) $(FFLAGS) $(EXTRA_FFLAGS) -D_FORTRAN_VERSION_ -c $< -o $@

%.o: %.c glm.h
	$(CC) $(CFLAGS) $(EXTRA_FLAGS) -D_C_VERSION_ -c $< -o $@

# Dependancy rules

glm_fabm.o: glm_fabm.F90 glm_types.o ode_solvers.o
glm_types.o: glm_types.F90 glm.h
ode_solvers.o: ode_solvers.F90

glm_globals.o: glm_globals.c glm_globals.h glm.h
glm_mobl.o: glm_mobl.c glm.h
glm_util.o: glm_util.c glm.h
glm_deep.o: glm_deep.c glm.h
glm_mixu.o: glm_mixu.c glm.h
glm_mixer.o: glm_mixer.c glm.h
glm_layers.o: glm_layers.c glm.h
glm_surface.o: glm_surface.c glm.h
glm_flow.o: glm_flow.c glm.h
glm_main.o: glm_main.c glm.h
glm_csv.o: glm_csv.c glm.h
glm_output.o: glm_output.c glm.h
glm_input.o: glm_input.c glm.h
glm_ncdf.o: glm_ncdf.c glm.h
glm_lnum.o: glm_lnum.c glm.h
glm_init.o: glm_init.c glm.h
glm_plot.o: glm_plot.c glm.h
glm_model.o: glm_model.c glm.h

