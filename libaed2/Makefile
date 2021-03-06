#
# Makefile to build the aed2 water quality library
#

srcdir=src
objdir=obj
incdir=include

ifeq ($(FC),f77)
FC=gfortran
FFLAGS+=-Wall -ffree-line-length-none -Wno-unused-dummy-argument
endif

INCLUDES=-I${incdir}

ifeq ($(F90),ifort)
  INCLUDES+=-I/opt/intel/include
  DEBUG_FFLAGS=-g -traceback
  OPT_FFLAGS=-O3
  FFLAGS=-fPIC -warn all -i-static -mp1 -stand f03 -warn nounused $(DEFINES) $(INCLUDES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-check
  endif
  ifeq ($(SINGLE),true)
    FFLAGS+=-real-size 32
  else
    FFLAGS+=-real-size 64
  endif
  LIBS+=-lifcore -lsvml
  LIBS+=-limf -lintlc
  LIBS+=-L/opt/intel/lib -Wl,-rpath=/opt/intel/lib
else ifeq ($(F90),open95)
  INCLUDES+=-I/opt/open64/include
  DEBUG_FFLAGS=-g -fbacktrace
  OPT_FFLAGS=-O3
  FFLAGS=-fPIC -Wall -O3 -freeform $(DEFINES) $(INCLUDES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-fcheck=all
  endif
  LIBS+=-L/opt/open64/lib -Wl,-rpath=/opt/open64/lib
else
  F90=gfortran
  INCLUDES+=-I/usr/include
  DEBUG_FFLAGS=-g -fbacktrace
  OPT_FFLAGS=-O3
  FFLAGS=-fPIC -Wall -ffree-line-length-none -std=f2003 $(DEFINES) $(INCLUDES) -fall-intrinsics -Wno-unused-dummy-argument -fno-range-check
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-fcheck=all
  endif
  FFLAGS+=-fdefault-real-8 -fdefault-double-8
  LIBS+=-lgfortran
endif

ifeq ($(COMPILATION_MODE),debug)
  DEBUG_CFLAGS=-g
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

LIBS+=-lnetcdff -lnetcdf
ifeq ($(PLOTS),true)
  LIBS+=-L$(PLOTDIR) -lplot -lgd -lpng -ljpeg -lm
  ifeq ($(XPLOTS),true)
    LIBS+=-lX11
  endif
endif

ifeq ($(SINGLE),true)
  FFLAGS += -DSINGLE=1
endif


FFLAGS+=$(DEBUG_FFLAGS) $(OPT_FFLAGS)


OBJS = \
${objdir}/aed2_core.o \
${objdir}/aed2_util.o \
${objdir}/aed2_sedflux.o \
${objdir}/aed2_ass.o \
${objdir}/aed2_chlorophylla.o \
${objdir}/aed2_oxygen.o \
${objdir}/ufz_oxygen.o \
${objdir}/aed2_silica.o \
${objdir}/aed2_carbon.o \
${objdir}/aed2_nitrogen.o \
${objdir}/aed2_phosphorus.o \
${objdir}/aed2_organic_matter.o \
${objdir}/aed2_phyto_utils.o \
${objdir}/aed2_phytoplankton.o \
${objdir}/aed2_zoop_utils.o \
${objdir}/aed2_zooplankton.o \
${objdir}/aed2_bivalve.o \
${objdir}/aed2_macrophyte.o \
${objdir}/aed2_pathogens.o \
${objdir}/aed2_isotope.o \
${objdir}/aed2_isotope_c.o \
${objdir}/aed2_iron.o \
${objdir}/aed2_sulfur.o \
${objdir}/aed2_tracer.o \
${objdir}/aed2_totals.o \
${objdir}/aed2_gctypes.o \
${objdir}/aed2_gclib.o \
${objdir}/aed2_gcsolver.o \
${objdir}/aed2_geochemistry.o \
${objdir}/aed2_radon.o \
${objdir}/aed2_sedvode.o \
${objdir}/aed2_sedcandi.o \
${objdir}/aed2_sedeqsolver.o \
${objdir}/aed2_seddiagenesis.o \
${objdir}/aed2_vegetation.o \
${objdir}/aed2_test.o \
${objdir}/aed2_common.o


all: libaed2.a


${objdir}:
	@mkdir ${objdir}

libaed2.a: ${objdir} ${OBJS}
	ar rv $@ ${OBJS}
	ranlib $@

clean: ${objdir}
	@touch ${objdir}/1.o
	@touch 1.mod
	/bin/rm ${objdir}/*.o
	/bin/rm *.mod

distclean: clean
	@touch libaed2.a
	/bin/rm libaed2.a
	/bin/rmdir obj

${objdir}/%.o: ${srcdir}/%.F90 ${srcdir}/aed2_core.F90 ${incdir}/aed2.h
	$(F90) $(FFLAGS) -g -c $< -o $@
