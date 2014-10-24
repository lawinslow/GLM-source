###############################################################################
#                                                                             #
# Makefile for libplot                                                        #
#                                                                             #
#  Developed by :                                                             #
#      AquaticEcoDynamics (AED) Group                                         #
#      School of Earth & Environment                                          #
#      The University of Western Australia                                    #
#                                                                             #
#  Copyright 2013, 2014 -  The University of Western Australia                #
#                                                                             #
#   libplot is free software: you can redistribute it and/or modify           #
#   it under the terms of the GNU General Public License as published by      #
#   the Free Software Foundation, either version 3 of the License, or         #
#   (at your option) any later version.                                       #
#                                                                             #
#   libplot is distributed in the hope that it will be useful,                #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU General Public License for more details.                              #
#                                                                             #
#   You should have received a copy of the GNU General Public License         #
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                             #
###############################################################################

srcdir=src
objdir=objs
incdir=include

SRCS=${srcdir}/xbasic.c \
     ${srcdir}/colours.c \
     ${srcdir}/plotter.c

OBJS=${objdir}/xbasic.o \
     ${objdir}/colours.o \
     ${objdir}/plotter.o

CFLAGS=-O3
INCLUDES=-I${incdir} -I.

ifeq ($(SINGLE),true)
  CFLAGS += -DSINGLE=1
endif

all: libplot.a

libplot.a: ${objdir} ${OBJS}
	ar rv libplot.a ${OBJS}
	ranlib libplot.a

clean: ${objdir}
	@touch ${objdir}/1.o
	@/bin/rm ${objdir}/*.o
	@/bin/rmdir ${objdir}
	@touch libplot.a
	@/bin/rm libplot.a
	@touch tstmap tstfont
	@/bin/rm tstmap tstfont

distclean: clean

${objdir}:
	@mkdir ${objdir}

${objdir}/%.o: ${srcdir}/%.c ${incdir}/%.h
	$(CC) -Wall -fPIC $(CFLAGS) $(INCLUDES) -g -c $< -o $@

tstmap: tests/tstmap.c ${srcdir}/xbasic.c ${srcdir}/colours.c
	$(CC) -DTRUE_COLOUR=1 -I${incdir} tests/tstmap.c ${srcdir}/xbasic.c ${srcdir}/colours.c -lgd -lpng -ljpeg -lm -lX11 -o $@

tstfont: tests/tstfont.c ${srcdir}/xbasic.c ${srcdir}/colours.c
	$(CC) -DTRUE_COLOUR=1 -I${incdir} tests/tstfont.c ${srcdir}/xbasic.c ${srcdir}/colours.c -lgd -lpng -ljpeg -lm -lX11 -o $@

