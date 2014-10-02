/******************************************************************************
 *                                                                            *
 * glm_plot.h                                                                 *
 *                                                                            *
 * plotting for glm                                                           *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Earth & Environment                                          *
 *     The University of Western Australia                                    *
 *                                                                            *
 *     http://aed.see.uwa.edu.au/                                             *
 *                                                                            *
 * Copyright 2013, 2014 -  The University of Western Australia                *
 *                                                                            *
 *  This file is part of GLM (General Lake Model)                             *
 *                                                                            *
 *  GLM is free software: you can redistribute it and/or modify               *
 *  it under the terms of the GNU General Public License as published by      *
 *  the Free Software Foundation, either version 3 of the License, or         *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  GLM is distributed in the hope that it will be useful,                    *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *
 *                                                                            *
 ******************************************************************************/
#ifndef _GLM_PLOT_H_
#define _GLM_PLOT_H_

#ifdef PLOTS

#define MAX_PLOTS  9

#ifdef _FORTRAN_VERSION_

  INTERFACE

     !##########################################################################
     SUBROUTINE init_plots(jstart,ndays,crest) BIND(C,name="init_plots_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jstart,ndays
        REALTYPE,INTENT(in) :: crest
     END SUBROUTINE init_plots

     SUBROUTINE put_xplot_val(name,len,wlev,val) BIND(C,name="put_xplot_val_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in):: name(*)
        CINTEGER,INTENT(in)  :: len, wlev
        REALTYPE,INTENT(in)  :: val(*)
     END SUBROUTINE put_xplot_val
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  END INTERFACE

  CLOGICAL,PUBLIC,BIND(C,name="do_plots") :: do_plots

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#ifdef XPLOTS
    extern int xdisp;
#endif
    extern int do_plots,saveall;
    extern int nplots, theplots[MAX_PLOTS+1];
    extern char **vars;
    extern int today;

/******************************************************************************/
void init_plots(int jstart, int ndays, REALTYPE crest);
void put_xplot_val(char *name, int wlev, REALTYPE *val);

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
void init_plots_(int *jstart, int *ndays, REALTYPE *crest);
void put_xplot_val_(char *name, int *len, int *wlev, REALTYPE *val);

#endif

#endif

#endif
