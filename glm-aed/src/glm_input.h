/******************************************************************************
 *                                                                            *
 * glm_input.h                                                                *
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
#ifndef _GLM_INPUT_H_
#define _GLM_INPUT_H_

#ifdef _FORTRAN_VERSION_

  INTERFACE

     SUBROUTINE open_met_file(metname,nmlen,snow_sw,rain_sw) BIND(C, name="open_met_file_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: metname(*)
        CINTEGER,INTENT(in)   :: nmlen
        CLOGICAL,INTENT(in)   :: snow_sw, rain_sw
     END SUBROUTINE open_met_file

     SUBROUTINE open_inflow_file(idx,infname,nmlen,nvars,vars) BIND(C, name="open_inflow_file_")
        USE ISO_C_BINDING
        USE glm_types,ONLY : StringT
        CINTEGER,INTENT(in) :: idx
        CCHARACTER,INTENT(in) :: infname(*)
        CINTEGER,INTENT(in)   :: nmlen
        CINTEGER,INTENT(in)   :: nvars
        TYPE(StringT),INTENT(in) :: vars(*)
     END SUBROUTINE open_inflow_file

     SUBROUTINE open_outflow_file(idx,outfname,nmlen) BIND(C, name="open_outflow_file_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: idx
        CCHARACTER,INTENT(in) :: outfname(*)
        CINTEGER,INTENT(in)   :: nmlen
     END SUBROUTINE open_outflow_file

     SUBROUTINE read_daily_inflow(julian, NumInf, Flow, Temp, Salt, WQ) BIND(C, name="read_daily_inflow_")
        USE ISO_C_BINDING
        USE glm_types,ONLY : MetDataType
        CINTEGER,INTENT(in) :: julian, NumInf
        REALTYPE,INTENT(out) :: Flow(*), Temp(*), Salt(*), WQ(*)
     END SUBROUTINE read_daily_inflow

     SUBROUTINE read_daily_outflow(julian, NumOut, Drw) BIND(C, name="read_daily_outflow_")
        USE ISO_C_BINDING
        USE glm_types,ONLY : MetDataType
        CINTEGER,INTENT(in) :: julian, NumOut
        REALTYPE,INTENT(out) :: Drw(*)
     END SUBROUTINE read_daily_outflow

     SUBROUTINE read_daily_met(julian, met) BIND(C, name="read_daily_met_")
        USE ISO_C_BINDING
        USE glm_types,ONLY : MetDataType
        CINTEGER,INTENT(in) :: julian
        TYPE(MetDataType),INTENT(out) :: met
     END SUBROUTINE read_daily_met

     SUBROUTINE read_sub_daily_met(julian, iclock, met) BIND(C, name="read_sub_daily_met_")
        USE ISO_C_BINDING
        USE glm_types,ONLY : MetDataType
        CINTEGER,INTENT(in) :: julian, iclock
        TYPE(MetDataType),INTENT(out) :: met
     END SUBROUTINE read_sub_daily_met

  END INTERFACE

  CINTEGER,BIND(C),PUBLIC :: lw_ind

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
void open_inflow_file(int inf_id, const char *fname,
                            int nvars, const char *vars[], const char *timefmt);
void read_daily_inflow(int julian, int NumInf, REALTYPE *flow, REALTYPE *temp,
                                               REALTYPE *salt, REALTYPE *wq);

void open_outflow_file(int i, const char *fname, const char *timefmt);
void read_daily_outflow(int julian, int NumOut, REALTYPE *drw);

void open_met_file(const char *fname, int snow_sw, int rain_sw,
                                                           const char *timefmt);
void read_daily_met(int julian, MetDataType *met);
void read_sub_daily_met(int julian,int iclock, MetDataType *met);

void close_met_files(void);
void close_inflow_files(void);
void close_outflow_files(void);

/* for fortran */
void open_inflow_file_(int *inf_id, const char *fname, int *nlen,
                                                     int *nvars, StringT *vars);
void read_daily_inflow_(int *julian, int *NumInf, REALTYPE *flow, REALTYPE *temp,
                                                  REALTYPE *salt, REALTYPE *wq);

void open_outflow_file_(int *i, const char *fname, int *nlen);
void read_daily_outflow_(int *julian, int *NumOut, REALTYPE *drw);

void open_met_file_(const char *fname, int *nlen, int *snow, int*rain);
void read_daily_met_(int *julian, MetDataType *met);
void read_sub_daily_met_(int *julian, int *iclock, MetDataType *met);

extern int lw_ind;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#endif


#endif
