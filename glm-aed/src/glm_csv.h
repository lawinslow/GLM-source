/******************************************************************************
 *                                                                            *
 * glm_csv.h                                                                  *
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
#ifndef _GLM_CSV_H_
#define _GLM_CSV_H_

#define MaxPointCSV    10
#define MaxCSVOutVars   20


#ifdef _FORTRAN_VERSION_

  INTERFACE

     SUBROUTINE init_csv_output(out_dir, len, lkn) BIND(C, name="init_csv_output_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: out_dir(*)
        CINTEGER,INTENT(in)   :: len
        CLOGICAL,INTENT(in)   :: lkn
     END SUBROUTINE init_csv_output

     SUBROUTINE write_csv_point(f, name, len, val, cval, vlen, last) BIND(C, name="write_csv_point_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in)   :: f
        CCHARACTER,INTENT(in) :: name(*)
        CINTEGER,INTENT(in)   :: len
        REALTYPE,INTENT(in)  :: val
        CCHARACTER,INTENT(in) :: cval(*)
        CINTEGER,INTENT(in)   :: vlen
        CLOGICAL,INTENT(in)   :: last
     END SUBROUTINE write_csv_point

     SUBROUTINE write_csv_lake(name,len,val,cval,vlen,last) BIND(C,name="write_csv_lake_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: name(*)
        CINTEGER,INTENT(in)   :: len
        REALTYPE,INTENT(in)  :: val
        CCHARACTER,INTENT(in) :: cval(*)
        CINTEGER,INTENT(in)   :: vlen
        CLOGICAL,INTENT(in)   :: last
     END SUBROUTINE write_csv_lake

!    SUBROUTINE configure_csv(point_nlevs, point_at, point_fname, plen, &
!                          point_nvars, lake_fname, llen) BIND(C,name="configure_csv_")
!       USE ISO_C_BINDING
!       CINTEGER,INTENT(in)   :: point_nlevs
!       REALTYPE,INTENT(in)  :: point_at(point_nlevs)
!       CCHARACTER,INTENT(in) :: point_fname(*)
!       CINTEGER,INTENT(in)   :: plen
!       CINTEGER,INTENT(in)   :: point_nvars
!       CCHARACTER,INTENT(in) :: lake_fname(*)
!       CINTEGER,INTENT(in)   :: llen
!    END SUBROUTINE configure_csv

!    SUBROUTINE set_csv_varname(which, point_varnam, len) BIND(C,name="set_csv_varname_")
!       USE ISO_C_BINDING
!       CINTEGER,INTENT(in)   :: which
!       CCHARACTER,INTENT(in) :: point_varnam(*)
!       CINTEGER,INTENT(in)   :: len
!    END SUBROUTINE set_csv_varname

     SUBROUTINE close_csv_point_output() BIND(C, name="close_csv_point_output")
     END SUBROUTINE close_csv_point_output

     SUBROUTINE close_csv_lake_output() BIND(C, name="close_csv_lake_output")
     END SUBROUTINE close_csv_lake_output

  END INTERFACE
#else

/*############################################################################*/

extern REALTYPE csv_point_at[MaxPointCSV];
extern int csv_point_nlevs;
extern int csv_lake_file;

void init_csv_output(const char *out_dir, int lkn);
void init_csv_output_(const char *out_dir, int *len, int *lkn);
void write_csv_point(int f, const char *name, REALTYPE val, const char *cval, int last);
void write_csv_point_(int *f, const char *name, int *len, REALTYPE *val, const char *cval, int *vlen, int *last);
void write_csv_lake(const char *name, REALTYPE val, const char *cval, int last);
void write_csv_lake_(const char *name, int *len, REALTYPE *val, const char *cval, int *vlen, int *last);
void glm_close_csv_output(void);
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void configure_csv(int point_nlevs, REALTYPE *point_at, const char *point_fname,
                                        int point_nvars, const char *lake_fname);

void set_csv_point_varname(int which, const char *point_varnam);

void configure_outfl_csv(int outlet_allinone, const char *outfl_fname,
                                        int outfl_nvars, const char *ovrfl_fname);
void set_csv_outfl_varname(int which, const char *outfl_varnam);
void write_csv_outfl(int f, const char *name, REALTYPE val, const char *cval, int last);
void write_csv_outfl_idx(int ofl, int var, REALTYPE val, const char *cval, int last);

#endif

#endif
