/******************************************************************************
 *                                                                            *
 * glm_output.h                                                               *
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
#ifndef _GLM_OUTPUT_H_
#define _GLM_OUTPUT_H_

#ifdef _FORTRAN_VERSION_

INTERFACE

     SUBROUTINE init_output(jstart, out_dir, dlen, out_fn, flen, MaxLayers, Longitude, Latitude, lkn) &
                                                      BIND(C, name="init_output_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in)   :: jstart
        CCHARACTER,INTENT(in) :: out_dir(*), out_fn(*)
        CINTEGER,INTENT(in)   :: dlen, flen
        CINTEGER,INTENT(in)   :: MaxLayers
        REALTYPE,INTENT(in)   :: Longitude, Latitude
        CLOGICAL,INTENT(in)   :: lkn
     END SUBROUTINE init_output

     SUBROUTINE write_output(jday,iclock,nsave,stepnum) BIND(C, name="write_output_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jday,iclock,nsave,stepnum
     END SUBROUTINE write_output

     SUBROUTINE write_diags(jday,iclock,lakenum) BIND(C, name="write_diags_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jday,iclock
        REALTYPE,INTENT(in) :: LakeNum
     END SUBROUTINE write_diags

     SUBROUTINE close_output BIND(C, name="close_output_")
     END SUBROUTINE close_output

END INTERFACE

#else

void init_output(int jstart, const char *out_dir, const char *out_fn,
                   int MaxLayers, REALTYPE Longitude, REALTYPE Latitude, int lkn);
void write_output(int jday, int iclock, int nsave, int stepnum);
void write_diags(int jday, REALTYPE LakeNum);
void write_outflow(int of_idx, int jday, REALTYPE vol);
void close_output(void);

// for FORTRAN
void init_output_(int *jstart, const char *out_dir, int *dlen, const char *out_fn, int *flen,
                   int *MaxLayers, REALTYPE *Longitude, REALTYPE *Latitude, int *lkn);
void write_output_(int *jday, int *iclock, int *nsave, int *stepnum, REALTYPE *LakeNum);
void close_output_();

#endif

#endif
