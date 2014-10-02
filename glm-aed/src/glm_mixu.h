/******************************************************************************
 *                                                                            *
 * glm_mixu.h                                                                 *
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
#ifndef _GLM_MIXU_H_
#define _GLM_MIXU_H_

#if _FORTRAN_VERSION_

INTERFACE

     SUBROUTINE add_this_layer(VMbig,VMsml,Tbig,Tsml,Sbig,Ssml,VMLOC,TMLOC,SMLOC,DFLOC,idx) BIND(C, name="add_this_layer_")
        USE ISO_C_BINDING
        REALTYPE,intent(inout) :: VMbig
        REALTYPE,intent(inout) :: VMsml
        REALTYPE,intent(inout) :: Tbig
        REALTYPE,intent(inout) :: Tsml
        REALTYPE,intent(inout) :: Sbig
        REALTYPE,intent(inout) :: Ssml
        REALTYPE,intent(inout) :: VMLOC
        REALTYPE,intent(inout) :: TMLOC
        REALTYPE,intent(inout) :: SMLOC
        REALTYPE,intent(inout) :: DFLOC
        CINTEGER,intent(in)    :: idx
     END SUBROUTINE add_this_layer

     SUBROUTINE average_layer(j1,k1,MeanTemp,MeanSalt,Dens) BIND(C, name="average_layer_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(inout) :: j1, k1
        REALTYPE,INTENT(in)    :: MeanTemp,MeanSalt,Dens
     END SUBROUTINE average_layer

     SUBROUTINE resize_internals(icode,lnu) BIND(C, name="resize_internals_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: icode, lnu
     END SUBROUTINE resize_internals

END INTERFACE

#else

/******************************************************************************/

void add_this_layer(REALTYPE *VMbig, REALTYPE *VMsml, REALTYPE *Tbig, REALTYPE *Tsml,
                    REALTYPE *Sbig, REALTYPE *Ssml, REALTYPE *VMLOC, REALTYPE *TMLOC,
                    REALTYPE *SMLOC, REALTYPE *DFLOC, int idx);
void average_layer(int *j1, int *k1,
                           REALTYPE MeanTemp, REALTYPE MeanSalt, REALTYPE Dens);
void resize_internals(int icode, int lnu);


void add_this_layer_(REALTYPE *VMbig, REALTYPE *VMsml, REALTYPE *Tbig, REALTYPE *Tsml,
                     REALTYPE *Sbig, REALTYPE *Ssml, REALTYPE *VMLOC, REALTYPE *TMLOC,
                     REALTYPE *SMLOC, REALTYPE *DFLOC, int *idx);
void average_layer_(int *j1, int *k1,
                        REALTYPE *MeanTemp, REALTYPE *MeanSalt, REALTYPE *Dens);
void resize_internals_(int *icode, int *lnu);

#endif

#endif
