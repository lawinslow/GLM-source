/******************************************************************************
 *                                                                            *
 * glm_layers.h                                                               *
 *                                                                            *
 * Contains layer utility routines                                            *
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
#ifndef _GLM_LAYER_H_
#define _GLM_LAYER_H_

#include "glm.h"

#ifdef _FORTRAN_VERSION_
!###############################################################################

  INTERFACE

     SUBROUTINE check_layer_thickness() BIND(C,name="check_layer_thickness")
     END SUBROUTINE check_layer_thickness

     SUBROUTINE insert(q,di,bsl,t,s,wqx,ntims,width,ll) BIND(C,name="insert_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in)    :: Q
        REALTYPE,INTENT(in)    :: DI
        REALTYPE,INTENT(in)    :: BSL
        REALTYPE,INTENT(in)    :: T
        REALTYPE,INTENT(in)    :: S
        REALTYPE,INTENT(in)    :: wqx(*)
        INTEGER,INTENT(in)     :: NTIMS
        REALTYPE,intent(inout) :: WIDTH
        CINTEGER,INTENT(out)   :: ll
     END SUBROUTINE Insert

  END INTERFACE

!===============================================================================
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

   void check_layer_thickness(void);

   void insert(REALTYPE q, REALTYPE di, REALTYPE bsl, REALTYPE temp, REALTYPE salt,
                             REALTYPE *wqx, int ntims, REALTYPE *width, int *ll);

#endif

#endif
