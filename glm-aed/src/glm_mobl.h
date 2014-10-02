/******************************************************************************
 *                                                                            *
 * glm_mobl.h                                                                 *
 *                                                                            *
 * code for sedimentation - this version modified from gotm - to be rewritten *
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
#ifndef _GLM_MOBL_H_
#define _GLM_MOBL_H_

#include "glm.h"

#if _FORTRAN_VERSION_
!###############################################################################

  INTERFACE


    SUBROUTINE Mobility(N,dt,h,A,ww,min_C,cc) BIND(C, name="Mobility")
       USE ISO_C_BINDING
       CINTEGER,INTENT(in)     :: N       !# number of vertical layers
       REALTYPE,INTENT(in)     :: dt      !# time step (s)
       REALTYPE,INTENT(in)     :: h(*)    !# layer thickness (m)
       REALTYPE,INTENT(in)     :: A(*)    !# layer areas (m2)
       REALTYPE,INTENT(in)     :: ww(*)   !# vertical speed (m/s)
       REALTYPE,INTENT(in)     :: min_C   !# minimum allowed cell concentration
       REALTYPE,INTENT(inout)  :: cc(*)   !# cell concentration
    END SUBROUTINE Mobility

  END INTERFACE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else

   void Mobility(int *N, REALTYPE *dt, REALTYPE *h, REALTYPE *A,
                         REALTYPE *ww, REALTYPE *min_C, REALTYPE *cc);

#endif


#endif
