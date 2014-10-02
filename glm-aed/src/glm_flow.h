/******************************************************************************
 *                                                                            *
 * glm_flow.h                                                                 *
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
#ifndef _GLM_FLOW_H_
#define _GLM_FLOW_H_

#include "glm.h"

#ifdef _FORTRAN_VERSION_
!###############################################################################

  INTERFACE

     REALTYPE FUNCTION do_outflows(jday) BIND(C, name="do_outflows_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jday
     END FUNCTION do_outflows

     REALTYPE FUNCTION do_overflow(jday) BIND(C, name="do_overflow_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jday
     END FUNCTION do_overflow

     REALTYPE FUNCTION do_inflows() BIND(C, name="do_inflows_")
        USE ISO_C_BINDING
     END FUNCTION do_inflows

  END INTERFACE

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

  REALTYPE do_outflows(int jday);
  REALTYPE do_overflow(int jday);
  REALTYPE do_inflows(void);

#endif

#endif
