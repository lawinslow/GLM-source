/******************************************************************************
 *                                                                            *
 * glm_util.h                                                                 *
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
#ifndef _GLM_UTIL_H_
#define _GLM_UTIL_H_


#ifdef _FORTRAN_VERSION_

  INTERFACE

#ifndef sqr
     REALTYPE FUNCTION sqr(x)
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: x
     END FUNCTION
#endif
#ifndef gprime
     REALTYPE FUNCTION gprime(d1,d2)
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: d1,d2
     END FUNCTION
#endif
#ifndef combine_vol
     REALTYPE FUNCTION combine_vol(c1,v1,c2,v2) BIND(C, name="combine_vol_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: c1,v1,c2,v2
     END FUNCTION combine_vol
#endif
     REALTYPE FUNCTION dv_1(z1,z2,da,avdel,hh,delt,delb) BIND(C, name="dv_1_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: z1,z2,da,avdel,hh,delt,delb
     END FUNCTION dv_1
     REALTYPE FUNCTION calculate_density(Temp, Salt) BIND(C, name="calculate_density_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: Temp,Salt
     END FUNCTION calculate_density
     REALTYPE FUNCTION combine(c1, v1, d1, c2, v2, d2) BIND(C, name="combine_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: c1,c2,d1,d2,v1,v2
     END FUNCTION combine
     REALTYPE FUNCTION saturated_vapour(temp) BIND(C, name="saturated_vapour_")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: temp
     END FUNCTION saturated_vapour


  END INTERFACE

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

   #ifndef sqr
   REALTYPE sqr(REALTYPE x);
   REALTYPE sqr_(REALTYPE *x);
   #endif

   #ifndef gprime
   REALTYPE gprime(REALTYPE d1, REALTYPE d2);
   REALTYPE gprime_(REALTYPE *d1, REALTYPE *d2);
   #endif

   REALTYPE dv_1(REALTYPE z1, REALTYPE z2, REALTYPE da, REALTYPE avdel,
                 REALTYPE hh, REALTYPE delt, REALTYPE delb);
   REALTYPE dv_1_(REALTYPE *z1, REALTYPE *z2, REALTYPE *da, REALTYPE *avdel,
                                REALTYPE *hh, REALTYPE *delt, REALTYPE *delb);

   REALTYPE combine(REALTYPE c1, REALTYPE v1, REALTYPE d1,
                    REALTYPE c2, REALTYPE v2, REALTYPE d2);
   REALTYPE combine_(REALTYPE *c1, REALTYPE *v1, REALTYPE *d1,
                     REALTYPE *c2, REALTYPE *v2, REALTYPE *d2);

   #ifndef combine_vol
   REALTYPE combine_vol(REALTYPE c1, REALTYPE v1, REALTYPE c2, REALTYPE v2);
   REALTYPE combine_vol_(REALTYPE *c1, REALTYPE *v1, REALTYPE *c2, REALTYPE *v2);
   #endif

   REALTYPE calculate_density(REALTYPE temp, REALTYPE salt);
   REALTYPE calculate_density_(REALTYPE *temp, REALTYPE *salt);
   REALTYPE saturated_vapour(REALTYPE temp);
   REALTYPE saturated_vapour_(REALTYPE *temp);

   int internal_var(const char *name);

#endif

#endif
