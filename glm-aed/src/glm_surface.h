/******************************************************************************
 *                                                                            *
 * glm_surface.h                                                              *
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
#ifndef _GLM_SURFACE_H_
#define _GLM_SURFACE_H_


#if _FORTRAN_VERSION_
!###############################################################################

  INTERFACE

     SUBROUTINE do_surface_thermodynamics(jday,iclock,lwInd,Latitude, &
                            SWOld,SWNew) BIND(C,name="do_surface_thermodynamics_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: jday
        CINTEGER,INTENT(in) :: iclock
        CINTEGER,INTENT(in) :: lwInd !# type of longwave radiation
        REALTYPE,INTENT(in) :: Latitude
        REALTYPE,INTENT(in) :: SWOld !# Total solar radiation at the surface for yesterday
        REALTYPE,INTENT(in) :: SWNew !# Total solar radiation at the surface for today
     END SUBROUTINE do_surface_thermodynamics

  END INTERFACE

  CLOGICAL,PUBLIC,BIND(C,name="ice") :: ice  !# flag that tells if there is ice formation

! REALTYPE,PUBLIC,BIND(C, name="QSW") :: QSW     !# Solar radiation at water surface
! REALTYPE,PUBLIC,BIND(C, name="XCO") :: XCO
! REALTYPE,PUBLIC,BIND(C, name="XEV") :: XEV
! REALTYPE,PUBLIC,BIND(C, name="XLW") :: XLW


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void do_surface_thermodynamics(int jday, int iclock, int lwInd,
                                  REALTYPE Latitude, REALTYPE SWOld, REALTYPE SWNew);
void do_surface_thermodynamics_(int *jday, int *iclock, int *lwInd,
                                  REALTYPE *Latitude, REALTYPE *SWOld, REALTYPE *SWNew);

/*============================================================================*/
#endif


#endif
