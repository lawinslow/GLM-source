!###############################################################################
!#                                                                             #
!# glm_zones.F90                                                               #
!#                                                                             #
!# The sediment zone processing bit for WQ                                     #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#     School of Earth & Environment                                           #
!#     The University of Western Australia                                     #
!#                                                                             #
!#     http://aed.see.uwa.edu.au/                                              #
!#                                                                             #
!# Copyright 2013, 2014 -  The University of Western Australia                 #
!#                                                                             #
!#  This file is part of GLM (General Lake Model)                              #
!#                                                                             #
!#  GLM is free software: you can redistribute it and/or modify                #
!#  it under the terms of the GNU General Public License as published by       #
!#  the Free Software Foundation, either version 3 of the License, or          #
!#  (at your option) any later version.                                        #
!#                                                                             #
!#  GLM is distributed in the hope that it will be useful,                     #
!#  but WITHOUT ANY WARRANTY; without even the implied warranty of             #
!#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
!#  GNU General Public License for more details.                               #
!#                                                                             #
!#  You should have received a copy of the GNU General Public License          #
!#  along with this program.  If not, see <http://www.gnu.org/licenses/>.      #
!#                                                                             #
!###############################################################################

#include "aed2.h"

#undef MISVAL
#ifndef _FORTRAN_VERSION_
#define _FORTRAN_VERSION_ 1
#endif

#include "glm.h"

MODULE glm_zones

   USE ISO_C_BINDING

   USE aed2_common
   USE glm_types

   IMPLICIT NONE

   PRIVATE ! By default, make everything private

   AED_REAL,ALLOCATABLE,DIMENSION(:,:),TARGET :: z_cc  ! (nsed_zones, nsed_vars)

   AED_REAL,DIMENSION(:),POINTER :: zz
   AED_REAL,ALLOCATABLE,TARGET,DIMENSION(:) :: z_sed_zones

   AED_REAL,ALLOCATABLE,DIMENSION(:) :: z_pc_wet
   AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: zrad, zsalt, ztemp, zrho, zarea
   AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: zextc_coef, zlayer_stress, ztss

   AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: zdz, zpar, zdepth, zpres
   AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: znir, zuva, zuvb

   INTEGER :: n_zones, w_zones
   AED_REAL,DIMENSION(:),POINTER :: zone_heights
   INTEGER :: nvars, nbenv


   TYPE(LakeDataType),DIMENSION(:),POINTER :: theLake

   PUBLIC n_zones, zone_heights, zz, z_cc, theLake
   PUBLIC wq_set_glm_zones, copy_from_zone, copy_to_zone, calc_zone_areas

   PUBLIC zrad, zsalt, ztemp, zrho, zarea, zextc_coef, zlayer_stress, ztss, zdz, zpar
   PUBLIC zdepth, zpres, z_pc_wet, z_sed_zones
   PUBLIC znir, zuva, zuvb

CONTAINS

!###############################################################################
SUBROUTINE wq_set_glm_zones(z_heights, numZones, numVars, numBenV) BIND(C, name="wq_set_glm_zones")
!-------------------------------------------------------------------------------
!ARGUMENTS
   CINTEGER,INTENT(in) :: numZones, numVars, numBenV
   AED_REAL,TARGET,INTENT(in) :: z_heights(1:numZones)
!
!LOCALS
!  INTEGER :: i
!  AED_REAL :: surf
!
!-------------------------------------------------------------------------------
!BEGIN
   n_zones = numZones
   zone_heights => z_heights
   nvars = numVars
   nbenv = numBenV
   ALLOCATE(z_cc(n_zones, numVars+numBenV))
   ALLOCATE(zrad(n_zones))
   ALLOCATE(zsalt(n_zones))
   ALLOCATE(ztemp(n_zones))
   ALLOCATE(zrho(n_zones))
   ALLOCATE(zarea(n_zones))
   ALLOCATE(zextc_coef(n_zones))
   ALLOCATE(zlayer_stress(n_zones))
   ALLOCATE(ztss(n_zones))
   ALLOCATE(zdz(n_zones))
   ALLOCATE(zpar(n_zones))
   ALLOCATE(znir(n_zones))
   ALLOCATE(zuva(n_zones))
   ALLOCATE(zuvb(n_zones))
   ALLOCATE(zpres(n_zones))
   ALLOCATE(zdepth(n_zones))
   ALLOCATE(z_sed_zones(n_zones))
   ALLOCATE(z_pc_wet(n_zones))
!  zdz(1) = z_dep(1)
!  DO i=2,n_zones
!     zdz(i) = z_dep(i) - z_dep(i-1)
!  ENDDO
!  DO i=1,n_zones ; z_sed_zones(i) = i; ENDDO
   zarea = 0.
END SUBROUTINE wq_set_glm_zones
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE calc_zone_areas(areas, wlev, surf)
!-------------------------------------------------------------------------------
   AED_REAL,DIMENSION(:),INTENT(in) :: areas
   INTEGER,INTENT(in) :: wlev
   AED_REAL :: surf
!
   INTEGER  :: lev, zon
!
!-------------------------------------------------------------------------------
!BEGIN
   zarea = 0.  ; z_pc_wet = 0.
   w_zones = 0 ; zon = 1

   zarea(1) = areas(1)
   DO lev=2, wlev
!print*,"A Lev ", lev, " zz ", zz(lev), " zh ", zone_heights(zon)
      IF ( zz(lev) > zone_heights(zon) ) zon = zon + 1

      zarea(zon) = areas(lev) - areas(lev-1)

      IF ( zone_heights(lev) > surf ) THEN
         IF (w_zones == 0) THEN
            w_zones = lev
            z_pc_wet(zon) = surf / (zone_heights(zon) - zone_heights(zon-1))
         ENDIF
      ELSE
         z_pc_wet(zon) = 1.0
      ENDIF
   ENDDO
   zpres(1:n_zones) = -zone_heights(1:n_zones)
END SUBROUTINE calc_zone_areas
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE copy_from_zone(x_cc, x_diag, x_diag_hz, wlev)
!-------------------------------------------------------------------------------
   AED_REAL,DIMENSION(:,:),INTENT(inout) :: x_cc
   AED_REAL,DIMENSION(:,:),INTENT(inout) :: x_diag
   AED_REAL,DIMENSION(:),INTENT(inout) :: x_diag_hz
   INTEGER,INTENT(in) :: wlev
!
   INTEGER  :: zon, lev, v_start, v_end
   AED_REAL :: scale
!
!-------------------------------------------------------------------------------
!BEGIN
   v_start = nvars+1 ; v_end = nvars+nbenv

   zon = n_zones
   DO lev=wlev,1,-1
      IF ( zon .NE. 1 ) THEN
         IF ( zz(lev-1) < zone_heights(zon-1) ) THEN
            scale = (zone_heights(zon-1) - zz(lev-1)) / (zz(lev) - zz(lev-1))
            x_diag(lev,:) = x_diag(zon,:) * scale
            zon = zon - 1
            x_diag(lev,:) = x_diag(lev,:) + (x_diag(zon,:) * (1.0 - scale))
         ELSE
            x_diag(lev,:) = x_diag(zon,:)
         ENDIF
      ELSE
         x_diag(lev,:) = x_diag(zon,:)
      ENDIF
      x_cc(lev,v_start:v_end) = z_cc(zon,v_start:v_end)
!print*,"< Lev ", lev, " zz ", zz(lev), " zh ", zone_heights(zon)
   ENDDO
!stop
END SUBROUTINE copy_from_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE copy_to_zone(x_cc, wlev)
!-------------------------------------------------------------------------------
   AED_REAL,DIMENSION(:,:),INTENT(in) :: x_cc
   INTEGER,INTENT(in) :: wlev
!
   INTEGER  :: zon, lev
   AED_REAL :: scale, surf
!
!-------------------------------------------------------------------------------
!BEGIN
   zon = 1 ; z_cc = 0.
   zrad = 0. ; zsalt = 0. ; ztemp = 0. ; zrho = 0.
   zextc_coef = 0. ; zlayer_stress = 0. ; ztss = 0. ; zpar = 0.
   znir = 0. ; zuva = 0. ; zuvb = 0. ; z_sed_zones = 1.

   DO lev=1, wlev
!print*,"> Lev ", lev, " zz ", zz(lev), " zh ", zone_heights(zon)
      IF ( zz(lev) > zone_heights(zon) ) THEN
         IF ( lev /= 1 ) THEN
            scale =  ( zone_heights(zon) - zz(lev-1) )
            z_cc(zon,:) = z_cc(zon,:) + (x_cc(lev-1,:) * scale)

            ztemp(zon)         = ztemp(zon) + (theLake(lev)%Temp * scale)
            zsalt(zon)         = zsalt(zon) + (theLake(lev)%Salinity * scale)
            zrho(zon)          = zrho(zon)  + (theLake(lev)%Density * scale)
            zrad(zon)          = zrad(zon)  + (theLake(lev)%Light * scale)
            zextc_coef(zon)    = zextc_coef(zon) + (theLake(lev)%ExtcCoefSW * scale)
            zlayer_stress(zon) = zlayer_stress(zon) + (theLake(lev)%LayerStress * scale)
         ENDIF

         zon = zon + 1

         scale = ( zone_heights(zon) - zz(lev) )
         z_cc(zon,:) = z_cc(zon,:) + (x_cc(lev,:) * scale)

         ztemp(zon)         = ztemp(zon) + (theLake(lev)%Temp * scale)
         zsalt(zon)         = zsalt(zon) + (theLake(lev)%Salinity * scale)
         zrho(zon)          = zrho(zon)  + (theLake(lev)%Density * scale)
         zrad(zon)          = zrad(zon)  + (theLake(lev)%Light * scale)
         zextc_coef(zon)    = zextc_coef(zon) + (theLake(lev)%ExtcCoefSW * scale)
         zlayer_stress(zon) = zlayer_stress(zon) + (theLake(lev)%LayerStress * scale)
      ELSE
         IF ( lev /= 1 ) THEN
            scale = zz(lev) - zz(lev-1)
         ELSE
            scale = zz(lev)
         ENDIF
         z_cc(zon,:) = z_cc(zon,:) + (x_cc(lev,:) * scale)

         ztemp(zon)         = ztemp(zon) + (theLake(lev)%Temp * scale )
         zsalt(zon)         = zsalt(zon) + (theLake(lev)%Salinity * scale )
         zrho(zon)          = zrho(zon)  + (theLake(lev)%Density * scale )
         zrad(zon)          = zrad(zon)  + (theLake(lev)%Light * scale )
         zextc_coef(zon)    = zextc_coef(zon) + (theLake(lev)%ExtcCoefSW * scale )
         zlayer_stress(zon) = zlayer_stress(zon) + (theLake(lev)%LayerStress * scale )
      ENDIF
   ENDDO

   !# now divide by the total depth of the zone.
!  z_cc(1,:) = z_cc(1,:) / zone_heights(1)

   surf = zz(wlev)
   if ( surf > zone_heights(1) ) then
      zdepth(1) = zone_heights(1)
   else
      zdepth = surf
      w_zones = 1
   endif

   scale = zone_heights(1)
   ztemp(1)         = ztemp(1) / scale
   zsalt(1)         = zsalt(1) / scale
   zrho(1)          = zrho(1)  / scale
   zrad(1)          = zrad(1)  / scale
   zextc_coef(1)    = zextc_coef(1) / scale
   zlayer_stress(1) = zlayer_stress(1) / scale

   DO zon=2,n_zones
      z_sed_zones(zon) = zon
      IF ( w_zones == 0 ) THEN
          IF ( surf > zone_heights(zon) ) THEN
             zdepth(zon) = zone_heights(zon)
          ELSE
             zdepth(zon) = surf
             w_zones = zon
          ENDIF

         scale = (zone_heights(zon) - zone_heights(zon-1))
         z_cc(zon,:) = z_cc(zon,:) / scale

         ztemp(zon)         = ztemp(zon) / scale
         zsalt(zon)         = zsalt(zon) / scale
         zrho(zon)          = zrho(zon)  / scale
         zrad(zon)          = zrad(zon)  / scale
         zextc_coef(zon)    = zextc_coef(zon) / scale
         zlayer_stress(zon) = zlayer_stress(zon) / scale
      ELSE
         zdepth(zon) = surf
      ENDIF
   ENDDO
END SUBROUTINE copy_to_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE glm_zones
