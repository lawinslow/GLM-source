!###############################################################################
!#                                                                             #
!#  glm_types.F90                                                              #
!#                                                                             #
!#  A module to define constants and types.                                    #
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
#include "glm.h"

!*******************************************************************************
MODULE glm_types

   USE ISO_C_BINDING

   IMPLICIT NONE

!===============================================================================
!GLOBAL CONSTANTS

   AED_REAL,PARAMETER :: missing = MISVAL

!===============================================================================
!TYPE DECLARATIONS

   !#===========================================================#!
   TYPE,BIND(C) :: StringT
      CINTEGER :: Len
      CCHARACTER :: S(40)
   END TYPE StringT

   !#===========================================================#!
   !# Structured type for key global lake environmental vars
   !# A Lake will be an allocated array of MaxLayers of these
   TYPE,BIND(C) :: LakeDataType
      AED_REAL :: Density          !# density kg/m3
      AED_REAL :: Temp             !# temperature
      AED_REAL :: Salinity         !# salinity
      AED_REAL :: Height           !# 1-D depth array
      AED_REAL :: MeanHeight       !# Mean depth of a layer
      AED_REAL :: LayerVol         !# volume of layer
      AED_REAL :: LayerArea        !# area of layer

      AED_REAL :: Light            !# solar radiation over water layer depths
      AED_REAL :: ExtcCoefSW       !# light extinction coefficient

      AED_REAL :: Vol1
      AED_REAL :: Epsilon

      AED_REAL :: Umean            !# Mean velocity
      AED_REAL :: Uorb             !# Orbital velocity
      AED_REAL :: Ucur             !# Current velocity
      AED_REAL :: LayerStress      !# Layer Stress
   END TYPE LakeDataType

#if 1
   !#===========================================================#!
   !# Structured type for Met vars
   TYPE,BIND(C) :: MetDataType
      AED_REAL :: Rain             !# rainfall
      AED_REAL :: RelHum           !# relative humidty
      AED_REAL :: SatVapDef        !# vapour pressure
      AED_REAL :: LongWave         !# longwave radiation
      AED_REAL :: ShortWave        !# shortwave radiation
      AED_REAL :: AirTemp          !# temperature
      AED_REAL :: WindSpeed        !# windspeed
      AED_REAL :: Snow             !# snowfall
      AED_REAL :: RainConcPO4      !# Concentration of PO4 in rain
      AED_REAL :: RainConcTp       !# Concentration of TP in rain
      AED_REAL :: RainConcNO3      !# Concentration of NO3 in rain
      AED_REAL :: RainConcNH4      !# Concentration of NH4 in rain
      AED_REAL :: RainConcTn       !# Concentration of TN in rain
      AED_REAL :: RainConcSi       !# Concentration of SI in rain
   END TYPE MetDataType

   !#===========================================================#!
   !# Structured type for Surface Data vars
   TYPE,BIND(C) :: SurfaceDataType
      AED_REAL :: Evap             !# Evaporation
      AED_REAL :: HeightBlackIce   !# height of ice layer
      AED_REAL :: HeightWhiteIce   !# height of white ice layer
      AED_REAL :: HeightSnow       !# height of snow layer
      AED_REAL :: dHt              !# change in thickness of either the snow or ice layer
      AED_REAL :: RhoSnow          !# Density of snow layer in kg/m^3
      AED_REAL :: dailyEvap        !# Daily Evaporation (ML/day)
      AED_REAL :: dailyRain        !# Daily Rain (ML/day)
      AED_REAL :: dailySnow        !# Daily Snow (ML/day)
      AED_REAL :: dailyQsw         !# Daily Short Wave Radiation (J/day)
      AED_REAL :: dailyQe          !# Daily Latent Heat(J/day)
      AED_REAL :: dailyQh          !# Daily Sensible Heat (J/day)
      AED_REAL :: dailyQlw         !# Daily Long Wave Radiation (J/day)
      AED_REAL :: dailyInflow      !# Total Daily Inflow (ML/day)
      AED_REAL :: dailyOutflow     !# Total Daily Outflow (ML/day)
      AED_REAL :: dailyOverflow    !# Total Daily Overflow (ML/day)
      AED_REAL :: albedo           !# Daily surface albedo
   END TYPE SurfaceDataType
#endif

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


CONTAINS

!#
!# These are 2 useful routines for converting between fortran and C strings
!# They are in here because, well, I guess they are sort of type conversions
!#

!###############################################################################
SUBROUTINE make_string(s1,s2,len)
   CHARACTER(len=*),INTENT(out) :: s1
   CHARACTER,INTENT(in) :: s2(*)
   INTEGER,INTENT(in)   :: len
!LOCALS
   INTEGER :: i
!
!-------------------------------------------------------------------------------
!BEGIN
   s1 = ''
   DO i=1,len
      s1 = s1 // " "
      s1(i:i) = s2(i)
   ENDDO
END SUBROUTINE make_string
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
FUNCTION make_c_string(s1,s2) RESULT(len)
   CCHARACTER,INTENT(out) :: s1(*)
   CHARACTER(len=*),INTENT(in) :: s2
!LOCALS
   INTEGER :: i
   INTEGER :: len
!
!-------------------------------------------------------------------------------
!BEGIN
   len = len_trim(s2)
   DO i=1,len
      s1(i) = s2(i:i)
   ENDDO
   s1(len+1) = ACHAR(0)
END FUNCTION make_c_string
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END MODULE glm_types
