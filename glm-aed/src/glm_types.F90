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
   INTEGER,PARAMETER  :: MaxPar=37
   INTEGER,PARAMETER  :: MaxOut=20     !# Maximum number of outflows
   INTEGER,PARAMETER  :: MaxInf=20     !# Maximum number of inflows
!  INTEGER,PARAMETER  :: MaxSto=2500   !# Maximum number of storage table layers
!  INTEGER,PARAMETER  :: MaxLayers=100 !# Maximum number of layers in this sim
!  INTEGER,PARAMETER  :: MaxVars=28    !# Maximum number of variables
   INTEGER,PARAMETER  :: MaxVars=60    !# Maximum number of variables
   INTEGER,PARAMETER  :: MaxDif=MaxVars+2 !# Maximum number of diffusing substances
                                          !# ie: temp, salt and wq vars

   REALTYPE,PARAMETER :: Visc=0.00000114

   REALTYPE,PARAMETER :: zero=0.0
   REALTYPE,PARAMETER :: one=1.0
   REALTYPE,PARAMETER :: two=2.0
   REALTYPE,PARAMETER :: three=3.0
   REALTYPE,PARAMETER :: four=4.0
   REALTYPE,PARAMETER :: five=5.0
   REALTYPE,PARAMETER :: six=6.0
   REALTYPE,PARAMETER :: seven=7.0
   REALTYPE,PARAMETER :: eight=8.0
   REALTYPE,PARAMETER :: nine=9.0
   REALTYPE,PARAMETER :: ten=10.0
   REALTYPE,PARAMETER :: twenty=20.0
   REALTYPE,PARAMETER :: sixty=60.0
   REALTYPE,PARAMETER :: thsnd=1000.0

   REALTYPE,PARAMETER :: tenM8=1.E-8
   REALTYPE,PARAMETER :: tenM12=1.0E-12

   REALTYPE,PARAMETER :: g=9.81
   REALTYPE,PARAMETER :: Pi=3.14159265358979323846
   REALTYPE,PARAMETER :: PiDeg=180.0

   REALTYPE,PARAMETER :: SecsPerDay=86400

   REALTYPE,PARAMETER :: missing = MISVAL

!===============================================================================
!TYPE DECLARATIONS

   !#===========================================================#!
   TYPE,BIND(C) :: StringT
      CINTEGER :: Len
      CCHARACTER :: S(40)
   END TYPE StringT

   !#===========================================================#!
   !# Structured type for inflow vars
   !# An inflow will be an allocated array of MaxInf of these
   TYPE,BIND(C) :: InflowDataType
      REALTYPE :: Alpha             !# half angle of stream
      REALTYPE :: DragCoeff         !# streambed drag coefficient
      REALTYPE :: Phi               !# streambed slope
      REALTYPE :: FlowRate          !# inflow flow rate
      REALTYPE :: Factor            !# scaling factor for inflow
      REALTYPE :: TemInf            !# inflow temperature
      REALTYPE :: SalInf            !# inflow salinity
      REALTYPE :: Dlwst
      REALTYPE :: HFlow
      REALTYPE :: TotIn
      REALTYPE :: DIIns(MaxPar)     !# inflow density
      REALTYPE :: DDown(MaxPar)     !# downflow density
      REALTYPE :: QIns(MaxPar)      !# inflow volume
      REALTYPE :: QDown(MaxPar)     !# downflow volume
      REALTYPE :: TIns(MaxPar)      !# inflow temperature
      REALTYPE :: TDown(MaxPar)     !# downflow temperature
      REALTYPE :: SIns(MaxPar)      !# inflow salinity
      REALTYPE :: SDown(MaxPar)     !# downflow salinity
      REALTYPE :: DOld(MaxPar)

      REALTYPE :: WQIns(MaxVars,MaxPar)  !# inflow water quality
      REALTYPE :: WQDown(MaxVars,MaxPar) !# downflow water quality
      REALTYPE :: WQInf(MaxVars)

      CINTEGER :: iCnt
      CINTEGER :: NoIns
      CINTEGER :: InPar(MaxPar)
   END TYPE InflowDataType

   !#===========================================================#!
   !# Structured type for outflow vars
   !# An outflow will be an allocated array of MaxOut of these
   TYPE,BIND(C) :: OutflowDataType
      REALTYPE :: OLev        !# distance below surface level
      REALTYPE :: OLen        !# basin length at the outlet
      REALTYPE :: OWid        !# basin width at the outlet
      REALTYPE :: Draw        !# outflow volumes
      REALTYPE :: Factor      !# scaling factor for outflow
      CLOGICAL :: FloatOff    !#
   END TYPE OutflowDataType

   !#===========================================================#!
   !# Structured type for key global lake environmental vars
   !# A Lake will be an allocated array of MaxLayers of these
   TYPE,BIND(C) :: LakeDataType
      REALTYPE :: Density          !# density
      REALTYPE :: Temp             !# temperature
      REALTYPE :: Salinity         !# salinity
      REALTYPE :: Height           !# 1-D depth array
      REALTYPE :: MeanHeight       !# Mean depth of a layer
      REALTYPE :: LayerVol         !# volume of layer
      REALTYPE :: LayerArea        !# area of layer

      REALTYPE :: Light            !# solar radiation over water layer depths
      REALTYPE :: ExtcCoefSW       !# light extinction coefficient

      REALTYPE :: Vol1
      REALTYPE :: Epsilon
   END TYPE LakeDataType

   !#===========================================================#!
   !# Structured type for Met vars
   TYPE,BIND(C) :: MetDataType
      REALTYPE :: Rain             !# rainfall
      REALTYPE :: RelHum           !# relative humidty
      REALTYPE :: SatVapDef        !# vapour pressure
      REALTYPE :: LongWave         !# longwave radiation
      REALTYPE :: ShortWave        !# shortwave radiation
      REALTYPE :: AirTemp          !# temperature
      REALTYPE :: WindSpeed        !# windspeed
      REALTYPE :: Snow             !# snowfall
      REALTYPE :: RainConcPO4      !# Concentration of PO4 in rain
      REALTYPE :: RainConcTp       !# Concentration of TP in rain
      REALTYPE :: RainConcNO3      !# Concentration of NO3 in rain
      REALTYPE :: RainConcNH4      !# Concentration of NH4 in rain
      REALTYPE :: RainConcTn       !# Concentration of TN in rain
      REALTYPE :: RainConcSi       !# Concentration of SI in rain
   END TYPE MetDataType

   !#===========================================================#!
   !# Structured type for Surface Data vars
   TYPE,BIND(C) :: SurfaceDataType
      REALTYPE :: Evap             !# Evaporation
      REALTYPE :: HeightBlackIce   !# height of ice layer
      REALTYPE :: HeightWhiteIce   !# height of white ice layer
      REALTYPE :: HeightSnow       !# height of snow layer
      REALTYPE :: dHt              !# change in thickness of either the snow or ice layer
      REALTYPE :: dailyEvap        !# Daily Evaporation (ML/day)
      REALTYPE :: dailyRain        !# Daily Rain (ML/day)
      REALTYPE :: dailyQsw         !# Daily Short Wave Radiation (J/day)
      REALTYPE :: dailyQe          !# Daily Latent Heat(J/day)
      REALTYPE :: dailyQh          !# Daily Sensible Heat (J/day)
      REALTYPE :: dailyQlw         !# Daily Long Wave Radiation (J/day)
      REALTYPE :: dailyInflow      !# Total Daily Inflow (ML/day)
      REALTYPE :: dailyOutflow     !# Total Daily Outflow (ML/day)
      REALTYPE :: dailyOverflow    !# Total Daily Overflow (ML/day)
   END TYPE SurfaceDataType

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
