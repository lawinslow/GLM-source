!###############################################################################
!#                                                                             #
!# aed2_oxygen.F90                                                             #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#     School of Earth & Environment                                           #
!# (C) The University of Western Australia                                     #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created May 2011                                                            #
!#                                                                             #
!###############################################################################

#include "aed2.h"

MODULE aed2_oxygen
!-------------------------------------------------------------------------------
! aed2_oxygen --- oxygen biogeochemical model
!
! The AED module oxygen contains equations that describe exchange of
! oxygen across the air/water interface and sediment flux.
!-------------------------------------------------------------------------------
   USE aed2_core

   USE aed2_util,  ONLY: aed2_gas_piston_velocity, aed2_oxygen_sat

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed2_oxygen_data_t
!
   TYPE,extends(aed2_model_data_t) :: aed2_oxygen_data_t
      !# Variable identifiers
      INTEGER  :: id_oxy
      INTEGER  :: id_temp, id_salt, id_depth
      INTEGER  :: id_wind
      INTEGER  :: id_Fsed_oxy
      INTEGER  :: id_oxy_sat !, id_atm_oxy_exch3d
      INTEGER  :: id_atm_oxy_exch
      INTEGER  :: id_sed_oxy

      !# Model parameters
      AED_REAL :: maxdepth,BOD_oxy,SOD_oxy,q_oxy,Fsed_oxy,Ksed_oxy,theta_sed_oxy
      LOGICAL  :: use_LImod_oxygen_model,use_sed_model

     CONTAINS
         PROCEDURE :: define            => aed2_define_oxygen
         PROCEDURE :: calculate_surface => aed2_calculate_surface_oxygen
         PROCEDURE :: calculate         => aed2_calculate_oxygen
         PROCEDURE :: calculate_benthic => aed2_calculate_benthic_oxygen
!        PROCEDURE :: mobility          => aed2_mobility_oxygen
!        PROCEDURE :: light_extinction  => aed2_light_extinction_oxygen
!        PROCEDURE :: delete            => aed2_delete_oxygen

   END TYPE


!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed2_define_oxygen(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the aed2_oxygen model
!
!  Here, the oxygen namelist is read and te variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_oxygen_data_t),INTENT(inout) :: data
   INTEGER,INTENT(in) :: namlst

!
!LOCALS
   INTEGER  :: status

   LOGICAL  :: use_LImod_oxygen_model = .FALSE.
   AED_REAL :: maxdepth = 48.6
   AED_REAL :: BOD_oxy = -1.0
   AED_REAL :: SOD_oxy = -10.0
   AED_REAL :: q_oxy = 1. !non-dimensional exponent [0.5-2]

   AED_REAL :: oxy_initial=300.
   AED_REAL :: oxy_min=0.
   AED_REAL :: oxy_max=nan_
   AED_REAL :: Fsed_oxy = 48.0
   AED_REAL :: Ksed_oxy = 30.0
   AED_REAL :: theta_sed_oxy = 1.0
   CHARACTER(len=64) :: Fsed_oxy_variable=''

   AED_REAL,PARAMETER :: secs_pr_day = 86400.
   NAMELIST /aed2_oxygen/ use_LImod_oxygen_model,maxdepth,BOD_oxy, SOD_oxy,  &
                          q_oxy,oxy_initial,oxy_min,oxy_max,Fsed_oxy,Ksed_oxy,theta_sed_oxy,  &
                         Fsed_oxy_variable
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Read the namelist
   read(namlst,nml=aed2_oxygen,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed2_oxygen'

   ! Store parameter values in our own derived type
   ! NB: all rates must be provided in values per day,
   ! and are converted here to values per second.

   data%maxdepth = maxdepth
   data%BOD_oxy = BOD_oxy/secs_pr_day          !mmol/m^3/s
   data%SOD_oxy = SOD_oxy/secs_pr_day          !mmol/m^2/s
   data%q_oxy = q_oxy
   data%use_LImod_oxygen_model = use_LImod_oxygen_model
   data%Fsed_oxy = Fsed_oxy/secs_pr_day
   data%Ksed_oxy = Ksed_oxy
   data%theta_sed_oxy = theta_sed_oxy
   data%use_sed_model = Fsed_oxy_variable .NE. ''

   ! Register state variables
   data%id_oxy = aed2_define_variable('oxy','mmol/m**3','oxygen',   &
                                    oxy_initial,minimum=oxy_min,maximum=oxy_max)

   ! Register link to external pools

   IF (data%use_sed_model) data%id_Fsed_oxy = aed2_locate_global_sheet(Fsed_oxy_variable)

   ! Register diagnostic variables
   data%id_sed_oxy = aed2_define_sheet_diag_variable(       &
                     'sed_oxy', 'mmol/m**2/d', 'Oxygen sediment flux')
   !data%id_bod_oxy = aed2_define_sheet_diag_variable(       &
   !                  'bod_oxy', 'mmol/m**3/d', 'Oxygen volume sink')

   data%id_atm_oxy_exch = aed2_define_sheet_diag_variable(  &
                     'atm_oxy_exch', 'mmol/m**2/d', 'Oxygen exchange across atm/water interface')

!  data%id_atm_oxy_exch3d = aed2_define_sheet_diag_variable( &
!                    'atm_oxy_exch3d', 'mmol/m**2/d', 'Oxygen exchange across atm/water interface')

   data%id_oxy_sat = aed2_define_sheet_diag_variable(                  &
                     'sat', 'mmol/m**2/d', 'Oxygen saturation')

   ! Register environmental dependencies
   data%id_temp = aed2_locate_global('temperature') ! Temperature (degrees Celsius)
   data%id_salt = aed2_locate_global('salinity') ! Salinity (psu)
!  data%id_pres = aed2_locate_global_sheet('pressure') ! Pressure (dbar = 10 kPa)
   data%id_depth = aed2_locate_global('depth') ! Depth (m)
   data%id_wind = aed2_locate_global_sheet('wind_speed') ! Wind speed at 10 m above surface (m/s)

END SUBROUTINE aed2_define_oxygen
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_surface_oxygen(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Air-water exchange for the aed oxygen model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_oxygen_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Environment
   AED_REAL :: temp, salt, wind

   ! State
   AED_REAL :: oxy

   ! Temporary variables
   AED_REAL :: oxy_atm_flux = zero_
   AED_REAL :: Coxy_air = zero_ !Dissolved oxygen in the air phase
   AED_REAL :: koxy_trans = zero_
   AED_REAL :: windHt !, Tabs
   AED_REAL :: f_pres  = 1.0      ! Pressure correction function only applicable at high altitudes
!
!-------------------------------------------------------------------------------
!BEGIN

   !Get dependent state variables from physical driver
   temp = _STATE_VAR_(data%id_temp)    ! Temperature (degrees Celsius)
   salt = _STATE_VAR_(data%id_salt)    ! Salinity (psu)
   wind = _STATE_VAR_S_(data%id_wind) ! Wind speed at 10 m above surface (m/s)
   windHt = 10.

    ! Retrieve current (local) state variable values.
   oxy = _STATE_VAR_(data%id_oxy)! Concentration of oxygen in surface layer

   koxy_trans = aed2_gas_piston_velocity(windHt,wind,temp,salt)

   ! First get the oxygen concentration in the air phase at interface
   ! Taken from Riley and Skirrow (1974)
   f_pres = 1.0
   Coxy_air = f_pres * aed2_oxygen_sat(salt,temp)

   ! Get the oxygen flux
   oxy_atm_flux = koxy_trans * (Coxy_air - oxy)

   ! Transfer surface exchange value to AED2 (mmmol/m2) converted by driver.
   _FLUX_VAR_T_(data%id_oxy) = oxy_atm_flux

   ! Also store oxygen flux across the atm/water interface as diagnostic variable (mmmol/m2).
   _DIAG_VAR_S_(data%id_atm_oxy_exch) = oxy_atm_flux
   _DIAG_VAR_S_(data%id_oxy_sat) =  Coxy_air

END SUBROUTINE aed2_calculate_surface_oxygen
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_oxygen(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Right hand sides of aed2_oxygen model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS(aed2_oxygen_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
! State
   AED_REAL :: oxy
!LOCALS
   AED_REAL :: temp, depth
! Temporary variables
   AED_REAL :: diff_oxy, vol_oxy, BOD_oxy, area_oxy, SOD_oxy, alpha_oxy
!  AED_REAL,PARAMETER :: secs_pr_day = 86400.

!-------------------------------------------------------------------------------
!BEGIN
   ! Retrieve current (local) state variable values.
   oxy = _STATE_VAR_(data%id_oxy)! oxygen
   ! Retrieve current environmental conditions for the bottom pelagic layer.
   temp = _STATE_VAR_(data%id_temp)      ! local temperature
   !Get dependent state variables from physical driver
   depth = _STATE_VAR_(data%id_depth)    ! local depth (m)

   ! Set temporal derivatives
   IF (data%use_LImod_oxygen_model) THEN
       ! Volume sink dependent on oxygen and temperature
       BOD_oxy = data%BOD_oxy
       vol_oxy = BOD_oxy
!       print *, 'vol_oxy',vol_oxy

       ! Area sink dependent on oxygen and temperature
       SOD_oxy = data%SOD_oxy
       alpha_oxy = data%q_oxy/(data%maxdepth-depth)
       !alpha_oxy = data%q_oxy/(data%maxdepth-abs(data%maxdepth-depth))
       area_oxy = SOD_oxy * alpha_oxy
       _FLUX_VAR_(data%id_oxy) = _FLUX_VAR_(data%id_oxy) + (vol_oxy) + (area_oxy)
!       print *, 'layer_idx area_oxy depth',layer_idx,area_oxy,depth
   ELSE
   diff_oxy = 0.
   _FLUX_VAR_(data%id_oxy) = _FLUX_VAR_(data%id_oxy) + (diff_oxy)
   ENDIF

   ! If an externally maintained pool is present, change the pool according

   ! Export diagnostic variables

END SUBROUTINE aed2_calculate_oxygen
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_benthic_oxygen(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED oxygen.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_oxygen_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Environment
   AED_REAL :: temp !, layer_ht

   ! State
   AED_REAL :: oxy

   ! Temporary variables
   AED_REAL :: oxy_flux, Fsed_oxy

   ! Parameters
!   AED_REAL,PARAMETER :: secs_pr_day = 86400.
!
!-------------------------------------------------------------------------------
!BEGIN

   ! Retrieve current environmental conditions for the bottom pelagic layer.
   temp = _STATE_VAR_(data%id_temp) ! local temperature

    ! Retrieve current (local) state variable values.
   oxy = _STATE_VAR_(data%id_oxy)! oxygen

   IF (data%use_sed_model) THEN
       Fsed_oxy = _STATE_VAR_S_(data%id_Fsed_oxy)
       ! Sediment flux dependent on oxygen and temperature
       oxy_flux = Fsed_oxy * oxy/(data%Ksed_oxy+oxy) * (data%theta_sed_oxy**(temp-20.0))
   ELSE IF (data%use_LImod_oxygen_model) THEN
       oxy_flux = 0
   ELSE
       Fsed_oxy = data%Fsed_oxy
    ! Sediment flux dependent on oxygen and temperature
   oxy_flux = Fsed_oxy * oxy/(data%Ksed_oxy+oxy) * (data%theta_sed_oxy**(temp-20.0))
   ENDIF
  !print *, 'oxy_flux',oxy_flux
   ! Set bottom fluxes for the pelagic (change per surface area per second)
   ! Transfer sediment flux value to AED2.
   _FLUX_VAR_(data%id_oxy) = _FLUX_VAR_(data%id_oxy) + (oxy_flux)

   ! Set sink and source terms for the benthos (change per surface area per second)
   ! Note that this must include the fluxes to and from the pelagic.
   !_FLUX_VAR_B_(data%id_ben_oxy) = _FLUX_VAR_B_(data%id_ben_oxy) + (-oxy_flux)

   ! Also store sediment flux as diagnostic variable.
   _DIAG_VAR_S_(data%id_sed_oxy) = oxy_flux

END SUBROUTINE aed2_calculate_benthic_oxygen
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_oxygen
