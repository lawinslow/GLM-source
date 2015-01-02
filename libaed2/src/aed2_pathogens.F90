!###############################################################################
!#                                                                             #
!# aed2_pathogens.F90                                                          #
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
!# Created July 2012                                                           #
!#                                                                             #
!###############################################################################

#include "aed2.h"


MODULE aed2_pathogens
!-------------------------------------------------------------------------------
!  aed2_pathogens --- pathogen biogeochemical model
!-------------------------------------------------------------------------------
   USE aed2_core
   USE aed2_util,ONLY : find_free_lun

   IMPLICIT NONE

   PRIVATE   ! By default make everything private
!
   PUBLIC aed2_pathogens_data_t
!



   TYPE pathogen_nml_data
      CHARACTER(64) :: p_name
      AED_REAL      :: coef_grwth_uMAX                     !-- Max growth rate at 20C
      AED_REAL      :: coef_grwth_Tmin, coef_grwth_Tmax    !-- Tmin and Tmax, f(T)
      AED_REAL      :: coef_grwth_T1, coef_grwth_T2        !-- coef_grwth_T1  and  coef_grwth_T2
      AED_REAL      :: coef_grwth_Kdoc                     !-- Half-saturation for growth, coef_grwth_Kdoc
      AED_REAL      :: coef_grwth_ic                       !-- coef_grwth_ic
      AED_REAL      :: coef_mort_kd20                      !-- Mortality rate (Dark death rate) @ 20C and 0 psu
      AED_REAL      :: coef_mort_theta                     !-- Temperature multiplier for mortality: coef_mort_theta
      AED_REAL      :: coef_mort_c_SM, coef_mort_alpha, coef_mort_beta  !-- Salinity effect on mortality
      AED_REAL      :: coef_mort_c_PHM, coef_mort_K_PHM, coef_mort_delta_M  !-- pH effect on mortality
      AED_REAL      :: coef_mort_fdoc                      !-- Fraction of mortality back to doc
      AED_REAL      :: coef_light_kb_vis, coef_light_kb_uva, coef_light_kb_uvb !-- Light inactivation
      AED_REAL      :: coef_light_cSb_vis, coef_light_cSb_uva, coef_light_cSb_uvb !-- Salinity effect on light inactivation
      AED_REAL      :: coef_light_kDOb_vis, coef_light_kDOb_uva, coef_light_kDOb_uvb !-- DO effect on light
      AED_REAL      :: coef_light_cpHb_vis, coef_light_cpHb_uva, coef_light_cpHb_uvb !-- pH effect on light inactivation
      AED_REAL      :: coef_light_KpHb_vis, coef_light_KpHb_uva, coef_light_KpHb_uvb !-- pH effect on light inactivation
      AED_REAL      :: coef_light_delb_vis, coef_light_delb_uva, coef_light_delb_uvb !-- exponent for pH effect on light inactivation
      AED_REAL      :: coef_pred_kp20, coef_pred_theta_P   !-- Loss rate due to predation and temp multiplier
      AED_REAL      :: coef_sett_fa                        !-- Attached fraction in water column
      AED_REAL      :: coef_sett_w_path      !-- Sedimentation velocity (m/d) at 20C (-ve means down) for NON-ATTACHED orgs
   END TYPE

!  TYPE pathogen_data
!     ! General Attributes
!     TYPE(pathogen_nml_data) :: par
!  END TYPE

   TYPE,extends(aed2_model_data_t) :: aed2_pathogens_data_t
      !# Variable identifiers
      INTEGER,ALLOCATABLE :: id_p(:)
      INTEGER  :: id_growth, id_mortality, id_sunlight, id_grazing
      INTEGER  :: id_par, id_tem, id_sal
      INTEGER  :: id_oxy, id_pH,  id_doc, id_tss
      INTEGER  :: id_I_0

      !# Model parameters
      INTEGER  :: num_pathogens
      TYPE(pathogen_nml_data),DIMENSION(:),ALLOCATABLE :: pathogens
      LOGICAL                                   :: do_Pexc, do_Nexc, do_Cexc, do_Siexc
      INTEGER  :: nnup, npup
      AED_REAL :: dic_per_n

     CONTAINS
         PROCEDURE :: define            => aed2_define_pathogens
         PROCEDURE :: calculate         => aed2_calculate_pathogens
         PROCEDURE :: calculate_benthic => aed2_calculate_benthic_pathogens
!        PROCEDURE :: mobility          => aed2_mobility_pathogens
!        PROCEDURE :: light_extinction  => aed2_light_extinction_pathogens
!        PROCEDURE :: delete            => aed2_delete_pathogens

   END TYPE

   AED_REAL, parameter :: secs_pr_day = 86400.

!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed2_define_pathogens(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the pathogen biogeochemical model
!
!  Here, the aed2_p_m namelist is read and te variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed2_pathogens_data_t),INTENT(inout) :: data

!
!LOCALS
   INTEGER  :: status

   INTEGER  :: num_pathogens
   INTEGER  :: the_pathogens(MAX_PATHO_TYPES)


   NAMELIST /aed2_pathogens/ num_pathogens, the_pathogens
!-----------------------------------------------------------------------
!BEGIN
   ! Read the namelist
   read(namlst,nml=aed2_pathogens,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed2_pathogens'

   ! Store parameter values in our own derived type
   ! NB: all rates must be provided in values per day,
   ! and are converted here to values per second.
   CALL aed2_pathogens_load_params(data, num_pathogens, the_pathogens)

   ! Register state dependancies
!  data%do_Pexc = p_excretion_target_variable .NE. ''
!  IF (data%do_Pexc) THEN
!    data%id_Pexctarget  = data%register_state_dependency(p_excretion_target_variable)
!  ENDIF


   ! Register environmental dependencies
   data%id_tem = aed2_locate_global('temperature')
   data%id_sal = aed2_locate_global('salinity')
   data%id_par = aed2_locate_global('par')
   data%id_I_0 = aed2_locate_global_sheet('par_sf')

END SUBROUTINE aed2_define_pathogens
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!###############################################################################
SUBROUTINE aed2_pathogens_load_params(data, count, list)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_pathogens_data_t),INTENT(inout) :: data
   INTEGER,INTENT(in) :: count
   INTEGER,INTENT(in) :: list(*)
!
!LOCALS
   INTEGER  :: status

   INTEGER  :: i,tfil
   AED_REAL :: minPath

   TYPE(pathogen_nml_data) :: pd(MAX_PATHO_TYPES)
   NAMELIST /pathogen_data/ pd
!-------------------------------------------------------------------------------
!BEGIN
    minPath = 1e-10
    tfil = find_free_lun()
    open(tfil,file="aed2_pathogen_pars.nml", status='OLD',iostat=status)
    IF (status /= 0) STOP 'Error opening namelist pathogen_data'
    read(tfil,nml=pathogen_data,iostat=status)
    IF (status /= 0) STOP 'Error reading namelist pathogen_data'
    close(tfil)

    data%num_pathogens = count
    ALLOCATE(data%pathogens(count))
    ALLOCATE(data%id_p(count))
    DO i=1,count
       ! Assign parameters from database to simulated groups
       !data%pathogens(i)%p_name       = pd(list(i))%p_name
       data%pathogens(i)          = pd(list(i))

       ! Register group as a state variable
       data%id_p(i) = aed2_define_variable(                        &
                             TRIM(data%pathogens(i)%p_name),                  &
                             'mmol/m**3', 'pathogen',                         &
                             minPath,                                         &
                            ! pd(list(i))%p_initial,                          &
                             minimum=minPath,                                 &
                             !minimum=pd(list(i))%p0,                         &
                             mobility = data%pathogens(i)%coef_sett_w_path)


!      IF (data%pathogens(i)%p_name == 'crypto') THEN
!         ! Register IN group as a state variable
!         data%id_in(i) = data%register_state_variable(                       &
!                             TRIM(data%pathogens(i)%p_name)//'_dd',          &
!                             'mmol/m**3', 'pathogen dd',                     &
!                             0.0,                                            &
!                             0.0,                                            &
!                             mobility = data%pathogens(i)%coef_sett_w_path)

!      ENDIF
    ENDDO
END SUBROUTINE aed2_pathogens_load_params
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_pathogens(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Right hand sides of pathogen biogeochemical model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_pathogens_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL           :: pth
   AED_REAL           :: temp,par,Io,salinity
   AED_REAL           :: growth,light,mortality, predation
   AED_REAL           :: f_AOC,f_pH,f_DO,phi,lightBW,phstar

   INTEGER  :: pth_i,c

!-------------------------------------------------------------------------------
!BEGIN

   ! Retrieve current environmental conditions.
   temp = _STATE_VAR_(data%id_tem)    ! local temperature
   salinity = _STATE_VAR_(data%id_sal)! local salinity
   par = _STATE_VAR_(data%id_par)     ! local photosynthetically active radiation
   Io = _STATE_VAR_S_(data%id_I_0)      ! surface short wave radiation
   !doc = _STATE_VAR_(data%id_doc)    ! local DOC
   !oxy = _STATE_VAR_(data%id_oxy)    ! local oxygen
   !ph = _STATE_VAR_(data%id_ph)      ! local pH
   phstar = 0.0 !abs(ph-7.)


   DO pth_i=1,data%num_pathogens

      ! Retrieve this pathogen group
      pth = _STATE_VAR_(data%id_p(pth_i))

      growth    = zero_
      predation = zero_

      ! Natural mortality (as impacted by T, S, pH)
      f_AOC = 1.0 ! aoc / (K_AOC + aoc)
      f_pH  = 1.0 ! + c_PH * ( pH_star**delta / (pH_star**delta+K_PH**delta) )
      mortality = data%pathogens(pth_i)%coef_mort_kd20/86400.  &
                + (data%pathogens(pth_i)%coef_mort_c_SM*salinity**data%pathogens(pth_i)%coef_mort_alpha) &
                * ((1.0-f_AOC)**data%pathogens(pth_i)%coef_mort_beta) * f_pH
      mortality = mortality * (data%pathogens(pth_i)%coef_mort_theta**(temp-20.0))


      ! Sunlight inactivation (as impacted by S, DO and pH)
      light     = zero_
      lightBW   = zero_
      phi  = 1e-6  ! Convert J to MJ as kb is in m2/MJ)
      ! Visible
      f_DO = 1.0 !oxy / (coef_light_kDOb_vis + oxy)
      f_pH = 1.0 !(1.0 + coef_light_cpHb_vis*(pH_star**coef_light_delb_vis / (coef_light_KpHb_vis**coef_light_delb_vis+pH_star**coef_light_delb_vis)))
      lightBW = phi * (data%pathogens(pth_i)%coef_light_kb_vis + data%pathogens(pth_i)%coef_light_cSb_vis*salinity)
      lightBW = lightBW * par * f_pH * f_DO
      light     = light + lightBW
      ! UV-A
      f_DO = 1.0 !oxy / (coef_light_kDOb_uva + oxy)
      f_pH = 1.0 !(1.0 + coef_light_cpHb_uva*(pH_star**coef_light_delb_uva / (coef_light_KpHb_uva**coef_light_delb_uva+pH_star**coef_light_delb_uva)))
      lightBW = phi * (data%pathogens(pth_i)%coef_light_kb_uva + data%pathogens(pth_i)%coef_light_cSb_uva*salinity)
      lightBW = lightBW * (par*0.03) * f_pH * f_DO
      light     = light + lightBW
      ! UV-B
      f_DO = 1.0 !oxy / (coef_light_kDOb_uvb + oxy)
      f_pH = 1.0 !(1.0 + coef_light_cpHb_uvb*(pH_star**coef_light_delb_uvb / (coef_light_KpHb_uvb**coef_light_delb_uvb+pH_star**coef_light_delb_uvb)))
      lightBW = phi * (data%pathogens(pth_i)%coef_light_kb_uvb + data%pathogens(pth_i)%coef_light_cSb_uvb*salinity)
      lightBW = lightBW * (par*0.003) * f_pH * f_DO
      light     = light + lightBW

      !-----------------------------------------------------------------
      ! SET TEMPORAL DERIVATIVES FOR ODE SOLVER

      ! Pathogen production / losses
      _FLUX_VAR_(data%id_p(pth_i)) = _FLUX_VAR_(data%id_p(pth_i)) + ( (growth - light - mortality - predation)*pth )

   ENDDO


END SUBROUTINE aed2_calculate_pathogens
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_benthic_pathogens(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic sedimentation of pathogen.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_pathogens_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL :: pth        ! State
   INTEGER  :: pth_i
   AED_REAL :: pth_flux

   ! Parameters
!
!-------------------------------------------------------------------------------
!BEGIN


   DO pth_i=1,data%num_pathogens
      ! Retrieve current (local) state variable values.
      pth = _STATE_VAR_(data%id_p(pth_i))! pathogen

      pth_flux = zero_  !data%pathogens(pth_i)%w_p*MAX(pth,zero_)

     ! Set bottom fluxes for the pelagic (change per surface area per second)
     ! Transfer sediment flux value to AED2.
     _FLUX_VAR_(data%id_p(pth_i)) = _FLUX_VAR_(data%id_p(pth_i)) + (pth_flux)

   ENDDO

END SUBROUTINE aed2_calculate_benthic_pathogens
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_pathogens
