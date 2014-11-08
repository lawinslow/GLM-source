!###############################################################################
!#                                                                             #
!# aed2_phosphorus.F90                                                         #
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
!# Created 24 August 2011                                                      #
!#                                                                             #
!###############################################################################

#include "aed2.h"

!
MODULE aed2_phosphorus
!-------------------------------------------------------------------------------
! aed2_phosphorus --- phosphorus biogeochemical model
!
! The AED module phosphorus contains equations that describe exchange of
! soluable reactive phosphorus across the air/water interface and sediment flux.
!-------------------------------------------------------------------------------
   USE aed2_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed2_phosphorus_data_t
!
   TYPE,extends(aed2_model_data_t) :: aed2_phosphorus_data_t
      !# Variable identifiers
      INTEGER  :: id_frp, id_frpads, id_oxy,  id_tss, id_pH
      INTEGER  :: id_Fsed_frp
      INTEGER  :: id_temp, id_tssext
      INTEGER  :: id_sed_frp

      !# Model parameters
      AED_REAL :: Fsed_frp,Ksed_frp,theta_sed_frp      ! Benthic
      LOGICAL  :: ben_use_oxy,ben_use_aedsed
      AED_REAL :: Kpo4p,Kadsratio,Qmax, w_po4ads       ! Adsorption
      LOGICAL  :: simPO4Adsorption, ads_use_pH, ads_use_external_tss
      INTEGER  :: PO4AdsorptionModel

     CONTAINS
         PROCEDURE :: define            => aed2_define_phosphorus
         PROCEDURE :: calculate_benthic => aed2_calculate_benthic_phosphorus
         PROCEDURE :: equilibrate       => aed2_equilibrate_phosphorus
!        PROCEDURE :: mobility          => aed2_mobility_phosphorus
!        PROCEDURE :: light_extinction  => aed2_light_extinction_phosphorus
!        PROCEDURE :: delete            => aed2_delete_phosphorus

   END TYPE


!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed2_define_phosphorus(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and te variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed2_phosphorus_data_t),INTENT(inout) :: data

!
!LOCALS
   INTEGER  :: status

   AED_REAL          :: frp_initial   = 4.5
   ! Benthic
   AED_REAL          :: Fsed_frp      = 3.5
   AED_REAL          :: Ksed_frp      = 30.0
   AED_REAL          :: theta_sed_frp = 1.05
   CHARACTER(len=64) :: phosphorus_reactant_variable=''
   CHARACTER(len=64) :: Fsed_frp_variable=''
   ! Adsorption
   LOGICAL           :: simPO4Adsorption = .FALSE.
   LOGICAL           :: ads_use_external_tss = .FALSE.
   LOGICAL           :: ads_use_pH = .FALSE.
   AED_REAL          :: Kpo4p = 1.05
   AED_REAL          :: Kadsratio = 1.05
   AED_REAL          :: Qmax = 1.05
   AED_REAL          :: w_po4ads = 0.00
   INTEGER  :: PO4AdsorptionModel = 1
   CHARACTER(len=64) :: po4sorption_target_variable=''

   AED_REAL, parameter :: secs_pr_day = 86400.

   NAMELIST /aed2_phosphorus/ frp_initial,Fsed_frp,Ksed_frp,theta_sed_frp,      &
                             phosphorus_reactant_variable,Fsed_frp_variable,   &
                             simPO4Adsorption,ads_use_external_tss,            &
                             po4sorption_target_variable, PO4AdsorptionModel,  &
                             ads_use_pH,Kpo4p,Kadsratio,Qmax,w_po4ads
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Read the namelist
   read(namlst,nml=aed2_phosphorus,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed2_phosphorus'

   ! Store parameter values in our own derived type
   ! NB: all rates must be provided in values per day,
   ! and are converted here to values per second.
   data%Fsed_frp             = Fsed_frp/secs_pr_day
   data%Ksed_frp             = Ksed_frp
   data%theta_sed_frp        = theta_sed_frp
   data%simPO4Adsorption     = simPO4Adsorption
   data%ads_use_external_tss = ads_use_external_tss
   data%PO4AdsorptionModel   = PO4AdsorptionModel
   data%ads_use_pH           = ads_use_pH
   data%Kpo4p                = Kpo4p
   data%Kadsratio            = Kadsratio
   data%Qmax                 = Qmax
   data%w_po4ads             = w_po4ads/secs_pr_day


   ! Register main state variable
   data%id_frp = aed2_define_variable( 'frp', 'mmol/m**3', 'phosphorus',     &
                                    frp_initial,minimum=zero_)

   ! Register external state variable dependencies (for benthic flux)
   data%ben_use_oxy = phosphorus_reactant_variable .NE. '' !This means oxygen module switched on
   IF (data%ben_use_oxy) THEN
     data%id_oxy = aed2_locate_variable(phosphorus_reactant_variable)
   ENDIF

   data%ben_use_aedsed = Fsed_frp_variable .NE. '' !This means aed sediment module switched on
   IF (data%ben_use_aedsed) THEN
     data%id_Fsed_frp = aed2_locate_global_sheet(Fsed_frp_variable)
   ENDIF

   ! Check if particles and PO4 adsorption are simulated
   IF (data%simPO4Adsorption) THEN
     IF (data%ads_use_external_tss) THEN
         PRINT *,'PO4 adsorption is configured to use external TSS'
         data%id_tssext = aed2_locate_global('tss')
     ELSE
       IF (po4sorption_target_variable .NE. '' ) THEN
          data%id_tss = aed2_locate_variable(po4sorption_target_variable)
       ELSE
          PRINT *,'PO4 adsorption is configured but no internal or external target variable is set'
          STOP
       ENDIF
     ENDIF

     data%id_frpads = aed2_define_variable('frp_ads','mmol/m**3','adsorbed phosphorus',     &
                      zero_,minimum=zero_,mobility=data%w_po4ads)

     IF (data%ads_use_pH) THEN
       data%id_pH = aed2_locate_variable('aed2_carbon_pH')
     ENDIF
   ENDIF

   ! Register diagnostic variables
   data%id_sed_frp = aed2_define_sheet_diag_variable('sed_frp','mmol/m**2/d', &
                                         'Filterable reactive phosphorus')

   ! Register environmental dependencies
   data%id_temp = aed2_locate_global('temperature')
END SUBROUTINE aed2_define_phosphorus
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_equilibrate_phosphorus(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Update partitioning of phosphate between dissolved and particulate pools
! after kinetic transformations are applied
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_phosphorus_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Environment
   AED_REAL :: temp, tss

   ! State
   AED_REAL :: frp,frpads,pH

   ! Temporary variables
   AED_REAL :: SSconc, PO4dis, PO4par, PO4tot, buffer, f_pH, K, Qm

   AED_REAL,PARAMETER :: one_e_neg_ten = 1e-10

!-------------------------------------------------------------------------------
!BEGIN
   IF(.NOT. data%simPO4Adsorption) RETURN


   ! Retrieve current environmental conditions for the cell.
   temp = _STATE_VAR_(data%id_temp) ! local temperature
   IF(data%ads_use_external_tss) THEN
     tss = _STATE_VAR_(data%id_tssext) ! externally supplied total susp solids
   END IF

    ! Retrieve current (local) state variable values.
   frp = _STATE_VAR_(data%id_frp)        ! dissolved PO4
   frpads = _STATE_VAR_(data%id_frpads)  ! adsorped PO4
   IF (.NOT.data%ads_use_external_tss) THEN
      tss = _STATE_VAR_(data%id_tss)      ! local total susp solids
   END IF


   PO4dis   = zero_
   PO4par   = zero_
   buffer   = zero_
   f_pH     = one_
   SSconc   = tss

   ! calculate the total possible PO4 for sorption, and solids
   PO4tot  = MAX(one_e_neg_ten, frp + frpads)  ! Co in Chao (mg)
   SSconc  = MAX(one_e_neg_ten, SSconc )       ! s in Chao  (mg = mol/L * g/mol * mg/g)


   IF(data%PO4AdsorptionModel == 1) THEN
     !-----------------------------------------------------
     ! This is the model for PO4 sorption from Ji 2008:
     !
     ! Ji, Z-G. 2008. Hydrodynamics and Water Quality. Wiley Press.
     !
     PO4par = (data%Kpo4p*SSconc) / (one_+data%Kpo4p*SSconc) * PO4tot
     PO4dis = one_ / (one_+data%Kpo4p*SSconc) * PO4tot
   ELSEIF(data%PO4AdsorptionModel == 2) THEN
     !-----------------------------------------------------
     ! This is the model for PO4 sorption from Chao et al. 2010:
     !
     ! Chao, X. et al. 2010. Three-dimensional numerical simulation of
     !   water quality and sediment associated processes with application
     !   to a Mississippi delta lake. J. Environ. Manage. 91 p1456-1466.
     !
     IF(data%ads_use_pH) THEN
       pH = _STATE_VAR_(data%id_pH)! pH

       IF(pH > 11.) pH = 11.0
       IF(pH < 3.)  pH = 3.0

       ! -0.0094x2 + 0.0428x + 0.9574
       ! (ursula.salmon@uwa.edu.au: fPH for PO4 sorption to Fe in Mine Lakes)
       f_pH = -0.0094*pH*pH + 0.0428*pH + 0.9574

     ELSE

       f_pH = one_

     END IF

     ! calculate particlate fraction based on quadratic solution
     K  = data%Kadsratio
     Qm = data%Qmax

     ! Chao Eq 16
     buffer = SQRT(((PO4tot+(1./K)-(SSconc*Qm*f_pH)))**2. + (4.*f_pH*SSconc*Qm/K))
     PO4par  = 0.5 * ((PO4tot+(1./K)+(SSconc*Qm*f_pH))  - buffer  )

     ! Check for stupid solutions
     IF(PO4par > PO4tot) PO4par = PO4tot
     IF(PO4par < zero_) PO4par = zero_

     ! Now set dissolved portion
     PO4dis = PO4tot - PO4par

   ELSE
     !-----------------------------------------------------
     ! No model is selected
     RETURN
   END IF

   _STATE_VAR_(data%id_frp) = PO4dis       ! Dissolved PO4
   _STATE_VAR_(data%id_frpads) = PO4par    ! Adsorped PO4

END SUBROUTINE aed2_equilibrate_phosphorus
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_benthic_phosphorus(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED phosphorus.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_phosphorus_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Environment
   AED_REAL :: temp

   ! State
   AED_REAL :: frp,oxy

   ! Temporary variables
   AED_REAL :: frp_flux, Fsed_frp

   ! Parameters
   AED_REAL,PARAMETER :: secs_pr_day = 86400.

!-------------------------------------------------------------------------------
!BEGIN

   ! Retrieve current environmental conditions for the bottom pelagic layer.
   temp = _STATE_VAR_(data%id_temp) ! local temperature

    ! Retrieve current (local) state variable values.
   frp = _STATE_VAR_(data%id_frp)! phosphorus

   IF (data%ben_use_aedsed) THEN
      Fsed_frp = _STATE_VAR_S_(data%id_Fsed_frp)
   ELSE
      Fsed_frp = data%Fsed_frp
   ENDIF

   IF (data%ben_use_oxy) THEN
      ! Sediment flux dependent on oxygen and temperature
      oxy = _STATE_VAR_(data%id_oxy)
      frp_flux = Fsed_frp * data%Ksed_frp/(data%Ksed_frp+oxy) * (data%theta_sed_frp**(temp-20.0))
   ELSE
      ! Sediment flux dependent on temperature only.
      frp_flux = Fsed_frp * (data%theta_sed_frp**(temp-20.0))
   ENDIF

   ! TODO:
   ! (1) Get benthic sink and source terms (sccb?) for current environment
   ! (2) Get pelagic bttom fluxes (per surface area - division by layer height will be handled at a higher level)

   ! Set bottom fluxes for the pelagic (change per surface area per second)
   ! Transfer sediment flux value to AED2.
   !_SET_BOTTOM_FLUX_(data%id_frp,frp_flux/secs_pr_day)
   !_SET_SED_FLUX_(data%id_frp,frp_flux)
   _FLUX_VAR_(data%id_frp) = _FLUX_VAR_(data%id_frp) + (frp_flux)

   ! Set sink and source terms for the benthos (change per surface area per second)
   ! Note that this must include the fluxes to and from the pelagic.
   !_FLUX_VAR_B_(data%id_ben_frp) = _FLUX_VAR_B_(data%id_ben_frp) + (-frp_flux/secs_pr_day)

   ! Also store sediment flux as diagnostic variable.
   _DIAG_VAR_S_(data%id_sed_frp) = frp_flux


END SUBROUTINE aed2_calculate_benthic_phosphorus
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_phosphorus
