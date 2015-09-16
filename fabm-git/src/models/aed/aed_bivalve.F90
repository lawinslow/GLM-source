!###############################################################################
!#                                                                             #
!# aed_bivalve.F90                                                             #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#     School of Earth & Environment                                           #
!# (C) The University of Western Australia                                     #
!#                                                                             #
!# In collaboration with :                                                     #
!#     Cornell University, Biology Department                                  #
!#                                                                             #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created January 2015                                                        #
!#                                                                             #
!###############################################################################

#include "aed.h"

#define _PHYLEN_ 17
#define _PHYMOD_ 'aed_phytoplankton'
#define _OGMPOC_ 'aed_organic_matter_poc'

MODULE aed_bivalve
!-------------------------------------------------------------------------------
!  aed_bivalve --- multi-group bivalve biogeochemical model
!-------------------------------------------------------------------------------
   USE aed_core
   USE aed_util,ONLY : find_free_lun,aed_bio_temp_function,fTemp_function,qsort
!  USE aed_zoop_utils

   IMPLICIT NONE

   PRIVATE   ! By default make everything private
!
   PUBLIC aed_type_bivalve
!
   TYPE type_bivalve_prey
      !State variable name for bivalvelankton prey
      CHARACTER(64) :: bivalve_prey
      !Preference factors for bivalvelankton predators grazing on prey
      AED_REAL      :: Pbiv_prey
   END TYPE type_bivalve_prey


   TYPE type_bivalve_params
      ! General Attributes
      CHARACTER(64) :: name
      AED_REAL :: initial_conc, min
      INTEGER  :: Length

      ! Nutrient parameters
       AED_REAL :: INC, IPC

      ! Growth rate parameters
      AED_REAL :: Rgrz
      AED_REAL :: Ing, WaI, WbI
      AED_REAL :: fassim
      ! Minumum prey concentration parameters
      AED_REAL :: Cmin_grz, Kgrz
      AED_REAL :: minT, Tmin, Tmax, maxT, Dmax, maxD, SSmax, maxSS

      ! Respiration, mortaility and excretion parameters
      AED_REAL :: Rexcr, Regst, gegst, Rresp

      ! Salinity parameters
      INTEGER  :: saltfunc
      AED_REAL :: minS, Smin, Smax, maxS

      AED_REAL :: fR20, War, Wbr, fR

      AED_REAL :: theta_resp
      AED_REAL :: TmaxR, maxTR, Qresp

      AED_REAL :: SDA, Rmort, Rpred
      AED_REAL :: fDO, K_BDO, KDO

      ! The prey
      INTEGER  :: num_prey
      TYPE(type_bivalve_prey) :: prey(MAX_ZOOP_PREY)
   END TYPE

   TYPE,extends(type_bivalve_params) :: type_bivalve_data
      TYPE (type_state_variable_id)  :: id_prey(MAX_ZOOP_PREY)
      TYPE (type_state_variable_id)  :: id_phyIN(MAX_ZOOP_PREY), id_phyIP(MAX_ZOOP_PREY)
   END TYPE


   TYPE,extends(type_base_model) :: aed_type_bivalve
!     Variable identifiers
      TYPE (type_bottom_state_variable_id) :: id_biv(MAX_ZOOP_TYPES)
      TYPE (type_state_variable_id)      :: id_Nexctarget,id_Nmorttarget
      TYPE (type_state_variable_id)      :: id_Pexctarget,id_Pmorttarget
      TYPE (type_state_variable_id)      :: id_Cexctarget,id_Cmorttarget
      TYPE (type_state_variable_id)      :: id_DOupttarget
      TYPE (type_state_variable_id)      :: id_SSupttarget
      TYPE (type_dependency_id)          :: id_tem, id_sal, id_extc
      TYPE (type_horizontal_diagnostic_variable_id) :: id_grz,id_resp,id_mort


!     Model parameters
      INTEGER                                       :: num_biv
      TYPE(type_bivalve_data),DIMENSION(:),ALLOCATABLE :: bivalves
      LOGICAL  :: simDNexcr, simDPexcr, simDCexcr
      LOGICAL  :: simPNexcr, simPPexcr, simPCexcr

      CONTAINS     ! Model Methods
        PROCEDURE :: initialize               => aed_init_bivalve
!       PROCEDURE :: do                       => aed_bivalve_do
        PROCEDURE :: do_ppdd                  => aed_bivalve_do_ppdd
        PROCEDURE :: do_benthos               => aed_bivalve_do_benthos
   END TYPE

   LOGICAL :: debug = .TRUE.

CONTAINS
!===============================================================================


!###############################################################################
SUBROUTINE aed_bivalve_load_params(self, dbase, count, list)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(inout) :: self
   CHARACTER(len=*),INTENT(in) :: dbase
   INTEGER,INTENT(in)          :: count   !Number of bivalve groups
   INTEGER,INTENT(in)          :: list(*) !List of bivalve groups to simulate
!
!LOCALS
   INTEGER  :: status

   INTEGER  :: i,j,tfil,sort_i(MAX_ZOOP_PREY)
   AED_REAL :: Pbiv_prey(MAX_ZOOP_PREY)

   AED_REAL,PARAMETER :: secs_pr_day = 86400.
   TYPE(type_bivalve_params)  :: bivalve_param(MAX_ZOOP_TYPES)
   NAMELIST /bivalve_params/ bivalve_param
!-------------------------------------------------------------------------------
!BEGIN
    tfil = find_free_lun()
    open(tfil,file=dbase, status='OLD',iostat=status)
    IF (status /= 0) STOP 'Error opening bivalves_params namelist file'
    read(tfil,nml=bivalve_params,iostat=status)
    close(tfil)
    IF (status /= 0) STOP 'Error reading namelist bivalves_params'

    self%num_biv = count
    allocate(self%bivalves(count))
    DO i=1,count
       ! General
       self%bivalves(i)%name          = bivalve_param(list(i))%name
       self%bivalves(i)%initial_conc  = bivalve_param(list(i))%initial_conc
       self%bivalves(i)%min           = bivalve_param(list(i))%min
       self%bivalves(i)%Length        = bivalve_param(list(i))%Length
       self%bivalves(i)%INC           = bivalve_param(list(i))%INC
       self%bivalves(i)%IPC           = bivalve_param(list(i))%IPC
       ! Filtration & Ingestion
       self%bivalves(i)%Rgrz          = bivalve_param(list(i))%Rgrz/secs_pr_day
       self%bivalves(i)%Ing           = bivalve_param(list(i))%Ing
       self%bivalves(i)%WaI           = bivalve_param(list(i))%WaI
       self%bivalves(i)%WbI           = bivalve_param(list(i))%WbI
       self%bivalves(i)%fassim        = bivalve_param(list(i))%fassim
       self%bivalves(i)%Cmin_grz      = bivalve_param(list(i))%Cmin_grz
       self%bivalves(i)%Kgrz          = bivalve_param(list(i))%Kgrz
       self%bivalves(i)%minT          = bivalve_param(list(i))%minT
       self%bivalves(i)%Tmin          = bivalve_param(list(i))%Tmin
       self%bivalves(i)%Tmax          = bivalve_param(list(i))%Tmax
       self%bivalves(i)%maxT          = bivalve_param(list(i))%maxT
       self%bivalves(i)%Dmax          = bivalve_param(list(i))%Dmax
       self%bivalves(i)%maxD          = bivalve_param(list(i))%maxD
       self%bivalves(i)%SSmax         = bivalve_param(list(i))%SSmax
       self%bivalves(i)%maxSS         = bivalve_param(list(i))%maxSS
       ! Excretion & Egestion
       self%bivalves(i)%Rexcr         = bivalve_param(list(i))%Rexcr/secs_pr_day
       self%bivalves(i)%Regst         = bivalve_param(list(i))%Regst/secs_pr_day
       self%bivalves(i)%gegst         = bivalve_param(list(i))%gegst
       ! Respiration
       self%bivalves(i)%Rresp         = bivalve_param(list(i))%Rresp/secs_pr_day
       self%bivalves(i)%saltfunc      = bivalve_param(list(i))%saltfunc
       self%bivalves(i)%minS          = bivalve_param(list(i))%minS
       self%bivalves(i)%Smin          = bivalve_param(list(i))%Smin
       self%bivalves(i)%Smax          = bivalve_param(list(i))%Smax
       self%bivalves(i)%maxS          = bivalve_param(list(i))%maxS
       self%bivalves(i)%fR20          = bivalve_param(list(i))%fR20
       self%bivalves(i)%War           = bivalve_param(list(i))%War
       self%bivalves(i)%Wbr           = bivalve_param(list(i))%Wbr
       self%bivalves(i)%fR            = bivalve_param(list(i))%fR
       self%bivalves(i)%theta_resp    = bivalve_param(list(i))%theta_resp
       self%bivalves(i)%TmaxR         = bivalve_param(list(i))%TmaxR
       self%bivalves(i)%maxTR         = bivalve_param(list(i))%maxTR
       self%bivalves(i)%Qresp         = bivalve_param(list(i))%Qresp
       self%bivalves(i)%SDA           = bivalve_param(list(i))%SDA
       ! Mortality
       self%bivalves(i)%Rmort         = bivalve_param(list(i))%Rmort/secs_pr_day
       self%bivalves(i)%Rpred         = bivalve_param(list(i))%Rpred/secs_pr_day
       self%bivalves(i)%fDO           = bivalve_param(list(i))%fDO
       self%bivalves(i)%K_BDO         = bivalve_param(list(i))%K_BDO
       self%bivalves(i)%KDO           = bivalve_param(list(i))%KDO

       self%bivalves(i)%num_prey      = bivalve_param(list(i))%num_prey

       !Loop through prey variables assigning a target variable and preference factor
       !First sort in descending order of food preferences
       DO j=1,self%bivalves(i)%num_prey
          sort_i(j) = j
          Pbiv_prey(j) = bivalve_param(list(i))%prey(j)%Pbiv_prey
       ENDDO
       CALL qsort(Pbiv_prey,sort_i,1,self%bivalves(i)%num_prey)
       DO j=1,self%bivalves(i)%num_prey
          self%bivalves(i)%prey(j)%bivalve_prey = &
                   bivalve_param(list(i))%prey(sort_i(self%bivalves(i)%num_prey-j+1))%bivalve_prey
          self%bivalves(i)%prey(j)%Pbiv_prey = &
                   bivalve_param(list(i))%prey(sort_i(self%bivalves(i)%num_prey-j+1))%Pbiv_prey
       ENDDO

       ! Register group as a state variable
       CALL self%register_state_variable(self%id_biv(i),            &
                              bivalve_param(list(i))%name,          &
                              'mmolC/m**2', 'bivalve',              &
                              bivalve_param(list(i))%initial_conc,  &
                              minimum=bivalve_param(list(i))%min)
    ENDDO
!
END SUBROUTINE aed_bivalve_load_params
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_init_bivalve(self,namlst)
!-------------------------------------------------------------------------------
! Initialise the bivalve biogeochemical model
!
!  Here, the aed_bivalve namelist is read and te variables exported
!  by the model are registered with FABM.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),TARGET,INTENT(inout) :: self
   INTEGER,INTENT(in) :: namlst

!
!LOCALS
   INTEGER  :: status

   INTEGER            :: num_biv
   INTEGER            :: the_biv(MAX_ZOOP_TYPES)

   CHARACTER(len=64)  :: dn_target_variable='' !dissolved nitrogen target variable
   CHARACTER(len=64)  :: pn_target_variable='' !particulate nitrogen target variable
   CHARACTER(len=64)  :: dp_target_variable='' !dissolved phosphorus target variable
   CHARACTER(len=64)  :: pp_target_variable='' !particulate phosphorus target variable
   CHARACTER(len=64)  :: dc_target_variable='' !dissolved carbon target variable
   CHARACTER(len=64)  :: pc_target_variable='' !particulate carbon target variable
   CHARACTER(len=64)  :: do_uptake_variable='' !oxy uptake variable
   CHARACTER(len=128) :: dbase='aed_bivalve_pars.nml'

   AED_REAL,PARAMETER :: secs_pr_day = 86400.
   INTEGER            :: biv_i, prey_i, phy_i

   NAMELIST /aed_bivalve/ num_biv, the_biv, &
                    dn_target_variable, pn_target_variable, dp_target_variable, &
                    pp_target_variable, dc_target_variable, pc_target_variable, &
                    do_uptake_variable, dbase
!-----------------------------------------------------------------------
!BEGIN
!print *,'**** Reading /aed_bivalve/ namelist'
   ! Read the namelist
   read(namlst,nml=aed_bivalve,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed_bivalve'

    self%num_biv = 0
   ! Store parameter values in our own derived type
   ! NB: all rates must be provided in values per day,
   ! and are converted in aed_bivalve_load_params to values per second.
   CALL aed_bivalve_load_params(self, dbase, num_biv, the_biv)


   ! Not required if we use the Spillman quadratic fT
   !CALL aed_bio_temp_function(self%num_biv,                 &
   !                           self%bivalves%theta_resp_zoo, &
   !                           self%bivalves%Tstd_zoo,       &
   !                           self%bivalves%Topt_zoo,       &
   !                           self%bivalves%Tmax_zoo,       &
   !                           self%bivalves%aTn,            &
   !                           self%bivalves%bTn,            &
   !                           self%bivalves%kTn,            &
   !                           self%bivalves%name)


   !Register link to prey state variables
   DO biv_i = 1,num_biv
      phy_i = 0
      DO prey_i = 1,self%bivalves(biv_i)%num_prey
          CALL self%register_state_dependency(self%bivalves(biv_i)%id_prey(prey_i), &
                                       self%bivalves(biv_i)%prey(prey_i)%bivalve_prey)
          !If the prey is phytoplankton then also register state dependency on
          !internal nitrogen and phosphorus
          IF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
              phy_i = phy_i + 1
              CALL self%register_state_dependency(self%bivalves(biv_i)%id_phyIN(phy_i), &
                                       TRIM(self%bivalves(biv_i)%prey(prey_i)%bivalve_prey)//'_IN')
              CALL self%register_state_dependency(self%bivalves(biv_i)%id_phyIP(phy_i), &
                                       TRIM(self%bivalves(biv_i)%prey(prey_i)%bivalve_prey)//'_IP')

          ENDIF
      ENDDO
   ENDDO

   ! Register link to nutrient pools, if variable names are provided in namelist.
   self%simDNexcr = dn_target_variable .NE. ''
   IF (self%simDNexcr) THEN
     CALL self%register_state_dependency(self%id_Nexctarget,dn_target_variable)
   ENDIF
   self%simDPexcr = dp_target_variable .NE. ''
   IF (self%simDPexcr) THEN
     CALL self%register_state_dependency(self%id_Pexctarget,dp_target_variable)
   ENDIF
   self%simDCexcr = dc_target_variable .NE. ''
   IF (self%simDCexcr) THEN
     CALL self%register_state_dependency(self%id_Cexctarget,dc_target_variable)
   ENDIF

   self%simPNexcr = pn_target_variable .NE. ''
   IF (self%simPNexcr) THEN
     CALL self%register_state_dependency(self%id_Nmorttarget,pn_target_variable)
   ENDIF
   self%simPPexcr = pp_target_variable .NE. ''
   IF (self%simPPexcr) THEN
     CALL self%register_state_dependency(self%id_Pmorttarget,pp_target_variable)
   ENDIF
   self%simPCexcr = pc_target_variable .NE. ''
   IF (self%simPCexcr) THEN
     CALL self%register_state_dependency(self%id_Cmorttarget,pc_target_variable)
   ENDIF

   if (do_uptake_variable .EQ. '') STOP 'bivalve needs DO uptake variable'
   CALL self%register_state_dependency(self%id_DOupttarget, do_uptake_variable)

   ! Register diagnostic variables
   CALL self%register_diagnostic_variable(self%id_grz,'grz','mmolC/m**3',  'net bivalve grazing')
   CALL self%register_diagnostic_variable(self%id_resp,'resp','mmolC/m**3',  'net bivalve respiration')
   CALL self%register_diagnostic_variable(self%id_mort,'mort','mmolC/m**3/d','net bivalve mortality')

   ! Register environmental dependencies
   CALL self%register_dependency(self%id_tem,standard_variables%temperature)
   CALL self%register_dependency(self%id_sal,standard_variables%practical_salinity)
   CALL self%register_dependency(self%id_extc,standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux)

END SUBROUTINE aed_init_bivalve
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_bivalve_do_benthos(self,_ARGUMENTS_DO_BOTTOM_)
!-------------------------------------------------------------------------------
! Right hand sides of bivalve biogeochemical model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self
   _DECLARE_ARGUMENTS_DO_BOTTOM_
!
!LOCALS
   AED_REAL           :: biv,temp,salinity,oxy,ss !State variables
   AED_REAL           :: prey(MAX_ZOOP_PREY), grazing_prey(MAX_ZOOP_PREY) !Prey state variables
   AED_REAL           :: phy_INcon(MAX_ZOOP_PREY), phy_IPcon(MAX_ZOOP_PREY) !Internal nutrients for phytoplankton
   AED_REAL           :: dn_excr, dp_excr, dc_excr !Excretion state variables
   AED_REAL           :: pon, pop, poc !Mortaility and fecal pellet state variables
   AED_REAL           :: FGrazing_Limitation, f_Temp, f_Salinity, f_SS, I_max
   AED_REAL           :: pref_factor, Ctotal_prey !total concentration of available prey
   AED_REAL           :: food, grazing, respiration, mortality !Growth & decay functions
   AED_REAL           :: grazing_n, grazing_p !Grazing on nutrients
   AED_REAL           :: pon_excr, pop_excr, poc_excr !POM excretion rates
   AED_REAL           :: don_excr, dop_excr, doc_excr, delta_C !DOM excretion rates
   INTEGER            :: biv_i,prey_i,prey_j,phy_i

   !CAB added
   AED_REAL :: f_dens, W, Imax, psuedofaeces, ingestion, excretion, egestion, iteg, R20


   AED_REAL,PARAMETER :: secs_pr_day = 86400.
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Enter spatial loops (if any)
   _HORIZONTAL_LOOP_BEGIN_

   ! Retrieve current environmental conditions.
   _GET_(self%id_tem,temp)        ! local temperature
   _GET_(self%id_sal,salinity)    ! local salinity
   _GET_(self%id_DOupttarget,oxy) ! local oxygen
! CAB - not used ?
!  _GET_(self%id_SSupttarget,ss)  ! local suspended solids (inorganic)

   ! Retrieve current (local) state variable values.
   IF (self%simDNexcr) _GET_(self%id_Nexctarget, dn_excr)
   IF (self%simDPexcr) _GET_(self%id_Pexctarget, dp_excr)
   IF (self%simDCexcr) _GET_(self%id_Cexctarget, dc_excr)

   IF (self%simPNexcr) _GET_(self%id_Nmorttarget, pon)
   IF (self%simPPexcr) _GET_(self%id_Pmorttarget, pop)
   IF (self%simPCexcr) _GET_(self%id_Cmorttarget, poc)

   DO biv_i=1,self%num_biv

      ! Retrieve this bivalve group
      _GET_HORIZONTAL_(self%id_biv(biv_i),biv)

      grazing       = zero_
      respiration   = zero_
      mortality     = zero_

      !Retrieve prey groups
      Ctotal_prey   = zero_
      DO prey_i=1,self%bivalves(biv_i)%num_prey
         _GET_(self%bivalves(biv_i)%id_prey(prey_i),prey(prey_i))
         Ctotal_prey = Ctotal_prey + prey(prey_i)
      ENDDO

      ! Get the grazing limitation function
       !fGrazing_Limitation = fPrey_Limitation(self%bivalves,biv_i,Ctotal_prey)
       fGrazing_Limitation = min(Ctotal_prey/self%bivalves(biv_i)%Kgrz,one_)


      ! Get the temperature function
       f_Temp = fTemp_function_biv(self,biv_i, temp)
       !f_T = fTemp_function(1, self%bivalves(biv_i)%Tmax,       &
       !                        self%bivalves(biv_i)%Tstd,       &
       !                        self%bivalves(biv_i)%theta_resp, &
       !                        self%bivalves(biv_i)%aTn,        &
       !                        self%bivalves(biv_i)%bTn,        &
       !                        self%bivalves(biv_i)%kTn,        &
       !                        temp)


       ! Get the suspended solids function
       f_SS = fSS_function(self,biv_i,SS)

       ! Get the density limitation function
       f_dens = fD_function(self,biv_i,biv)

      ! Get the final ingestion rate (/ s)
      ! amount grazed in units of mass consumed/mass bivalve/unit time
      IF(self%bivalves(biv_i)%Ing==1) THEN
        W = (0.071/1000.) * self%bivalves(biv_i)%Length**2.8
        Imax = self%bivalves(biv_i)%WaI * W** self%bivalves(biv_i)%WbI
      ELSE
        Imax = self%bivalves(biv_i)%Rgrz
      END IF
      grazing = Imax * fGrazing_Limitation * f_Temp * f_dens * f_SS

      ! Now dertermine available prey and limit grazing amount to availability of prey
      ! food is total amount of food in units of mass/unit volume/unit time
      food = grazing * biv
      IF (Ctotal_prey < self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz ) THEN
          food = zero_
          grazing = zero_
      ELSEIF (food > Ctotal_prey - self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz ) THEN
          food = Ctotal_prey - self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz
          grazing = food / biv
      ENDIF


      ! Now determine prey composition based on preference factors and availability of prey

      ! Prey has been ordered in grazing preference
      ! So take food in order of preference up to availability minus value of minimum residual
      ! grazing_prey is in units of mass consumed/unit volumne/unit time

      DO prey_i = 1,self%bivalves(biv_i)%num_prey
          !Add up preferences for remaining prey
          pref_factor = zero_
          DO prey_j = prey_i,self%bivalves(biv_i)%num_prey
             pref_factor = pref_factor + self%bivalves(biv_i)%prey(biv_i)%Pbiv_prey
          ENDDO
          IF (food * self%bivalves(biv_i)%prey(prey_i)%Pbiv_prey / pref_factor <= &
                        prey(prey_i) - self%bivalves(biv_i)%Cmin_grz) THEN
             !Take fraction of left over food based on preference factor
             grazing_prey(prey_i) = food * self%bivalves(biv_i)%prey(prey_i)%Pbiv_prey / pref_factor
          ELSEIF (prey(prey_i) > self%bivalves(biv_i)%Cmin_grz) THEN
             grazing_prey(prey_i) = prey(prey_i) - self%bivalves(biv_i)%Cmin_grz
          ELSE
             grazing_prey(prey_i) = zero_
          ENDIF
          !Food remaining after grazing from current prey
          food = food - grazing_prey(prey_i)
      ENDDO

      ! Now determine nutrient composition of food based on prey type
      ! At this stage only the AED model state variables have multiple
      ! nutrients (C,N&P) so assume all others have a single nutrient
      ! and thus not need to calculate nutrient excretion as is taken
      ! care of in the respiration term.  22/12/2011
      ! grazing_n is in units of mass N consumed/unit volume/unit time
      ! grazing_p is in units of mass P consumed/unit volume/unit time

      grazing_n = zero_
      grazing_p = zero_
      phy_i = 0
      DO prey_i = 1,self%bivalves(biv_i)%num_prey
         IF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey .EQ. _OGMPOC_) THEN
            IF (poc > zero_) THEN
                grazing_n = grazing_n + grazing_prey(prey_i) * pon/poc
                grazing_p = grazing_p + grazing_prey(prey_i) * pop/poc
            ELSE
                grazing_n = zero_
                grazing_p = zero_
            ENDIF
         ELSEIF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
            phy_i = phy_i + 1
            _GET_(self%bivalves(biv_i)%id_phyIN(phy_i),phy_INcon(phy_i))
            _GET_(self%bivalves(biv_i)%id_phyIP(phy_i),phy_IPcon(phy_i))
            grazing_n = grazing_n + grazing_prey(prey_i) / prey(prey_i) * phy_INcon(phy_i) /14.0
            grazing_p = grazing_p + grazing_prey(prey_i) / prey(prey_i) * phy_IPcon(phy_i) /31.0
         ELSEIF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey(1:15).EQ.'aed_bivalve') THEN
            grazing_n = grazing_n + grazing_prey(prey_i) * self%bivalves(biv_i)%INC
            grazing_p = grazing_p + grazing_prey(prey_i) * self%bivalves(biv_i)%IPC
         ENDIF
      ENDDO



      psuedofaeces = (one_ - self%bivalves(biv_i)%fassim) * grazing
      ingestion = self%bivalves(biv_i)%fassim * grazing

      IF (biv <= self%bivalves(biv_i)%min) THEN
        ! Don't excrete or die if we are at the min biomass otherwise we have a
        ! mass conservation leak in the C mass balance

        respiration = zero_
        mortality   = zero_
        excretion   = zero_
        egestion    = zero_

      ELSE

        egestion = self%bivalves(biv_i)%Regst * exp(self%bivalves(biv_i)%gegst + &
                   min(Ctotal_prey/self%bivalves(biv_i)%Kgrz,one_)) * ingestion

        ! Get the respiration rate (/ s)
        iteg = ingestion - egestion
        respiration = aed_bivalve_respiration(self,biv_i,iteg,temp,salinity)

        ! Get the excretion rate (of carbon!) (/s)
        excretion =  self%bivalves(biv_i)%Rexcr * iteg

        ! Get the mortality rate (/ s)
        mortality = self%bivalves(biv_i)%Rmort * f_DO(self,biv_i,oxy)

        ! Add the predation losses to mortality
        mortality = mortality + self%bivalves(biv_i)%Rpred


      ENDIF


      ! Calculate losses into the particulate organic matter pools - Units mmol/s
      poc_excr = (psuedofaeces + egestion + mortality)*biv

      pon_excr = (psuedofaeces * grazing_n / grazing  &
               +  egestion*self%bivalves(biv_i)%INC + mortality*self%bivalves(biv_i)%INC)*biv

      pop_excr = (psuedofaeces * grazing_p / grazing  &
               +  egestion*self%bivalves(biv_i)%IPC + mortality*self%bivalves(biv_i)%IPC)*biv

      ! Now we know the rates of carbon consumption and excretion, calculate rates
      ! of n & p excretion to maintain internal nutrient stores

      ! First, compute rate of change so far of bivalve carbon biomass (mmolC / m2 /s)
      delta_C = (ingestion - respiration - egestion - excretion - mortality) * biv


      ! Then calc nutrient excretion require to balance internal nutrient store
      ! Note pon_excr includes loss due to messy feeding so no need to include assimilation fraction on grazing_n & grazing_p
      don_excr = grazing_n - pon_excr - delta_C * self%bivalves(biv_i)%INC
      dop_excr = grazing_p - pop_excr - delta_C * self%bivalves(biv_i)%IPC

      ! If nutrients are limiting then must excrete doc to maintain balance
      IF ((don_excr < zero_) .AND. (dop_excr < zero_)) THEN
         !Determine which nutrient is more limiting
         IF ((self%bivalves(biv_i)%INC * (grazing_n - pon_excr) - delta_C) .GT. &
            (self%bivalves(biv_i)%IPC * (grazing_p - pop_excr) - delta_C)) THEN
             don_excr = zero_
             doc_excr =  (grazing_n - pon_excr) / self%bivalves(biv_i)%INC - delta_C
             delta_C = delta_C - doc_excr
             dop_excr = grazing_p - pop_excr - delta_C*self%bivalves(biv_i)%IPC
         ELSE
             dop_excr = zero_
             doc_excr = (grazing_p - pop_excr) / self%bivalves(biv_i)%IPC - delta_C
             delta_C = delta_C - doc_excr
             don_excr = grazing_n - pon_excr - delta_C*self%bivalves(biv_i)%INC
         ENDIF
      ELSEIF (don_excr < zero_) THEN !nitrogen limited
         don_excr = zero_
         doc_excr = (grazing_n - pon_excr) / self%bivalves(biv_i)%INC - delta_C
         delta_C = delta_C - doc_excr
         dop_excr = grazing_p - pop_excr - delta_C*self%bivalves(biv_i)%IPC
      ELSEIF (dop_excr < zero_) THEN !phosphorus limited
         dop_excr = zero_
         doc_excr = (grazing_p - pop_excr) / self%bivalves(biv_i)%IPC - delta_C
         delta_C = delta_C - doc_excr
         don_excr = grazing_n - pon_excr - delta_C*self%bivalves(biv_i)%INC
      ELSE !just excrete nutrients no need to balance c
          doc_excr = zero_
      ENDIF


      !write(*,"(4X,'limitations (f_T,f_Salinity): ',2F8.2)")f_T,f_Salinity
      !write(*,"(4X,'sources/sinks (grazing,respiration,mortaility): ',3F8.2)")grazing,excretion,mortality


      ! SET TEMPORAL DERIVATIVES FOR ODE SOLVER

      ! Production / losses in mmolC/s

      _SET_ODE_BEN_(self%id_biv(biv_i),(ingestion - respiration - excretion - egestion - mortality)*biv )


      ! Now take food grazed by zooplankton from food pools in mmolC/s
      phy_i = 0
      DO prey_i = 1,self%bivalves(biv_i)%num_prey
         _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_prey(prey_i), -1.0 * grazing_prey(prey_i))
          IF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey .EQ. _OGMPOC_) THEN
              IF (poc > zero_) THEN
                 _SET_BOTTOM_EXCHANGE_(self%id_Nmorttarget, -1.0 * grazing_prey(prey_i) * pon/poc)
                 _SET_BOTTOM_EXCHANGE_(self%id_Pmorttarget, -1.0 * grazing_prey(prey_i) * pop/poc)
              ENDIF
          ELSEIF (self%bivalves(biv_i)%prey(prey_i)%bivalve_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
            phy_i = phy_i + 1
            _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_phyIN(phy_i), -1.0 * grazing_prey(prey_i) / prey(prey_i) * phy_INcon(phy_i))
            _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_phyIP(phy_i), -1.0 * grazing_prey(prey_i) / prey(prey_i) * phy_IPcon(phy_i))
         ENDIF
      ENDDO


      ! Now manage excretion contributions to DOM
      IF (self%simDCexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Cexctarget, excretion + doc_excr)
      ENDIF
      IF (self%simDNexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Nexctarget,don_excr)
      ENDIF
      IF (self%simDPexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Pexctarget,dop_excr)
      ENDIF

      ! Now manage psuedofaeces, egestion and mortality contributions to POM
      IF (self%simPCexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Cmorttarget, poc_excr)
      ENDIF
      IF (self%simPNexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Nmorttarget, pon_excr)
      ENDIF
      IF (self%simPPexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Pmorttarget, pop_excr)
      ENDIF

      ! Export diagnostic variables
      _SET_HORIZONTAL_DIAGNOSTIC_(self%id_grz  ,grazing*secs_pr_day)
      _SET_HORIZONTAL_DIAGNOSTIC_(self%id_resp ,respiration*secs_pr_day)
      _SET_HORIZONTAL_DIAGNOSTIC_(self%id_mort ,mortality*secs_pr_day)

   ENDDO

   ! Leave spatial loops (if any)
   _HORIZONTAL_LOOP_END_
END SUBROUTINE aed_bivalve_do_benthos
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_bivalve_do_ppdd(self,_ARGUMENTS_DO_PPDD_)
!-------------------------------------------------------------------------------
! Right hand sides of zooplankton biogeochemical model exporting
! production/destruction matrices
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self
   _DECLARE_ARGUMENTS_DO_PPDD_
!
!LOCALS
   AED_REAL           :: zoo,temp,salinity !State variables
   AED_REAL, ALLOCATABLE,DIMENSION(:)  :: prey !Prey state variables
   AED_REAL           :: dn_excr, dp_excr, dc_excr !Excretion state variables
   AED_REAL           :: pon, pop, poc !Mortaility and fecal pellet state variables
   AED_REAL           :: FGrazing_Limitation, f_T, f_Salinity
   AED_REAL           :: Ctotal_prey !total concentration of available prey
   AED_REAL           :: grazing, respiration, mortality !Growth & decay functions
   INTEGER            :: biv_i,prey_i
   AED_REAL,PARAMETER :: secs_pr_day = 86400.
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Enter spatial loops (if any)
   _LOOP_BEGIN_

   ! Retrieve current environmental conditions.
   _GET_(self%id_tem,temp)     ! local temperature
   _GET_(self%id_sal,salinity) ! local salinity



   ! Leave spatial loops (if any)
   _LOOP_END_
END SUBROUTINE aed_bivalve_do_ppdd
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
FUNCTION aed_bivalve_respiration(self,biv_i,iteg,temp,sal) RESULT(resp)
!-------------------------------------------------------------------------------
! Right hand sides of zooplankton biogeochemical model exporting
! production/destruction matrices
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self  ! Module data, with params
   INTEGER                             :: biv_i ! Invertebrate group
   AED_REAL, INTENT(IN)                :: temp  ! Temp value being used
   AED_REAL, INTENT(IN)                :: sal   ! Salinity value being used
   AED_REAL, INTENT(IN)                :: iteg  ! Ingestion-Egestion
!
!LOCALS
   AED_REAL :: W, TmaxR, maxTR, VV,WW,YY,XX,fT
   AED_REAL :: resp, Q, R20
!
!-------------------------------------------------------------------------------
!BEGIN

   ! Get R20 value
   IF(self%bivalves(biv_i)%fR20==1) THEN
     ! Compute respiration rate coefficient from length
     W = (0.071/1000.) * self%bivalves(biv_i)%Length**2.8
     R20 = self%bivalves(biv_i)%War * W** self%bivalves(biv_i)%Wbr
   ELSE
     ! Use constant respiration rate coefficient
     R20 = self%bivalves(biv_i)%Rresp
   END IF

   ! Now compute actual respiration
   IF(self%bivalves(biv_i)%fR==1) THEN
     ! Option 1) Spillman et al 2008; Bocioniv et al 2013
     resp = R20 * self%bivalves(biv_i)%theta_resp**(temp-20.0)

   ELSEIF(self%bivalves(biv_i)%fR==2) THEN
     ! Option 2) Modified Schneider 1992; Bierman 2005; Gudimov et al. 2015

     TmaxR = self%bivalves(biv_i)%TmaxR
     maxTR = self%bivalves(biv_i)%maxTR
     Q     = self%bivalves(biv_i)%Qresp

     VV = ((TmaxR - temp)/(TmaxR - maxTR))
     WW = log(Q)*(TmaxR - maxTR)
     YY = log(Q)*(TmaxR - maxTR + 2.)
     XX = (WW * (1. + SQRT(1. + (40. / YY))) / 20.)**2
     fT = VV**XX * exp(XX*(1.-VV))

     resp = R20 * fT + self%bivalves(biv_i)%SDA * iteg

   ELSE
     ! Unknown option
     resp = 1.0
   END IF

!   ! Get the salinity limitation.
!   resp = resp * fSalinity_Limitation(self%bivalves,biv_i,sal)


END FUNCTION aed_bivalve_respiration
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
PURE AED_REAL FUNCTION f_DO(self,biv,oxy)
!-------------------------------------------------------------------------------
! Dissolved oxygen effect on bivalve mortality
!
! Add f(DO) to base mortality, ie. mort is only ever enhanced by low DO
! Two options:
! 1) Abrupt increase in mortality once below a minimum DO threshold is exceeded
! 2) Steady exponential increase as DO decreases, enhancing mortality
!    by K_BDO so that when DO = 0, f(DO) = 1 + K_BDO
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self
   INTEGER,INTENT(in)   :: biv              ! Invertebrate group
   AED_REAL, INTENT(IN) :: oxy              ! Dissolved oxygen
!
!LOCALS
   AED_REAL :: bot     ! Work Array
!
!-------------------------------------------------------------------------------
!BEGIN

   IF(self%bivalves(biv)%fDO==0) THEN
     ! Option 0) Abrupt increase below DO threshold
      IF (oxy < self%bivalves(biv)%KDO) THEN
        f_DO = 10.0
      ELSE
        f_DO = 1.0
      END IF

   ELSEIF(self%bivalves(biv)%fDO==1) THEN
     ! Option 1) Exponential increase: steepness determined by KDO and K_BDO
     ! i.e. when DO = 0, final f(DO) = 1 + K_BDO
     bot  = self%bivalves(biv)%KDO + oxy
     f_DO = 1.0 + self%bivalves(biv)%K_BDO * self%bivalves(biv)%KDO/bot

   ELSE
     ! Unknown option
     f_DO = 1.0
   END IF


END FUNCTION f_DO
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
PURE AED_REAL FUNCTION fTemp_function_biv(self,biv,temp)
!-------------------------------------------------------------------------------
! Temperature growth multiplier for bivalves
!
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self  ! Module data, with params
   INTEGER, INTENT(in)                 :: biv   ! Invertebrate group
   AED_REAL, INTENT(IN)                :: temp  ! Temp value being used
!
!LOCALS
   AED_REAL , PARAMETER :: a = 1.00
   AED_REAL             :: MINt,Tmin,Tmax,MAXt

!
!-------------------------------------------------------------------------------
!BEGIN

   MINt = self%bivalves(biv)%MINt
   Tmin = self%bivalves(biv)%Tmin
   Tmax = self%bivalves(biv)%Tmax
   MAXt = self%bivalves(biv)%MAXt

   ! If temp below extreme, temp fn = 0, ie filtration ceases
   IF(temp <= MINt) &
      fTemp_function_biv = zero_

   ! If below min temp for optimal range, limited as below
   IF (temp>MINt .and. temp<Tmin) THEN
      fTemp_function_biv = a*(1.0/(-((Tmin-MINt)*(Tmin-MINt)/ &
            (Tmin*Tmin))+(2.0*(Tmin-MINt)/ &
            Tmin)))*(-((temp-MINt)*(temp-MINt)/&
            (Tmin*Tmin))+2.0*((temp-MINt) / Tmin))
   END IF

   ! If between Tmin and Tmax, ie in the opt temp range, then not limited
   IF (temp >=Tmin .and. temp<=Tmax) THEN
      fTemp_function_biv = a
   END IF

   ! If above max temp for optimal range, limited as below
   IF (temp>Tmax .and. temp <MAXt) THEN
      fTemp_function_biv = a*((-(temp*temp)+2.0*(Tmax*temp)- &
            2.0*(Tmax*MAXt)+(MAXt*MAXt))/ &
            ((Tmax-MAXt)*(Tmax-MAXt)))
   END IF

   ! If temp above extreme, temp fn = 0, ie filtration ceases
   IF (temp>=MAXt) &
       fTemp_function_biv = zero_

 END FUNCTION fTemp_function_biv
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
 PURE AED_REAL FUNCTION fSS_function(self,biv,SS)
!-------------------------------------------------------------------------------
! Suspended solids function for clams
!
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self  ! Module data, with params
   INTEGER, INTENT(in)                 :: biv   ! Invertebrate group
   AED_REAL, INTENT(IN)                :: SS    ! Susp solids value being used
!
!LOCALS
   AED_REAL , PARAMETER :: a = 1.00
   AED_REAL             :: pseudo, maxSS

!
!-------------------------------------------------------------------------------
!BEGIN

   fSS_function = one_

   pseudo =  self%bivalves(biv)%SSmax    ! SS conc where ingestion decreases
   maxSS  =  self%bivalves(biv)%maxSS    ! SS conc where mussel is buggered


   IF(SS <= pseudo) THEN
        fSS_function = one_
   ELSEIF(SS > pseudo .AND. SS <= maxSS) THEN
        ! From Spillman et al 2008
        fSS_function = (-SS*SS + 2.0*pseudo*SS - &
               2.0*pseudo*maxSS + maxSS*maxSS)/  &
               ((pseudo-maxSS) * (pseudo-maxSS))
   ELSE
        fSS_function = zero_
   END IF

   ! Ensure suspended solids function is not negative
   IF (fSS_function <= zero_) &
        fSS_function = zero_

 END FUNCTION fSS_function
 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
 PURE AED_REAL FUNCTION fD_function(self,biv,dens)
!-------------------------------------------------------------------------------
! Suspended solids function for clams
!
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self  ! Module data, with params
   INTEGER, INTENT(in)                 :: biv   ! Invertebrate group
   AED_REAL, INTENT(in)                :: dens  ! Density value being used
!
!LOCALS
   AED_REAL             :: maxD, Dmax

!
!-------------------------------------------------------------------------------
!BEGIN

   fD_function = one_

   Dmax =  self%bivalves(biv)%Dmax    ! density where ingestion decreases
   maxD =  self%bivalves(biv)%maxD    ! density where mussel is buggered


   IF(dens <= Dmax) THEN
        fD_function = one_
   ELSEIF(dens > Dmax .AND. dens <= maxD) THEN
        fD_function = (-dens*dens + 2.0*Dmax*dens - &
                       2.0*Dmax*maxD + maxD*maxD)/  &
                      ((Dmax-maxD) * (Dmax-maxD))
   ELSE
        fD_function = zero_
   END IF

   ! Ensure fD is not negative
   IF (fD_function <= zero_) &
        fD_function = zero_

 END FUNCTION fD_function
 !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#if 0
!-----------------------------------------------------------------------!
!-----------------------------------------------------------------------!
! Salinity tolerance of clams/mussels                                   !
!-----------------------------------------------------------------------!
! Directly cut and pasted from Maya's clam model                        !
 FUNCTION clamSalinity(Sbot) RESULT(ret)                                !
   !-- Incoming                                                         !
   REAL (r_wq) :: Sbot(:)            ! Salinity in the bottom layer     !
   !-- Returns the salinity function                                    !
   REAL (r_wq), DIMENSION(SIZE(Sbot)) :: ret
   REAL (r_wq), DIMENSION(SIZE(Sbot)) :: tmp1,tmp2

   ! BSmin minimum sal and BSmax is max sal set in WQcons               !
   ! Don't seem to need Bep, Aep or Sop from consts file                !
   ! Salinity is non-limiting for sals > BSmin and <BSmax               !
   ! clamLowCut is salinity when shells close up and feeding ceases     !
   ! CS 020609 Need to make an exception if FW species and BSmin and    !
   ! clamLowCut are both equal to zero.

   ! Salinity is within the tolerance; no limitation.                   !
   ! If sal in bott cell is > min sal & < max sal set in WQcons, pf=1   !
   WHERE(Sbot >= BSmin .and. Sbot <= BSmax)
     ret = wq_one
   END WHERE

   ! Salinity is greater than the upper bound                           !
   ! maxS is set in caedym_globals at 45psu                             !
   WHERE(Sbot > BSmax)
     ret = (-Sbot*Sbot+2.0*BSmax*Sbot-  &
            2.0*BSmax*maxS+maxS*maxS)/((BSmax-maxS)*(BSmax-maxS))
   END WHERE

   ! Salinity is less than the lower bound but greater than low cut     !
   ! If sal is < min set in WQcons but > clamLowCut set at clamCons.dat)!
   tmp1 = wq_zero
   tmp2 = wq_zero
   WHERE(Sbot < BSmin .AND. Sbot > clamLowCut)
     tmp1 = Sbot-clamLowCut
     tmp2 = BSmin-clamLowCut
     ret =  (2*tmp1/BSmin-(tmp1*tmp1/(BSmin*BSmin)))/ &
            (2*tmp2/BSmin-(tmp2*tmp2/(BSmin*BSmin)))
   END WHERE

   ! Salinity is less than the clamLowCut                               !
   ! If sal < lowest sal (hardwired at start of fn), shells close       !
   WHERE(Sbot <= clamLowCut)
     ret = wq_zero
   END WHERE

   ! If lower bound and low cut are both zero i.e. Freshwater species   !
   ! then need to set f(S) to one                                       !
   IF(BSmin==wq_zero) THEN
     WHERE(Sbot <= BSmin)
         ret =  wq_one
     END WHERE
   ENDIF

   ! Ensure temp function is not negative                               !
   WHERE(ret <= wq_zero)
     ret = wq_zero
   END WHERE

 END FUNCTION clamSalinity
!-----------------------------------------------------------------------!


#endif


END MODULE aed_bivalve
