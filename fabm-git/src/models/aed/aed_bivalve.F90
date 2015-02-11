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
   USE aed_util,ONLY : find_free_lun,aed_bio_temp_function, fTemp_function,qsort
   USE aed_zoop_utils

   IMPLICIT NONE

   PRIVATE   ! By default make everything private
!
   PUBLIC aed_type_bivalve
!

   TYPE,extends(type_base_model) :: aed_type_bivalve
!     Variable identifiers
      TYPE (type_bottom_state_variable_id) :: id_biv(MAX_ZOOP_TYPES)
      TYPE (type_state_variable_id)      :: id_Nexctarget,id_Nmorttarget
      TYPE (type_state_variable_id)      :: id_Pexctarget,id_Pmorttarget
      TYPE (type_state_variable_id)      :: id_Cexctarget,id_Cmorttarget
      TYPE (type_state_variable_id)      :: id_DOupttarget
      TYPE (type_dependency_id)          :: id_tem, id_sal, id_extc
      TYPE (type_horizontal_diagnostic_variable_id) :: id_grz,id_resp,id_mort


!     Model parameters
      INTEGER                                       :: num_biv
      TYPE(type_zoop_data),DIMENSION(:),ALLOCATABLE :: bivalves
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
   AED_REAL :: Pzoo_prey(MAX_ZOOP_PREY)

   AED_REAL,PARAMETER :: secs_pr_day = 86400.
   TYPE(type_zoop_params)  :: bivalves_param(MAX_ZOOP_TYPES)
   NAMELIST /bivalves_params/ bivalves_param
!-------------------------------------------------------------------------------
!BEGIN
    tfil = find_free_lun()
    open(tfil,file=dbase, status='OLD',iostat=status)
    IF (status /= 0) STOP 'Error opening bivalves_params namelist file'
    read(tfil,nml=bivalves_params,iostat=status)
    close(tfil)
    IF (status /= 0) STOP 'Error reading namelist bivalves_params'

    self%num_biv = count
    allocate(self%bivalves(count))
    DO i=1,count
       ! Assign parameters from database to simulated groups
       self%bivalves(i)%zoop_name         = bivalves_param(list(i))%zoop_name
       self%bivalves(i)%zoop_initial      = bivalves_param(list(i))%zoop_initial
       self%bivalves(i)%min_zoo           = bivalves_param(list(i))%min_zoo
       self%bivalves(i)%Rgrz_zoo          = bivalves_param(list(i))%Rgrz_zoo/secs_pr_day
       self%bivalves(i)%fassim_zoo        = bivalves_param(list(i))%fassim_zoo
       self%bivalves(i)%Kgrz_zoo          = bivalves_param(list(i))%Kgrz_zoo
       self%bivalves(i)%theta_grz_zoo     = bivalves_param(list(i))%theta_grz_zoo
       self%bivalves(i)%Rresp_zoo         = bivalves_param(list(i))%Rresp_zoo/secs_pr_day
       self%bivalves(i)%Rmort_zoo         = bivalves_param(list(i))%Rmort_zoo/secs_pr_day
       self%bivalves(i)%ffecal_zoo        = bivalves_param(list(i))%ffecal_zoo
       self%bivalves(i)%fexcr_zoo         = bivalves_param(list(i))%fexcr_zoo
       self%bivalves(i)%ffecal_sed        = bivalves_param(list(i))%ffecal_sed
       self%bivalves(i)%theta_resp_zoo    = bivalves_param(list(i))%theta_resp_zoo
       self%bivalves(i)%Tstd_zoo          = bivalves_param(list(i))%Tstd_zoo
       self%bivalves(i)%Topt_zoo          = bivalves_param(list(i))%Topt_zoo
       self%bivalves(i)%Tmax_zoo          = bivalves_param(list(i))%Tmax_zoo
       self%bivalves(i)%saltfunc_zoo      = bivalves_param(list(i))%saltfunc_zoo
       self%bivalves(i)%Smin_zoo          = bivalves_param(list(i))%Smin_zoo
       self%bivalves(i)%Smax_zoo          = bivalves_param(list(i))%Smax_zoo
       self%bivalves(i)%Sint_zoo          = bivalves_param(list(i))%Sint_zoo
       self%bivalves(i)%INC_zoo           = bivalves_param(list(i))%INC_zoo
       self%bivalves(i)%IPC_zoo           = bivalves_param(list(i))%IPC_zoo
       self%bivalves(i)%simDOlim          = bivalves_param(list(i))%simDOlim
       self%bivalves(i)%DOmin_zoo         = bivalves_param(list(i))%DOmin_zoo
       self%bivalves(i)%Cmin_grz_zoo      = bivalves_param(list(i))%Cmin_grz_zoo
       self%bivalves(i)%num_prey          = bivalves_param(list(i))%num_prey
       !Loop through prey variables assigning a target variable and preference factor
       !First sort in decending order of food preferences
       DO j=1,self%bivalves(i)%num_prey
          sort_i(j) = j
          Pzoo_prey(j) = bivalves_param(list(i))%prey(j)%Pzoo_prey
       ENDDO
       CALL qsort(Pzoo_prey,sort_i,1,self%bivalves(i)%num_prey)
       DO j=1,self%bivalves(i)%num_prey
          self%bivalves(i)%prey(j)%zoop_prey = bivalves_param(list(i))%prey(sort_i(self%bivalves(i)%num_prey-j+1))%zoop_prey
          self%bivalves(i)%prey(j)%Pzoo_prey = bivalves_param(list(i))%prey(sort_i(self%bivalves(i)%num_prey-j+1))%Pzoo_prey
       ENDDO

       ! Register group as a state variable
       CALL self%register_state_variable(self%id_biv(i),             &
                              bivalves_param(list(i))%zoop_name,     &
                              'mmolC/m**2', 'bivalve',               &
                              bivalves_param(list(i))%zoop_initial,      &
                              minimum=bivalves_param(list(i))%min_zoo)
    ENDDO
!
END SUBROUTINE aed_bivalve_load_params
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_init_bivalve(self,namlst)
!-------------------------------------------------------------------------------
! Initialise the zooplankton biogeochemical model
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
   CHARACTER(len=128) :: dbase='aed_bivalve_pars.nml'

   AED_REAL,PARAMETER :: secs_pr_day = 86400.
   INTEGER            :: biv_i, prey_i, phy_i

   NAMELIST /aed_bivalve/ num_biv, the_biv, &
                    dn_target_variable, pn_target_variable, dp_target_variable, &
                    pp_target_variable, dc_target_variable, pc_target_variable, &
                    dbase
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


   CALL aed_bio_temp_function(self%num_biv,                 &
                              self%bivalves%theta_resp_zoo, &
                              self%bivalves%Tstd_zoo,       &
                              self%bivalves%Topt_zoo,       &
                              self%bivalves%Tmax_zoo,       &
                              self%bivalves%aTn,            &
                              self%bivalves%bTn,            &
                              self%bivalves%kTn,            &
                              self%bivalves%zoop_name)


   !Register link to prey state variables
   DO biv_i = 1,num_biv
      phy_i = 0
      DO prey_i = 1,self%bivalves(biv_i)%num_prey
          CALL self%register_state_dependency(self%bivalves(biv_i)%id_prey(prey_i), &
                                       self%bivalves(biv_i)%prey(prey_i)%zoop_prey)
          !If the prey is phytoplankton then also register state dependency on
          !internal nitrogen and phosphorus
          IF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
              phy_i = phy_i + 1
              CALL self%register_state_dependency(self%bivalves(biv_i)%id_phyIN(phy_i), &
                                       TRIM(self%bivalves(biv_i)%prey(prey_i)%zoop_prey)//'_IN')
              CALL self%register_state_dependency(self%bivalves(biv_i)%id_phyIP(phy_i), &
                                       TRIM(self%bivalves(biv_i)%prey(prey_i)%zoop_prey)//'_IP')

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
! Right hand sides of zooplankton biogeochemical model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_bivalve),INTENT(in) :: self
   _DECLARE_ARGUMENTS_DO_BOTTOM_
!
!LOCALS
   AED_REAL           :: zoo,temp,salinity !State variables
   AED_REAL           :: prey(MAX_ZOOP_PREY), grazing_prey(MAX_ZOOP_PREY) !Prey state variables
   AED_REAL           :: phy_INcon(MAX_ZOOP_PREY), phy_IPcon(MAX_ZOOP_PREY) !Internal nutrients for phytoplankton
   AED_REAL           :: dn_excr, dp_excr, dc_excr !Excretion state variables
   AED_REAL           :: pon, pop, poc !Mortaility and fecal pellet state variables
   AED_REAL           :: FGrazing_Limitation, f_T, f_Salinity
   AED_REAL           :: pref_factor, Ctotal_prey !total concentration of available prey
   AED_REAL           :: food, grazing, respiration, mortality !Growth & decay functions
   AED_REAL           :: grazing_n, grazing_p !Grazing on nutrients
   AED_REAL           :: pon_excr, pop_excr, poc_excr !POM excretion rates
   AED_REAL           :: don_excr, dop_excr, doc_excr, delta_C !DOM excretion rates
   INTEGER            :: biv_i,prey_i,prey_j,phy_i
   AED_REAL,PARAMETER :: secs_pr_day = 86400.
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Enter spatial loops (if any)
   _HORIZONTAL_LOOP_BEGIN_

   ! Retrieve current environmental conditions.
   _GET_(self%id_tem,temp)     ! local temperature
   _GET_(self%id_sal,salinity) ! local salinity

   ! Retrieve current (local) state variable values.
   IF (self%simDNexcr) _GET_(self%id_Nexctarget, dn_excr)
   IF (self%simDPexcr) _GET_(self%id_Pexctarget, dp_excr)
   IF (self%simDCexcr) _GET_(self%id_Cexctarget, dc_excr)

   IF (self%simPNexcr) _GET_(self%id_Nmorttarget, pon)
   IF (self%simPPexcr) _GET_(self%id_Pmorttarget, pop)
   IF (self%simPCexcr) _GET_(self%id_Cmorttarget, poc)

   DO biv_i=1,self%num_biv

      ! Retrieve this bivalve group
      _GET_HORIZONTAL_(self%id_biv(biv_i),zoo)
      !Retrieve prey groups
      Ctotal_prey   = zero_
      DO prey_i=1,self%bivalves(biv_i)%num_prey
         _GET_(self%bivalves(biv_i)%id_prey(prey_i),prey(prey_i))
         Ctotal_prey = Ctotal_prey + prey(prey_i)
      ENDDO

      grazing       = zero_
      respiration   = zero_
      mortality     = zero_

      ! Get the grazing limitation function
       fGrazing_Limitation = fPrey_Limitation(self%bivalves,biv_i,Ctotal_prey)

      ! Get the temperature function
       f_T = fTemp_function(1, self%bivalves(biv_i)%Tmax_zoo,       &
                               self%bivalves(biv_i)%Tstd_zoo,       &
                               self%bivalves(biv_i)%theta_resp_zoo, &
                               self%bivalves(biv_i)%aTn,            &
                               self%bivalves(biv_i)%bTn,            &
                               self%bivalves(biv_i)%kTn, temp)

      ! Get the salinity limitation.
       f_Salinity = fSalinity_Limitation(self%bivalves,biv_i,salinity)

      ! Get the growth rate (/ s)
      ! grazing is in units of mass consumed/mass zoops/unit time
      grazing = self%bivalves(biv_i)%Rgrz_zoo * fGrazing_Limitation * f_T

      ! Now dertermine available prey and limit grazing amount to
      ! availability of prey
      ! food is total amount of food in units of mass/unit volume/unit time
      food = grazing * zoo
      IF (Ctotal_prey < self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz_zoo ) THEN
          food = zero_
          grazing = food / zoo
      ELSEIF (food > Ctotal_prey - self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz_zoo ) THEN
          food = Ctotal_prey - self%bivalves(biv_i)%num_prey * self%bivalves(biv_i)%Cmin_grz_zoo
          grazing = food / zoo
      ENDIF


      ! Now determine prey composition based on preference factors and
      ! availability of prey

      ! Prey has been ordered in grazing preference
      ! So take food in order of preference up to availability minus
      !value of minimum residual
      ! grazing_prey is in units of mass consumed/unit volumne/unit time

      DO prey_i = 1,self%bivalves(biv_i)%num_prey
          !Add up preferences for remaining prey
          pref_factor = zero_
          DO prey_j = prey_i,self%bivalves(biv_i)%num_prey
             pref_factor = pref_factor + self%bivalves(biv_i)%prey(biv_i)%Pzoo_prey
          ENDDO
          IF (food * self%bivalves(biv_i)%prey(prey_i)%Pzoo_prey / pref_factor <= &
                        prey(prey_i) - self%bivalves(biv_i)%Cmin_grz_zoo) THEN
             !Take fraction of left over food based on preference factor
             grazing_prey(prey_i) = food * self%bivalves(biv_i)%prey(prey_i)%Pzoo_prey / pref_factor
          ELSEIF (prey(prey_i) > self%bivalves(biv_i)%Cmin_grz_zoo) THEN
             grazing_prey(prey_i) = prey(prey_i) - self%bivalves(biv_i)%Cmin_grz_zoo
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
         IF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey .EQ. _OGMPOC_) THEN
            IF (poc > zero_) THEN
                grazing_n = grazing_n + grazing_prey(prey_i) * pon/poc
                grazing_p = grazing_p + grazing_prey(prey_i) * pop/poc
            ELSE
                grazing_n = zero_
                grazing_p = zero_
            ENDIF
         ELSEIF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
            phy_i = phy_i + 1
            _GET_(self%bivalves(biv_i)%id_phyIN(phy_i),phy_INcon(phy_i))
            _GET_(self%bivalves(biv_i)%id_phyIP(phy_i),phy_IPcon(phy_i))
            grazing_n = grazing_n + grazing_prey(prey_i) / prey(prey_i) * phy_INcon(phy_i) /14.0
            grazing_p = grazing_p + grazing_prey(prey_i) / prey(prey_i) * phy_IPcon(phy_i) /31.0
         ELSEIF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey(1:15).EQ.'aed_bivalve') THEN
            grazing_n = grazing_n + grazing_prey(prey_i) * self%bivalves(biv_i)%INC_zoo
            grazing_p = grazing_p + grazing_prey(prey_i) * self%bivalves(biv_i)%IPC_zoo
         ENDIF
      ENDDO


      ! Get the respiration rate (/ s)
      respiration = self%bivalves(biv_i)%Rresp_zoo * f_Salinity

      ! Get the mortality rate (/ s)
      mortality = self%bivalves(biv_i)%Rmort_zoo * f_T

      ! Don't excrete or die if we are at the min biomass otherwise we have a
      ! mass conservation leak in the C mass balance
      IF (zoo <= self%bivalves(biv_i)%min_zoo) THEN
        respiration = zero_
        mortality = zero_
      ENDIF

      ! Now we know the rates of carbon consumption and excretion,
      ! calculate rates of n & p excretion to maintain internal
      ! nutrient stores
      ! Calculate excretion of particulate organic matter - Units mmol/s
      poc_excr = ((1 - self%bivalves(biv_i)%fassim_zoo) * grazing + &
               (1 - self%bivalves(biv_i)%ffecal_sed) * self%bivalves(biv_i)%ffecal_zoo * respiration +  &
                                                        mortality) * zoo
      pon_excr = (1 - self%bivalves(biv_i)%fassim_zoo) * grazing_n  + &
               ((1 - self%bivalves(biv_i)%ffecal_sed) * self%bivalves(biv_i)%ffecal_zoo * respiration + &
                              mortality) * self%bivalves(biv_i)%INC_zoo * zoo
      pop_excr = (1 - self%bivalves(biv_i)%fassim_zoo) * grazing_p + &
               ((1 - self%bivalves(biv_i)%ffecal_sed) * self%bivalves(biv_i)%ffecal_zoo * respiration + &
                              mortality) * self%bivalves(biv_i)%IPC_zoo * zoo


      ! Calculate rate of change of zooplankton carbon (mmolC/s)          !
      delta_C = (self%bivalves(biv_i)%fassim_zoo * grazing - respiration - mortality) * zoo
      ! Calculate nutrient excretion require to balance internal nutrient store
      ! Note pon_excr includes loss due to messy feeding so no need to include assimilation fraction on grazing_n & grazing_p
      don_excr = grazing_n - pon_excr - delta_C * self%bivalves(biv_i)%INC_zoo
      dop_excr = grazing_p - pop_excr - delta_C * self%bivalves(biv_i)%IPC_zoo
      !If nutrients are limiting then must excrete doc to maintain balance
      IF ((don_excr < zero_) .AND. (dop_excr < zero_)) THEN
         !Determine which nutrient is more limiting
         IF ((self%bivalves(biv_i)%INC_zoo * (grazing_n - pon_excr) - delta_C) .GT. &
            (self%bivalves(biv_i)%IPC_zoo * (grazing_p - pop_excr) - delta_C)) THEN
             don_excr = zero_
             doc_excr =  (grazing_n - pon_excr) / self%bivalves(biv_i)%INC_zoo - delta_C
             delta_C = delta_C - doc_excr
             dop_excr = grazing_p - pop_excr - delta_C*self%bivalves(biv_i)%IPC_zoo
         ELSE
             dop_excr = zero_
             doc_excr = (grazing_p - pop_excr) / self%bivalves(biv_i)%IPC_zoo - delta_C
             delta_C = delta_C - doc_excr
             don_excr = grazing_n - pon_excr - delta_C*self%bivalves(biv_i)%INC_zoo
         ENDIF
      ELSEIF (don_excr < zero_) THEN !nitrogen limited
         don_excr = zero_
         doc_excr = (grazing_n - pon_excr) / self%bivalves(biv_i)%INC_zoo - delta_C
         delta_C = delta_C - doc_excr
         dop_excr = grazing_p - pop_excr - delta_C*self%bivalves(biv_i)%IPC_zoo
      ELSEIF (dop_excr < zero_) THEN !phosphorus limited
         dop_excr = zero_
         doc_excr = (grazing_p - pop_excr) / self%bivalves(biv_i)%IPC_zoo - delta_C
         delta_C = delta_C - doc_excr
         don_excr = grazing_n - pon_excr - delta_C*self%bivalves(biv_i)%INC_zoo
      ELSE !just excrete nutrients no need to balance c
          doc_excr = zero_
      ENDIF


      !write(*,"(4X,'limitations (f_T,f_Salinity): ',2F8.2)")f_T,f_Salinity
      !write(*,"(4X,'sources/sinks (grazing,respiration,mortaility): ',3F8.2)")grazing,excretion,mortality


      ! SET TEMPORAL DERIVATIVES FOR ODE SOLVER

      ! Production / losses in mmolC/s

      _SET_ODE_BEN_(self%id_biv(biv_i), (self%bivalves(biv_i)%fassim_zoo * grazing - respiration - mortality)*zoo )


      ! Now take food grazed by zooplankton from food pools in mmolC/s
      phy_i = 0
      DO prey_i = 1,self%bivalves(biv_i)%num_prey
         _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_prey(prey_i), -1.0 * grazing_prey(prey_i))
          IF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey .EQ. _OGMPOC_) THEN
              IF (poc > zero_) THEN
                 _SET_BOTTOM_EXCHANGE_(self%id_Nmorttarget, -1.0 * grazing_prey(prey_i) * pon/poc)
                 _SET_BOTTOM_EXCHANGE_(self%id_Pmorttarget, -1.0 * grazing_prey(prey_i) * pop/poc)
              ENDIF
          ELSEIF (self%bivalves(biv_i)%prey(prey_i)%zoop_prey(1:_PHYLEN_).EQ. _PHYMOD_) THEN
            phy_i = phy_i + 1
            _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_phyIN(phy_i), -1.0 * grazing_prey(prey_i) / prey(prey_i) * phy_INcon(phy_i))
            _SET_BOTTOM_EXCHANGE_(self%bivalves(biv_i)%id_phyIP(phy_i), -1.0 * grazing_prey(prey_i) / prey(prey_i) * phy_IPcon(phy_i))
         ENDIF
      ENDDO


      ! Now manage excretion contributions to DOM
      IF (self%simDCexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Cexctarget,self%bivalves(biv_i)%fexcr_zoo * respiration * zoo + doc_excr)
      ENDIF
      IF (self%simDNexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Nexctarget,don_excr)
      ENDIF
      IF (self%simDPexcr) THEN
         _SET_BOTTOM_EXCHANGE_(self%id_Pexctarget,dop_excr)
      ENDIF

      ! Now manage messy feeding, fecal pellets and mortality contributions to POM
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
      _SET_HORIZONTAL_DIAGNOSTIC_(self%id_grz ,grazing*secs_pr_day)
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


#if 0
!-----------------------------------------------------------------------!
! Dissolved oxygen limitation of clams/mussels                          !
!-----------------------------------------------------------------------!
! Add 1 to function ie mort is only ever enhanced by DO (always > 1)    !
! Two options (based on fish model):                                    !
! 1) Abrupt increase in mortality once below a minimum Do threshold     !
!    Add to clamCons.dat if want to vary DOmin. Hardwired currently     !
! 2) Steady exponenetial increase as DO decreases with mort enhanced    !
!    by BDOi so that when DO = 0, f(DO) = 1 + BDOi                      !

 FUNCTION clamOxygen(group,DOa) RESULT(ret)
   !-- Incoming                                                         !
   INTEGER :: group,gp                         ! Invertebrate group     !
   REAL (r_wq), INTENT(IN) :: DOa(:)           ! Dissolved oxygen       !
   !-- Returns the dissolved oxygen limitation                          !
   REAL (r_wq), DIMENSION(SIZE(DOa)) :: ret    ! f(DO)                  !
   REAL (r_wq), DIMENSION(SIZE(DOa)) :: d1     ! Work Array             !
   ! Minimum tolerable DO concentrations - used for option 1            !
   REAL (r_wq), PARAMETER            :: clamDOmin = 2.00

   ! 1) Abrupt increase below DO threshold                              !
   !WHERE(DOa >= clamDOmin)
   !  d1  = KDOi(group) + DOa
   !  ret = KDOi(group)*(wq_one/d1)
   !ELSEWHERE
   !  ret = 1.0
   !END WHERE
   ! ret = 1.0 + ret

   ! 2) Exponential increase: steepness determined by KDO and BDO       !
   ! i.e. when DO = 0, final f(DO) = 1 + BDO                            !
   d1  = KDOi(group) + DOa
   ret = 1.0 + BDOi(group)*KDOi(group)*(wq_one/d1)

 END FUNCTION clamOxygen
!-----------------------------------------------------------------------!


!-----------------------------------------------------------------------!
! Temperature limitation of clams/mussels                               !
!-----------------------------------------------------------------------!
 FUNCTION clamTemp(temp) RESULT(ret)                                    !
   !-- Incoming                                                         !
   REAL (r_wq), INTENT(IN) :: temp(:)                        ! temp     !
   !-- Returns the temperature limitation                               !
   REAL (r_wq), DIMENSION(SIZE(temp)) :: ret                            !
   !-- Local                                                            !
   REAL (r_wq), PARAMETER :: a = 1.00                                   !

   ! If temp below extreme, temp fn = 0 ie grazing ceases               !
   WHERE(temp<=clamMINt)
      ret = wq_zero
   END WHERE

   ! If below min temp for optimal range, limited as below              !
   WHERE (temp>clamMINt .and. temp<clamTmin)
      ret = a*(1.0/(-((clamTmin-clamMINt)*(clamTmin-clamMINt)/ &
            (clamTmin*clamTmin))+(2.0*(clamTmin-clamMINt)/ &
            clamTmin)))*(-((temp-clamMINt)*(temp-clamMINt)/&
            (clamTmin*clamTmin))+2.0*((temp-clamMINt) / clamTmin))
   END WHERE

   ! If between clamTmin and clamTmax ie in the opt temp range, not ltd !
   WHERE (temp >=clamTmin .and. temp<=clamTmax)
      ret = a
   END WHERE

   ! If above max temp for optimal range, limited as below              !
   WHERE (temp>clamTmax .and. temp <clamMAXt)
      ret = a*((-(temp*temp)+2.0*(clamTmax*temp)- &
            2.0*(clamTmax*clamMAXt)+(clamMAXt*clamMAXt))/ &
            ((clamTmax-clamMAXt)*(clamTmax-clamMAXt)))
   END WHERE

   ! If temp above extreme, temp fn = 0 ie grazing ceases               !
   WHERE (temp>=clamMAXt)
      ret = wq_zero
   END WHERE

 END FUNCTION clamTemp
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


!-----------------------------------------------------------------------!
! Suspended solids function for clams                                   !
!-----------------------------------------------------------------------!
 FUNCTION clamSuspSolids(SSbot) RESULT(ret)                             !
   !-- Incoming                                                         !
   REAL (r_wq) :: SSbot(:)            ! Susp solids (PIM) in bott layer !
   !-- Returns the susp solids function                                 !
   REAL (r_wq), DIMENSION(SIZE(SSbot)) :: ret                           !

   WHERE(SSbot <= pseudo)
        ret = wq_one
   ELSEWHERE
        ! From the clam salinity function
        ret = (-SSbot*SSbot + 2.0*pseudo*SSbot - &
               2.0*pseudo*maxSS + maxSS*maxSS)/  &
               ((pseudo-maxSS) * (pseudo-maxSS))
   END WHERE

   ! Ensure suspended solids function is not negative                   !
   WHERE(ret <= wq_zero)
        ret = wq_zero
   END WHERE

 END FUNCTION clamSuspSolids
!-----------------------------------------------------------------------!
#endif


END MODULE aed_bivalve
