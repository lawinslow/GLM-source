!###############################################################################
!#                                                                             #
!# aed_test.F90                                                                #
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
!# Created Feb 2015                                                            #
!#                                                                             #
!###############################################################################

#include "aed.h"

!
MODULE aed_test
!-------------------------------------------------------------------------------
! aed_test --- test model
!
! The AED module test contains basic equations that have no dependencies
!-------------------------------------------------------------------------------
   USE aed_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed_type_test
!
   TYPE,extends(type_base_model) :: aed_type_test
!     Variable identifiers
      TYPE (type_state_variable_id)        :: id_tst_pel
      TYPE (type_bottom_state_variable_id) :: id_tst_ben

      CONTAINS      ! Model Methods
        PROCEDURE :: initialize               => aed_init_test
        PROCEDURE :: do                       => aed_test_do
        PROCEDURE :: do_benthos               => aed_test_do_benthos
   END TYPE


!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed_init_test(self,namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and the variables exported
!  by the model are registered with FABM.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed_type_test),TARGET,INTENT(inout) :: self
!
!LOCALS
!
!-------------------------------------------------------------------------------
!BEGIN
   CALL self%register_state_variable(self%id_tst_pel, "pel",'mmol/m**3','test_pel', &
                                   zero_,minimum=zero_)
   CALL self%register_state_variable(self%id_tst_ben, "ben",'mmol/m**2','test_ben', &
                                   zero_,minimum=zero_)
END SUBROUTINE aed_init_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_test_do(self,_ARGUMENTS_DO_)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_test),INTENT(in) :: self
   _DECLARE_ARGUMENTS_DO_
!
!LOCALS
   AED_REAL :: pel
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Enter spatial loops (if any)
   _LOOP_BEGIN_

   _GET_(self%id_tst_pel, pel)
   pel = 20 - pel
   _SET_ODE_(self%id_tst_pel, pel / secs_per_day)

   ! Leave spatial loops (if any)
   _LOOP_END_
END SUBROUTINE aed_test_do
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_test_do_benthos(self,_ARGUMENTS_DO_BOTTOM_)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED test.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_type_test),INTENT(in) :: self
   _DECLARE_ARGUMENTS_DO_BOTTOM_
!
!LOCALS
   AED_REAL :: ben
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Enter spatial loops (if any)
   _HORIZONTAL_LOOP_BEGIN_

   _GET_HORIZONTAL_(self%id_tst_ben, ben)
   ben = 10 - ben
   _SET_ODE_BEN_(self%id_tst_ben, ben / secs_per_day)

   ! Leave spatial loops (if any)
   _HORIZONTAL_LOOP_END_
END SUBROUTINE aed_test_do_benthos
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed_test
