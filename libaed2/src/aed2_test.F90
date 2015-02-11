!###############################################################################
!#                                                                             #
!# aed2_test.F90                                                               #
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

#include "aed2.h"

!
MODULE aed2_test
!-------------------------------------------------------------------------------
! aed2_test --- test model
!
! The AED2 module test contains basic equations that have no dependencies
!-------------------------------------------------------------------------------
   USE aed2_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed2_test_data_t
!
   TYPE,extends(aed2_model_data_t) :: aed2_test_data_t
      !# Variable identifiers
      INTEGER :: id_tst_pel, id_tst_ben

     CONTAINS
         PROCEDURE :: define            => aed2_define_test
         PROCEDURE :: calculate         => aed2_calculate_test
         PROCEDURE :: calculate_benthic => aed2_calculate_benthic_test
!        PROCEDURE :: mobility          => aed2_mobility_test
!        PROCEDURE :: light_extinction  => aed2_light_extinction_test
!        PROCEDURE :: delete            => aed2_delete_test

   END TYPE


!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed2_define_test(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and the variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed2_test_data_t),INTENT(inout) :: data
!
!LOCALS
!
!-------------------------------------------------------------------------------
!BEGIN
   data%id_tst_pel = aed2_define_variable("pel",'mmol/m**3','test_pel', zero_)
   data%id_tst_ben = aed2_define_sheet_variable("ben",'mmol/m**2','test_ben', zero_)
END SUBROUTINE aed2_define_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_test(data,column,layer_idx)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_test_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL :: pel
!
!-------------------------------------------------------------------------------
!BEGIN
   pel = _STATE_VAR_(data%id_tst_pel)
   pel = 20 - pel
   _FLUX_VAR_(data%id_tst_pel) = pel / secs_per_day
END SUBROUTINE aed2_calculate_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_benthic_test(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED test.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_test_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Temporary variables
   AED_REAL :: ben
!
!-------------------------------------------------------------------------------
!BEGIN
   ben = _STATE_VAR_S_(data%id_tst_ben)
   ben = 10 - ben
   _FLUX_VAR_B_(data%id_tst_ben) = ben / secs_per_day
END SUBROUTINE aed2_calculate_benthic_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_test
