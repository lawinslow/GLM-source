!###############################################################################
!#                                                                             #
!# aed2_totals.F90                                                             #
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

!
MODULE aed2_totals
!-------------------------------------------------------------------------------
! aed2_totals --- totals biogeochemical model
!
! The AED module totals contains only diagnostic variables to provide
! totals of other variables (eg tss)
!-------------------------------------------------------------------------------
   USE aed2_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed2_totals_data_t
!
   TYPE,extends(aed2_model_data_t) :: aed2_totals_data_t
      !# Variable identifiers
      INTEGER  :: id_totals_tn, id_totals_tp, id_totals_toc,                   &
                                            id_totals_tss, id_totals_turbidity
      INTEGER,ALLOCATABLE :: id_dep_tn(:), id_dep_tp(:), id_dep_toc(:),        &
                                           id_dep_tss(:)
      AED_REAL,ALLOCATABLE :: tn_varscale(:), tp_varscale(:), toc_varscale(:), &
                                             tss_varscale(:)


      !# Model parameters
      AED_REAL :: Fsed_dic,Ksed_dic,theta_sed_dic
      LOGICAL  :: use_oxy,use_dic

     CONTAINS
         PROCEDURE :: define            => aed2_define_totals
         PROCEDURE :: calculate         => aed2_calculate_totals
!        PROCEDURE :: mobility          => aed2_mobility_totals
!        PROCEDURE :: light_extinction  => aed2_light_extinction_totals
!        PROCEDURE :: delete            => aed2_delete_totals

   END TYPE


!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed2_define_totals(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and te variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed2_totals_data_t),INTENT(inout) :: data

!
!LOCALS
   INTEGER  :: status

   INTEGER  :: i, num_tn,num_tp,num_toc,num_tss
   CHARACTER(len=40) :: tn_vars(100), tp_vars(100), toc_vars(100), tss_vars(100)
   AED_REAL          :: tn_varscale(100), tp_varscale(100)
   AED_REAL          :: toc_varscale(100), tss_varscale(100)

   NAMELIST /aed2_totals/ tn_vars,  tn_varscale,  tp_vars,  tp_varscale,  &
                          toc_vars, toc_varscale, tss_vars, tss_varscale
!
!-------------------------------------------------------------------------------
!BEGIN
   tn_vars = '' ;      tp_vars = '' ;      toc_vars = '' ;      tss_vars = ''
   tn_varscale = 1.0 ; tp_varscale = 1.0 ; toc_varscale = 1.0 ; tss_varscale = 1.0

   ! Read the namelist
   read(namlst,nml=aed2_totals,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed2_totals'

   DO i=1,100 ; IF (tn_vars(i)  .EQ. '' ) THEN ; num_tn  = i-1 ; EXIT ; ENDIF ; ENDDO
   DO i=1,100 ; IF (tp_vars(i)  .EQ. '' ) THEN ; num_tp  = i-1 ; EXIT ; ENDIF ; ENDDO
   DO i=1,100 ; IF (toc_vars(i) .EQ. '' ) THEN ; num_toc = i-1 ; EXIT ; ENDIF ; ENDDO
   DO i=1,100 ; IF (tss_vars(i) .EQ. '' ) THEN ; num_tss = i-1 ; EXIT ; ENDIF ; ENDDO

!  print *,"totl tn = ",num_tn," num_tp = ",num_tp," num toc = ",num_toc," num_tss = ",num_tss
   ALLOCATE(data%id_dep_tn(num_tn))   ; ALLOCATE(data%tn_varscale(num_tn))
   ALLOCATE(data%id_dep_tp(num_tp))   ; ALLOCATE(data%tp_varscale(num_tp))
   ALLOCATE(data%id_dep_toc(num_toc)) ; ALLOCATE(data%toc_varscale(num_toc))
   ALLOCATE(data%id_dep_tss(num_tss)) ; ALLOCATE(data%tss_varscale(num_tss))

   ! Register external state variable dependencies
   DO i=1,num_tn
      data%id_dep_tn(i) =  aed2_locate_variable(tn_vars(i))
      data%tn_varscale(i) =  tn_varscale(i)
!     print*,'TN : ', TRIM(tn_vars(i)), ' * ', data%tn_varscale(i)
   ENDDO
   DO i=1,num_tp
      data%id_dep_tp(i) =  aed2_locate_variable(tp_vars(i))
      data%tp_varscale(i) =  tp_varscale(i)
!     print*,'TP : ', TRIM(tp_vars(i)), ' * ', data%tp_varscale(i)
   ENDDO
   DO i=1,num_toc
      data%id_dep_toc(i) = aed2_locate_variable(toc_vars(i))
      data%toc_varscale(i) = toc_varscale(i)
!     print*,'TOC : ', TRIM(toc_vars(i)), ' * ', data%toc_varscale(i)
   ENDDO
   DO i=1,num_tss
      data%id_dep_tss(i) = aed2_locate_variable(tss_vars(i))
      data%tss_varscale(i) = tss_varscale(i)
!     print*,'TSS : ', TRIM(tss_vars(i)), ' * ', data%tss_varscale(i)
   ENDDO

   ! Register diagnostic variables
   data%id_totals_tn = aed2_define_diag_variable('tn',               &
                     'mmol/m**2/d', 'Filterable reactive totals')

   data%id_totals_tp = aed2_define_diag_variable('tp',               &
                     'mmol/m**2/d', 'Filterable reactive totals')

   data%id_totals_toc = aed2_define_diag_variable('toc',             &
                     'mmol/m**2/d', 'Filterable reactive totals')

   data%id_totals_tss = aed2_define_diag_variable('tss',             &
                     'mmol/m**2/d', 'Filterable reactive totals')

   data%id_totals_turbidity = aed2_define_diag_variable('turbidity', &
                     'mmol/m**2/d', 'Filterable reactive totals')
END SUBROUTINE aed2_define_totals
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_totals(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Right hand sides of aed2_totals model
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_totals_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   INTEGER  :: i,count
   AED_REAL :: val, tot, tot2

!-------------------------------------------------------------------------------
!BEGIN

   ! Retrieve current (local) state variable values.
   tot = 0.
   count = ubound(data%id_dep_tn,1)
   DO i=1,count ; val = _STATE_VAR_(data%id_dep_tn(i)); tot = tot + (val*data%tn_varscale(i)) ; ENDDO
   _DIAG_VAR_(data%id_totals_tn) =  tot

   tot = 0.
   count = ubound(data%id_dep_tp,1)
   DO i=1,count ; val = _STATE_VAR_(data%id_dep_tp(i)); tot = tot + (val*data%tp_varscale(i)) ; ENDDO
   _DIAG_VAR_(data%id_totals_tp) =  tot

   tot = 0.
   count = ubound(data%id_dep_toc,1)
   DO i=1,count ; val = _STATE_VAR_(data%id_dep_toc(i)); tot = tot + (val*data%toc_varscale(i)) ; ENDDO
   _DIAG_VAR_(data%id_totals_toc) =  tot

   tot = 0.
   tot2 = 0.
   count = ubound(data%id_dep_tss,1)
   DO i=1,count
      val = _STATE_VAR_(data%id_dep_tss(i))
      tot = tot + val
      tot2 = tot2 + val*(data%tss_varscale(i))
   ENDDO

   _DIAG_VAR_(data%id_totals_tss) =  tot
   _DIAG_VAR_(data%id_totals_turbidity) =  tot2

END SUBROUTINE aed2_calculate_totals
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_totals
