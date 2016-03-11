!###############################################################################
!#                                                                             #
!# aed2_ass.F90                                                                #
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
!# Created Oct 2015                                                            #
!#                                                                             #
!###############################################################################

! /AED/StudySites/CoorongLowerLakes/simulations/CoorongLowerLakes_i7/SeawaterEIS/010_anc_896_ASS01_yAB/caedym
! ! -----------------------------------------------------------------------------!
! ! Depth to perching (m)                                                        !
!  1.50000E+00                 : ASS SOIL 1 - Sand with Low OM content           !
!  1.50000E+00                 : ASS SOIL 2 - Clay with Low OM content           !
!  1.50000E+00                 : ASS SOIL 3 - Sand with High OM content          !
!  1.50000E+00                 : ASS SOIL 4 - Clay with High OM content          !
! ! Potential Acidity (mol H+/kg)                                                !
!  0.041000E+00                : ASS SOIL 1 - Sand with Low OM content           !
!  0.250000E+00                : ASS SOIL 2 - Clay with Low OM content           !
!  0.041000E+00                : ASS SOIL 3 - Sand with High OM content          !
!  0.250000E+00                : ASS SOIL 4 - Clay with High OM content          !
! ! Sediment density (kg/m3)                                                     !
!  1530.000000                 : ASS SOIL 1 - Sand with Low OM content           !
!  1230.000000                 : ASS SOIL 2 - Clay with Low OM content           !
!  1530.000000                 : ASS SOIL 3 - Sand with High OM content          !
!  1230.000000                 : ASS SOIL 4 - Clay with High OM content          !
! ! Acidity production rate (- scaling factor for Taylor OxCOn algorithm)        !
!  1.000000000                 : ASS SOIL 1 - Sand with Low OM content           !
!  1.000000000                 : ASS SOIL 2 - Clay with Low OM content           !
!  1.000000000                 : ASS SOIL 3 - Sand with High OM content          !
!  1.000000000                 : ASS SOIL 4 - Clay with High OM content          !
! ! ASS flux parameter - baseflow (/day)                                         !
!  0.000000000                 : ASS SOIL 1 - Sand with Low OM content           !
!  0.000000000                 : ASS SOIL 2 - Clay with Low OM content           !
!  0.000000000                 : ASS SOIL 3 - Sand with High OM content          !
!  0.000000000                 : ASS SOIL 4 - Clay with High OM content          !
! ! ASS flux parameter - rain pulsing (/day) & (mm)                              !
!  0.000 0.000                 : ASS SOIL 1 - Sand with Low OM content           !
!  0.000 0.000                 : ASS SOIL 2 - Clay with Low OM content           !
!  0.000 0.000                 : ASS SOIL 3 - Sand with High OM content          !
!  0.000 0.000                 : ASS SOIL 4 - Clay with High OM content          !
! ! ASS flux parameter - rewetting (Day1: mol H+/m2/day & Day2-90:mol H+/m2/day) !
! 0.1380 0.0070                : ASS SOIL 1 - Sand with Low OM content           !
! 0.1610 0.0100                : ASS SOIL 2 - Clay with Low OM content           !
! 0.1380 0.0070                : ASS SOIL 3 - Sand with High OM content          !
! 0.1610 0.0100                : ASS SOIL 4 - Clay with High OM content          !
!#
!# Translates to :
!#
!     AED_REAL :: aep(MAX_ASS_PARAMS)
!     AED_REAL :: ass(MAX_ASS_PARAMS)
!     AED_REAL :: Bss(MAX_ASS_PARAMS)
!     AED_REAL :: Debs(MAX_ASS_PARAMS)
!     AED_REAL :: Dper(MAX_ASS_PARAMS)
!     AED_REAL :: Porosity(MAX_ASS_PARAMS)
!     AED_REAL :: Density(MAX_ASS_PARAMS)
!     AED_REAL :: zASS
!
! &aed2_ass
!    nPars   = 4
!    dPer    =  1.50000E+00,  1.50000E+00,  1.50000E+00,  1.50000E+00  ! Depth to perching (m)
!    potAcid = 0.041000E+00, 0.250000E+00, 0.041000E+00, 0.250000E+00  ! Potential Acidity (mol H+/kg)
!    Density =  1530.000000,  1230.000000,  1530.000000,  1230.000000  ! Sediment density (kg/m3)
!    acidProd=  1.000000000,  1.000000000,  1.000000000,  1.000000000  ! Acidity production rate
!    fluxbas =  0.000000000,  0.000000000,  0.000000000,  0.000000000  ! ASS flux parameter - baseflow (/day)
!    fluxrain=        0.000,        0.000,        0.000,        0.000  ! ASS flux parameter - rain pulsing (/day)
!    fr_mm   =        0.000,        0.000,        0.000,        0.000  ! ASS flux parameter - rain pulsing (mm)
!    flxrwet1=       0.1380,       0.1610,       0.1380,       0.1610  ! ASS flux parameter - rewetting (Day1
!    flxrwet2=       0.0070,       0.0100,       0.0070,       0.0100  ! ASS flux parameter - rewetting (Day2
! /


#include "aed2.h"

!
MODULE aed2_ass
!------------------------------------------------------------------------------+
! AED2 module for Acid Sulfate Soils                                           |
!------------------------------------------------------------------------------+
   USE aed2_core
   USE aed2_util

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed2_ass_data_t
!
   TYPE,extends(aed2_model_data_t) :: aed2_ass_data_t
      !# Variable identifiers
      INTEGER :: id_St       !# total soil water storage (mm)
      INTEGER :: id_Ssat
      INTEGER :: id_Theta    !# unsaturated moisture content (-)
      INTEGER :: id_Phreatic !# depth of phreatic surface below surface (m)

      !# Environmental variables
      INTEGER :: id_E_rain
      INTEGER :: id_E_area
      INTEGER :: id_E_height
      INTEGER :: id_E_density
      INTEGER :: id_E_material
      INTEGER :: id_E_bathy

      !# Diagnostic variables
      INTEGER :: id_depth
      INTEGER :: id_area
      INTEGER :: id_bathy
      INTEGER :: id_Sb
      INTEGER :: id_qss
      INTEGER :: id_qse
      INTEGER :: id_qcap
      INTEGER :: id_qper

      INTEGER :: id_inited

      !# ASS params
      INTEGER  :: nPars
      AED_REAL :: aep(MAX_ASS_PARAMS)
      AED_REAL :: ass(MAX_ASS_PARAMS)
      AED_REAL :: Bss(MAX_ASS_PARAMS)
      AED_REAL :: Debs(MAX_ASS_PARAMS)
      AED_REAL :: Dper(MAX_ASS_PARAMS)
      AED_REAL :: Porosity(MAX_ASS_PARAMS)
      AED_REAL :: Density(MAX_ASS_PARAMS)
      AED_REAL :: zASS

      ! Moisture disaggregation model
      INTEGER  :: nlay

      AED_REAL :: Dcap
      AED_REAL :: Zcap
      AED_REAL :: Dtrn
      AED_REAL :: Ztrn

     CONTAINS
         PROCEDURE :: define             => aed2_define_ass
!        PROCEDURE :: calculate_surface  => aed2_calculate_surface_ass
!        PROCEDURE :: calculate          => aed2_calculate_ass
!        PROCEDURE :: calculate_riparian => aed2_calculate_riparian_ass
         PROCEDURE :: calculate_benthic  => aed2_calculate_benthic_ass
         PROCEDURE :: calculate_dry      => aed2_calculate_dry_ass
!        PROCEDURE :: equilibrate        => aed2_equilibrate_ass
!        PROCEDURE :: mobility           => aed2_mobility_ass
!        PROCEDURE :: light_extinction   => aed2_light_extinction_ass
!        PROCEDURE :: delete             => aed2_delete_ass
   END TYPE

   TYPE SoilHydroUnit
        INTEGER   :: UnitNum                    = 0
        INTEGER   :: Substrate                  = 0

        ! Physical properties
        AED_REAL  :: Depth
        AED_REAL  :: Area
        AED_REAL  :: Bathy
        AED_REAL  :: Sb
        ! Hydrologic dynamics
        AED_REAL  :: St
        AED_REAL  :: Ssat
        AED_REAL  :: PhreaticHgt
        AED_REAL  :: PhreaticDepth
        AED_REAL  :: qss
        AED_REAL  :: qse
        AED_REAL  :: recharge
        AED_REAL  :: qsuc
        ! Hydrologic parameters
        AED_REAL  :: aep
        AED_REAL  :: ass
        AED_REAL  :: Bss
        AED_REAL  :: Debs
        AED_REAL  :: Dper
        AED_REAL  :: Porosity
        AED_REAL  :: Density
        ! Moisture disaggregation model
        INTEGER   :: nlay
        AED_REAL  :: Dcap
        AED_REAL  :: Zcap
        AED_REAL  :: Dtrn
        AED_REAL  :: Ztrn
        AED_REAL, DIMENSION(:), ALLOCATABLE :: Moisture
        AED_REAL  :: fc
        AED_REAL  :: Sus
        AED_REAL  :: S_top
        AED_REAL  :: S_trn
        AED_REAL  :: S_cap
        AED_REAL  :: pastMaxLevel
        AED_REAL  :: rn
        AED_REAL  :: et
        AED_REAL  :: es
   END TYPE SoilHydroUnit


!-------------------------------------------------------------------------------
!MODULE VARIABLES

   TYPE(SoilHydroUnit), ALLOCATABLE :: SoilHydroData

   INTEGER, PARAMETER :: Hydrology = 1
   INTEGER, PARAMETER :: BUCKET = 1

   !#CAB Dont know where these will come from
   AED_REAL :: day_rain, bathy
   AED_REAL :: angfreq, epdayav, epamp, epphase, DDT

!===============================================================================
CONTAINS

!###############################################################################
!#CAB again, not sure what this should do (layer_height perhaps?)
AED_REAL FUNCTION GetBathy()
   GetBathy = 0.
END FUNCTION GetBathy
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_define_ass(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and the variables exported
!  by the model are registered with AED2.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_ass_data_t),INTENT(inout) :: data
   INTEGER,INTENT(in) :: namlst
!
!LOCALS
   INTEGER :: status, i

   !# The parameters
   INTEGER  :: nPars

   ! Hydrologic parameters
   AED_REAL :: aep(MAX_ASS_PARAMS)
   AED_REAL :: ass(MAX_ASS_PARAMS)
   AED_REAL :: Bss(MAX_ASS_PARAMS)
   AED_REAL :: Debs(MAX_ASS_PARAMS)
   AED_REAL :: Dper(MAX_ASS_PARAMS)
   AED_REAL :: Porosity(MAX_ASS_PARAMS)
   AED_REAL :: Density(MAX_ASS_PARAMS)

   ! Moisture disaggregation model
   INTEGER  :: nlay

   AED_REAL :: Dcap
   AED_REAL :: Zcap
   AED_REAL :: Dtrn
   AED_REAL :: Ztrn

   NAMELIST /aed2_ass/ nPars,                                        &
                       aep, ass, Bss, Debs, Dper, Porosity, Density, &
                       nlay, Dcap, Zcap, Dtrn, Ztrn
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Read the namelist
   read(namlst,nml=aed2_ass,iostat=status)
   IF (status /= 0) STOP 'Error reading namelist aed2_ass'

   data%nPars = nPars
   DO i=1, nPars
      data%aep(i)  = aep(i)
      data%ass(i)  = ass(i)
      data%Bss(i)  = Bss(i)
      data%Debs(i) = Debs(i)
      data%Dper(i) = Dper(i)
      data%Porosity(i) = Porosity(i)
      data%Density(i)  = Density(i)
   ENDDO

   data%nlay  = nlay

   data%Dcap  = Dcap
   data%Zcap  = Zcap
   data%Dtrn  = Dtrn
   data%Ztrn  = Ztrn

   !# Register state variables
   data%id_St = aed2_define_sheet_variable('St','mm',   &
                                         'total soil water storage')
   data%id_Ssat = aed2_define_sheet_variable('Ssat','mm',   &
                                         'saturated zone soil water storage')
   data%id_theta  = aed2_define_sheet_variable('theta','-',   &
                                         'unsaturated moisture content')
   data%id_phreatic = aed2_define_sheet_variable('phreatic','m',   &
                                         'depth of phreatic surface below surface')

   ! Register diagnostic variables
   data%id_depth = aed2_define_sheet_diag_variable('depth','m','soil depth (to datum)')
   data%id_area = aed2_define_sheet_diag_variable('area','m2','soil area')
   data%id_bathy = aed2_define_sheet_diag_variable('bathy','m','soil surface height')
   data%id_Sb = aed2_define_sheet_diag_variable('Sb','mm','bucket capacity for water storage')
   data%id_qss = aed2_define_sheet_diag_variable('qss','mm','sat zone seepage')
   data%id_qse = aed2_define_sheet_diag_variable('qse','mm','surface runoff')
   data%id_qcap = aed2_define_sheet_diag_variable('qcap','mm','capillarity')
   data%id_qper = aed2_define_sheet_diag_variable('qper','mm','recharge')
   data%id_inited = aed2_define_sheet_diag_variable('inited','','cell initialised')

   !# Register environmental dependencies
   data%id_E_rain = aed2_locate_global_sheet('rain')   ! daily rainfall
   data%id_E_area = aed2_locate_global('layer_area')   ! cell area
   data%id_E_height = aed2_locate_global('layer_ht')   ! cell height
   data%id_E_density = aed2_locate_global('density')   ! cell density
   data%id_E_material = aed2_locate_global('material') ! material index
   data%id_E_bathy = aed2_locate_global('bathy')       ! cell bethy
END SUBROUTINE aed2_define_ass
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_dry_ass(data, column, layer_idx)
!-------------------------------------------------------------------------------
! Main interface routine for the hydrology/ass module. Assumes the module has
! already been configured (ConfigureHydrology) and initialised (InitialHydrology)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_ass_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL :: rain

!  INTEGER  :: t_val(8)                               ! Time counter array
   INTEGER  :: var, i, sub, MatZoneID
   AED_REAL :: avgLevel
!
!-------------------------------------------------------------------------------
!BEGIN
   IF (_DIAG_VAR_S_(data%id_inited) .EQ. 0) THEN
      CALL InitialHydrology(data, column, layer_idx, .TRUE.)
      _DIAG_VAR_S_(data%id_inited) = 1
   ENDIF

   ! Cumulate rain over the day
   rain = _STATE_VAR_S_(data%id_E_rain)
   day_rain = day_rain + rain * secs_per_day * DDT


  ! Update moving average of lake water level here
  ! HL =

   ! Check if modelling cells as buckets or ...
   IF (Hydrology == BUCKET) THEN

!CAB  avgLevel =  AverageWaterLevel()
!MH   AverageWaterLevel needs to get the average water height across the model domain
      avgLevel = 0.0

      ! Populate SoilHydroData with data from cell we are currently operating on
      ALLOCATE(SoilHydroData)

      MatZoneID = _STATE_VAR_S_(data%id_E_material)
      ! Localize fixed parameters based on Material Type
      CALL SetBucketParameters(data, SoilHydroData, MatZoneID)

      ! Physical properties (stored in global diagnostics)
      SoilHydroData%Depth = _DIAG_VAR_S_(data%id_depth)
      SoilHydroData%Area  = _DIAG_VAR_S_(data%id_area)
      SoilHydroData%Bathy = _DIAG_VAR_S_(data%id_bathy)
      SoilHydroData%Sb    = _DIAG_VAR_S_(data%id_Sb)
      ! Dynamic properties (stored in global state array)
      SoilHydroData%St    = _STATE_VAR_S_(data%id_St)
      SoilHydroData%Ssat  = _STATE_VAR_S_(data%id_Ssat)
      SoilHydroData%Sus   = SoilHydroData%St - SoilHydroData%Ssat
      SoilHydroData%PhreaticDepth  = _STATE_VAR_S_(data%id_Phreatic)
      SoilHydroData%PhreaticHgt    = SoilHydroData%Bathy - SoilHydroData%Depth + SoilHydroData%PhreaticDepth
      ! Process rates (stored in diagnostics)
      SoilHydroData%qss   = _DIAG_VAR_S_(data%id_qss)
      SoilHydroData%qse   = _DIAG_VAR_S_(data%id_qse)
      SoilHydroData%qsuc  = _DIAG_VAR_S_(data%id_qcap)
      SoilHydroData%recharge = _DIAG_VAR_S_(data%id_qper)


      ! Undertake the update the soil (vertical) water balance
      CALL UpdateSoilHydrology(SoilHydroData, day_rain, avgLevel)


      ! Update the main AED arrays with SoilHydroData
      _STATE_VAR_S_(data%id_St)       = SoilHydroData%St
      _STATE_VAR_S_(data%id_Ssat)     = SoilHydroData%Ssat
      _STATE_VAR_S_(data%id_Phreatic) = SoilHydroData%PhreaticDepth
      _STATE_VAR_S_(data%id_Theta)    = SoilHydroData%Sus/(SoilHydroData%Porosity*MAX(SoilHydroData%PhreaticHgt,0.001))

      _DIAG_VAR_S_(data%id_qss)  = SoilHydroData%qss
      _DIAG_VAR_S_(data%id_qse)  = SoilHydroData%qse
      _DIAG_VAR_S_(data%id_qper) = SoilHydroData%qsuc
      _DIAG_VAR_S_(data%id_qcap) = SoilHydroData%recharge

      DEALLOCATE(SoilHydroData)

   ELSE

   ENDIF
END SUBROUTINE aed2_calculate_dry_ass
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed2_calculate_benthic_ass(data, column, layer_idx)
!-------------------------------------------------------------------------------
! Dummy benthic interface used to force values of hydrology/ass variables to 0
! when they go under water
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_ass_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL :: porosity, Sb, density
!
!-------------------------------------------------------------------------------
!BEGIN
   IF (_DIAG_VAR_S_(data%id_inited) .EQ. 0) THEN
      CALL InitialHydrology(data, column, layer_idx, .FALSE.)
      _DIAG_VAR_S_(data%id_inited) = 1
   ENDIF


   ! Get static properties and force bucket hydrology
   Sb = _STATE_VAR_S_(data%id_Sb)
   bathy = _STATE_VAR_S_(data%id_bathy)
!# where did this come in?
!  porosity = _STATE_VAR_S_(data%id_poro)
   density = _STATE_VAR_S_(data%id_E_density)

   ! Force the variables to saturated defaults
   _STATE_VAR_S_(data%id_St) = Sb
   _STATE_VAR_S_(data%id_Ssat) = Sb
   _STATE_VAR_S_(data%id_phreatic) = 0.0
   _STATE_VAR_S_(data%id_theta) = porosity
!  _STATE_VAR_S_(data%id_Sus) = 0.0

   _DIAG_VAR_S_(data%id_qss)  = 0.0
   _DIAG_VAR_S_(data%id_qse)  = 0.0
   _DIAG_VAR_S_(data%id_qper) = 0.0
   _DIAG_VAR_S_(data%id_qcap) = 0.0

   !SoilHydroData%St             = SoilHydroData%Sb
   !SoilHydroData%Ssat           = SoilHydroData%Sb
   !SoilHydroData%PhreaticDepth  = 0.0
   !SoilHydroData%PhreaticHgt    = SoilHydroData%Bathy
   !SoilHydroData%Sus            = 0.0
   !SoilHydroData%Moisture       = 1.0
   !SoilHydroData%Moisture       = SoilHydroData%Moisture * SoilHydroData%Porosity * 1000. &
   !            /(SoilHydroData%Moisture*SoilHydroData%Porosity*1000. + (1.0-SoilHydroData%Porosity)*SoilHydroData%Density)
   !SoilHydroData%qss            = 0.0
   !SoilHydroData%qse            = 0.0
   !SoilHydroData%recharge       = 0.0
END SUBROUTINE aed2_calculate_benthic_ass
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE InitialHydrology(data, column, layer_idx, CellIsDry)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_ass_data_t),INTENT(in) :: data
   TYPE (aed2_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
   LOGICAL,INTENT(in) :: CellIsDry

!LOCALS
   INTEGER :: i, sub,  n, chnk, mincell, sIndex
   INTEGER :: MatZoneID
   CHARACTER(LEN=4) :: filetype
   AED_REAL :: livearea, hgtInc, ivec(1), initWTDepth, avgLevel, minbathy
   AED_REAL :: area, height
!
!-------------------------------------------------------------------------------
!BEGIN
   print*,'InitialHydrology'

   bathy = GetBathy()  !MH we need to get the cell height from TFV

   avgLevel =  0.0

   IF (Hydrology == BUCKET) THEN

     initWTDepth = 0.05
     minbathy = 0!MH set by user, or computed as 2m below lowest cell in mesh

     SoilHydroData%nlay = 20
     ALLOCATE( SoilHydroData%Moisture(SoilHydroData%nlay) )

     ! SoilHydroData%UnitNum  = i

     !sIndex = SubstrateType - 3

     MatZoneID = _STATE_VAR_S_(data%id_E_material)
     CALL SetBucketParameters(data, SoilHydroData, MatZoneID)

!     ! Set Substrate Specific Constants
!     IF( SubstrateType == BEDROCK ) THEN
!         ! BEDROCK
!         SoilHydroData%aep      = 0.5
!         SoilHydroData%ass      = 0.005
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.5
!         SoilHydroData%Porosity = 0.5
!         SoilHydroData%fc       = 0.0 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = 0.0
!         SoilHydroData%Density  = 2650.
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     ELSE IF( SubstrateType == SOFTSAND ) THEN
!         ! SOFTSAND
!         SoilHydroData%aep      = 0.5
!         SoilHydroData%ass      = 0.005
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.5
!         SoilHydroData%Porosity = 0.42
!         SoilHydroData%fc       = 0.15 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = 2.0
!         SoilHydroData%Density  = 1530.
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     ELSE IF( SubstrateType == DENSEMUD ) THEN
!         ! DENSEMUD
!         SoilHydroData%aep      = 0.5
!         SoilHydroData%ass      = 0.005
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.5
!         SoilHydroData%Porosity = 0.53
!         SoilHydroData%fc       = 0.40 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = 1.0
!         SoilHydroData%Density  = 1300.
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     ELSE IF( SubstrateType == ASSSANDL ) THEN
!         ! ASSSANDL
!         SoilHydroData%aep      = 0.75
!         SoilHydroData%ass      = data%flux_bf(sIndex)
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.35
!         SoilHydroData%Porosity = 0.45
!         SoilHydroData%fc       = 0.15 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = data%zASS(sIndex)
!         SoilHydroData%Density  = data%sedDensity(sIndex)
!         SoilHydroData%Zcap     = 0.2
!         SoilHydroData%Ztrn     = 0.2
!     ELSE IF( SubstrateType == ASSSANDH ) THEN
!         ! ASSSANDH
!         SoilHydroData%aep      = 0.75
!         SoilHydroData%ass      = data%flux_bf(sIndex)
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.40
!         SoilHydroData%Porosity = 0.42
!         SoilHydroData%fc       = 0.15 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = data%zASS(sIndex)
!         SoilHydroData%Density  = data%sedDensity(sIndex)
!         SoilHydroData%Zcap     = 0.2
!         SoilHydroData%Ztrn     = 0.2
!     ELSE IF( SubstrateType == ASSCLAYL ) THEN
!          ! ASSCLAYL
!         SoilHydroData%aep      = 0.75
!         SoilHydroData%ass      = data%flux_bf(sIndex)
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.25
!         SoilHydroData%Porosity = 0.6
!         SoilHydroData%fc       = 0.40 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = data%zASS(sIndex)
!         SoilHydroData%Density  = data%sedDensity(sIndex)
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     ELSE IF( SubstrateType == ASSCLAYH ) THEN
!         ! ASSCLAYH
!         SoilHydroData%aep      = 0.75
!         SoilHydroData%ass      = data%flux_bf(sIndex)
!         SoilHydroData%Bss      = 1.5
!         SoilHydroData%Debs     = 0.25
!         SoilHydroData%Porosity = 0.6
!         SoilHydroData%fc       = 0.40 / SoilHydroData%Porosity
!         SoilHydroData%Dper     = data%zASS(sIndex)
!         SoilHydroData%Density  = data%sedDensity(sIndex)
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     ELSE
!         ! Default
!         SoilHydroData%aep      = 0.5
!         SoilHydroData%ass      = 0.0002
!         SoilHydroData%Bss      = 1.4
!         SoilHydroData%Debs     = 0.5
!         SoilHydroData%fc       = 0.4
!         SoilHydroData%Porosity = 0.5
!         SoilHydroData%Dper     = 2.0
!         SoilHydroData%Density  = 1500.
!         SoilHydroData%Zcap     = 0.3
!         SoilHydroData%Ztrn     = 0.3
!     END IF


     area = _STATE_VAR_(data%id_E_area)
     bathy = _STATE_VAR_(data%id_E_height)

     ! Cell geometry
     SoilHydroData%Area     = area !MH Area of the cell
     SoilHydroData%Bathy    = bathy !MH Height in the layer

     !SoilHydroData%Depth   = minbathy - bathy ! - minbathy ! 2 for padding
     SoilHydroData%Depth    = MIN( minbathy - bathy , data%zASS )  !MH zASS needs to be read in

     SoilHydroData%pastMaxLevel = SoilHydroData%Bathy

     SoilHydroData%Debs     = SoilHydroData%Depth - SoilHydroData%Debs
     IF(SoilHydroData%Debs > SoilHydroData%Depth) THEN
         SoilHydroData%Debs = SoilHydroData%Depth
     END IF

     ! Soil capacity
     SoilHydroData%Sb = SoilHydroData%Depth * SoilHydroData%Porosity

     ! Initialisation
     IF (CellIsDry) THEN
         !-- Cell is exposed    ( may get from WQ%sed(i,PHREATIC) to initialise)
         IF(SoilHydroData%Bathy + initWTDepth > avgLevel) THEN
           ! Soil surface is less than initWTDepth above lake
           SoilHydroData%PhreaticDepth = SoilHydroData%Bathy + avgLevel
         ELSE
           ! Soil surface is > initWTDepth above lake
           SoilHydroData%PhreaticDepth = initWTDepth
         END IF

         SoilHydroData%Ssat  = (SoilHydroData%Depth-SoilHydroData%PhreaticDepth) &
                                * SoilHydroData%Porosity

         SoilHydroData%Moisture = 0.5 ! WQ%sed(i,UZMOIST)
         SoilHydroData%Moisture = SoilHydroData%Moisture * SoilHydroData%Porosity * 1000. &
          /(SoilHydroData%Moisture*SoilHydroData%Porosity*1000. + (1.0-SoilHydroData%Porosity)*SoilHydroData%Density)

         SoilHydroData%Sus   = SoilHydroData%PhreaticDepth* SoilHydroData%Porosity &
                                * SoilHydroData%Moisture(1)   !! FIX THIS
         SoilHydroData%St    = SoilHydroData%Ssat + SoilHydroData%Sus
     ELSE
         !-- Cell is underwater
         SoilHydroData%PhreaticDepth = 0.0
         SoilHydroData%St            = SoilHydroData%Sb
         SoilHydroData%Ssat          = SoilHydroData%Sb
         SoilHydroData%Sus           = 0.0
         SoilHydroData%Moisture      = 1.0 * SoilHydroData%Porosity * 1000. &
         /(SoilHydroData%Porosity * 1000. + (1.0-SoilHydroData%Porosity)*SoilHydroData%Density)
     END IF

     ! Initialise processes to 0
     SoilHydroData%qss            = 0.0
     SoilHydroData%qse            = 0.0
     SoilHydroData%recharge       = 0.0
     SoilHydroData%qsuc           = 0.0
     SoilHydroData%rn             = 0.0
     SoilHydroData%et             = 0.0
     SoilHydroData%es             = 0.0

     SoilHydroData%PhreaticHgt = SoilHydroData%Bathy+SoilHydroData%PhreaticDepth

     IF(SoilHydroData%PhreaticHgt-SoilHydroData%Zcap < SoilHydroData%Bathy) THEN
         SoilHydroData%Dcap = SoilHydroData%Bathy
     ELSE
         SoilHydroData%Dcap = SoilHydroData%PhreaticHgt - SoilHydroData%Zcap
     ENDIF
     SoilHydroData%S_cap = 0.9*(SoilHydroData%PhreaticHgt - SoilHydroData%Dcap)* SoilHydroData%Porosity

     IF(SoilHydroData%Dcap - SoilHydroData%Ztrn < SoilHydroData%Bathy) THEN
         SoilHydroData%Dtrn = SoilHydroData%Bathy
     ELSE
         SoilHydroData%Dtrn = SoilHydroData%Dcap - SoilHydroData%Ztrn
     ENDIF
     SoilHydroData%S_trn = 0.5*(SoilHydroData%Dcap - SoilHydroData%Dtrn)* SoilHydroData%Porosity

     SoilHydroData%S_top = 0.4*(SoilHydroData%Dtrn - SoilHydroData%Bathy)* SoilHydroData%Porosity

     SoilHydroData%Sus = SoilHydroData%S_top + SoilHydroData%S_trn + SoilHydroData%S_cap

     _FLUX_VAR_B_(data%id_Phreatic) = SoilHydroData%PhreaticDepth
     _FLUX_VAR_B_(data%id_Theta)    = SoilHydroData%Moisture(1)
     _FLUX_VAR_B_(data%id_St)       = SoilHydroData%St
   ENDIF
END SUBROUTINE InitialHydrology
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE UpdateSoilHydrology(theSoil, rain, avgLevel)
!-------------------------------------------------------------------------------
! Opens an input file. If the file is opened successfully the routine          !
! returns normally, if not then the routine terminates.                        !
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(SoilHydroUnit) :: theSoil
   AED_REAL :: rain, avgLevel
!LOCALS
   AED_REAL :: flow, evap, evapsat
   AED_REAL :: Sttm1, Ssattm1, Susfc, Susp
   INTEGER  :: lay
   AED_REAL :: dep, depm1, middep, botmoist, topmoist, top_depth

!-------------------------------------------------------------------------------
!BEGIN

   flow = 0.0
   evap = 0.0
   evapsat = 0.0

   IF(Hydrology == BUCKET) THEN
       ! Set prev timestep vars
       Sttm1   =   theSoil%St
       Ssattm1 =   theSoil%Ssat

       ! Unsaturated Zone capacity
    !   Susfc = (theSoil%Sb - Ssattm1 - theSoil%S_trn - theSoil%S_cap ) * theSoil%fc
    !   Susp  = theSoil%S_top + rain

       top_depth =  ( theSoil%Dtrn - theSoil%Bathy )
       Susfc =  top_depth * theSoil%Porosity * theSoil%fc
       Susp  = theSoil%S_top + rain

       IF(Susp > (theSoil%Dtrn - theSoil%Bathy)* theSoil%Porosity) THEN
          theSoil%qse = Susp - (0.9 * (top_depth * theSoil%Porosity))
          rain = rain - theSoil%qse
          Susp  = theSoil%S_top + rain
       ELSE
           theSoil%qse = 0.0
       END IF

       ! Recharge/percolation
       IF(Susp>Susfc) THEN
         theSoil%recharge = 0.75 * (Susp - Susfc)
       ELSE
         theSoil%recharge = 0.0
       END IF

       !print *,'ss: ',Sttm1, Ssattm1, Susfc, Susp, theSoil%recharge

       ! Evaporation
       IF(Ssattm1 <=  theSoil%Porosity*theSoil%Debs) THEN
           ! Only take from unsaturated volume
           !evap = theSoil%aep * theSoil%Sus / ( theSoil%Sb - Ssattm1 )
           evap = theSoil%aep * MIN(theSoil%Sus / MAX(theSoil%Sb - Ssattm1,1e-2),1.0)

!#CAB jm and jd are not defined
!#CAB      evap = evap * GetPotlEvap(jm,jd) * theSoil%Porosity
           evapsat = 0.0
       ELSE
           ! Take from unsaturated and saturated regions
           evap = theSoil%aep * (theSoil%Sus + (Ssattm1 - theSoil%Porosity*theSoil%Debs)) &
                / MIN( theSoil%Sb - theSoil%Porosity*theSoil%Debs, 1e-2 )
!#CAB jm and jd are not defined
!#CAB      evap = evap * GetPotlEvap(jm,jd) * theSoil%Porosity
           evapsat = (Ssattm1 - theSoil%Porosity*theSoil%Debs) &
                   / (theSoil%Sus + (Ssattm1-theSoil%Porosity*theSoil%Debs) )
           evapsat = MIN(evapsat,1.0) * evap
       END IF

       ! intermediate stage estimate of top region of unsat zone
       Susp =  Susp - theSoil%recharge - (evap - evapsat) !* top_depth/theSoil%PhreaticDepth

       ! Suction
       IF( Susp <0.0 ) THEN
          theSoil%qsuc = -1.05*Susp
       ELSE
          theSoil%qsuc = 0.0
       END IF

       ! Runoff
       IF( (theSoil%PhreaticHgt) < avgLevel) THEN
         IF(avgLevel < theSoil%Bathy) THEN
           theSoil%qss = 0.
         ELSE
           theSoil%qss = ( avgLevel - theSoil%PhreaticHgt ) / ( avgLevel - theSoil%Bathy )
           theSoil%qss = theSoil%ass * theSoil%qss**theSoil%Bss
         END IF
       ELSE
        theSoil%qss = 0.0
       END IF
       flow        =  theSoil%qss

       !print *,'ee: ',evap, evapsat, theSoil%recharge, theSoil%qss
       ! Update bucket water balance for timestep
       theSoil%St = Sttm1 + rain - flow - evap

       IF(theSoil%St<=0.0) THEN
       ! Empty
         theSoil%St = 0.0
         theSoil%Ssat = 0.0
         theSoil%recharge = 0.0
         evapsat = 0.0
         flow = 0.0
         theSoil%qse = 0.0
         ! Update Unsaturated Zone Moisture Fraction (wt% water)
         theSoil%Moisture(1:theSoil%nlay) = 0.0
         theSoil%PhreaticDepth   = (theSoil%Sb - theSoil%Ssat) / theSoil%Porosity
         theSoil%PhreaticHgt     =  theSoil%Bathy +  theSoil%PhreaticDepth
         theSoil%Dcap = theSoil%PhreaticHgt
         theSoil%Dtrn = theSoil%PhreaticHgt
         theSoil%S_cap  = 0.0
         theSoil%S_trn  = 0.0
         theSoil%S_top  = 0.0
       ELSE IF(theSoil%St >= theSoil%Sb) THEN
       ! Full
         theSoil%qse = theSoil%St - theSoil%Sb
         theSoil%St  = theSoil%Sb
         theSoil%Sus = 0.0
         theSoil%Ssat = theSoil%St
         ! Update Unsaturated Zone Moisture Fraction (wt% water)
         theSoil%Moisture(1:theSoil%nlay) = 1.0 * theSoil%Porosity * 1000. &
                                          /(theSoil%Porosity * 1000. + (1.0-theSoil%Porosity)*theSoil%Density)
         theSoil%recharge        = theSoil%Ssat - Ssattm1
         theSoil%PhreaticDepth   = (theSoil%Sb - theSoil%Ssat) / theSoil%Porosity
         theSoil%PhreaticHgt     =  theSoil%Bathy +  theSoil%PhreaticDepth
         theSoil%Dcap = theSoil%Bathy
         theSoil%Dtrn = theSoil%Bathy
         theSoil%S_cap   = 0.0
         theSoil%S_trn   = 0.0
         theSoil%S_top   = 0.0
       ELSE
         ! Normal
         !theSoil%qse = 0.0

         theSoil%Ssat = Ssattm1 + theSoil%recharge - evapsat - flow - theSoil%qsuc
         theSoil%Sus  = theSoil%St - theSoil%Ssat !+ theSoil%qsuc

         theSoil%PhreaticDepth = (theSoil%Sb - theSoil%Ssat) / theSoil%Porosity
         theSoil%PhreaticHgt   =  theSoil%Bathy +  theSoil%PhreaticDepth

         !PRINT *,'ss: ',theSoil%St,theSoil%Ssat,Ssattm1,theSoil%Sus!,theSoil%S_top

         ! Update unsat zone levels
         IF(theSoil%PhreaticHgt-theSoil%Zcap < theSoil%Bathy) THEN
           theSoil%Dcap = theSoil%Bathy
         ELSE
           theSoil%Dcap = theSoil%PhreaticHgt - theSoil%Zcap
         ENDIF

         IF(theSoil%Dcap - theSoil%Ztrn < theSoil%Bathy) THEN
           theSoil%Dtrn = theSoil%Bathy
         ELSE
           theSoil%Dtrn = theSoil%Dcap - theSoil%Ztrn
         ENDIF

         !PRINT *,'dcap1: ',theSoil%Bathy,theSoil%PhreaticHgt,theSoil%Dcap,theSoil%Dtrn, theSoil%Sus

         ! Calculate unsat zone storage values
         botmoist = 0.9
         IF( ABS(theSoil%Dtrn - theSoil%Bathy) <1e-3) THEN
           topmoist = 0.9
         ELSE
           topmoist = MAX(Susp,0.001) /  ((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity)
         END IF

         theSoil%S_cap = 0.9 * (theSoil%PhreaticHgt - theSoil%Dcap) * theSoil%Porosity
         theSoil%S_trn = ((botmoist-topmoist)/2.) * (theSoil%Dcap - theSoil%Dtrn) * theSoil%Porosity
         theSoil%S_top = theSoil%Sus - theSoil%S_trn - theSoil%S_cap

         IF(theSoil%S_top > ((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity) ) THEN
           theSoil%S_top = ((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity)
         END IF
         IF(theSoil%S_top < 0.0 ) THEN
           theSoil%S_top = 0.001*((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity)
         END IF
         topmoist = (theSoil%S_top) /  MAX( (theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity,0.001)

         theSoil%S_trn = ((botmoist-topmoist)/2.) * (theSoil%Dcap - theSoil%Dtrn) * theSoil%Porosity
         theSoil%S_top = theSoil%Sus - theSoil%S_trn - theSoil%S_cap

         IF(theSoil%S_top > ((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity) ) THEN
           theSoil%S_top = ((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity)
         END IF
         IF(theSoil%S_top < 0.0 ) THEN
           theSoil%S_top = 0.001*((theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity)
         END IF
         topmoist = (theSoil%S_top) /  MAX( (theSoil%Dtrn - theSoil%Bathy) * theSoil%Porosity,0.001)

         !! Check for shortfall and suck up some if necessary
         !IF( (theSoil%Sus - theSoil%S_trn - theSoil%S_cap) < 0.0) THEN
         !  theSoil%qsuc = -1.0 * (theSoil%Sus - theSoil%S_trn - theSoil%S_cap)
         !ELSE
         !   theSoil%qsuc = 0.0  ! Use Darrens EQ here
         !END IF
         !theSoil%Ssat = theSoil%Ssat - theSoil%qsuc
         !theSoil%PhreaticDepth = (theSoil%Sb - theSoil%Ssat) / theSoil%Porosity
         !theSoil%PhreaticHgt   =  theSoil%Bathy +  theSoil%PhreaticDepth
         !theSoil%Sus  = theSoil%Sus  + theSoil%qsuc
         !theSoil%S_top = MAX(theSoil%Sus - theSoil%S_trn - theSoil%S_cap,0.0)

!   print *,'dcap2: ',theSoil%Bathy,theSoil%PhreaticHgt,theSoil%Dcap,theSoil%Dtrn
!   print *, theSoil%Sus, theSoil%S_top,theSoil%S_trn,theSoil%S_cap,theSoil%qsuc

         ! Update Unsaturated Zone Moisture Fraction (wt% water)
         DO lay = 1,theSoil%nlay
           dep   = theSoil%Bathy  + (  lay *    ( theSoil%Depth / theSoil%nlay ) )
           depm1 = theSoil%Bathy  + ( (lay-1) * ( theSoil%Depth / theSoil%nlay ) )

           middep = (dep+depm1)/2.

           theSoil%Moisture(lay) = 0.0

           IF( middep  >= theSoil%PhreaticHgt ) THEN
             ! Saturated Zone: % VWC/theta
             theSoil%Moisture(lay) = 1.000
           ELSE IF(middep < theSoil%PhreaticHgt .AND. middep >= theSoil%Dcap) THEN
             ! Near saturated region above the water table: % VWC/theta
             theSoil%Moisture(lay) = botmoist
           ELSE IF(middep < theSoil%Dcap .AND. middep >= theSoil%Dtrn) THEN
             ! Transition region: % VWC/theta
             !print *,'middep',topmoist,theSoil%Ztrn, (botmoist-topmoist) , ((middep)-theSoil%Bathy),(theSoil%Dcap - middep)
             theSoil%Moisture(lay) = botmoist - (botmoist-topmoist) *  (theSoil%Dcap - middep) / theSoil%Ztrn  ! linear gradient
           ELSE
             ! Top of unsaturated zone: % VWC/theta
             theSoil%Moisture(lay) = topmoist !&
               ! MIN(theSoil%S_top / MAX(theSoil%Sb - theSoil%Ssat - theSoil%S_trn - theSoil%S_cap,1e-2),1.0)
           END IF

           !print *,'moist',lay,dep,middep,theSoil%Moisture(lay)
           ! convert to % wtWC
           theSoil%Moisture(lay) = theSoil%Moisture(lay) * theSoil%Porosity * 1000. &
                                  /(theSoil%Moisture(lay) * theSoil%Porosity * 1000. &
                                  + (1.0-theSoil%Porosity)*theSoil%Density)
         END DO
       END IF

       theSoil%rn = rain
       theSoil%et = evap
       theSoil%es = evapsat

       ! print *,'rain',rain,theSoil%St, theSoil%Ssat, theSoil%Moisture(2),theSoil%Sus,theSoil%S_cap, theSoil%S_trn, theSoil%S_top
       ! print *,'hgt;',theSoil%PhreaticHgt,theSoil%Bathy, theSoil%PhreaticDepth, avgLevel
   ENDIF
END SUBROUTINE UpdateSoilHydrology
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE SetBucketParameters(data, theSoil, MatZoneID)
!-------------------------------------------------------------------------------
! Opens an input file. If the file is opened successfully the routine          !
! returns normally, if not then the routine terminates.                        !
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed2_ass_data_t),INTENT(in) :: data
   TYPE(SoilHydroUnit) :: theSoil
   INTEGER :: MatZoneID
!LOCALS

!-------------------------------------------------------------------------------
!BEGIN
        theSoil%UnitNum = 0
        theSoil%Substrate = MatZoneID

        ! Hydrologic dynamics (initialise here, they'll be updated with state vars later)
        theSoil%St = ZERO_
        theSoil%Ssat = ZERO_
        theSoil%PhreaticHgt = ZERO_
        theSoil%PhreaticDepth = ZERO_
        theSoil%qss = ZERO_
        theSoil%qse = ZERO_
        theSoil%recharge = ZERO_
        theSoil%qsuc = ZERO_
        theSoil%Sus = ZERO_

        ! Physical properties
        theSoil%Depth = ZERO_
        theSoil%Area = ZERO_
        theSoil%Bathy = ZERO_
        theSoil%Sb = ZERO_

        ! Hydrologic parameters
        theSoil%aep = data%aep(theSoil%Substrate)
        theSoil%ass = data%ass(theSoil%Substrate)
        theSoil%Bss = data%Bss(theSoil%Substrate)
        theSoil%Debs = data%Debs(theSoil%Substrate)
        theSoil%Dper = data%Dper(theSoil%Substrate)
        theSoil%Porosity = data%Porosity(theSoil%Substrate)
        theSoil%Density = data%Density(theSoil%Substrate)

        ! Moisture disaggregation model
        theSoil%nlay = data%nlay
        ALLOCATE(theSoil%Moisture(theSoil%nlay))

        theSoil%Dcap = data%Dcap
        theSoil%Zcap = data%Zcap
        theSoil%Dtrn = data%Dtrn
        theSoil%Ztrn = data%Ztrn
        theSoil%fc = data%Ztrn

        ! Are these guys dynamic or what?
!# CAB removed for now
!       theSoil%S_top
!       theSoil%S_trn =??
!       theSoil%S_cap
!       theSoil%pastMaxLevel
!       theSoil%rn
!       theSoil%et
!       theSoil%es

END SUBROUTINE SetBucketParameters
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
FUNCTION GetPotlEvap(jm,jd) RESULT (evaporation)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER      :: jm
   REAL         :: jd
!
!LOCALS
   AED_REAL     :: evaporation
!
!-------------------------------------------------------------------------------
!BEGIN
   evaporation = 1. + epamp*sin(angfreq*((30.44*REAL(jm-1)+jd)+epphase))

   evaporation = epdayav * evaporation
END FUNCTION GetPotlEvap
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed2_ass
