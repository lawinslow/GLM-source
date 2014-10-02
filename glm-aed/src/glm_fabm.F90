!###############################################################################
!#                                                                             #
!# glm_fabm.F90                                                                #
!#                                                                             #
!# The interface between glm and fabm                                          #
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

#include "fabm_driver.h"

#undef REALTYPE
#ifndef _FORTRAN_VERSION_
#define _FORTRAN_VERSION_ 1
#endif

#include "glm.h"

!# In 2014 fabm stopped beclaring these macros
#ifndef _ATTR_DIMENSIONS_1_
#define _ATTR_DIMENSIONS_1_ ,dimension(:)
#endif
#ifndef _ATTR_LOCATION_DIMENSIONS_
#define _ATTR_LOCATION_DIMENSIONS_ ,dimension(_LOCATION_DIMENSIONS_)
#endif
#ifndef _ATTR_LOCATION_DIMENSIONS_HZ_
#define _ATTR_LOCATION_DIMENSIONS_HZ_
#endif

!# As of june 2014 these were no longer defined
#define varname_wind_sf  standard_variables%wind_speed
#define varname_extc     standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux
#define varname_dens     standard_variables%density
#define varname_layer_ht standard_variables%cell_thickness
#define varname_taub     standard_variables%bottom_stress
#define varname_temp     standard_variables%temperature
#define varname_salt     standard_variables%practical_salinity
#define varname_tss      standard_variables%mass_concentration_of_suspended_matter
#define varname_par      standard_variables%downwelling_photosynthetic_radiative_flux
#define varname_par_sf   standard_variables%surface_downwelling_photosynthetic_radiative_flux
#define varname_pres     standard_variables%pressure

#ifdef __GFORTRAN__
#  define _LINK_POINTER_(dst, src)  CALL link_pointer(dst, src)
#  warning   "gfortran version does not work yet"
#else
#  define _LINK_POINTER_(dst, src)  dst => src
#endif


!-------------------------------------------------------------------------------
MODULE glm_fabm
!
   USE ISO_C_BINDING

   USE fabm
   USE fabm_types
   USE ode_solvers

   USE glm_types

   IMPLICIT NONE

   PRIVATE ! By default, make everything private
!
#include "glm_globals.h"
#include "glm_plot.h"
#include "glm_ncdf.h"
#include "glm_csv.h"
#include "glm_mobl.h"
!
   PUBLIC init_glm_fabm, init_glm_fabm_output
   PUBLIC set_glm_fabm_data
   PUBLIC clean_glm_fabm, write_glm_fabm, do_glm_fabm
   PUBLIC WQVar_Index
!
!-------------------------------------------------------------------------------
!
!MODULE DATA

   REALTYPE :: lKw    !# background light attenuation (m**-1)
   LOGICAL  :: lIce = .FALSE.

   !# Namelist variables
   CINTEGER,PUBLIC,BIND(C) :: ode_method = 1, split_factor = 1
   CLOGICAL,PUBLIC,BIND(C) :: bioshade_feedback = .TRUE., repair_state = .TRUE., multi_ben = .FALSE.
   CLOGICAL,PUBLIC,BIND(C) :: mobility_off = .FALSE.  !# flag to turn mobility off

   !# Model
   TYPE (type_model),POINTER :: model

   !# Arrays for state and diagnostic variables
   REALTYPE,ALLOCATABLE,DIMENSION(:,:) :: cc !# water quality array, nlayers, nvars
   REALTYPE,ALLOCATABLE,DIMENSION(:,:) :: cc_diag
   REALTYPE,ALLOCATABLE,DIMENSION(:,:) :: rhs_flux
   REALTYPE,ALLOCATABLE,DIMENSION(:)   :: cc_diag_hz
   REALTYPE,ALLOCATABLE,DIMENSION(:)   :: tss
   REALTYPE,ALLOCATABLE,DIMENSION(:)   :: sed_zones

   !# Arrays for work, vertical movement, and cross-boundary fluxes
   REALTYPE,ALLOCATABLE,DIMENSION(:,:) :: ws
   REALTYPE,ALLOCATABLE,DIMENSION(:)   :: sfl
   REALTYPE,ALLOCATABLE,DIMENSION(:)   :: total
   REALTYPE,ALLOCATABLE _ATTR_DIMENSIONS_1_ :: local

   !# Arrays for environmental variables not supplied externally.
   REALTYPE,ALLOCATABLE,DIMENSION(:) :: par,pres

   !# External variables
   REALTYPE :: dt, dt_eff   ! External and internal time steps
   INTEGER  :: w_adv_ctr    ! Scheme for vertical advection (0 IF not used)
   REALTYPE,POINTER,DIMENSION(:) :: rad, z, salt, temp, rho, area
   REALTYPE,POINTER,DIMENSION(:) :: extc_coef
   REALTYPE,POINTER              :: precip, evap, bottom_stress
   REALTYPE,TARGET               :: bs_dummy

   CHARACTER(len=30),ALLOCATABLE :: names(:)

   REALTYPE,ALLOCATABLE :: dz(:)         !# layer thickness

!===============================================================================
CONTAINS



!###############################################################################
FUNCTION MYTRIM(str) RESULT(res)
!-------------------------------------------------------------------------------
! Useful for passing string arguments to C functions
!-------------------------------------------------------------------------------
   CHARACTER(*),TARGET :: str
   CHARACTER(:),POINTER :: res
   INTEGER :: len

   len = LEN_TRIM(str)+1
   str(len:len) = achar(0)
   res => str
END FUNCTION MYTRIM
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
INTEGER FUNCTION f_get_lun()
!-------------------------------------------------------------------------------
! Find the first free logical unit number
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER :: lun
   LOGICAL :: opened
!
!-------------------------------------------------------------------------------
!BEGIN
   DO lun = 10,99
      inquire(unit=lun, opened=opened)
      IF ( .not. opened ) THEN
         f_get_lun = lun
         RETURN
      ENDIF
   ENDDO
   f_get_lun = -1
END FUNCTION f_get_lun
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE init_glm_fabm(i_fname,len,MaxLayers,NumFABMVars,pKw) BIND(C, name="init_glm_wq")
!-------------------------------------------------------------------------------
! Initialize the GLM-FABM driver by reading settings from fabm.nml.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CCHARACTER,INTENT(in) :: i_fname(*)
   CINTEGER,INTENT(in)   :: len
   CINTEGER,INTENT(in)   :: MaxLayers
   CINTEGER,INTENT(out)  :: NumFABMVars
   REALTYPE,INTENT(in)   :: pKw
!
!LOCALS
   INTEGER :: i,rc,namlst
   CHARACTER(len=80) :: fname
!
!-------------------------------------------------------------------------------
!BEGIN

   CALL make_string(fname, i_fname, len)

   lKw = pKw

   print *,'init_glm_fabm from ', TRIM(fname)
   namlst = f_get_lun()

   !# Create model tree
   model => fabm_create_model_from_file(namlst,fname)

   !# Initialize model tree (creates metadata and assigns variable identifiers)
   CALL fabm_set_domain(model,MaxLayers)

   !# Report prognostic variable descriptions
   print *, 'FABM pelagic state variables:'
   DO i=1,ubound(model%info%state_variables,1)
         print *, trim(model%info%state_variables(i)%name), '  ',              &
                trim(model%info%state_variables(i)%units),'  ',                &
                trim(model%info%state_variables(i)%long_name)
   ENDDO

   print *, 'FABM benthic state variables:'
   DO i=1,ubound(model%info%state_variables_ben,1)
      print *, trim(model%info%state_variables_ben(i)%name), '  ',             &
             trim(model%info%state_variables_ben(i)%units),'  ',               &
             trim(model%info%state_variables_ben(i)%long_name)
   ENDDO

   !# Report diagnostic variable descriptions
   print *, 'FABM diagnostic variables defined on the full model domain:'
   DO i=1,ubound(model%info%diagnostic_variables,1)
      print *, trim(model%info%diagnostic_variables(i)%name), '  ',            &
             trim(model%info%diagnostic_variables(i)%units),'  ',              &
             trim(model%info%diagnostic_variables(i)%long_name)
   ENDDO

   print *, 'FABM diagnostic variables defined on a horizontal slice of the model domain:'
   DO i=1,ubound(model%info%diagnostic_variables_hz,1)
      print *, trim(model%info%diagnostic_variables_hz(i)%name), '  ',         &
             trim(model%info%diagnostic_variables_hz(i)%units),'  ',           &
             trim(model%info%diagnostic_variables_hz(i)%long_name)
   ENDDO

#if 0
   init_solver(ode_method,ode_solver)
#else
   !# Report type of solver
   print *, "Using Eulerian solver"
   SELECT CASE (ode_method)
      CASE (1)
         print *, 'Using euler_forward()'
      CASE (2)
         print *, 'Using runge_kutta_2()'
      CASE (3)
         print *, 'Using runge_kutta_4()'
      CASE (4)
         print *, 'Using patankar()'
      CASE (5)
         print *, 'Using patankar_runge_kutta_2()'
      CASE (6)
         print *, 'Using patankar_runge_kutta_4()'
      CASE (7)
         print *, 'Using modified_patankar()'
      CASE (8)
         print *, 'Using modified_patankar_2()'
      CASE (9)
         print *, 'Using modified_patankar_4()'
      CASE (10)
         print *, 'Using emp_1()'
      CASE (11)
         print *, 'Using emp_2()'
      CASE (1003)
         print *, 'Using runge_kutta_4() with pp/dd matrices'
      CASE DEFAULT
         STOP 'init_glm_fabm: no valid ode_method specified in fabm.nml!'
   END SELECT
#endif

   NumFABMVars = ubound(model%info%state_variables,1) + ubound(model%info%state_variables_ben,1)

   ALLOCATE(dz(MaxLayers))
   dz = 0.  !# initialise to zero

   !# Now that we know how many vars we need, we can allocate space for them
   ALLOCATE(cc(NumFABMVars,MaxLayers))
   cc = 0.         !# initialise to zero

   CALL set_c_wqvars_ptr(cc)

!  print *,"Variable names :"
   ALLOCATE(names(1:ubound(model%info%state_variables,1)+ubound(model%info%state_variables_ben,1)),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (names)'
   DO i=1,ubound(model%info%state_variables,1)
      names(i) = trim(model%info%state_variables(i)%name)
!     print *,trim(model%info%state_variables(i)%name)
   ENDDO
   DO i=1,ubound(model%info%state_variables_ben,1)
      names(ubound(model%info%state_variables,1)+i) = trim(model%info%state_variables_ben(i)%name)
   ENDDO

!  print *,"Diag-Variable names :"
!  IF ( .not. allocated(diagnames) ) ALLOCATE(diagnames(ubound(model%info%diagnostic_variables,1)))
!  DO i=1,ubound(model%info%diagnostic_variables,1)
!     diagnames(i) = trim(model%info%diagnostic_variables(i)%name)
!     print *,trim(model%info%diagnostic_variables(i)%name)
!  ENDDO
!
!  -----------------------------------------------------------------------------
!
   !# In terms of memory use, it is a waste to allocate storage for benthic variables across the entire
   !# column (the bottom layer should suffice). However, it is important that all values at a given point
   !# in time are integrated simultaneously in multi-step algorithms. This currently can only be arranged
   !# By storing benthic values together with the pelagic, in a fully depth-explicit array.
   DO i=1,ubound(model%info%state_variables,1)
      cc(i,:) = model%info%state_variables(i)%initial_value
      CALL fabm_link_bulk_state_data(model,i,cc(i,1:))
   ENDDO
   DO i=1,ubound(model%info%state_variables_ben,1)
      cc(ubound(model%info%state_variables,1)+i,1) = model%info%state_variables_ben(i)%initial_value
      CALL fabm_link_bottom_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
   ENDDO

   !# Allocate diagnostic variable array and set all values to zero.
   !# (needed because time-integrated/averaged variables will increment rather than set the array)
   ALLOCATE(cc_diag(1:ubound(model%info%diagnostic_variables,1),1:MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_diag)'
   cc_diag = _ZERO_

   !# Allocate diagnostic variable array and set all values to zero.
   !# (needed because time-integrated/averaged variables will increment rather than set the array)
   ALLOCATE(cc_diag_hz(1:ubound(model%info%diagnostic_variables_hz,1)),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_diag_hz)'
   cc_diag_hz = _ZERO_

   !# Allocate array with vertical movement rates (m/s, positive for upwards),
   !# and set these to the values provided by the model.
   ALLOCATE(ws(1:ubound(model%info%state_variables,1),1:MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (ws)'
   ws = _ZERO_
   DO i=1,ubound(model%info%state_variables,1)
      ws(i,:) = model%info%state_variables(i)%vertical_movement
   ENDDO

   !# Allocate array for surface fluxes and initialize these to zero (no flux).
   ALLOCATE(sfl(1:ubound(model%info%state_variables,1)),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (sfl)'
   sfl = _ZERO_

   !# Allocate array for mass fluxes and initialize these to zero (no flux).
   ALLOCATE(rhs_flux(1:ubound(model%info%state_variables,1),1:MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (rhs_flux)'
   rhs_flux = _ZERO_

   !# Allocate array for photosynthetically active radiation (PAR).
   !# This will be calculated internally during each time step.
   ALLOCATE(par(1:MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (par)'
   par = _ZERO_
   CALL fabm_link_bulk_data(model,varname_par,par(1:MaxLayers))

   !# Allocate array for local pressure.
   !# This will be calculated [approximated] from layer depths internally during each time step.
   ALLOCATE(pres(1:MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (pres)'
   pres = _ZERO_
   CALL fabm_link_bulk_data(model,varname_pres,pres(1:MaxLayers))

   !# Allocate arrays for storing local and column-integrated values of diagnostic variables.
   !# These are used during each save.
   ALLOCATE(total(1:ubound(model%info%conserved_quantities,1)),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (total)'
   total = _ZERO_
   ALLOCATE(local(1:ubound(model%info%conserved_quantities,1)),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (local)'

   ALLOCATE(tss(MaxLayers),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): Error allocating (tss)'
   tss = _ZERO_

   CALL fabm_link_bulk_data(model,varname_tss,tss)
END SUBROUTINE init_glm_fabm
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#ifdef __GFORTRAN__
!###############################################################################
!# This is a fudge for gFortran. I'm guessing intel fortran creates the
!# required structures in global space for the associations, but gfortran
!# only creates them on the local stack so they are lost once the subroutine
!# that was called from C exits.
!# Linking fails with "undefined reference to `span.0'"
!# It only applies to Lake which is the only array.
!# This routine is accessed by the macro _LINK_POINTER_ which for non-gfortran
!# compilers will just be dst => src but for gfortran becauses a routine
!# which is passed the temporary structure which it copies to global space.
!#
!# As it happens, this only gets us past the build problems - the pointers are
!# clearly not right yet - it doesnt run - and it may be a while before it can
!###############################################################################
SUBROUTINE link_pointer(dst, src)
!ARGUMENTS
   REALTYPE,POINTER _ATTR_LOCATION_DIMENSIONS_,INTENT(out) :: dst
   REALTYPE _ATTR_LOCATION_DIMENSIONS_,TARGET,INTENT(in)   :: src
!-------------------------------------------------------------------------------
!BEGIN
   dst => src
END SUBROUTINE link_pointer
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#endif


!###############################################################################
SUBROUTINE set_glm_fabm_data(Lake, MaxLayers, NumLayers, MetData, SurfData, dt_) &
                                                 BIND(C, name="set_glm_wq_data")
!ARGUMENTS
   CINTEGER, INTENT(in) :: MaxLayers, NumLayers
   TYPE(LakeDataType),TARGET    :: Lake(1:MaxLayers)
   TYPE(MetDataType),TARGET     :: MetData  !# Meteorological data
   TYPE(SurfaceDataType),TARGET :: SurfData !# Surface Data
   REALTYPE,INTENT(in)  :: dt_
!
!LOCALS
   INTEGER i
!-------------------------------------------------------------------------------
!BEGIN
   !# Save pointers to external dynamic variables that we need later (in do_glm_fabm)
   _LINK_POINTER_(z, Lake%Height)
   _LINK_POINTER_(temp, Lake%Temp)
   _LINK_POINTER_(salt, Lake%Salinity)
   _LINK_POINTER_(rho, Lake%Density)
   _LINK_POINTER_(area, Lake%LayerArea)
   _LINK_POINTER_(rad, Lake%Light)
   _LINK_POINTER_(extc_coef, Lake%ExtcCoefSW)

   precip => MetData%Rain
   evap   => SurfData%Evap
   bs_dummy = 0.
   bottom_stress => bs_dummy

   !# Copy scalars that will not change during simulation, and are needed in do_glm_fabm)
   dt = dt_

   !# Compute the layer heights
   dz(1) = z(1)
   DO i=2,NumLayers
      dz(i) = z(i) - z(i-1)
   ENDDO

   !# Provide pointers to arrays with environmental variables to FABM.
   CALL fabm_link_bulk_data(model,varname_temp,     temp)
   CALL fabm_link_bulk_data(model,varname_salt,     salt)
   CALL fabm_link_bulk_data(model,varname_dens,     rho)
   CALL fabm_link_bulk_data(model,varname_layer_ht, dz)
   CALL fabm_link_bulk_data(model,varname_extc,     extc_coef)
   IF ( ASSOCIATED(bottom_stress) ) &
      CALL fabm_link_horizontal_data(model,varname_taub,    bottom_stress)
   CALL fabm_link_horizontal_data(model,varname_wind_sf, MetData%WindSpeed)
   CALL fabm_link_horizontal_data(model,varname_par_sf,  MetData%ShortWave)

   ! Calculate and save internal time step.
   dt_eff = dt/FLOAT(split_factor)

   ! Trigger an error if FABM hasn't got all it needs from us.
   CALL fabm_check_ready(model)
END SUBROUTINE set_glm_fabm_data
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE do_glm_fabm(wlev, pIce) BIND(C, name="do_glm_wq")
!-------------------------------------------------------------------------------
!                           wlev is the number of levels used;
!                           nlev is the total num levels in the array
!-------------------------------------------------------------------------------
!ARGUMENTS
   CINTEGER,INTENT(in) :: wlev
   CLOGICAL,INTENT(in) :: pIce
!
!LOCALS
   REALTYPE :: min_C
   INTEGER  :: i, split
   REALTYPE, POINTER :: fdhz
   REALTYPE, DIMENSION(:),POINTER :: fdiag

!
!-------------------------------------------------------------------------------
!BEGIN
   lIce = pIce

   !# re-compute the layer heights
   dz(1) = z(1)
   DO i=2,wlev
      dz(i) = z(i) - z(i-1)
   ENDDO

   !# Calculate local pressure
   pres(1:wlev) = -z(1:wlev)

   DO i=1,ubound(model%info%state_variables,1)
      CALL fabm_link_bulk_state_data(model,i,cc(i,1:wlev))
   ENDDO
   DO i=1,ubound(model%info%state_variables_ben,1)
      CALL fabm_link_bottom_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
   ENDDO

   !# Get updated vertical movement (m/s, positive for upwards) for biological state variables.
   DO i=1,wlev
      CALL fabm_get_vertical_movement(model,i,ws(:,i))
   ENDDO

   !# (3) Calculate source/sink terms due to settling rising of state variables in the water
   !# column (note that settling into benthos is done in fabm_do_benthos)

   IF ( .NOT. mobility_off ) THEN
      DO i=1,ubound(model%info%state_variables,1)
         IF (model%info%state_variables(i)%vertical_movement .NE. _ZERO_) THEN
            min_C = model%info%state_variables(i)%minimum
            CALL Mobility(wlev, dt, dz, area, ws(i,:), min_C, cc(i,:))
         ENDIF
      ENDDO
   ENDIF

!#CAB I have removed the fabm_get_surface_exchange call here for a number of reasons :
!#CAB
!#CAB  1) I dont think it actually does anything - it gets fluxes into sfl, but never integrates
!#CAB     them back into cc so they are lost
!#CAB  2) there is no ice check
!#CAB  3) the call is done (correctly) in right_hand_side_rhs
!#CAB
!#CAB I also tried removing the do_repair_state after these calls thinking that
!#CAB it also would be doing nothing, but it seems it does [ things are quite different
!#CAB without the repairstate ] which means that the "Mobility" call above might be causing
!#CAB a problem (either too high or too low, given that it has a min check built in, I
!#CAB would guess it's too high)
!#CAB
#if 0
   !# Get updated air-sea fluxes for biological state variables.
   sfl = _ZERO_
   CALL fabm_get_surface_exchange(model,wlev,sfl)

   !# Calculate dilution due to surface freshwater flux (m/s)
   !# If surface freshwater flux is not specified, but surface salinity is relaxed to observations,
   !# calculate the effective dilution from the relation term, and use that instead.
   dilution = precip+evap

   DO i=1,ubound(model%info%state_variables,1)
      !# Add surface flux due to evaporation/precipitation, unless the model explicitly says otherwise.
      IF (.NOT. model%info%state_variables(i)%no_precipitation_dilution) &
         sfl(i) = sfl(i)-cc(i,wlev)*dilution

      !# Determine whether the variable is positive-definite based on its lower allowed bound.
      posconc = 0
      IF (model%info%state_variables(i)%minimum .GE. _ZERO_) posconc = 1
   ENDDO
#endif

   !# Repair state before calling FABM
   CALL do_repair_state(wlev,'glm_fabm::do_glm_fabm, after advection/diffusion')

   DO split=1,split_factor
      !# Update local light field (self-shading may have changed through changes in biological state variables)
      !# changed to update_light to be inline with current aed_phyoplankton that requires only surface par then integrates over
      CALL update_light(wlev,bioshade_feedback)

      !# Time-integrate one biological time step
      CALL ode_solver(ode_method,ubound(cc,1),wlev,dt_eff,cc(:,1:wlev),right_hand_side_rhs,right_hand_side_ppdd)

      !# Provide FABM with (pointers to) updated state variables.
      DO i=1,ubound(model%info%state_variables,1)
         CALL fabm_link_bulk_state_data(model,i,cc(i,1:wlev))
      ENDDO
      DO i=1,ubound(model%info%state_variables_ben,1)
         CALL fabm_link_bottom_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
      ENDDO

      !# Repair state
      CALL do_repair_state(wlev,'glm_fabm::do_glm_fabm, after time integration')

      !# Time-integrate diagnostic variables defined on horizontonal slices, where needed.
      DO i=1,ubound(model%info%diagnostic_variables_hz,1)
         fdhz => fabm_get_horizontal_diagnostic_data(model,i)
         cc_diag_hz(i) = fdhz     !# Simply use last value
      ENDDO

      !# Time-integrate diagnostic variables defined on the full domain, where needed.
      DO i=1,ubound(model%info%diagnostic_variables,1)
         fdiag => fabm_get_bulk_diagnostic_data(model,i)
         cc_diag(i,1:wlev) = fdiag(1:wlev)    !# Simply use last value
      ENDDO
   ENDDO
END SUBROUTINE do_glm_fabm
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE do_repair_state(wlev,location)
!-------------------------------------------------------------------------------
! Check the current values of all state variables
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)          :: wlev
   CHARACTER(len=*),INTENT(in) :: location
!
!LOCALS
   LOGICAL :: valid = .true., l_repair_state
   INTEGER :: ci
!
!-------------------------------------------------------------------------------
!BEGIN

   l_repair_state = repair_state
   DO ci=1,wlev
      CALL fabm_check_state(model, ci, l_repair_state, valid)
      IF (.NOT. (valid .OR. repair_state)) EXIT
   ENDDO

   IF (.NOT. (valid .OR. repair_state)) THEN
      write(stderr,*) 'FATAL ERROR:  State variable values are invalid and repair is not allowed.'
      write(stderr,*) 'FATAL ERROR: ', location
      STOP 'glm_fabm::do_repair_state'
   ENDIF
END SUBROUTINE do_repair_state
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE right_hand_side_rhs(first,numc,nlev,cc,rhs)
!-------------------------------------------------------------------------------
! Calculate the temporal derivatives as a derivative vector
!-------------------------------------------------------------------------------
!ARGUMENTS
   LOGICAL, INTENT(in)     :: first
   INTEGER, INTENT(in)     :: numc,nlev
   REALTYPE, INTENT(in)    :: cc(:,:)
   REALTYPE, INTENT(out)   :: rhs(:,:)
!
!LOCALS
   INTEGER :: i,nvars,k
!
!-------------------------------------------------------------------------------
!BEGIN
   !# Shortcut to the number of pelagic state variables.
   nvars = ubound(model%info%state_variables,1)

   !# Provide FABM with (pointers to) the current state.
   DO i=1,ubound(model%info%state_variables,1)
      CALL fabm_link_bulk_state_data(model,i,cc(i,1:nlev))
   ENDDO
   DO i=1,ubound(model%info%state_variables_ben,1)
      CALL fabm_link_bottom_state_data(model,i,cc(nvars+i,1))
   ENDDO

   !# If this is not the first step in the (multi-step) integration scheme,
   !# THEN first make sure that the intermediate state variable values are valid.
   IF (.not. first) CALL do_repair_state(nlev,'glm_fabm::right_hand_side_rhs')

   !# Initialization is needed because the different biogeochemical models increment or decrement
   !# the temporal derivatives, rather than setting them directly. This is needed for the simultaenous
   !# running of different coupled BGC models.
   rhs = _ZERO_
   rhs_flux = _ZERO_

   !# Start with calculating all flux terms for rhs in mass/m3/s
   !# Includes (1) surface exchange, (2) benthic flux and (3) settling/rising as calculated by glm

   !# (1) surface exchange
   !# Calculate temporal derivatives due to air water exchange.
   IF (.NOT. lIce) THEN !# no surface exchange under ice cover
      CALL fabm_get_surface_exchange(model,nlev,rhs_flux(1:nvars,nlev))
      !# Distribute surface flux into pelagic surface layer volume (i.e., divide by layer height).
      rhs(1:nvars,nlev) = rhs(1:nvars,nlev) + rhs_flux(1:nvars,nlev)/dz(nlev)
      rhs_flux = _ZERO_
   ENDIF

   !# (2) benthic flux
   !# Calculate temporal derivatives due to exchanges at the sediment/water interface

   CALL fabm_do_benthos(model,1,rhs_flux(1:nvars,1),rhs(nvars+1:,1))
   !# Limit flux out of bottom layers to concentration of that layer
   !# i.e. don't flux out more than is there
   rhs_flux(1:nvars,1) = max(-1.0 * cc(1:nvars,1)*dz(1), rhs_flux(1:nvars,1))
   rhs(1:nvars,1) = rhs(1:nvars,1) + rhs_flux(1:nvars,1)/dz(1)

   IF ( multi_ben ) THEN
      DO k=2,nlev
         !# Calculate temporal derivatives due to benthic processes.
         CALL fabm_do_benthos(model,1,rhs_flux(1:nvars,k),rhs(nvars+1:,k))

         !# Limit flux out of bottom layers to concentration of that layer
         !# i.e. don't flux out more than is there
         rhs_flux(1:nvars,k) = max(-1.0 * cc(1:nvars,k)*dz(k)/dt_eff, rhs_flux(1:nvars,k))

         !# Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
         rhs(1:nvars,k) = rhs(1:nvars,k) + rhs_flux(1:nvars,k)/dz(k) * (area(k)-area(k-1))/area(k)
      ENDDO
   ENDIF

   !# Add pelagic sink and source terms for all depth levels.
   DO i=1,nlev
      CALL fabm_do(model,i,rhs(1:nvars,i))
   ENDDO
END SUBROUTINE right_hand_side_rhs
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE right_hand_side_ppdd(first,numc,nlev,cc,pp,dd)
!-------------------------------------------------------------------------------
! Calculate the temporal derivatives as production/destruction matrices
!-------------------------------------------------------------------------------
!ARGUMENTS
   LOGICAL, INTENT(in)    :: first
   INTEGER, INTENT(in)    :: numc,nlev
   REALTYPE, INTENT(in)   :: cc(:,:)
   REALTYPE, INTENT(out)  :: pp(:,:,:)
   REALTYPE, INTENT(out)  :: dd(:,:,:)
!
!LOCALS
   INTEGER :: i,nvars
!
!-------------------------------------------------------------------------------
!BEGIN
   !# Shortcut to the number of pelagic state variables.
   nvars = ubound(model%info%state_variables,1)

   !# Provide FABM with (pointers to) the current state.
   DO i=1,ubound(model%info%state_variables,1)
      CALL fabm_link_bulk_state_data(model,i,cc(i,1:nlev))
   ENDDO
   DO i=1,ubound(model%info%state_variables_ben,1)
      CALL fabm_link_bottom_state_data(model,i,cc(nvars+i,1))
   ENDDO

   !# If this is not the first step in the (multi-step) integration scheme,
   !# then first make sure that the intermediate state variable values are valid.
   IF (.not. first) CALL do_repair_state(nlev,'glm_fabm::right_hand_side_ppdd')

   !# Initialiaze production and destruction matrices to zero because FABM
   !# biogeochemical models increment these, rather than set these.
   pp = _ZERO_
   dd = _ZERO_

   !# Calculate temporal derivatives due to benthic processes.
   CALL fabm_do_benthos(model,1,pp(:,:,1),dd(:,:,1),nvars)

   !# Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
   pp(1:nvars,:,1) = pp(1:nvars,:,1)/dz(1)
   dd(1:nvars,:,1) = dd(1:nvars,:,1)/dz(1)

   !# Add pelagic sink and source terms for all depth levels.
   DO i=1,nlev
      CALL fabm_do(model,i,pp(1:nvars,1:nvars,i),dd(1:nvars,1:nvars,i))
   ENDDO
END SUBROUTINE right_hand_side_ppdd
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE clean_glm_fabm() BIND(C, name="clean_glm_wq")
!-------------------------------------------------------------------------------
! Finish biogeochemical model
!-------------------------------------------------------------------------------
!BEGIN

   ! Deallocate internal arrays
   IF (ALLOCATED(cc_diag))    DEALLOCATE(cc_diag)
   IF (ALLOCATED(cc_diag_hz)) DEALLOCATE(cc_diag_hz)
   IF (ALLOCATED(ws))         DEALLOCATE(ws)
   IF (ALLOCATED(sfl))        DEALLOCATE(sfl)
   IF (ALLOCATED(rhs_flux))   DEALLOCATE(rhs_flux)
   IF (ALLOCATED(total))      DEALLOCATE(total)
   IF (ALLOCATED(local))      DEALLOCATE(local)
   IF (ALLOCATED(par))        DEALLOCATE(par)
   IF (ALLOCATED(pres))       DEALLOCATE(pres)

END SUBROUTINE clean_glm_fabm
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE update_light(nlev, bioshade_feedback)
!-------------------------------------------------------------------------------
! Calculate photosynthetically active radiation over entire column
! based on surface radiation, and background and biotic extinction.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)  :: nlev
   CLOGICAL,INTENT(in) :: bioshade_feedback
!
!LOCALS
   INTEGER :: i
   REALTYPE :: zz,localext
!
!-------------------------------------------------------------------------------
!BEGIN
   zz = _ZERO_
   localext = _ZERO_

   DO i=nlev,1,-1
      CALL fabm_get_light_extinction(model,i,localext)

      zz = zz + 0.5*dz(i)

      IF (i .EQ. nlev) THEN
         par(i) = 0.45 * rad(i) * EXP( -(lKw + localext) * zz )
      ELSE
         par(i) = par(i+1) * EXP( -(lKw + localext) * zz )
      ENDIF
      zz = zz + 0.5*dz(i)

      IF (bioshade_feedback) extc_coef(i) = lKw + localext
   ENDDO
END SUBROUTINE update_light
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE init_glm_fabm_output(ncid,x_dim,y_dim,z_dim,time_dim) BIND(C, name="init_glm_wq_output")
!-------------------------------------------------------------------------------
!  Initialize the output by defining biogeochemical variables.
!-------------------------------------------------------------------------------
!
!ARGUMENTS
   CINTEGER,INTENT(in) :: ncid,x_dim,y_dim,z_dim,time_dim
!
!LOCALS
   INTEGER n
   INTEGER dims(4)

!
!-------------------------------------------------------------------------------
!BEGIN
   !# Put NetCDF library in define mode.
   CALL define_mode_on(ncid)

   !# Set up dimension indices for 3D (+ time) variables (longitude,latitude,depth,time).
   dims(1) = x_dim
   dims(2) = y_dim
   dims(3) = z_dim
   dims(4) = time_dim

   !# Add a NetCDF variable for each 3D (+ time) biogeochemical state variable.
   DO n=1,ubound(model%info%state_variables,1)
      model%info%state_variables(n)%externalid = NEW_NC_VARIABLE(ncid,         &
                            TRIM(model%info%state_variables(n)%name),          &
                            LEN_TRIM(model%info%state_variables(n)%name),      &
                            NF90_REALTYPE, 4, dims(1:4))
      CALL set_nc_attributes(ncid,model%info%state_variables(n)%externalid,    &
                            MYTRIM(model%info%state_variables(n)%units),       &
                            MYTRIM(model%info%state_variables(n)%long_name)    &
                            PARAM_FILLVALUE)
   ENDDO

   !# Add a NetCDF variable for each 3D (+ time) biogeochemical diagnostic variable.
   DO n=1,ubound(model%info%diagnostic_variables,1)
      model%info%diagnostic_variables(n)%externalid = NEW_NC_VARIABLE(ncid,    &
                       TRIM(model%info%diagnostic_variables(n)%name),          &
                       LEN_TRIM(model%info%diagnostic_variables(n)%name),      &
                       NF90_REALTYPE, 4, dims(1:4))
      CALL set_nc_attributes(ncid,model%info%diagnostic_variables(n)%externalid, &
                            MYTRIM(model%info%diagnostic_variables(n)%units),  &
                            MYTRIM(model%info%diagnostic_variables(n)%long_name)&
                            PARAM_FILLVALUE)
   ENDDO

   !# Set up dimension indices for 2D (+ time) variables (longitude,latitude,time).
   dims(1) = x_dim
   dims(2) = y_dim
   dims(3) = time_dim

   !# Add a NetCDF variable for each horizontal slice (+ time) biogeochemical state variable.
   DO n=1,ubound(model%info%state_variables_ben,1)
      model%info%state_variables_ben(n)%externalid = NEW_NC_VARIABLE(ncid,     &
                       TRIM(model%info%state_variables_ben(n)%name),           &
                       LEN_TRIM(model%info%state_variables_ben(n)%name), NF90_REALTYPE, 3, dims(1:3))
      CALL set_nc_attributes(ncid,model%info%state_variables_ben(n)%externalid,&
                            MYTRIM(model%info%state_variables_ben(n)%units),   &
                            MYTRIM(model%info%state_variables_ben(n)%long_name)&
                            PARAM_FILLVALUE)
   ENDDO

   !# Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical diagnostic variable.
   DO n=1,ubound(model%info%diagnostic_variables_hz,1)
      model%info%diagnostic_variables_hz(n)%externalid = NEW_NC_VARIABLE(ncid, &
                TRIM(model%info%diagnostic_variables_hz(n)%name),              &
                LEN_TRIM(model%info%diagnostic_variables_hz(n)%name), NF90_REALTYPE, 3, dims(1:3))
      CALL set_nc_attributes(ncid,model%info%diagnostic_variables_hz(n)%externalid, &
                            MYTRIM(model%info%diagnostic_variables_hz(n)%units),    &
                            MYTRIM(model%info%diagnostic_variables_hz(n)%long_name) &
                            PARAM_FILLVALUE)
   ENDDO

   !# Add a variable for each conserved quantity
   DO n=1,ubound(model%info%conserved_quantities,1)
      model%info%conserved_quantities(n)%externalid = NEW_NC_VARIABLE(ncid,    &
          TRIM(TRIM(model%info%conserved_quantities(n)%name)//'_tot'),         &
          LEN_TRIM(TRIM(model%info%conserved_quantities(n)%name)//'_tot'), NF90_REALTYPE, 3, dims(1:3))
      CALL set_nc_attributes(ncid,model%info%conserved_quantities(n)%externalid, &
                         TRIM('m*'//model%info%conserved_quantities(n)%units), &
                         MYTRIM(model%info%conserved_quantities(n)%long_name)  &
                         PARAM_FILLVALUE)
   ENDDO

   !# Take NetCDF library out of define mode (ready for storing data).
   CALL define_mode_off(ncid)
END SUBROUTINE init_glm_fabm_output
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
!  Save properties of biogeochemical model, including state variable
!  values, diagnostic variable values, and sums of conserved quantities.
!-------------------------------------------------------------------------------
SUBROUTINE write_glm_fabm(ncid,wlev,nlev,lvl,point_nlevs) BIND(C, name="write_glm_wq_")
!-------------------------------------------------------------------------------
!ARGUMENTS
   CINTEGER,INTENT(in) :: ncid,wlev,nlev
   CINTEGER,INTENT(in) :: lvl(*),point_nlevs
!
!LOCALS
   INTEGER  :: n,i
   REALTYPE :: val_out
   CLOGICAL :: last = .FALSE.
!
!-------------------------------------------------------------------------------
!BEGIN
   DO i=1,ubound(model%info%state_variables,1)
      CALL fabm_link_bulk_state_data(model,i,cc(i,1:wlev))
   ENDDO

   !# Store pelagic biogeochemical state variables.
   DO n=1,ubound(model%info%state_variables,1)
      CALL store_nc_array(ncid,model%info%state_variables(n)%externalid,XYZT_SHAPE,wlev,nlev,array=cc(n,:))
      DO i=1,point_nlevs
         IF (lvl(i) .GE. 0) THEN ; val_out = cc(n,lvl(i)+1)
         ELSE                    ; val_out = missing     ; ENDIF
         CALL write_csv_point(i,model%info%state_variables(n)%name,  &
                             len_trim(model%info%state_variables(n)%name), val_out,"",0,last=last)
      ENDDO
#ifdef PLOTS
      IF ( do_plots ) CALL put_xplot_val(model%info%state_variables(n)%name,     &
                                len_trim(model%info%state_variables(n)%name),wlev,cc(n,1:wlev))
#endif
   ENDDO

   !# Store benthic biogeochemical state variables.
   DO n=1,ubound(model%info%state_variables_ben,1)
      CALL store_nc_scalar(ncid,model%info%state_variables_ben(n)%externalid, &
                                 XYT_SHAPE,scalar=cc(ubound(model%info%state_variables,1)+n,1))
   ENDDO

   !# Process and store diagnostic variables defined on the full domain.
   DO n=1,ubound(model%info%diagnostic_variables,1)
      !# Store diagnostic variable values.
      CALL store_nc_array(ncid,model%info%diagnostic_variables(n)%externalid,XYZT_SHAPE,wlev,nlev,array=cc_diag(n,:))

#ifdef PLOTS
      IF ( do_plots ) CALL put_xplot_val(model%info%diagnostic_variables(n)%name,     &
                                len_trim(model%info%diagnostic_variables(n)%name),wlev,cc_diag(n,1:wlev))
#endif
   ENDDO

   !# Process and store diagnostic variables defined on horizontal slices of the domain.
   DO n=1,ubound(model%info%diagnostic_variables_hz,1)
      !# Store diagnostic variable values.
      CALL store_nc_scalar(ncid,model%info%diagnostic_variables_hz(n)%externalid,XYT_SHAPE,scalar=cc_diag_hz(n))
   ENDDO

   !# Integrate conserved quantities over depth.
   total = _ZERO_
   DO n=1,wlev
      CALL fabm_get_conserved_quantities(model,n,local)
      total = total + dz(n)*local
   ENDDO

   !# Store conserved quantity integrals.
   DO n=1,ubound(model%info%conserved_quantities,1)
      CALL store_nc_scalar(ncid,model%info%conserved_quantities(n)%externalid,XYT_SHAPE,scalar=total(n))
   ENDDO
END SUBROUTINE write_glm_fabm
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
CINTEGER FUNCTION wqvar_index_c(name, len) BIND(C, name="wqvar_index_c")
!-------------------------------------------------------------------------------
!ARGUMENTS
   CCHARACTER,INTENT(in) :: name(*)
   CINTEGER,INTENT(in)   :: len
!LOCALS
   CHARACTER(len=len+1) :: tn
   INTEGER              :: i
!BEGIN
   tn = ''
   DO i=1,len
     tn=tn//' '
     tn(i:i) = name(i)
   ENDDO
   WQVar_Index_c = WQVar_Index(tn) - 1
END FUNCTION wqvar_index_c
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
INTEGER FUNCTION WQVar_Index(name)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CHARACTER(len=*) :: name
!
!LOCALS
   INTEGER i
!
!-------------------------------------------------------------------------------
!BEGIN
   DO i=1,ubound(model%info%state_variables,1)+ubound(model%info%state_variables_ben,1)
      IF (name .EQ. names(i)) THEN
         WQVar_Index = i
         RETURN
      ENDIF
   ENDDO
   WQVar_Index = -1
END FUNCTION WQVar_Index
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END MODULE glm_fabm
