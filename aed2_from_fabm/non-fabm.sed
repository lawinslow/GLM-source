#


s/_ARGUMENTS_DO_PPDD_/_FABM_ARGS_DO_PPDD_/
s/_ARGUMENTS_DO_BOTTOM_/_FABM_ARGS_DO_BENTHOS_RHS_/
s/_ARGUMENTS_DO_BOTTOM_PPDD_/_FABM_ARGS_DO_BENTHOS_PPDD_/
s/_ARGUMENTS_DO_SURFACE_/_FABM_ARGS_GET_SURFACE_EXCHANGE_/
s/_ARGUMENTS_DO_/_FABM_ARGS_DO_RHS_/
s/_ARGUMENTS_GET_EXTINCTION_/_FABM_ARGS_GET_EXTINCTION_/
s/_ARGUMENTS_GET_DRAG_/_FABM_ARGS_GET_DRAG_/
s/_ARGUMENTS_GET_ALBEDO_/_FABM_ARGS_GET_ALBEDO_/
s/_ARGUMENTS_GET_VERTICAL_MOVEMENT_/_FABM_ARGS_GET_VERTICAL_MOVEMENT_/
s/_ARGUMENTS_GET_CONSERVED_QUANTITIES_/_FABM_ARGS_GET_CONSERVED_QUANTITIES_/
s/_ARGUMENTS_CHECK_STATE_/_FABM_ARGS_CHECK_STATE_/
s/\<procedure\>/PROCEDURE/


s/_SET_ODE_SED_FLUX_/_SET_SED_FLUX_/

# These 4 are temporary
s/_SET_BOTTOM_EXCHANGE_/_SET_FLUX_BOTTOM_/
s/_SET_ODE_BEN_/_SET_FLUX_BEN_/
s/_SET_ODE_/_SET_FLUX_PEL_/
s/_SET_SURFACE_EXCHANGE_/_SET_FLUX_ATM_/

# This is what will replace them
s/_SET_FLUX_BOTTOM_(\([^,]*\),\(.*\))$/_FLUX_VAR_(\1) = _FLUX_VAR_(\1) + (\2)/
s/_SET_FLUX_PEL_(\([^,]*\),\(.*\))$/_FLUX_VAR_(\1) = _FLUX_VAR_(\1) + (\2)/
s/_SET_FLUX_BEN_(\([^,]*\),\(.*\))$/_FLUX_VAR_B_(\1) = _FLUX_VAR_B_(\1) + (\2)/
s/_SET_FLUX_ATM_(\([^,]*\),\(.*\))/_FLUX_VAR_T_(\1) = \2/

#s/_SET_FLUX_BOTTOM_(\([^,]*\),\(.*\))$/column(\1)%flux_pel(layer_idx) = column(\1)%flux_pel(layer_idx) + (\2)/
#s/_SET_FLUX_PEL_(\([^,]*\),\(.*\))$/column(\1)%flux_pel(layer_idx) = column(\1)%flux_pel(layer_idx) + (\2)/
#s/_SET_FLUX_BEN_(\([^,]*\),\(.*\))$/column(\1)%flux_ben = column(\1)%flux_ben + (\2)/
#s/_SET_FLUX_ATM_(\([^,]*\),\(.*\))/column(\1)%flux_atm = \2/

#In the meantime this is what we get
#define _SET_FLUX_BOTTOM_(variable,value) column(variable)%flux_pel(layer_idx) = column(variable)%flux_pel(layer_idx) + (value)
#define _SET_FLUX_PEL_(variable,value)    column(variable)%flux_pel(layer_idx) = column(variable)%flux_pel(layer_idx) + (value)
#define _SET_FLUX_BEN_(variable,value)    column(variable)%flux_ben = column(variable)%flux_ben + (value)
#define _SET_FLUX_ATM_(variable,value)    column(variable)%flux_atm = value


s/_CLASS_/CLASS/
#s/\<CLASS\>/TYPE/
s/real(rk)/AED_REAL/
s/REALTYPE/AED_REAL/
s/type_base_model/aed_model_data_t/
s/USE fabm_driver/USE aed_core/
/USE fabm_types/d
/_FABM_HORIZONTAL_LOOP_BEGIN_/d
/_FABM_HORIZONTAL_LOOP_END_/d
/_FABM_HZ_LOOP_BEGIN_/d
/_FABM_HZ_LOOP_END_/d
/_FABM_LOOP_BEGIN_/d
/_FABM_LOOP_END_/d

/_HORIZONTAL_LOOP_BEGIN_/d
/_HORIZONTAL_LOOP_END_/d
/_HZ_LOOP_BEGIN_/d
/_HZ_LOOP_END_/d
/_LOOP_BEGIN_/d
/_LOOP_END_/d

s/_ONE_/one_/g
s/_ZERO_/zero_/g

s/_TYPE_CONSERVED_QUANTITY_ID_/INTEGER/
s/_TYPE_DEPENDENCY_ID_/INTEGER/
s/_TYPE_DIAGNOSTIC_VARIABLE_ID_/INTEGER/
s/_TYPE_STATE_VARIABLE_ID_/INTEGER/
s/_VARIABLE_REGISTERED_(\(.*\)))/ \1 > 0)/

#-------------------------------------------------------------------------------

s/_DECLARE_FABM_ARGS_DO_BENTHOS_RHS_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx/
s/_FABM_ARGS_DO_BENTHOS_RHS_/column,layer_idx/

s/_DECLARE_FABM_ARGS_DO_PPDD_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx\n   AED_REAL,DIMENSION(:,:),INTENT(inout) :: pp,dd/
s/_FABM_ARGS_DO_PPDD_/column,layer_idx,pp,dd/

s/_DECLARE_FABM_ARGS_DO_RHS_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx/
s/_FABM_ARGS_DO_RHS_/column,layer_idx/

s/_DECLARE_FABM_ARGS_GET_CONSERVED_QUANTITIES_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx\n   AED_REAL,DIMENSION(:),INTENT(inout) :: sums/
s/_FABM_ARGS_GET_CONSERVED_QUANTITIES_/column,layer_idx,sums/

s/_DECLARE_FABM_ARGS_GET_VERTICAL_MOVEMENT_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx\n   AED_REAL,INTENT(inout) :: mobility(:)/
s/_FABM_ARGS_GET_VERTICAL_MOVEMENT_/column,layer_idx,mobility/

s/_DECLARE_FABM_ARGS_GET_EXTINCTION_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx\n   AED_REAL,INTENT(inout) :: extinction/
s/_FABM_ARGS_GET_EXTINCTION_/column,layer_idx,extinction/

s/_DECLARE_FABM_ARGS_GET_SURFACE_EXCHANGE_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx/
s/_FABM_ARGS_GET_SURFACE_EXCHANGE_/column,layer_idx/

s/_DECLARE_FABM_ARGS_CHECK_STATE_/TYPE (aed_column_t),INTENT(inout) :: column(:)\n   INTEGER,INTENT(in) :: layer_idx/
s/_FABM_ARGS_CHECK_STATE_/column,layer_idx/

#-------------------------------------------------------------------------------

# Sort out the alterante names
s/_GET_STATE_BEN_/_GET_HORIZONTAL_/
s/_GET_STATE_/_GET_/
s/_GET_DEPENDENCY_HZ_/_GET_HORIZONTAL_/
s/_GET_DEPENDENCY_/_GET_/
s/_SET_DIAG_HZ_/_SET_HORIZONTAL_DIAGNOSTIC_/
s/_SET_DIAG_/_SET_DIAGNOSTIC_/
s/_SET_ODE_SED_FLUX_/_SET_ODE_/
s/_GET_[ ]*(/_GET_(/

#-------------------------------------------------------------------------------

#define _GET_(id, val)   val = column(id)%cell(layer_idx)
#s/_GET_(\(.*\),\(.*\)) \(.*\)/\2 = column(\1)%cell(layer_idx)\3/
#s/_GET_(\(.*\),\(.*\))$/\2 = column(\1)%cell(layer_idx)/

#define _GET_(id, val)   val = _STATE_VAR_(layer_idx)
# First the special case for aed_totals
s/_GET_(\(.*\),\(.*\));\(.*\)/\2 = _STATE_VAR_(\1);\3/
s/_GET_(\(.*\),\(.*\)) \(.*\)/\2 = _STATE_VAR_(\1)\3/
s/_GET_(\(.*\),\(.*\))$/\2 = _STATE_VAR_(\1)/

#define _GET_HORIZONTAL_(id, val)   val = column(id)%cell_sheet
#s/_GET_HORIZONTAL_(\(.*\),\(.*\)) \([ ].*\)/\2 = column(\1)%cell_sheet\3/
#s/_GET_HORIZONTAL_(\(.*\),\(.*\))$/\2 = column(\1)%cell_sheet/
s/_GET_HORIZONTAL_(\(.*\),\(.*\)) \([ ].*\)/\2 = _STATE_VAR_S_(\1)\3/
s/_GET_HORIZONTAL_(\(.*\),\(.*\))$/\2 = _STATE_VAR_S_(\1)/

#define _SET_CONSERVED_QUANTITY_(id, val)   sums(id) = sums(id) + (val)
s/_SET_CONSERVED_QUANTITY_(\(.*\),\(.*\))/sums(\1) = sums(\1) + (\2)/

#define _SET_DIAGNOSTIC_(id, val)   column(id)%diag(layer_idx) = val
#s/_SET_DIAGNOSTIC_(\(.*\),\(.*\))/column(\1)%diag(layer_idx) = \2/
s/_SET_DIAGNOSTIC_(\(.*\),\(.*\))/_DIAG_VAR_(\1) = \2/

#define _SET_HORIZONTAL_DIAGNOSTIC_(id, val)   column(id)%diag_sheet = val
#s/_SET_HORIZONTAL_DIAGNOSTIC_(\(.*\),\(.*\))/column(\1)%diag_sheet = \2/
s/_SET_HORIZONTAL_DIAGNOSTIC_(\(.*\),\(.*\))/_DIAG_VAR_S_(\1) = \2/

#define _SET_STATE_(id, val)   column(id)%cell(layer_idx) = val
#s/_SET_STATE_(\(.*\),\(.*\))/column(\1)%cell(layer_idx) = \2/
s/_SET_STATE_(\(.*\),\(.*\))/_STATE_VAR_(\1) = \2/

#define _SET_STATE_BEN_(id, val)   column(id)%cell_sheet = val
#s/_SET_STATE_BEN_(\(.*\),\(.*\))/column(\1)%cell_sheet = \2/
s/_SET_STATE_BEN_(\(.*\),\(.*\))/_STATE_VAR_S_(\1) = \2/

#define _SET_VERTICAL_MOVEMENT_(idx,val)  mobility(idx) = (val)
s/_SET_VERTICAL_MOVEMENT_(\(.*\),\(.*\))/mobility(\1) = \2/

#define _SET_EXTINCTION_(val)  extinction = extinction + (val)
s/_SET_EXTINCTION_(\(.*\))/extinction = extinction + (\1)/

#define _SET_BOTTOM_EXCHANGE_(id, val)   flux_pel(id) = flux_pel(id) + (val)
#s/_SET_BOTTOM_EXCHANGE_(\(.*\),\(.*\))$/column(\1)%flux_pel = column(\1)%flux_pel + (\2)/
s/_SET_BOTTOM_EXCHANGE_(\(.*\),\(.*\))$/_FLUX_VAR_(\1) = _FLUX_VAR_(\1) + (\2)/

#define _SET_ODE_(id, val)   flux_pel(id) = flux_pel(id) + (val)
#s/_SET_ODE_(\(.*\),\(.*\))$/column(\1)%flux_pel = column(\1)%flux_pel + (\2)/
s/_SET_ODE_(\(.*\),\(.*\))$/_FLUX_VAR_(\1) = _FLUX_VAR_(\1) + (\2)/

#define _SET_ODE_BEN_(id, val)   flux_ben(id) = flux_ben(id) + (val)
#s/_SET_ODE_BEN_(\(.*\),\(.*\))$/column(\1)%flux_ben = column(\1)%flux_ben + (\2)/
s/_SET_ODE_BEN_(\(.*\),\(.*\))$/_FLUX_VAR_B_(\1) = _FLUX_VAR_B_(\1) + (\2)/

#define _SET_SURFACE_EXCHANGE_(id, val)   flux_atm(id) = val
#s/_SET_SURFACE_EXCHANGE_(\(.*\),\(.*\))/column(\1)%flux_atm = \2/
s/_SET_SURFACE_EXCHANGE_(\(.*\),\(.*\))/_FLUX_VAR_T_(\1) = \2/

#-------------------------------------------------------------------------------

## These should not really be needed because the ppdd routines will be deleted
#define _SET_DD_SYM_(id1, id2, val)   _SET_DD_(id1,id2,val);_SET_PP_(id2,id1,val)
s/_SET_DD_SYM_(\(.*\),\(.*\),\(.*\))/_SET_DD_(\1,\2,\3) ; _SET_PP_(\2,\1,\3)/

#define _SET_PP_SYM_(id1, id2, val)   _SET_PP_(id1,id2,val);_SET_DD_(id2,id1,val)
s/_SET_PP_SYM_(\(.*\),\(.*\),\(.*\))/_SET_PP_(\1,\2,\3) ; _SET_DD_(\2,\1,\3)/

#define _SET_DD_(id1, id2, val)   dd(id1,id2) = dd(id1,id2) + (val)
s/_SET_DD_(\(.*\),\(.*\),\(.*\))/dd(\1,\2) = dd(\1,\2) + (\3)/

#define _SET_PP_(id1, id2, val)   pp(id1,id2) = pp(id1,id2) + (val)
s/_SET_PP_(\(.*\),\(.*\),\(.*\))/pp(\1,\2) = pp(\1,\2) + (\3)/

#-------------------------------------------------------------------------------
#
# remove blank lines at the head of the file
/./,$!d

# strip trailing blanks
s/[ \t]*$//

# and replace with a proper include statement.
s/^INCLUDE_AED_H/\#include \"aed.h\"/

# delete lines from "START OF AED.H to END OF AED.H
# because the preprocessor leaves a lot of blanks and other detritus
/START OF AED.H/,/END OF AED.H/d

# delete lines like :
/! Enter spatial loops (if any)/d
/!# Enter spatial loops (if any)/d
/! Leave spatial loops (if any)/d
/!# Leave spatial loops (if any)/d
/USE fabm_driver/d

# replace
s/USE fabm_types/USE aed_core/
s/type_base_model/aed_model_data_t/
#s/secs_pr_day/secs_per_day/

# replace lower case word "call" with uppercase.
s/\<call\>/CALL/

# These will replace :
#   CALL self%register_state_variable(a,
# with
#   a = aed_define_variable(
s/CALL self%register_state_variable(\([^,]*\),/\1 = aed_define_variable(/
s/CALL self%register_diagnostic_variable(\([^,]*\),/\1 = aed_define_diag_variable(/
s/CALL self%register_horizontal_diagnostic_variable(\([^,]*\),/\1 = aed_define_sheet_diag_variable(/
s/CALL self%register_state_dependency(\([^,]*\),/\1 = aed_locate_variable(/
s/CALL self%register_dependency(\([^,]*\),/\1 = aed_locate_global(/
s/CALL self%register_bottom_state_dependency(\([^,]*\),/\1 = aed_locate_sheet_variable(/
s/CALL self%register_horizontal_dependency_sn(\([^,]*\),/\1 = aed_locate_global_sheet(/
s/CALL self%register_horizontal_dependency(\([^,]*\),/\1 = aed_locate_global_sheet(/

# CALL self%request_coupling is not needed
/self%request_coupling/d

# replace lower case word "type" with uppercase.
s/\<type (/TYPE (/

# These types will now be integers
s/TYPE (type_state_variable_id)/INTEGER/
s/TYPE (type_dependency_id)/INTEGER/
s/TYPE (type_horizontal_dependency_id)/INTEGER/
s/TYPE (type_diagnostic_variable_id)/INTEGER/

s/TYPE (type_bottom_state_variable_id)/INTEGER/
s/TYPE (type_horizontal_diagnostic_variable_id)/INTEGER /

# delete the CONTAINS section from types
#/  CONTAINS/d
s/  CONTAINS.*$/_AED_CONTAINS_\n/
/ PROCEDURE ::/d
/Model Procedures/d

# The preprocessor fudge that expands to multiple lines uses ;EOLN; to mark
# what should be a newline. This makes it so
s/;EOLN;/\n/g

# Looking for a pattern aed_type_<something> and replace it with aed_<something>_data
# where the something is terminated by either a ) or end-of-line
#    s/aed_type_\([^,]*\)\()\|$\)/aed_\1_data\2/
# and even better - any word terminator will do it :
#    s/aed_type_\([^,]*\)\(\>\)/aed_\1_data\2/
# except that : has a special meaning in regexp, so do :: as a special case
s/aed_type_\([^,]*\)\(::\)/aed_\1_data_t\2/
# then do regular word terminator
s/aed_type_\([^,]*\)\(\>\)/aed_\1_data_t\2/

# delete do_ppdd subroutine
/^SUBROUTINE aed_.*_do_ppdd/,/^END SUBROUTINE aed_.*_do_ppdd/d

# delete conserved quantity stuff
/^SUBROUTINE aed_.*_get_conserved_quantities/,/^END SUBROUTINE aed_.*_get_conserved_quantities/d
/TYPE (type_conserved_quantity_id)/d
/register_conserved_quantity/d
/Register conserved quantities/d

# These were to do with conserved quantities, only we dont want conserveds anymore
#s/type (type_conserved_quantity_id)/INTEGER/
#s/CALL self%register_conserved_quantity(\([^,]*\),/\1 = aed_define_sums(/

# Looking for a pattern 'SUBROUTINE aed_init_<something>(self,<something2>)'
# and replace it with 'FUNCTION aed_define_<something>(<something2>) RESULT(data)'
#s/SUBROUTINE aed_init_\([^,]*\)(self,\([^,]*\))/FUNCTION aed_define_\1(\2) RESULT(data)/
s/SUBROUTINE aed_init_\(.*\)(self,\(.*\))/SUBROUTINE aed_define_\1(data, \2)/
# strip the TARGET attribute
s/,TARGET//

s/END SUBROUTINE aed_init_\(.*\)\>/END SUBROUTINE aed_define_\1/

# Look for 'aed_<something>_do' and replace it with aed_calculate_<something>
s/aed_\(.*\)_do$/aed_calculate_\1/
s/aed_\(.*\)_do(\(.*\)/aed_calculate_\1(\2/
# Now make the benthic one more sensible
s/aed_\(.*\)_do_benthos/aed_calculate_benthic_\1/
s/aed_\(.*\)_check_state/aed_equilibrate_\1/
# Now something similar for the surface exchange
s/aed_\(.*\)_get_surface_exchange\(.*\)/aed_calculate_surface_\1\2/
# Now something similar for the vertical motion
s/aed_\(.*\)_get_vertical_movement\(.*\)/aed_mobility_\1\2/
# Now something similar for the light_extinction
s/aed_\(.*\)_get_light_extinction\(.*\)/aed_light_extinction_\1\2/

# replace all instances of "self" with "data"
s/\<self\>/data/g
s/\secs_pr_day/secs_per_day/g

# These have changed, but a backward compatibility means both forms need to be checked for
s/\<varname_temp\>/'temperature'/
s/\<varname_salt\>/'salinity'/
s/\<varname_par_sf\>/'par_sf'/
s/\<varname_par\>/'par'/
s/\<varname_layer_ht\>/'layer_ht'/
s/\<varname_extc\>/'extc_coef'/
s/\<varname_tss\>/'tss'/
s/\<varname_sed_zone\>/'sed_zone'/
s/\<varname_wind_sf\>/'wind_speed'/
s/\<varname_pres\>/'pressure'/
s/standard_variables%temperature/'temperature'/
s/standard_variables%practical_salinity/'salinity'/
s/standard_variables%wind_speed/'wind_speed'/
s/standard_variables%downwelling_photosynthetic_radiative_flux/'par'/
s/standard_variables%surface_downwelling_photosynthetic_radiative_flux/'par_sf'/
s/standard_variables%mass_concentration_of_suspended_matter/'tss'/
s/standard_variables%cell_thickness/'layer_ht'/
s/standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux/'extc_coef'/
s/standard_variables%env_sed_zone/'sed_zone'/
s/standard_variables%pressure/'pressure'/
s/aed_locate_global('pressure'/aed_locate_global_sheet('pressure'/
s/aed_locate_global('wind_speed'/aed_locate_global_sheet('wind_speed'/
s/aed_locate_global('par_sf'/aed_locate_global_sheet('par_sf'/
s/aed_locate_global('sed_zone'/aed_locate_global_sheet('sed_zone'/

s/, output=output_instantaneous//

# These are sheet globals
s/aed_locate_global('wind_speed')/aed_locate_global_sheet('wind_speed')/
s/aed_locate_global('par_sf')/aed_locate_global_sheet('par_sf')/
s/aed_locate_global('sed_zone')/aed_locate_global_sheet('sed_zone')/
s/aed_locate_global('pressure')/aed_locate_global_sheet('pressure')/

# Fsed variables are probably sheet vars
s/aed_define_variable('Fsed/aed_define_sheet_variable('Fsed/


s/INTEGER  *::/INTEGER  ::/

s/!     Model parameters/      !# Model parameters/
s/!     Variable identifiers/      !# Variable identifiers/

s/\<vertical_movement\>/mobility/
s/no_river_dilution=.false.,//
s/no_river_dilution=.true.,//
s/,no_river_dilution=.*)/)/
s/, no_river_dilution=.*)/)/

s/\<FABM\>/AED2/

# These are the macros for given variables type
#define _STATE_VAR_(id)   column(id)%cell(layer_idx)
#define _STATE_VAR_S_(id) column(id)%cell_sheet
#define _DIAG_VAR_(id)    column(id)%diag(layer_idx)
#define _DIAG_VAR_S_(id)  column(id)%diag_sheet

#define _FLUX_VAR_(id)    column(id)%flux_pel(layer_idx)
#define _FLUX_VAR_T_(id)  column(id)%flux_atm
#define _FLUX_VAR_B_(id)  column(id)%flux_ben

