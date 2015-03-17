#!/bin/tcsh

set PREFIX="aed2"
#set PREFIX="aed"

/bin/cp ../fabm-git/src/models/aed/aed* .
# These 3 will be completely new versions
/bin/rm aed.h
/bin/rm aed_core.F90
/bin/rm aed_models.F90

foreach i (aed_phyto_utils aed_zoop_utils aed_util)
    sed -i -e 's/_ONE_/one_/g' $i.F90
    sed -i -e 's/_ZERO_/zero_/g' $i.F90
end


set LIST=( \
           aed_bivalve.F90        \
           aed_carbon.F90         \
           aed_chlorophylla.F90   \
           aed_iron.F90           \
           aed_nitrogen.F90       \
           aed_organic_matter.F90 \
           aed_oxygen.F90         \
           aed_pathogens.F90      \
           aed_phosphorus.F90     \
           aed_phytoplankton.F90  \
           aed_sedflux.F90        \
           aed_silica.F90         \
           aed_sulfur.F90         \
           aed_test.F90           \
           aed_totals.F90         \
           aed_tracer.F90         \
           aed_zoop_utils.F90     \
           aed_zooplankton.F90    )

mkdir ../libaed2/cnv

foreach file ( $LIST )
   echo $file

   sed -i -f non-fabm.sed $file

   if ( "${PREFIX}" == "aed2" ) then
      sed -i 's/\<aed_/aed2_/g' $file
      sed -i 's/aed.h/aed2.h/' $file
   endif

   set NAME=`echo $file:r | sed -e 's/aed_//'`
   # echo $NAME
   if ( $NAME == 'chlorophylla' ) set NAME='chla'
   sed -i -e "s/_AED_CONTAINS_/ CONTAINS\n         PROCEDURE :: define            => ${PREFIX}_define_$NAME\n_AED_CONTAINS_/" $file
   grep -w ${PREFIX}_calculate_surface_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: calculate_surface => ${PREFIX}_calculate_surface_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_calculate_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: calculate         => ${PREFIX}_calculate_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_calculate_benthic_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: calculate_benthic => ${PREFIX}_calculate_benthic_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_equilibrate_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: equilibrate       => ${PREFIX}_equilibrate_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_mobility_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: mobility          => ${PREFIX}_mobility_$NAME\n_AED_CONTAINS_/" $file
   else
      sed -i -e "s/_AED_CONTAINS_/!        PROCEDURE :: mobility          => ${PREFIX}_mobility_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_light_extinction_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: light_extinction  => ${PREFIX}_light_extinction_$NAME\n_AED_CONTAINS_/" $file
   else
      sed -i -e "s/_AED_CONTAINS_/!        PROCEDURE :: light_extinction  => ${PREFIX}_light_extinction_$NAME\n_AED_CONTAINS_/" $file
   endif
   grep -w ${PREFIX}_delete_$NAME $file >& /dev/null
   if ( $status == 0 ) then
      sed -i -e "s/_AED_CONTAINS_/         PROCEDURE :: delete            => ${PREFIX}_delete_$NAME/" $file
   else
      sed -i -e "s/_AED_CONTAINS_/!        PROCEDURE :: delete            => ${PREFIX}_delete_$NAME/" $file
   endif

   if ( $NAME == 'chla' ) set NAME='chlorophylla'
   mv $file ../libaed2/cnv/${PREFIX}_${NAME}.F90
end

/bin/mv aed_phyto_utils.F90 ../libaed2/cnv/${PREFIX}_phyto_utils.F90
/bin/mv aed_util.F90 ../libaed2/cnv/${PREFIX}_util.F90

if ( "${PREFIX}" == "aed2" ) then
   sed -i 's/\<aed_/aed2_/g' ../libaed2/cnv/${PREFIX}_phyto_utils.F90
   sed -i 's/aed.h/aed2.h/' ../libaed2/cnv/${PREFIX}_phyto_utils.F90
   sed -i 's/\<aed_/aed2_/g' ../libaed2/cnv/${PREFIX}_util.F90
   sed -i 's/aed.h/aed2.h/' ../libaed2/cnv/${PREFIX}_util.F90
endif

