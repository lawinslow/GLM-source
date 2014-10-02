#!/bin/bash

source /opt/intel/bin/compilervars.sh intel64

export FORTRAN_COMPILER=IFORT
export FC=ifort
export NETCDFHOME=/opt/intel

export F77=$FC
export F90=$FC
export F95=$FC

export MPI=OPENMPI
export NETCDFINC=$NETCDFHOME/include
export NETCDFINCL=${NETCDFINC}
export NETCDFLIBDIR=$NETCDFHOME/lib
export NETCDFLIB=${NETCDFLIBDIR}
export NETCDFLIBNAME="-lnetcdff -lnetcdf"
export CURDIR=`pwd`
export FABMDIR=${CURDIR}/fabm-git
export PLOTDIR=${CURDIR}/libplot
export UTILDIR=${CURDIR}/libutil
export COMPILATION_MODE=production
export FABM=true

cd ${UTILDIR}
make clean

cd ${PLOTDIR}
make clean

cd ${FABMDIR}/src
make distclean

cd ${CURDIR}/glm-aed/src
make clean

cd ${CURDIR}/glm-aed
fakeroot make -f debian/rules clean

cd ${CURDIR}/glm-tests/
LIST1=`find . -name WQ\*.csv`
LIST2=`find . -name output.nc`
LIST3=`find . -name lake.csv`
LIST4=`find . -name outlet_\?\?.csv`
LIST5=`find . -name overflow.csv`
LIST=`echo $LIST1 $LIST2 $LIST3 $LIST4 $LIST5`
#echo $LIST1
#echo $LIST2
#echo $LIST3
#echo $LIST
if [ "$LIST" != "" ] ; then
  /bin/rm $LIST
fi
# for i in * ; do
#   cd $i
#   # /bin/rm WQ*.csv output.nc lake.csv
#   # grep out_dir glm.nml
#   cd ..
# done
