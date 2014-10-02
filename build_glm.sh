#!/bin/bash

export FORTRAN_COMPILER=IFORT
# export FORTRAN_COMPILER=GFORTRAN
# export FORTRAN_COMPILER=OPEN64

if [ "$FORTRAN_COMPILER" = "IFORT" ] ; then
   . /opt/intel/bin/compilervars.sh intel64
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
   export NETCDFHOME=/opt/intel
elif [ "$FORTRAN_COMPILER" = "IFORT11" ] ; then
   . /opt/intel/Compiler/11.1/072/bin/ifortvars.sh intel64
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
   #export NETCDFHOME=/opt/intel
   export NETCDFHOME=/usr
elif [ "$FORTRAN_COMPILER" = "IFORT12" ] ; then
   . /opt/intel/bin/compilervars.sh intel64
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
   export NETCDFHOME=/opt/intel
elif [ "$FORTRAN_COMPILER" = "OPEN64" ] ; then
   . /opt/open64/open64_env.sh
   #export PATH="/opt/open64/bin:$PATH"
   export FC=openf95
   export NETCDFHOME=/opt/open64
else
   export FC=gfortran
   export NETCDFHOME=/usr
fi

export OSTYPE=`uname -s`

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
export COMPILATION_MODE=production
if [ "$DEBUG" = "true" ] ; then
  export COMPILATION_MODE=debug
fi
export FABM=true

export FABMHOST=glm

mkdir -p ${FABMDIR}/lib/${FABMHOST}/${FORTRAN_COMPILER}
mkdir -p ${FABMDIR}/modules/${FABMHOST}/${FORTRAN_COMPILER}

cd ${FABMDIR}
#make VERSION || exit 1
cd ${FABMDIR}/src
make || exit 1

if [ "$OSTYPE" != "Darwin" ] ; then
  cd ${PLOTDIR}
  make || exit 1
fi

cd ${CURDIR}/libutil
make || exit 1

/bin/rm ${CURDIR}/glm-aed/src/glm
cd ${CURDIR}/glm-aed/src
make || exit 1

cd ${CURDIR}/glm-aed

if [ "$OSTYPE" != "Darwin" ] ; then
  VERSION=`grep GLM_VERSION src/glm.h | cut -f2 -d\"`
  echo glm version $VERSION
  VERSDEB=`head -1 debian/changelog | cut -f2 -d\( | cut -f1 -d-`
  echo debian version $VERSDEB
  if [ "$VERSION" != "$VERSDEB" ] ; then
     echo updating debian version
     dch --newversion ${VERSION}-0 "new version ${VERSION}"
  fi

  fakeroot make -f debian/rules binary || exit 1

  cd ${CURDIR}/glm-aed/win
  ${CURDIR}/vers.sh $VERSION

  cd ${CURDIR}
  if [ ! -d binaries/ubuntu/$(lsb_release -rs) ] ; then
    mkdir -p ubuntu/$(lsb_release -rs)/
  fi
  mv glm*.deb ubuntu/$(lsb_release -rs)/
fi

exit 0
