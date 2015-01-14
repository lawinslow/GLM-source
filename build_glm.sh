#!/bin/bash

export FABM=true
export AED2=true
export USE_DL=false

export FORTRAN_COMPILER=IFORT
# export FORTRAN_COMPILER=GFORTRAN
# export FORTRAN_COMPILER=OPEN64

export OSTYPE=`uname -s`

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
   if [ "$OSTYPE" == "Darwin" ] ; then
     export NETCDFHOME=/opt/local
   fi
fi

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
export AED2DIR=${CURDIR}/libaed2

if [ ! -d $FABMDIR ] ; then
   echo "FABM directory not found - building a non-FABM version"
   export FABM=false
fi

export COMPILATION_MODE=production

cd ${AED2DIR}
make || exit 1

if [ "${FABM}" = "true" ] ; then
  export FABMHOST=glm
  cd ${FABMDIR}
  mkdir build
  cd build
  export EXTRA_FFLAGS+=-fPIC
  if [ "${USE_DL}" = "true" ] ; then
    cmake ${FABMDIR}/src -DBUILD_SHARED_LIBS=1 || exit 1
  else
    cmake ${FABMDIR}/src || exit 1
  fi
  make || exit 1
fi

if [ "$OSTYPE" != "Darwin" ] ; then
  cd ${PLOTDIR}
  make || exit 1
fi

cd ${CURDIR}/libutil
make || exit 1

if [ -f ${CURDIR}/glm-aed/src/glm ] ; then
  /bin/rm ${CURDIR}/glm-aed/src/glm
fi
cd ${CURDIR}/glm-aed/src
make || exit 1

cd ${CURDIR}/glm-aed

if [ "$OSTYPE" == "Linux" ] ; then
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
  cd ${CURDIR}/glm-aed/win-dll
  ${CURDIR}/vers.sh $VERSION

  cd ${CURDIR}
  if [ ! -d binaries/ubuntu/$(lsb_release -rs) ] ; then
    mkdir -p ubuntu/$(lsb_release -rs)/
  fi
  mv glm*.deb ubuntu/$(lsb_release -rs)/
fi

exit 0
