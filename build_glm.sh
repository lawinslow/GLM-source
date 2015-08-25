#!/bin/bash

. GLM_CONFIG

export OSTYPE=`uname -s`

if [ "$FORTRAN_COMPILER" = "IFORT" ] ; then
   if [ -d /opt/intel/bin ] ; then
      . /opt/intel/bin/compilervars.sh intel64
   fi
   which ifort >& /dev/null
   if [ $? != 0 ] ; then
      echo ifort compiler requested, but not found
      exit 1
   fi
fi

if [ "$FORTRAN_COMPILER" = "IFORT" ] ; then
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
elif [ "$FORTRAN_COMPILER" = "IFORT11" ] ; then
   . /opt/intel/Compiler/11.1/072/bin/ifortvars.sh intel64
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
elif [ "$FORTRAN_COMPILER" = "IFORT12" ] ; then
   . /opt/intel/bin/compilervars.sh intel64
   export PATH="/opt/intel/bin:$PATH"
   export FC=ifort
elif [ "$FORTRAN_COMPILER" = "OPEN64" ] ; then
   . /opt/open64/open64_env.sh
   #export PATH="/opt/open64/bin:$PATH"
   export FC=openf95
else
   export FC=gfortran
fi

export F77=$FC
export F90=$FC
export F95=$FC

export MPI=OPENMPI


export CURDIR=`pwd`
if [ "${FABM_NEW_BUILD}" = "true" ] ; then
  export FABMDIR=${CURDIR}/FABM-new/fabm-git
else
  export FABMDIR=${CURDIR}/fabm-git
fi
export PLOTDIR=${CURDIR}/libplot
export AED2DIR=${CURDIR}/libaed2

if [ "$FABM" = "true" ] ; then
   if [ ! -d $FABMDIR ] ; then
      echo "FABM directory not found"
      export FABM=false
   else
      which cmake >& /dev/null
      if [ $? != 0 ] ; then
         echo "cmake not found - FABM cannot be built"
         export FABM=false
      fi
   fi
   if [ "$FABM" = "false" ] ; then
      echo build with FABM requested but FABM cannot be built
      exit 1
   fi
fi

export COMPILATION_MODE=production

if [ "${AED2}" = "true" ] ; then
  cd ${AED2DIR}
  make || exit 1
fi

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

if [ "$WITH_PLOTS" = "true" ] ; then
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

if [ "$OSTYPE" = "Linux" ] ; then
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
if [ "$OSTYPE" = "Darwin" ] ; then
  if [ ! -d ${CURDIR}/macos ] ; then
     mkdir ${CURDIR}/macos
  fi
  cd ${CURDIR}/glm-aed/macos
  /bin/bash macpkg.sh
  mv ${CURDIR}/glm-aed/macos/glm_*.zip ${CURDIR}/macos/
fi

exit 0
