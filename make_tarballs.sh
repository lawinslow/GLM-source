#!/bin/sh

CWD=`pwd`

./clean.sh # start by cleaning the respositories

PLOTVRS=`grep LIB_PLOT_VERSION libplot/libplot.h | cut -f2 -d\"`
UTILVRS=`grep LIB_UTIL_VERSION libutil/include/libutil.h | cut -f2 -d\"`
GLM_VRS=`grep GLM_VERSION glm-aed/src/glm.h | cut -f2 -d\"`

if [ -d tttt ] ; then
   /bin/rm -rf tttt
fi

do_tarball () {
   BASEN=$1
   VERSN=$2
   echo $1 $2

   if [ -f ${BASEN}-${VERSN}.tar.gz ] ; then
      echo already have a tarball for ${BASEN}-${VERSN}.tar.gz
   else
      if [ -d ${BASEN} ] ; then
         /bin/mv ${BASEN} tttt/${BASEN}-${VERSN}
         cd tttt/${BASEN}-${VERSN}
         /bin/mv .git* ..
         cd ..
         tar czf ../${BASEN}-${VERSN}.tar.gz ${BASEN}-${VERSN}
         /bin/mv .git* ${BASEN}-${VERSN}
         /bin/mv ${BASEN}-${VERSN} ../${BASEN}
         cd ..
      fi
   fi
}

mkdir tttt

do_tarball libplot ${PLOTVRS}
do_tarball libutil ${UTILVRS}
do_tarball glm-aed ${GLM_VRS}

rmdir tttt

exit 0
