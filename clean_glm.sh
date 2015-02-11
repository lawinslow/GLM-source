#!/bin/bash

export CURDIR=`pwd`
export FABMDIR=${CURDIR}/fabm-git
export PLOTDIR=${CURDIR}/libplot
export UTILDIR=${CURDIR}/libutil
export AED2DIR=${CURDIR}/libaed2

cd ${AED2DIR}
make distclean

cd ${UTILDIR}
make distclean

cd ${PLOTDIR}
make distclean

if [ -d ${FABMDIR}/build ] ; then
  /bin/rm -rf ${FABMDIR}/build
fi

cd ${CURDIR}/glm-aed/src
make clean

if [ `uname -s` == "Linux" ] ; then
  cd ${CURDIR}/glm-aed
  fakeroot make -f debian/rules clean
fi

clean_outputs() {
   cd ${CURDIR}/$1
   LIST1=`find . -name WQ\*.csv`
   LIST2=`find . -name output.nc`
   LIST3=`find . -name lake.csv`
   LIST4=`find . -name outlet_\?\?.csv`
   LIST5=`find . -name overflow.csv`
   LIST6=`find . -name stress_dbg.csv`
   LIST=`echo $LIST1 $LIST2 $LIST3 $LIST4 $LIST5 $LIST6`
   #echo $LIST1
   #echo $LIST2
   #echo $LIST3
   #echo $LIST
   if [ "$LIST" != "" ] ; then
     /bin/rm $LIST
   fi

}

clean_outputs "glm-tests"
clean_outputs "glm-egs"

