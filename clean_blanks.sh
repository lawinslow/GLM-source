#!/bin/bash

# Strip trailing blanks off source files

CWD=`pwd`
OSTYPE=`uname -s`

for k in fabm-git/src/models/aed fabm-git/src/drivers/glm glm-aed libplot libutil libaed2 ; do
   if [ -d $k ] ; then
      cd $k
      for j in 'F90' 'c' 'h' 'sln' 'vfproj' 'vcproj' 'vcxproj' 'icproj' 'vcxproj.filters' ; do
         echo "cleaning trailing spaces in $k/\*.$j"
         for i in `find . -name \*.$j` `find . -name Makefile`;  do
            if [ "$OSTYPE" != "Darwin" ] ; then
               sed 's/[ \t\r]*$//' $i > tmpx
            else
               #  the above works for gnu-sed, but we need to replace \t with
               #  a tab character for Mac
               sed 's/[ 	]*$//' $i > tmpx
            fi
            \diff $i tmpx > /dev/null 2>&1
            if [ $? != 0 ] ; then
               echo changed $i
               /bin/rm $i
               /bin/mv tmpx $i
            else
               /bin/rm tmpx
            fi
         done
      done
      cd $CWD
   else
      echo "No directory called" $k
   fi
done

tr -d '\r' < fabm-git/compilers/vs2008/fabm-glm.vfproj > .tmpx
\diff fabm-git/compilers/vs2008/fabm-glm.vfproj .tmpx > /dev/null 2>&1
if [ $? != 0 ] ; then
    echo changed fabm-git/compilers/vs2008/fabm-glm.vfproj
    /bin/rm fabm-git/compilers/vs2008/fabm-glm.vfproj
    /bin/mv .tmpx fabm-git/compilers/vs2008/fabm-glm.vfproj
else
    /bin/rm .tmpx
fi

tr -d '\r' < fabm-git/compilers/vs2010/fabm-glm.vfproj > .tmpx
\diff fabm-git/compilers/vs2010/fabm-glm.vfproj .tmpx > /dev/null 2>&1
if [ $? != 0 ] ; then
    echo changed fabm-git/compilers/vs2010/fabm-glm.vfproj
    /bin/rm fabm-git/compilers/vs2010/fabm-glm.vfproj
    /bin/mv .tmpx fabm-git/compilers/vs2010/fabm-glm.vfproj
else
    /bin/rm .tmpx
fi

