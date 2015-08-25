#!/bin/bash
#
# This script is used to bundle the glm binaries and uncommon library dependancies into an app.
#  The bundle rpaths are modified to find the libraries in the bundle.

/bin/rm -rf glm.app

VERSION=`grep GLM_VERSION ../src/glm.h | cut -f2 -d\"`

mkdir glm.app
mkdir glm.app/Contents
mkdir glm.app/Contents/Resources
mkdir glm.app/Contents/Resources/English.lproj
mkdir glm.app/Contents/MacOS
cp Info.plist glm.app/Contents
sed -i '' -e "s/VERSION/${VERSION}/" glm.app/Contents/Info.plist

cp PkgInfo    glm.app/Contents
cp ../src/glm glm.app/Contents/MacOS
cp glm.icns   glm.app/Contents/Resources
cp glm_files.icns  glm.app/Contents/Resources
cp InfoPlist.strings glm.app/Contents/Resources/English.lproj

# find_libs path bin
find_libs () {
   L2=`otool -L glm.app/Contents/MacOS/$2 | grep \/opt\/$1 | cut -d\  -f1 | sed "s^/opt/$1/lib/^^"`
   LIST=""
   while [ "$L2" != "$LIST" ] ; do
      LIST=$L2
      for i in $LIST ; do
         if [ ! -f glm.app/Contents/MacOS/$i ] ; then
            cp /opt/local/lib/$i glm.app/Contents/MacOS
            if [ $? != 0 ] ; then
               echo " ####### Failed to copy $i" 1>&2
            else
               chmod +w glm.app/Contents/MacOS/$i
            fi
         fi
         NLST=`otool -L /opt/$1/lib/$i | grep -v $i | grep \/opt\/$1 | cut -d\  -f1 | sed "s^/opt/$1/lib/^^"`
         for j in $NLST ; do
            echo $L2 | grep $j > /dev/null 2>&1
            if [ $? != 0 ] ; then
               L2+=" $j"
            fi
         done
      done
   done
   echo $LIST
}


LIBS1=`find_libs local glm`
#LIBS2=`find_libs intel glm`

if [ $FORTRAN_COMPILER = IFORT ] ; then
  PATH2=/opt/intel/lib
  PATH3=
  LIBS2="libifcore.dylib libsvml.dylib libimf.dylib libintlc.dylib"
else
  PATH2=/usr/local/lib
  PATH3=/usr/local/lib/
  LIBS2="libgfortran.3.dylib"
fi

echo "LIBS1 = $LIBS1"
echo "LIBS2 = $LIBS2"

# These general libraries
for i in $LIBS1 ; do
   echo "*** Configuring : $i ***"
   cp /opt/local/lib/$i glm.app/Contents/MacOS
   install_name_tool -id $i glm.app/Contents/MacOS/$i
   install_name_tool -change /opt/local/lib/$i '@executable_path/'$i glm.app/Contents/MacOS/glm
   # now update the paths for libraries that this library references
#  NLST=`otool -L /opt/local/lib/$i | grep -v $i | grep \/opt\/local | cut -d\  -f1 | sed "s^/opt/local/lib/^^"`
#  for j in $NLST ; do
#    install_name_tool -change /opt/local/lib/$j '@executable_path/'$j glm.app/Contents/MacOS/$i
#  done
done

# These fortran libraries
for i in $LIBS2 ; do
   cp $PATH2/$i glm.app/Contents/MacOS
# These are redundant since it seems intel fortran dylibs exclude the path from their names
   install_name_tool -id $i glm.app/Contents/MacOS/$i
   install_name_tool -change ${PATH3}$i '@executable_path/'$i glm.app/Contents/MacOS/glm
done

# now update these paths in the libraries as well
for j in $LIBS1 $LIBS2 ; do
   for i in $LIBS1 ; do
      install_name_tool -change /opt/local/lib/$i '@executable_path/'$i glm.app/Contents/MacOS/$j
   done

   for i in $LIBS2 ; do
      install_name_tool -change ${PATH3}$i '@executable_path/'$i glm.app/Contents/MacOS/$j
   done
done

# ln -s glm.app/Contents/MacOS/glm glm
zip -r glm_$VERSION.zip glm.app # glm

exit 0
