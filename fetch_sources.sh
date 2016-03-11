#!/bin/sh

GITHOST="192.168.1.3"

fetch_it () {
   src=$1
   if [ "$2" = "" ] ; then
      dst=$src
   else
      dst=$2
   fi

   echo "fetching $dst"

   if [ -d $dst ] ; then
      cd $dst
      git pull # origin master
      git fetch --all --prune
      git branch -a
      cd ..
   else
      git  clone git@${GITHOST}:$src $dst
   fi

   if [ "$3" != "" ] ; then
      cd $dst
      git checkout $3
      cd ..
   fi
}

fetch_it glm glm-aed
for src in libplot libutil libaed2 ; do
   fetch_it $src
done
for src in glm-egs glm-manual glm-tests ; do
   fetch_it $src $src
done

exit 0
