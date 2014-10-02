#!/bin/sh

GITHOST="192.168.1.3"

fetch_it () {
   src=$1
   if [ "$2" = "" ] ; then
      dst=$src
   else
      dst=$2
   fi

   if [ -d $dst ] ; then
      cd $dst
      git pull origin master
      cd ..
   else
      git  clone git@${GITHOST}:$src $dst
   fi
}

fetch_it glm glm-aed
for src in libplot libutil glm-egs glm-manual ; do
   fetch_it $src
done
fetch_it fabm-aed fabm-git

exit 0
