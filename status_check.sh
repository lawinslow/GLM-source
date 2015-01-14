#!/bin/sh

echo '*** checking status for .'
git status

CWD=`pwd`
for src in libplot libutil libaed2 glm-aed glm-egs glm-tests ; do
   echo '*** checking status for' $src
   cd $src
   git status
   cd $CWD
done

exit 0
