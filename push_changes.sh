#!/bin/sh

for src in glm-aed glm-manual glm-egs ; do
   echo pushing $src
   cd $src
   git add .
   git commit -a -m 'update changes'
   git push origin master
   cd ..
done

for src in libaed2 libutil libplot ; do
   echo pushing $src
   cd $src
   git add .
   git commit -a -m 'update changes'
   git push origin master
   cd ..
done
