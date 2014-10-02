#!/bin/sh

for src in glm-aed glm-manual glm-egs libutil libplot ; do
   echo pushing $src
   cd $src
   git add .
   git commit -a -m 'update changes'
   git push origin master
   cd ..
done
