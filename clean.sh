#!/bin/bash

./clean_glm.sh

# NB Some versions of sed may do horrid things to your source
#    if you use this script.
./clean_blanks.sh
