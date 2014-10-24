#!/bin/bash
#
# DYLD_LIBRARY_PATH should provide the paths in a : separated list
# to the libraries in this example the paths are
#  /opt/intel/lib
# and
#  /Users/hydro/devel/lib
#
# the glm executable is assumed to be in the directory where this
# script is run from (a symlink will do)
# ln -s ~/Work/Models/GLM/MACexe/MacGLM/glm

export DYLD_LIBRARY_PATH="/opt/intel/lib:/Users/hydro/devel/lib"
./glm $*
exit 0
