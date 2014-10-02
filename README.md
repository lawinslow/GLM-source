#GLM full directory structure

##Environment variables
There are several environment variables that need 


First, we need to define the FORTRAN compiler. Currently
only works with Intel Fortran compiler. Match this to your version, 
current newest is v15
````
export FORTRAN_COMPILER=IFORT15
````

FABM must be included to build GLM successfully. To build FABM,
there must be an environment variable set to TRUE
````
export FABM=true
````

##External Libraries

###libnetcdf
On a Mac, installed using Macports
````
sudo port install netcdf
sudo port install netcdf-cxx
sudo port install netcdf-fortran
````


###libgb 
Install on Mac with Brew


Notes from original README
````
# Notes 2013-11-26
#
# Compiling, at this stage, requires Intel Fortran as well as
# the C compilers.  Some effort has been put into making it
# compile with gfortran, however there is a problem in the
# C/Fortran interoperability, specifically in structured arrays.
# There sre some notes in glm_fabm.F90
#
# Some effort was made toward a Macintosh OS-X version, however
# it seems there may be a bug in the OS-X version of Intel's 
# fortran compiler version 13.0.0(088) which means it hangs
# while trying to load the fabm module ( USE fabm )
#
````
