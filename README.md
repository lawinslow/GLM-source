GLM-source
==========
Development code for General Lake Model (GLM).

##Model Access

The primary source of GLM-AED for most users is the [University of Western Australia 
GLM website](http://aed.see.uwa.edu.au/research/models/GLM/). The code and versions
available here represent unstable development releases that should be avoided
by non-expert users.


##Dev Environment Setup

Instructions for getting linux dev environment for GLM.
need 64bit ubuntu VM. These are not guaranteed to work.

I needed to get the dev libraries of netcdf and libgd2

	sudo apt-get install libnetcdf-dev libgd2-xpm-dev

build essential covered many support libraries:

	sudo apt-get install build-essential

not sure if all of this was needed too for [R] ncdf4 package: 

	sudo apt-get install libnetcdf6 libgd2-xpm
	sudo apt-get install netcdf-doc
	sudo apt-get install netcdf-bin



Lastly, GLM also requires the intel fortran support libraries which do not have a .deb install package so cannot be checked for. We built and tested against l_fcompxe_2013.2.146_redist.tgz which is available here: http://software.intel.com/en-us/articles/redistributable-libraries-for-the-intel-c-and-fortran-composer-xe-2013-for-linux

We install the libraries in /opt/intel  after which we copy the libifort.conf file from the Support directory  (in dropbox) to /etc/ld.so.conf.d and run :

	sudo ldconfig


To build deb package (at the end of build_glm.sh), I needed

	sudo apt-get install debhelper

to build package (from GLM_Developers root):

	./build_glm.sh

Run a test case

	cd glm-aed/test/BigMuskie/
	../../src/glm




**NOTE, below is no longer relevant. This is handled very well in [GLMr](https://github.com/GLEON/GLMr) and is the preferred way of distribution to novice and intermediate users**

## updates for mac 2014-09-17
today the dependencies for macGLM1.3.2 were modified so that the baked in dylib dependencies could be put in a sim/bin directory instead of root/hydro. 

from the sim/ directory (which contains glm unix exe),
 `otool -L glm` # which lists dependencies
This shows that libnetncdf.7.dylib is default in /Users/hydro/devel/lib/libnetcdf.7.dylib

Then I used install_name_tool on mac to modify the dependency location:  
`install_name_tool -change /Users/hydro/devel/lib/libnetcdf.7.dylib ../bin/devel/lib/libnetcdf.7.dylib glm`  
The bin directory was added as well, and the libraries in hydro were moved here. 

Ran glm from the sim dir with 
`./glm`

```
       ------------------------------------------------
       |  General Lake Model (GLM)   Version 1.3.2    |
       ------------------------------------------------
Reading config from glm.nml
No fabm config
nDays 730 timestep 3600.000000
values for base_elev, crest_elev and V are no longer used
No diffuser data, setting default values
Simulation begins...
Running day  2455195, 100.00% of days complete
------------------------------------------------
              Run Complete
```
Now we need to try this on other computers. 
The goal is to be able to set up glm for macs using a thumb drive (as we can with windows).


When we get a new dump of code on dropbox, we remove the .git subfolders: `rm -R -f */.git`
