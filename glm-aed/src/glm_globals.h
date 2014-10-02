/******************************************************************************
 *                                                                            *
 * glm_globals.h                                                              *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Earth & Environment                                          *
 *     The University of Western Australia                                    *
 *                                                                            *
 *     http://aed.see.uwa.edu.au/                                             *
 *                                                                            *
 * Copyright 2013, 2014 -  The University of Western Australia                *
 *                                                                            *
 *  This file is part of GLM (General Lake Model)                             *
 *                                                                            *
 *  GLM is free software: you can redistribute it and/or modify               *
 *  it under the terms of the GNU General Public License as published by      *
 *  the Free Software Foundation, either version 3 of the License, or         *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  GLM is distributed in the hope that it will be useful,                    *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *
 *                                                                            *
 ******************************************************************************/
#ifndef _GLM_GLOBALS_H_
#define _GLM_GLOBALS_H_

#if _FORTRAN_VERSION_

INTERFACE

     SUBROUTINE set_c_wqvars_ptr(iwqvars) BIND(C, name="set_c_wqvars_ptr")
        USE ISO_C_BINDING
        REALTYPE,INTENT(in) :: iwqvars(*)
     END SUBROUTINE set_c_wqvars_ptr

# if DEBUG
     SUBROUTINE debug_print_lake() BIND(C, name="debug_print_lake")
     END SUBROUTINE debug_print_lake

     SUBROUTINE debug_initialisation(which) BIND(C, name="debug_initialisation_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: which
     END SUBROUTINE debug_initialisation
# endif


END INTERFACE

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#include "glm.h"
#include "glm_types.h"

/* from glm_ncdf */
extern int ncid;

/* from glm_lnum */
extern int lkn;

/* from glm_surf.F90 */
extern int ice;

/*----------------------------------------------------------------------------*/
extern int MaxLayers;   //* Maximum number of layers in this sim
extern int NumLayers;   //* current number of layers
extern LakeDataType *Lake;


extern REALTYPE Latitude, Longitude;

extern REALTYPE DMin;    //* minimum layer thickness
extern REALTYPE DMax;    //* maximum layer thickness
extern REALTYPE VMin;    //* minimum layer volume
extern REALTYPE VMax;    //* maximum layer volume

extern REALTYPE Kw;      //* background light attenuation (m**-1)

extern int wq_calc;      //* are we doing water quality calcs

extern int Num_WQ_Vars;      //* number of water quality variables
extern REALTYPE *WQ_Vars;    //* water quality array, nlayers, nvars

extern REALTYPE *dz;   //* layer thickness

/*----------------------------------------------------------------------------*/

extern REALTYPE CrestLevel; //* crest elevation of reservoir
extern REALTYPE LenAtCrest; //* length of reservoir at crest
extern REALTYPE WidAtCrest; //* width of reservoir at crest
extern REALTYPE VolAtCrest; //* volume at crest level
extern REALTYPE Base;       //* bottom elevation of reservoir
extern REALTYPE Benthic_Light_pcArea;
extern REALTYPE Benthic_Imin;

/*----------------------------------------------------------------------------*/

extern int NumInf;                 //* number of inflows
extern InflowDataType Inflows[];    //* Array of Inflows

extern int NumOut;                 //* Number of outflows
extern OutflowDataType Outflows[];  //* Array of Outflows

/*----------------------------------------------------------------------------*/

extern int NumDif;
extern REALTYPE mol_diffusivity[];
extern REALTYPE coef_wind_drag;   //* = 0.0013;
extern REALTYPE CE;   //* = 0.0013;
extern REALTYPE CH;   //* = 0.0013;

/*----------------------------------------------------------------------------*/

extern MetDataType MetData;      //* Meteorological data
extern SurfaceDataType SurfData; //* Surface Data
extern int subdaily; //* = FALSE;

/*----------------------------------------------------------------------------*/

extern int Nmorph;             //* Number of data points

extern REALTYPE *MphLevelArea;    //* area of each layer determined by linear interpolation
extern REALTYPE *dMphLevelArea;   //* gradients of area between 0.1m layers
extern REALTYPE *dMphLevelVol;    //* gradients of volume between 0.1m layers
extern REALTYPE *dMphLevelVolda;  //*
extern REALTYPE *MphLevelVol;     //* volume of each layer determined by linear interpolation
extern REALTYPE *MphLevelVoldash; //*

/*----------------------------------------------------------------------------*/
extern REALTYPE vel;
extern REALTYPE WaveNumSquared;
extern REALTYPE XMoment1;

/*----------------------------------------------------------------------------*/

extern REALTYPE einff;   //* change in potential energy (see do_inflows)
extern REALTYPE coef_mix_KH;    //* Kelvin-Helmholtz billows
extern REALTYPE coef_mix_conv;  //* convective overturn
extern REALTYPE coef_mix_shear; //* shear efficiency
extern REALTYPE coef_mix_turb;  //* unsteady effects
extern REALTYPE coef_wind_stir; //* wind stirring
extern REALTYPE coef_mix_hyp;   //# efficiency of hypolimnetic mixing

extern CLOGICAL mobility_off;
extern CLOGICAL non_avg;
extern int      deep_mixing;    //# = 0 => off > 0 => on

/*----------------------------------------------------------------------------*/

extern REALTYPE db;  //* = 0.;

/*----------------------------------------------------------------------------*/

extern int nDays;          //* number of days to simulate
extern REALTYPE timestep;
extern int noSecs;

/*----------------------------------------------------------------------------*
 *  These for debugging                                                       *
 *----------------------------------------------------------------------------*/
extern CLOGICAL no_evap;   //* turn off evaporation

/******************************************************************************/
void allocate_storage(void);
void set_c_wqvars_ptr(REALTYPE *iwqvars);

void debug_print_lake(void);
void debug_initialisation(int which);
void debug_initialisation_(int *which);

#define _WQ_Vars(i,j) WQ_Vars[_IDX_2d(Num_WQ_Vars,MaxLayers,i,j)]
#endif

/*============================================================================*/
#endif
