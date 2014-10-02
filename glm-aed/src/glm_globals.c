/******************************************************************************
 *                                                                            *
 * glm_globals.c                                                              *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "glm.h"
#include "glm_types.h"
#include "glm_globals.h"
#include "aed_csv.h"


int MaxLayers;   //# Maximum number of layers in this sim
int NumLayers;   //# current number of layers
LakeDataType *Lake = NULL;


REALTYPE Latitude, Longitude;

REALTYPE DMin;    //# minimum layer thickness
REALTYPE DMax;    //# maximum layer thickness
REALTYPE VMin;    //# minimum layer volume
REALTYPE VMax;    //# maximum layer volume

int wq_calc = FALSE;

REALTYPE Kw;             //# background light attenuation (m**-1)

int Num_WQ_Vars;         //# number of water quality variables

REALTYPE *dz = NULL;     //# layer thickness

//------------------------------------------------------------------------------

REALTYPE CrestLevel; //# crest elevation of reservoir
REALTYPE LenAtCrest; //# length of reservoir at crest
REALTYPE WidAtCrest; //# width of reservoir at crest
REALTYPE VolAtCrest; //# volume at crest level
REALTYPE Base;       //# bottom elevation of reservoir
REALTYPE Benthic_Light_pcArea;
REALTYPE Benthic_Imin = 0.;

//------------------------------------------------------------------------------

int NumInf = 0;                    //# number of inflows
InflowDataType Inflows[MaxInf];    //# Array of Inflows

int NumOut = 0;                    //# Number of outflows
OutflowDataType Outflows[MaxOut];  //# Array of Outflows

//------------------------------------------------------------------------------

int NumDif;
REALTYPE mol_diffusivity[MaxDif];
REALTYPE coef_wind_drag = 0.0013;
REALTYPE CE = 0.0013;
REALTYPE CH = 0.0013;

//------------------------------------------------------------------------------

MetDataType MetData;         //# Meteorological data
SurfaceDataType SurfData;    //# Surface Data

int subdaily = FALSE;

//------------------------------------------------------------------------------

int Nmorph = 0;             //# Number of data points in internal morphometry vector

REALTYPE *MphLevelArea    = NULL; //# area at each internal levels determined by linear interpolation
REALTYPE *dMphLevelArea   = NULL; //# gradients of area between 0.1m levels
REALTYPE *dMphLevelVol    = NULL; //# gradients of volume between 0.1m levels
REALTYPE *dMphLevelVolda  = NULL; //#
REALTYPE *MphLevelVol     = NULL; //# volume at each level determined by linear interpolation
REALTYPE *MphLevelVoldash = NULL; //#

//------------------------------------------------------------------------------
REALTYPE vel;
REALTYPE WaveNumSquared;
REALTYPE XMoment1;

//------------------------------------------------------------------------------

REALTYPE einff;   //# change in potential energy (see do_inflows)
REALTYPE coef_mix_KH = 0.3;     //# Kelvin-Helmholtz billowing effects
REALTYPE coef_mix_conv = 0.125; //# convective overturn
REALTYPE coef_mix_shear = 0.2;  //# shear efficiency
REALTYPE coef_mix_turb = 0.51;  //# unsteady effects
REALTYPE coef_wind_stir = 0.23; //# wind stirring
REALTYPE coef_mix_hyp = 0.5;    //# efficiency of hypolimnetic mixing

CLOGICAL non_avg = FALSE;
int deep_mixing = 1;

//------------------------------------------------------------------------------

REALTYPE db = 0.;

//------------------------------------------------------------------------------

int nDays;          //# number of days to simulate
REALTYPE timestep;
int noSecs;

//------------------------------------------------------------------------------

REALTYPE *WQ_Vars = NULL;  //# water quality array, nlayers, nvars

//------------------------------------------------------------------------------
//  These for debugging
//------------------------------------------------------------------------------
CLOGICAL no_evap = FALSE;

void set_c_wqvars_ptr(REALTYPE *iwqv) { WQ_Vars = iwqv; }

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
void allocate_storage()
{
    MphLevelArea =    malloc(sizeof(REALTYPE) * Nmorph);
    dMphLevelArea =   malloc(sizeof(REALTYPE) * Nmorph);
    dMphLevelVol =    malloc(sizeof(REALTYPE) * Nmorph);
    dMphLevelVolda =  malloc(sizeof(REALTYPE) * Nmorph);
    MphLevelVol =     malloc(sizeof(REALTYPE) * Nmorph);
    MphLevelVoldash = malloc(sizeof(REALTYPE) * Nmorph);

    memset(MphLevelArea,    0, sizeof(REALTYPE) * Nmorph);
    memset(dMphLevelArea,   0, sizeof(REALTYPE) * Nmorph);
    memset(dMphLevelVol,    0, sizeof(REALTYPE) * Nmorph);
    memset(dMphLevelVolda,  0, sizeof(REALTYPE) * Nmorph);
    memset(MphLevelVol,     0, sizeof(REALTYPE) * Nmorph);
    memset(MphLevelVoldash, 0, sizeof(REALTYPE) * Nmorph);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


#if DEBUG
/******************************************************************************/
void _debug_print_lake(FILE *of) {
    int i;

/*
    fprintf(of, "----------DEPTH----------------TEMP-----------------SALT-----------------DENS-----------------LVol------\n");
    for (i = 0; i < NumLayers; i++)
        fprintf(of, "%3d %16.11f %20.11f %20.11f %20.11f %20.11f %16.10f\n",
                    i, Lake[i].Height, Lake[i].Temp, Lake[i].Salinity, Lake[i].Density, Lake[i].LayerVol,Lake[i].Vol1);
*/
    fprintf(of, "MaxLayers %d NumLayers %d\n", MaxLayers, NumLayers);
    fprintf(of, "----------DEPTH----------------TEMP-----------------SALT-----------------DENS-----------------LVol--------------LArea----\n");
    for (i = 0; i < NumLayers; i++)
        fprintf(of, "%3d %16.11f %20.11f %20.11f %20.11f %20.11f %16.10f\n",
                    i, Lake[i].Height, Lake[i].Temp, Lake[i].Salinity, Lake[i].Density, Lake[i].LayerVol, Lake[i].LayerArea);
    fprintf(of, "-------------------------------------------------------------------------------------------------------------------------\n\n");
}
void debug_print_lake() { _debug_print_lake(stderr); }

/******************************************************************************/
void debug_initialisation(int which) {
    int i ;
    FILE *of = stderr;

    if (which) fprintf(of, "FORTRAN\n"); else fprintf(of, "C-----\n");

    _debug_print_lake(of);

    fprintf(of, "crest = %20.15f base = %20.15f VolAtCrest = %20.15f\n", CrestLevel, Base, VolAtCrest);

    fprintf(of, " Nmorph = %d\n", Nmorph);
    fprintf(of, "IDX----------StoLA----------------MphLevelVol--------------------dMphLevelVol------------------dMphLevelArea--------------\n");
    for (i = 0; i < Nmorph; i++)
        fprintf(of, "%3d, %20.15f %20.15f %20.15f %20.15f\n", i, MphLevelArea[i], MphLevelVol[i], dMphLevelVol[i], dMphLevelArea[i]);
    fputc('\n', of); fputc('\n', of);

    fprintf(of, "DMin %20.15f DMax %20.15f\nVMin %20.15f VMax %20.15f\n", DMin, DMax, VMin, VMax);
    fprintf(of, "EinFF %20.15f coef_mix_KH %20.15f coef_mix_conv %20.15f\nCS %f coef_mix_turb %f coef_wind_stir %f coef_mix_hyp %f\n",
                                       einff, coef_mix_KH, coef_mix_conv, coef_mix_shear, coef_mix_turb, coef_wind_stir, coef_mix_hyp);

    fprintf(of, "NumInf %d NumOut %d NumDif %d\n", NumInf, NumOut, NumDif);

    if (which) fprintf(of, "FORTRAN\n"); else fprintf(of, "C-----\n");

//exit(0);
}
void debug_initialisation_(int *which) { debug_initialisation(*which); }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif
