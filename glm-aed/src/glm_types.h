/******************************************************************************
 *                                                                            *
 * glm_types.h                                                                *
 *                                                                            *
 * Type declaration                                                           *
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
#ifndef _GLM_TYPES_H_
#define _GLM_TYPES_H_

#include "glm.h"

#define MaxPar        37
#define MaxOut        20     /* Maximum number of outflows */
#define MaxInf        20     /* Maximum number of inflows */
#define MaxVars       60     /* Maximum number of variables */
#define MaxDif   (MaxVars+2) /* Maximum number of diffusing substances */

typedef int  LOGICAL;
typedef char varname[40];
typedef char filname[80];

/******************************************************************************
 *                                                                            *
 * Indexing macros                                                            *
 *                                                                            *
 * for an array declared in fortran as C(MaxI,MaxJ) and accessed as C(i,j)    *
 * we use this in C as C[_IDX_2d(MaxI,MaxJ,i,j)                               *
 *                                                                            *
 * a bit messy, but until we are all C we must live with the cards we are...  *
 *                                                                            *
 ******************************************************************************/
#define _IDX_2d(di,dj,i,j) (((di) * (j)) + (i))

#define _IDX_3d(di,dj,dk,   i,j,k)                         ((dk * dj * i) + (dk * j) + k)
#define _IDX_4d(di,dj,dk,dl,i,j,k,l)  ((dl * dk * dj * i) + (dl * dk * j) + (dl * k) + l)

/******************************************************************************
 * Macros for max and min - the first are the "standard" macros but suffer    *
 * from side effects - the latter are safer but have a greater overhead       *
 ******************************************************************************/
#ifdef _WIN32
  #define MAX(a,b) (((a) > (b)) ? (a):(b))
  #define MIN(a,b) (((a) < (b)) ? (a):(b))
#else
  #define MAX(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })
  #define MIN(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })
#endif

/******************************************************************************/

/*============================================================================*/
//TYPE DECLARATIONS

   /*===========================================================*/
   typedef struct StringT {
       int  Len;
       char S[40];
   } StringT;

   /*===========================================================*/
   // Structured type for inflow vars
   // An inflow will be an allocated array of MaxInf of these
   typedef struct InflowDataType {
       AED_REAL Alpha;           // half angle of stream
       AED_REAL DragCoeff;       // streambed drag coefficient
       AED_REAL Phi;             // streambed slope
       AED_REAL FlowRate;        // inflow flow rate
       AED_REAL Factor;          // scaling factor for inflow
       AED_REAL TemInf;          // inflow temperature
       AED_REAL SalInf;          // inflow salinity
       AED_REAL Dlwst;
       AED_REAL HFlow;
       AED_REAL TotIn;
       AED_REAL DIIns[MaxPar];   // inflow density
       AED_REAL DDown[MaxPar];   // downflow insertion depth
       AED_REAL QIns[MaxPar];    // inflow volume
       AED_REAL QDown[MaxPar];   // downflow volume
       AED_REAL TIns[MaxPar];    // inflow temperature
       AED_REAL TDown[MaxPar];   // downflow temperature
       AED_REAL SIns[MaxPar];    // inflow salinity
       AED_REAL SDown[MaxPar];   // downflow salinity
       AED_REAL DOld[MaxPar];

       AED_REAL WQIns[MaxPar][MaxVars];  // inflow water quality
       AED_REAL WQDown[MaxPar][MaxVars]; // downflow water quality
       AED_REAL WQInf[MaxVars];

       int  iCnt;
       int  NoIns;
       int  InPar[MaxPar];

       LOGICAL  SubmFlag;        // Is this a submerged inflow
   } InflowDataType;

   /*===========================================================*/
   // Structured type for outflow vars
   // An outflow will be an allocated array of MaxOut of these
   typedef struct OutflowDataType {
       int Type;                 // outflow type
       AED_REAL Hcrit;           // outlet height when crit O2
       int O2idx;                // O2 parameter idx in AED2/FABM
       char O2name;              // O2 parameter name in AED2/FABM
       AED_REAL TARGETtemp;      // Isotherm for withdrawal switch 4
       AED_REAL OLev;            // distance below surface level
       AED_REAL OLen;            // basin length at the outlet
       AED_REAL OWid;            // basin width at the outlet
       AED_REAL Draw;            // outflow volumes
       AED_REAL Factor;          // scaling factor for outflow
       LOGICAL  FloatOff;        // Is this a floating offtake
   } OutflowDataType;

   /*===========================================================*/
   // Structured type for key global lake environmental vars
   // A Lake will be an allocated array of MaxLayers of these
   typedef struct LakeDataType {
       AED_REAL Density;         // Density kg/m3
       AED_REAL Temp;            // temperature
       AED_REAL Salinity;        // salinity
       AED_REAL Height;          // 1-D depth array
       AED_REAL MeanHeight;      // Mean depth of a layer
       AED_REAL LayerVol;        // volume of layer
       AED_REAL LayerArea;       // area of layer

       AED_REAL Light;           // solar radiation over water layer depths
       AED_REAL ExtcCoefSW;      // light extinction coefficient

       AED_REAL Vol1;
       AED_REAL Epsilon;

       AED_REAL Umean;           // Mean velocity
       AED_REAL Uorb;            // Maximum orbital velocity
       AED_REAL Ucur;            // Current velocity
       AED_REAL LayerStress;    // Layer Stress
   } LakeDataType;

   /*===========================================================*/
   // Structured type for Met vars
   typedef struct MetDataType {
       AED_REAL Rain;            // raindfall
       AED_REAL RelHum;          // relative humidty
       AED_REAL SatVapDef;       // vapour pressure
       AED_REAL LongWave;        // longwave radiation
       AED_REAL ShortWave;       // shortwave radiation
       AED_REAL AirTemp;         // temperature
       AED_REAL WindSpeed;       // windspeed
       AED_REAL Snow;            // snowdfall
       AED_REAL RainConcPO4;     // Concentration of PO4 in rain
       AED_REAL RainConcTp;      // Concentration of TP in rain
       AED_REAL RainConcNO3;     // Concentration of NO3 in rain
       AED_REAL RainConcNH4;     // Concentration of NH4 in rain
       AED_REAL RainConcTn;      // Concentration of TN in rain
       AED_REAL RainConcSi;      // Concentration of SI in rain
   } MetDataType;

   /*===========================================================*/
   // Structured type for Surface Data vars
   typedef struct SurfaceDataType {
       AED_REAL Evap;            // Evaporation
       AED_REAL HeightBlackIce;  // height of ice layer
       AED_REAL HeightWhiteIce;  // height of white ice layer
       AED_REAL HeightSnow;      // height of snow layer
       AED_REAL dHt;             // change in thickness of either the snow or ice layer
       AED_REAL RhoSnow;          // Density of snow layer in kg/m^3
       AED_REAL dailyEvap;       // Daily Evaporation (ML/day)
       AED_REAL dailyRain;       // Daily Rain (ML/day)
       AED_REAL dailySnow;       // Daily Snow (ML/day)
       AED_REAL dailyQsw;        // Daily Heat Flux (J/day)
       AED_REAL dailyQe;         // Daily Latent Heat(J/day)
       AED_REAL dailyQh;         // Daily Sensible Heat (J/day)
       AED_REAL dailyQlw;        // Daily Long Wave Radiation (J/day)
       AED_REAL dailyInflow;     // Total Daily Inflow (ML/day)
       AED_REAL dailyOutflow;    // Total Daily Outflow (ML/day)
       AED_REAL dailyOverflow;   // Total Daily Overflow (ML/day)
       AED_REAL albedo;          // Daily surface albedo
   } SurfaceDataType;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif
