/******************************************************************************
 *                                                                            *
 * glm_types.h                                                                *
 *                                                                            *
 * Type declaration ported to C                                               *
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
#define MaxDif      (MaxVars+2) /* Maximum number of diffusing substances */

#define Visc      0.00000114

#define zero       0.0
#define one        1.0
#define two        2.0
#define three      3.0
#define four       4.0
#define five       5.0
#define six        6.0
#define seven      7.0
#define eight      8.0
#define nine       9.0
#define ten       10.0
#define twenty    20.0
#define sixty     60.0
#define thsnd   1000.0

#define tenM8   1.0E-8
#define tenM12  1.0E-12

#define g           9.81
#define Pi          3.14159265358979323846
#define PiDeg       180.0

#define SecsPerDay  86400.0

#define missing     MISVAL

typedef int LOGICAL;

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
      REALTYPE Alpha;           // half angle of stream
      REALTYPE DragCoeff;       // streambed drag coefficient
      REALTYPE Phi;             // streambed slope
      REALTYPE FlowRate;        // inflow flow rate
      REALTYPE Factor;          // scaling factor for inflow
      REALTYPE TemInf;          // inflow temperature
      REALTYPE SalInf;          // inflow salinity
      REALTYPE Dlwst;
      REALTYPE HFlow;
      REALTYPE TotIn;
      REALTYPE DIIns[MaxPar];   // inflow density
      REALTYPE DDown[MaxPar];   // downflow density
      REALTYPE QIns[MaxPar];    // inflow volume
      REALTYPE QDown[MaxPar];   // downflow volume
      REALTYPE TIns[MaxPar];    // inflow temperature
      REALTYPE TDown[MaxPar];   // downflow temperature
      REALTYPE SIns[MaxPar];    // inflow salinity
      REALTYPE SDown[MaxPar];   // downflow salinity
      REALTYPE DOld[MaxPar];

      REALTYPE WQIns[MaxPar][MaxVars];  // inflow water quality
      REALTYPE WQDown[MaxPar][MaxVars]; // downflow water quality
      REALTYPE WQInf[MaxVars];

      int  iCnt;
      int  NoIns;
      int  InPar[MaxPar];
   } InflowDataType;

   /*===========================================================*/
   // Structured type for outflow vars
   // An outflow will be an allocated array of MaxOut of these
   typedef struct OutflowDataType {
      REALTYPE OLev;            // distance below surface level
      REALTYPE OLen;            // basin length at the outlet
      REALTYPE OWid;            // basin width at the outlet
      REALTYPE Draw;            // outflow volumes
      REALTYPE Factor;          // scaling factor for outflow
      LOGICAL  FloatOff;        // Is this a floating offtake
   } OutflowDataType;

   /*===========================================================*/
   // Structured type for key global lake environmental vars
   // A Lake will be an allocated array of MaxLayers of these
   typedef struct LakeDataType {
      REALTYPE Density;         // density
      REALTYPE Temp;            // temperature
      REALTYPE Salinity;        // salinity
      REALTYPE Height;          // 1-D depth array
      REALTYPE MeanHeight;      // Mean depth of a layer
      REALTYPE LayerVol;        // volume of layer
      REALTYPE LayerArea;       // area of layer

      REALTYPE Light;           // solar radiation over water layer depths
      REALTYPE ExtcCoefSW;      // light extinction coefficient

      REALTYPE Vol1;
      REALTYPE Epsilon;
   } LakeDataType;

   /*===========================================================*/
   // Structured type for Met vars
   typedef struct MetDataType {
      REALTYPE Rain;            // raindfall
      REALTYPE RelHum;          // relative humidty
      REALTYPE SatVapDef;       // vapour pressure
      REALTYPE LongWave;        // longwave radiation
      REALTYPE ShortWave;       // shortwave radiation
      REALTYPE AirTemp;         // temperature
      REALTYPE WindSpeed;       // windspeed
      REALTYPE Snow;            // snowdfall
      REALTYPE RainConcPO4;     // Concentration of PO4 in rain
      REALTYPE RainConcTp;      // Concentration of TP in rain
      REALTYPE RainConcNO3;     // Concentration of NO3 in rain
      REALTYPE RainConcNH4;     // Concentration of NH4 in rain
      REALTYPE RainConcTn;      // Concentration of TN in rain
      REALTYPE RainConcSi;      // Concentration of SI in rain
   } MetDataType;

   /*===========================================================*/
   // Structured type for Surface Data vars
   typedef struct SurfaceDataType {
      REALTYPE Evap;            // Evaporation
      REALTYPE HeightBlackIce;  // height of ice layer
      REALTYPE HeightWhiteIce;  // height of white ice layer
      REALTYPE HeightSnow;      // height of snow layer
      REALTYPE dHt;             // change in thickness of either the snow or ice layer
      REALTYPE dailyEvap;       // Daily Evaporation (ML/day)
      REALTYPE dailyRain;       // Daily Rain (ML/day)
      REALTYPE dailyQsw;        // Daily Heat Flux (J/day)
      REALTYPE dailyQe;         // Daily Latent Heat(J/day)
      REALTYPE dailyQh;         // Daily Sensible Heat (J/day)
      REALTYPE dailyQlw;        // Daily Long Wave Radiation (J/day)
      REALTYPE dailyInflow;     // Total Daily Inflow (ML/day)
      REALTYPE dailyOutflow;    // Total Daily Outflow (ML/day)
      REALTYPE dailyOverflow;   // Total Daily Overflow (ML/day)
   } SurfaceDataType;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif
