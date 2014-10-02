/******************************************************************************
 *                                                                            *
 * glm_flow.c                                                                 *
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
#include <math.h>

#include "glm.h"

#include "glm_types.h"
#include "glm_globals.h"

#include "glm_util.h"
#include "glm_mixu.h"
#include "glm_layers.h"
#include "glm_output.h"

#define _WQ_VarsTmp(i,j,k)  WQ_VarsTmp[_IDX_3d(Num_WQ_Vars,NumInf,MaxPar,i,j,k)]

typedef REALTYPE wq_vars_t[MaxVars];
typedef wq_vars_t wq_partic_t[MaxPar];

/******************************************************************************/
static int flowing_depth(REALTYPE *HF, REALTYPE *DL, REALTYPE VOL,
                                  REALTYPE DEPTH, REALTYPE Phi, REALTYPE Alpha);
static REALTYPE extra_volume(REALTYPE d1, REALTYPE d2,
                                 REALTYPE SurfaceHeight, InflowDataType *Inflow);
static void do_an_outflow(REALTYPE HeightOfOutflow, REALTYPE flow, OutflowDataType *outf);
static int new_storage(int *iRiv);
/*----------------------------------------------------------------------------*/

static REALTYPE *WQ_VarsS = NULL;
static wq_partic_t *WQ_VarsTmp = NULL;

static REALTYPE *dv = NULL;

/******************************************************************************
 * Removes the outflow at level Outflow_LayerNum                              *
 ******************************************************************************/
void do_an_outflow(REALTYPE HeightOfOutflow, REALTYPE flow, OutflowDataType *outf)
{
    REALTYPE twopt9 = 2.9,
             TSecDay = 86.4,
             min_flow = 0.0001,
             arfac = 1.0e6;  // Multiplicative factor to get area to m**2

    REALTYPE LenAtOutflow;  //# Reservoir length at height of withdrawal
    REALTYPE AVDEL;         //#
    REALTYPE DASNK;         //# Level above W.L. from which fluid is drawn
    REALTYPE DEL;           //# Initial half withdrawal layer thickness
    REALTYPE DELB;          //# Lower half withdrawal layer thickness
    REALTYPE DELT;          //# Upper half withdrawal layer thickness
    REALTYPE DS;            //# Density difference
    REALTYPE DT;            //#
    REALTYPE DZ;            //# Width over which density gradient is calculated
    REALTYPE FlowRate;      //# Flow rate
    REALTYPE F2,F3;
    REALTYPE Grashof;       //# Grashof number
    REALTYPE QSTAR;
    REALTYPE R;             //# Outflow parameter
    REALTYPE TEL;
    REALTYPE Viscos;
    REALTYPE WidthAtOutflow; //# Reservoir width at height of withdrawal
    REALTYPE XN;            //# Brunt-Vaisala frequency
    REALTYPE XNSQ ;         //# B-V frequency squared
    REALTYPE zBot;          //# Height of bottom of withdrawal layer
    REALTYPE zTop;          //# Height of top of withdrawal layer

    int i,iBot,ILP,ISINK,iTop,Outflow_LayerNum;
    int NX;

    int flag;

/*----------------------------------------------------------------------------*/
//BEGIN
    //# Find number of layer (Outflow_LayerNum) opposite offtake
    for (i = botmLayer; i <= surfLayer; i++)
        if (Lake[i].Height >=  HeightOfOutflow) break;

    //# Return if reservoir surface is below outlet level
    if (i > surfLayer ) return;

    Outflow_LayerNum = i;

    LenAtOutflow = 0.;
    WidthAtOutflow = 0.;
    DEL = 0.0;
    DELT = 0.;
    ILP = FALSE;
    DZ = 0.0;

    for (i = botmLayer; i <= surfLayer; i++) dv[i]=0.0;

    /*---------------------------------------------------------------------*
     * Determine offtake level, length, width and flow                     *
     *                                                                     *
     * withdrawal height selection                                         *
     * switch control by FloatOff                                          *
     * Assume:                                                             *
     *      1) that lake approximates as an ellipse                        *
     *      2) area = pi/4 * Length * Width                                *
     *      3) ratio Length:Width at outflow = crest                       *
     *                                                                     *
     *---------------------------------------------------------------------*/
    if (outf == NULL) {
        LenAtOutflow = LenAtCrest;
        WidthAtOutflow = WidAtCrest;
    } else {
        if (outf->FloatOff) { // floating offtake
            //# Assume offtake level close to surface
            LenAtOutflow = Lake[surfLayer].LayerArea * arfac / WidAtCrest;
            WidthAtOutflow = WidAtCrest;
        } else { //# Fixed Offtake
            LenAtOutflow = sqrt(Lake[Outflow_LayerNum].LayerArea*arfac*four/Pi*(LenAtCrest/WidAtCrest));
            WidthAtOutflow = LenAtOutflow * WidAtCrest/LenAtCrest;
        }
    }

    FlowRate = flow / TSecDay;

    if (flow <= min_flow) return;

    //#  Calculate withdrawal layer thickness
    if (surfLayer == botmLayer)
        dv[0] = flow;
    else {
        ISINK = 1;
        NX = Outflow_LayerNum + 1;
        while (1) {
            if (NX > surfLayer) NX = surfLayer;
            if (NX < botmLayer) NX = botmLayer;
            DS = Lake[Outflow_LayerNum].Density - Lake[NX].Density;

            flag = FALSE;
            if (DS * (ISINK) <= zero)
                flag = TRUE;
            else {
                DZ = Lake[NX].MeanHeight - HeightOfOutflow;
                if ((DZ * (ISINK)) <= zero)
                    flag = TRUE;
                else {
                    XNSQ = DS * g / ((thsnd + Lake[Outflow_LayerNum].Density) * DZ);
                    if (XNSQ <= zero) flag = TRUE;
                }
            }

            if (flag) {
                //# check for zero gradient
                if (ISINK == 1)
                    DEL = Lake[surfLayer].Height - HeightOfOutflow;
                else
                    DEL = HeightOfOutflow;
            } else {
                XN = sqrt(XNSQ);
                Viscos = Lake[Outflow_LayerNum].Epsilon * twenty;
                if (Viscos <= zero) Viscos = Visc;
                Grashof = XNSQ * sqr(LenAtOutflow) * sqr(LenAtOutflow) / sqr(Viscos);
                flag = TRUE;

                if (!ILP) {
                    //# Point sink case
                    F3 = FlowRate/(XN*LenAtOutflow*sqr(LenAtOutflow));
                    DEL = LenAtOutflow* pow(F3, (one/three));
                    if ((two * DEL <= WidthAtOutflow) || (ISINK != 1)) {
                        R = F3*sqrt(Grashof);
                        if (R <= one) DEL = twopt9 * (pow((Viscos*Visc), (one/six)) * (pow((LenAtOutflow/XN), (one/three))));
                        flag = FALSE;
                    }
                }

                if ( flag ) {
                    //# Line sink case
                    F2 = FlowRate/WidthAtOutflow/XN/sqr(LenAtOutflow);
                    ILP = TRUE;
                    DEL = two * LenAtOutflow * sqrt(F2);
                    R = F2 * pow(Grashof, (one/three));
                    if (R <= one) DEL = two*LenAtOutflow/pow(Grashof, (one/six));
                }
            }

            //# Check that the withdrawal layer does not intersect the top or bottom
            //# Of the reservoir, and that del is less than the range over which
            //# the density gradient is calculated.
            if (ISINK == 1) {
                if (DEL <= fabs(DZ) || NX == surfLayer) {
                    DELT = DEL;
                    if (DELT > (Lake[surfLayer].Height-HeightOfOutflow)) DELT = Lake[surfLayer].Height - HeightOfOutflow;
                    ISINK = -1;
                    NX = Outflow_LayerNum-1;
                } else {
                    ILP = FALSE;
                    NX++;
                }
            } else if (DEL <= fabs(DZ) || NX == botmLayer) {
                DELB = DEL;
                if (DELB > HeightOfOutflow) DELB = HeightOfOutflow;
                break;
            } else
                NX--;
        }

        //# Calculate top and bottom of w.l., and distance above
        //# W.L. from which fluid is drawn.
        zTop = HeightOfOutflow + DELT;
        zBot = HeightOfOutflow - DELB;
        TEL = DELT + DELB;
        DASNK = flow / WidthAtOutflow / LenAtOutflow * thsnd;
        AVDEL = TEL / two;
        zTop += DASNK;
        if (zTop >  Lake[surfLayer].Height) zTop = Lake[surfLayer].Height;

        //# Find the indices of the layers containing
        //# zTop and zBot. locate iBot

        for (i = botmLayer; i <= Outflow_LayerNum; i++)
            if (Lake[i].Height  >=  zBot) break;

        //# If zBot higher than the bottom of the sink, note an error.
        if (i > Outflow_LayerNum)
            fprintf(stderr,"Error do_outflows - bottom of WL above outlet bottom\n");
        iBot = i;
        //# locate it.
        for (i = Outflow_LayerNum; i <= surfLayer; i++)
            if (Lake[i].Height >= zTop) break;

        iTop = i;

        //# if all drawn from one layer.
        if (iBot == iTop)
            dv[Outflow_LayerNum] = flow;
        else {
            //# Calculate the dv[i), the portion of fluid drawn from the ith layer.
            for (i = iBot; i <= iTop; i++) {
                db = zero;
                if (i != botmLayer) db = Lake[i-1].Height;
                if (i == iBot) db = zBot;
                DT = Lake[i].Height;
                if (i == iTop) DT = zTop;
                dv[i] = dv_1(db, DT, DASNK, AVDEL, HeightOfOutflow, DELT, DELB);
            }

            //# Proportion drawn from each layer is known. match the
            //# total volume to the actual volume drawn out.

            QSTAR = zero;
            for (i = botmLayer; i <= surfLayer; i++)
                QSTAR += dv[i];

            for (i = botmLayer; i <= surfLayer; i++)
                dv[i] = (flow / QSTAR) * dv[i];
        }

        //# Correction if any layer should be emptied.
        for (i = botmLayer; i < surfLayer; i++) {
            if (dv[i] >= Lake[i].LayerVol) {
                dv[i+1] = dv[i+1] + dv[i] - Lake[i].LayerVol + one;
                dv[i] = Lake[i].LayerVol - one;
            }
        }
    }

    //# Now have dv[i] for all layers and can remove it.
    for (i = botmLayer; i <= surfLayer; i++)
         if (dv[i] >= 1E-5) Lake[i].LayerVol -= dv[i];

    Lake[botmLayer].Vol1 = Lake[botmLayer].LayerVol;
    for (i = (botmLayer+1); i <= surfLayer; i++)
        Lake[i].Vol1 = Lake[i-1].Vol1 + Lake[i].LayerVol;

    resize_internals(2, botmLayer);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Loop through all outflows and process - return the difference between      *
 *  total volume before and after.                                            *
 ******************************************************************************/
REALTYPE do_outflows(int jday)
{
    int i;
    REALTYPE DrawHeight;
    REALTYPE VolSum = Lake[surfLayer].Vol1;

    if ( dv == NULL ) dv = malloc(sizeof(REALTYPE) * MaxLayers);

    //# Do withdrawal for each offtake
    //# Include water quality and particles
    //# offtake type is switch controlled by FloatOff
    for (i = 0; i < NumOut; i++) {
        REALTYPE tVolSum = Lake[surfLayer].Vol1;

        //# Floating offtake.
        //# OLev(i) is distance below surface level for the offtake
        if (Outflows[i].FloatOff) {
            DrawHeight = Lake[surfLayer].Height - Outflows[i].OLev;
            //# Let it sit on the bottom if the lake has drained
            if (DrawHeight < 0.) DrawHeight = 0.;
        } else
            DrawHeight = Outflows[i].OLev; //# Fixed height offtake

        Outflows[i].Draw *= Outflows[i].Factor;

        do_an_outflow(DrawHeight, Outflows[i].Draw, &Outflows[i]);

        write_outflow(i, jday, tVolSum - Lake[surfLayer].Vol1);
    }

    return VolSum - Lake[surfLayer].Vol1;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Calculate overflow.  Overflow occurs when the current reservoir            *
 * volume is greater than the volume of the reservoir at the crest            *
 * level.  Add in stack volumes when calculating overflow.                    *
 * After overflow, delete stack volumes from the structure.                   *
 ******************************************************************************/
REALTYPE do_overflow(int jday)
{
    REALTYPE VolSum = Lake[surfLayer].Vol1;

    if (VolSum > VolAtCrest)
        do_an_outflow(CrestLevel, VolSum - VolAtCrest, NULL);

    write_outflow(MaxOut, jday, VolSum - Lake[surfLayer].Vol1);

    return VolSum - Lake[surfLayer].Vol1;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Inserts the inflow at the level of neutral buoyancy, after calculating the *
 * entrainment                                                                *
 * Return the difference between total volume before and after.
 ******************************************************************************/
REALTYPE do_inflows()
{
    REALTYPE pt1 = 0.1, pt2 = 0.2, pt21 = 0.21, pt4 = 0.4, onept2 = 1.2,
             pt9 = 0.9, onept5 = 1.5, cwnsq2 = 39.48, TSecDay = 86.4;

   REALTYPE Alpha; //# Stream half angle in radians
   REALTYPE Phi;   //# Stream slope in radians
   REALTYPE DragCoeff;  //# Stream drag coefficient
   REALTYPE Delta_Q;    //# Delta Q the rate of entrainment of inflow aliquot [ML/day]
   REALTYPE Delta_t;    //# Delta t the time taken for downflow [days]
   REALTYPE Downflow_Depth;  //#Depth of downflow [m]
   REALTYPE Inflow_Height_t0;  //#Inflow thickness at beginning of the day [m]
   REALTYPE Inflow_Density; //# Density of the inflow
   REALTYPE Depth_t0;  //#Depth of lake before inflows [m]
   REALTYPE Inflow_dx;  //#Distance travelled by inflow [m]
   REALTYPE EntrainmentCoeff;  //# Entrainment coefficient
   REALTYPE Inflow_Energy; //# Energy of inflow
   REALTYPE Inflow_gprime;     //# Reduced gravity (gprime) between inflow and layer densities
   REALTYPE Inflow_gprime_t0;  //# Reduced gravity (gprime) given surface and inflow densities
   REALTYPE Inflow_height_prev;  //# Thickness of inflow from previous layer [m]
   REALTYPE Inflow_height;       //# Thickness of inflow post entrainment [m]
   REALTYPE Inflow_Flowrate; //# Inflow flowrate [ML/day]
   REALTYPE Ri;              //# Bulk Richardson number of the inflow [-]
   REALTYPE Inflow_Salinity; //# Inflow salinity [psu]
   REALTYPE Inflow_Temp; //# Inflow temperature [oC]
   REALTYPE Inflow_time;      //# Time taken for inflow to reach neutral buoyancy or bottom of lake [days]
   REALTYPE Inflow_Velocity; //# Inflow velocity [m/s]
   REALTYPE Inflow_width;    //# Width of inflow [m]

   int j, k, ll;
   int iRiv, ntims;
   int jk;
   int kk;
   int Layer;  //# Layer number adjacent to underflow, entrainment layer
   int Layer_t0;  //# Layer number at which inflow starts the day
   int iRiver;

   int flag;
   int wqidx;

   REALTYPE VolSum = Lake[surfLayer].Vol1;

/*----------------------------------------------------------------------------*/
//BEGIN
    if ( ! WQ_VarsS )   WQ_VarsS = malloc(Num_WQ_Vars * sizeof(REALTYPE));
    if ( ! WQ_VarsTmp ) WQ_VarsTmp = malloc(NumInf*sizeof(wq_partic_t));
    memset(WQ_VarsS, 0, (Num_WQ_Vars * sizeof(REALTYPE)));

    //# calculate WaveNumSquared and vel for use by sub DIFUSE; they're used to calculate
    //# the dispersion coefficient.
    //# Count down from smallest river, overwriting each time, in case river
    //# has zero inflow.
    WaveNumSquared = zero;
    vel = zero;
    for (iRiver = NumInf-1; iRiver >= 0; iRiver--) {
        if (Inflows[iRiver].FlowRate*Inflows[iRiver].Factor != zero) {
            Alpha = Inflows[iRiver].Alpha;
            Phi = Inflows[iRiver].Phi;
            Inflow_Density = calculate_density(Inflows[iRiver].TemInf,Inflows[iRiver].SalInf);
            Inflow_gprime_t0 = gprime(Lake[surfLayer].Density,Inflow_Density);
            Inflow_Height_t0 = Lake[surfLayer].Height-Lake[surfLayer-1].Height;
            if (Inflow_gprime_t0 > zero)
                Inflow_Height_t0 = pow((Inflows[iRiver].FlowRate * Inflows[iRiver].Factor / TSecDay), pt4)/pow(Inflow_gprime_t0, pt2);
            if (Inflow_Height_t0 > Lake[surfLayer].Height)
                Inflow_Height_t0 = Lake[surfLayer].Height;
            WaveNumSquared = cwnsq2/sqr(Inflow_Height_t0);
            vel = pt1 * Inflows[iRiver].FlowRate*Inflows[iRiver].Factor / (TSecDay*sqr(Inflow_Height_t0)*sin(Alpha)/cos(Alpha));
        }
    }

    Depth_t0 = Lake[surfLayer].Height;
    for (iRiver = 0; iRiver < NumInf; iRiver++) {
        if (Inflows[iRiver].FlowRate * Inflows[iRiver].Factor > zero) {
            //if (Inflows[iRiver].iCnt >= MaxPar) {
            //    fprintf(stderr, "Downflow stack limit too small will give incorrect results.  In river %d\n",iRiver);
            //    exit(1);
            //}
            Inflows[iRiver].QDown[Inflows[iRiver].iCnt] = Inflows[iRiver].FlowRate * Inflows[iRiver].Factor;
            Inflows[iRiver].TDown[Inflows[iRiver].iCnt] = Inflows[iRiver].TemInf;
            Inflows[iRiver].SDown[Inflows[iRiver].iCnt] = Inflows[iRiver].SalInf;
            Inflows[iRiver].DDown[Inflows[iRiver].iCnt] = thsnd;

            for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                Inflows[iRiver].WQDown[Inflows[iRiver].iCnt][wqidx] = Inflows[iRiver].WQInf[wqidx];

            Inflows[iRiver].iCnt++;
        }
    }

    //# Work through each element in the downflow stacks and calculate the
    //# travel distance and entrainment for the present day, and whether or not
    //# it reaches its level of neutral buoyancy and hence can be inserted.
    einff = zero;
    iRiver = 0;
    while(1) {
        while (iRiver < NumInf) {
            Alpha = Inflows[iRiver].Alpha;
            Phi = Inflows[iRiver].Phi;
            DragCoeff = Inflows[iRiver].DragCoeff;
            Ri = DragCoeff * ( one + pt21 * sqrt(DragCoeff) * sin(Alpha) ) / (sin(Alpha) * sin(Phi) / cos(Phi));

            //# entrainment
            EntrainmentCoeff = 1.6 * pow(DragCoeff, onept5) / Ri;

            Inflows[iRiver].NoIns = 0;  //# Initialise number of insertions as zero

            k = -1;

            //# Loop for calculation of each separate inflow element.
labl30:
            k++;
            Inflow_Energy = zero;
            if (k < Inflows[iRiver].iCnt) {
                if (Inflows[iRiver].DDown[k] == thsnd) {
                    Inflows[iRiver].TotIn += Inflows[iRiver].QDown[k];
                    Inflows[iRiver].DDown[k] = Depth_t0;
                }
                Inflow_time = zero;
                Inflows[iRiver].DOld[k] = Inflows[iRiver].DDown[k];
                Inflow_Density = calculate_density(Inflows[iRiver].TDown[k], Inflows[iRiver].SDown[k]);

                //# Calculate the layer in which this element starts the day.
                for (Layer_t0 = botmLayer; Layer_t0 <= surfLayer; Layer_t0++)
                    if (Lake[Layer_t0].Height >= Inflows[iRiver].DDown[k]) break;

                if (Layer_t0 > surfLayer) Layer_t0 = surfLayer;
                Layer = Layer_t0;

                //# Loop for progression of an element through the next layer down.
                Inflow_Flowrate = Inflows[iRiver].QDown[k];

                Inflow_Temp = Inflows[iRiver].TDown[k];
                Inflow_Salinity = Inflows[iRiver].SDown[k];

                for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                    WQ_VarsS[wqidx] = Inflows[iRiver].WQDown[k][wqidx];

                Downflow_Depth = Inflows[iRiver].DDown[k];

                //# Check if this element lies below level of neutral buoyancy.
                //# If it is insert it and renumber the stack elements.
                if (Inflow_Density <= Lake[Layer].Density){
                    Inflows[iRiver].InPar[Inflows[iRiver].NoIns] = k;
                    Inflows[iRiver].TIns[Inflows[iRiver].NoIns]  = Inflow_Temp;
                    Inflows[iRiver].SIns[Inflows[iRiver].NoIns]  = Inflow_Salinity;
                    Inflows[iRiver].DIIns[Inflows[iRiver].NoIns] = Inflow_Density;
                    Inflows[iRiver].QIns[Inflows[iRiver].NoIns]  = Inflow_Flowrate;

                    for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                        Inflows[iRiver].WQIns[Inflows[iRiver].NoIns][wqidx] = WQ_VarsS[wqidx];

                    Inflows[iRiver].NoIns++;

                    goto labl30;
                }

                //# Calculate the velocity of the inflow and hence the entrainment.
                while(1) {
                    Inflow_gprime = g*(Inflow_Density-Lake[Layer].Density)/(thsnd+Lake[Layer].Density);
                    Inflow_height_prev = pow((two*Ri*pow((Inflow_Flowrate*cos(Alpha)/(sin(Alpha)*TSecDay)), 2) / Inflow_gprime), pt2);
                    if (Layer == botmLayer) Inflow_dx = Downflow_Depth/sin(Phi);
                    else                    Inflow_dx = (Downflow_Depth-Lake[Layer-1].Height)/sin(Phi);
                    Inflow_height = onept2 * EntrainmentCoeff * Inflow_dx  +  Inflow_height_prev;
                    Inflow_Velocity = Inflow_Flowrate * thsnd * cos(Alpha) / (pow(Inflow_height, 2) * sin(Alpha));

                    Delta_t = Inflow_dx/Inflow_Velocity;
                    if ((Inflow_time + Delta_t) > one) {
                        Inflow_dx = Inflow_dx * (one-Inflow_time) / Delta_t;
                        Delta_t = one-Inflow_time;
                        Inflow_height = onept2 * EntrainmentCoeff * Inflow_dx  +  Inflow_height_prev;
                    }

                    Inflow_time += Delta_t;

                    Delta_Q = 0.2 * Inflow_Flowrate * (pow((Inflow_height/Inflow_height_prev), (five/three)) - one);

                    //# Check for negative inflow layer volume
                    if (Lake[Layer].LayerVol < 0.0)
                        fprintf(stderr, "vol(layer) is negative - layer no. %d surfLayer = %d\n", Layer, surfLayer);

                    //# Check that the entrainment is less than 90% of the layer volumne
                    if (Delta_Q > pt9*Lake[Layer].LayerVol) Delta_Q = pt9 * Lake[Layer].LayerVol;

                    Inflow_Salinity = combine(Inflow_Salinity, Inflow_Flowrate, Inflow_Density, Lake[Layer].Salinity, Delta_Q, Lake[Layer].Density);
                    Inflow_Temp = combine(Inflow_Temp, Inflow_Flowrate, Inflow_Density, Lake[Layer].Temp, Delta_Q, Lake[Layer].Density);
                    Inflow_Density = calculate_density(Inflow_Temp, Inflow_Salinity);

                    //# Add entrained water to inflow aliquot and take from adjacent layer
                    Inflow_Flowrate += Delta_Q;
                    Lake[Layer].LayerVol -= Delta_Q;

                    for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                        WQ_VarsS[wqidx] = combine_vol(WQ_VarsS[wqidx], Inflow_Flowrate, _WQ_Vars(wqidx, Layer), Delta_Q);

                    //# Calculate energy of inflowing streams.
                    if (Layer == botmLayer)
                        Inflow_Energy += (Inflow_Density - Lake[Layer].Density) * Downflow_Depth * g * Inflow_Flowrate / TSecDay;
                    else
                        Inflow_Energy += (Inflow_Density - Lake[Layer].Density) * (Downflow_Depth - Lake[Layer-1].Height) * g * Inflow_Flowrate / TSecDay;

                    //# Reset the downflow stacks.
                    Inflows[iRiver].QDown[k] = Inflow_Flowrate;
                    Inflows[iRiver].TDown[k] = Inflow_Temp;
                    Inflows[iRiver].SDown[k] = Inflow_Salinity;

                    for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                         Inflows[iRiver].WQDown[k][wqidx] = WQ_VarsS[wqidx];

                    Inflows[iRiver].TotIn = Inflows[iRiver].TotIn + Delta_Q;
                    Inflows[iRiver].DDown[k] = Downflow_Depth - Inflow_dx * sin(Phi);
                    Downflow_Depth = Inflows[iRiver].DDown[k];

                    if (Inflows[iRiver].DDown[k] < Inflows[iRiver].Dlwst)
                        Inflows[iRiver].Dlwst = Inflows[iRiver].DDown[k];

                    //# If the inflow parcel has ended it's days travel, reached the
                    //# level of neutral buoyancy or has reached the bottom, put it in
                    //# the insertion queue.
                    flag = TRUE;
                    if (Layer != botmLayer)
                        if (Inflow_Density > Lake[Layer-1].Density) flag = FALSE;
                    if (Inflow_time >= one)  flag = TRUE ;

                    if ( flag ) {
                        Inflows[iRiver].InPar[Inflows[iRiver].NoIns] = k;
                        Inflows[iRiver].TIns[Inflows[iRiver].NoIns]  = Inflow_Temp;
                        Inflows[iRiver].SIns[Inflows[iRiver].NoIns]  = Inflow_Salinity;
                        Inflows[iRiver].DIIns[Inflows[iRiver].NoIns] = Inflow_Density;
                        Inflows[iRiver].QIns[Inflows[iRiver].NoIns]  = Inflow_Flowrate;

                        for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                            Inflows[iRiver].WQIns[Inflows[iRiver].NoIns][wqidx] = WQ_VarsS[wqidx];

                        Inflows[iRiver].NoIns++;
                    }

                    //# If the inflow parcel has ended its days travel, reached its level of
                    //# Neutral buoyancy or the bottom of the reservoir go to the next parcel

                    flag = FALSE;
                    if (Layer == botmLayer)
                        flag = TRUE;
                    else if (Inflow_time >= one)
                        flag = TRUE;
                    else if (Inflow_Density <= Lake[Layer-1].Density)
                        flag = TRUE;

                    if ( flag ) {
                        einff += Inflow_Energy/Inflow_time;
                        if (Layer == botmLayer) Lake[botmLayer].Vol1 = Lake[botmLayer].LayerVol;
                        if (Layer != botmLayer) Lake[Layer].Vol1 = Lake[Layer-1].Vol1 + Lake[Layer].LayerVol;
                        for (kk = Layer+1; kk <= surfLayer; kk++)
                             Lake[kk].Vol1 = Lake[kk-1].Vol1 + Lake[kk].LayerVol;

                        goto labl30;
                    }
                    Layer--;
                }
            }
            iRiver++;
        }


        //# Insert all of the parcels which reached their level of NB on this day
        //# Adjust the stacking to note the removal.
        //#X Note that ntims should be nosecs if the timestep is variable

        ntims = 86400;
        Inflow_width = 0.0;
        for (iRiver = 0; iRiver < NumInf; iRiver++) {
            Phi = Inflows[iRiver].Phi;
            iRiv = iRiver;

            for (j = 0; j < Inflows[iRiver].NoIns; j++) {
                for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                    WQ_VarsTmp[iRiv][j][wqidx] = Inflows[iRiv].WQIns[j][wqidx];

                insert(Inflows[iRiv].QIns[j], Inflows[iRiv].DIIns[j], Phi,
                           Inflows[iRiv].TIns[j], Inflows[iRiv].SIns[j],
                                      WQ_VarsTmp[iRiv][j], ntims, &Inflow_width, &ll);

                for (jk = Inflows[iRiv].InPar[j]-1; jk < Inflows[iRiv].iCnt-1; jk++) {
                    Inflows[iRiv].QDown[jk] = Inflows[iRiv].QDown[jk+1];
                    Inflows[iRiv].TDown[jk] = Inflows[iRiv].TDown[jk+1];
                    Inflows[iRiv].SDown[jk] = Inflows[iRiv].SDown[jk+1];

                    for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                        Inflows[iRiv].WQDown[jk][wqidx] = Inflows[iRiv].WQDown[jk+1][wqidx];

                    Inflows[iRiv].DDown[jk] = Inflows[iRiv].DDown[jk+1];
                    Inflows[iRiv].DOld[jk] = Inflows[iRiv].DOld[jk+1];
                }
                Inflows[iRiv].TotIn -= Inflows[iRiv].QIns[j];
                Inflows[iRiv].iCnt--;
                if (Inflows[iRiv].iCnt == 0) {
                    Inflows[iRiv].TotIn = zero;
                    Inflows[iRiv].Dlwst = thsnd;
                }
                for (k = j; k < Inflows[iRiver].NoIns; k++)
                    Inflows[iRiver].InPar[k]--;
            }
        }

        //# Reset the number of insertions per river to be zero.
        for (k = 0; k < NumInf; k++) Inflows[k].NoIns = 0;

        //# Calculate the front of the downflow for each river.
        for (iRiver = 0; iRiver < NumInf; iRiver++) {
            Inflows[iRiver].Dlwst = thsnd;
            for (j = 0; j < Inflows[iRiver].iCnt; j++) {
                if (Inflows[iRiver].DDown[j] < Inflows[iRiver].Dlwst)
                    Inflows[iRiver].Dlwst = Inflows[iRiver].DDown[j];
            }
        }

        iRiv = -1;

        //# If a flow fit error has occured, go back and reroute first inflow for
        //# the river of concern - iRiv.
        if ( new_storage(&iRiv) ) break;

        iRiver = iRiv;
    }

    //# Make adjustments to correct layer volumes.
    resize_internals(2,botmLayer);
    check_layer_thickness();

    return Lake[surfLayer].Vol1 - VolSum;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Solve the cubic for flowing depth in a triangular river valley.            *
 ******************************************************************************/
static int flowing_depth(REALTYPE *HF, REALTYPE *DL, REALTYPE Volume,
                                   REALTYPE DEPTH, REALTYPE Phi, REALTYPE Alpha)
{
    REALTYPE pt25 = 0.25;

    REALTYPE Theta, H, TAG, CON;
    int all_ok = TRUE;

/*----------------------------------------------------------------------------*/

    //# Set the coefficients of the cubic.
    H = zero;
    if (Volume > zero) {
        TAG = tan(Alpha) / tan(Phi);

        while(1) {
            CON = DEPTH - *DL;
            if (fabs(one - six * Volume * thsnd / TAG / pow(CON, 3)) <= one) {
                Theta = acos(one - six * Volume * thsnd / TAG / pow(CON, 3));
                H = (two * cos(Theta / three) + one) * CON / two;
                if (H > zero && H < CON) break;
                H = (two * cos(Theta / three + two * Pi / three) + one) * CON / two;
                if (H > zero && H < CON) break;
                H = (two * cos(Theta / three + four * Pi / three) + one) * CON / two;
                if (H > zero && H < CON) break;
            }
            *DL -= pt25;

            if (*DL <= zero) {
                all_ok = FALSE;
                break;
            }
        }
    }

    //# Then the flowing depth for this river is H.
    *HF = H;
    return all_ok;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Calculate the new temporary storage table for use in resize_internals      *
 ******************************************************************************/
static int new_storage(int *iRiv)
{
   REALTYPE pt1 = 0.1;

   int iTop,j,k;

   REALTYPE D;
   REALTYPE DLOW;
   REALTYPE DOL;
   REALTYPE EXTRA;
   REALTYPE LAYPRO;
   REALTYPE LayerVol;
   REALTYPE VlSum;
   REALTYPE VOLSUM;
   REALTYPE MphLevelVoltemp;

/*----------------------------------------------------------------------------*/

    //# Must begin by calculating the new depth of the entire reservoir.
    VlSum = zero;
    for (k = 0; k < NumInf; k++)
        VlSum += Inflows[k].TotIn;

    VOLSUM = VlSum + Lake[surfLayer].Vol1;
    j = 0;
    while (j < Nmorph) {
        if (VOLSUM <= MphLevelVol[j]) {
            j--;
            break;
        }
        j++;
    }
    if (j >= Nmorph) j = Nmorph - 1;
    Lake[surfLayer].Height = ((j+1) + (VOLSUM - MphLevelVol[j])/dMphLevelVol[j]) * pt1;

    //# Calculate the flowing depths of the inflowing streams.
    //#
    DLOW = thsnd;
    for (k = 0; k < NumInf; k++) {
        if (!flowing_depth(&Inflows[k].HFlow, &Inflows[k].Dlwst, Inflows[k].TotIn,
                         Lake[surfLayer].Height, Inflows[k].Phi, Inflows[k].Alpha) ) {
             //# If flow error then go back to INFLOW
             *iRiv = k;
             return FALSE;
        }
        if (DLOW > Inflows[k].Dlwst) DLOW = Inflows[k].Dlwst;
        if (Inflows[k].HFlow > Lake[surfLayer].Height) {
            fprintf(stderr, "Error in height of flow\n");
            exit(1);
        }
    }

    //# Account for the extra volumes of the downflows.
    iTop = round(Lake[surfLayer].Height * ten) - 1;
    if ( iTop >= Nmorph ) iTop = Nmorph - 1;
    EXTRA = zero;
    DOL = zero;

    for (j = botmLayer; j <= iTop; j++) {
        D = (j+1) / ten;
        EXTRA = extra_volume(DOL, D, Lake[surfLayer].Height, Inflows);
        if (j == botmLayer)
            LayerVol = MphLevelVol[j];
        else
            LayerVol = MphLevelVol[j] - MphLevelVol[j-1];

        LAYPRO = (D-DLOW) / D;
        if (LAYPRO < zero) LAYPRO = zero;
        MphLevelVoltemp = zero;
        if (j != botmLayer) MphLevelVoltemp = MphLevelVoldash[j-1];
        MphLevelVoldash[j] = MphLevelVol[j] - EXTRA;
        if (MphLevelVoldash[j] < (MphLevelVoltemp + (one - LAYPRO) * LayerVol))
            MphLevelVoldash[j] =  MphLevelVoltemp + (one - LAYPRO) * LayerVol;
    }

    for (j = iTop+1; j < Nmorph; j++)
        MphLevelVoldash[j] = MphLevelVol[j] - VlSum;

    for (j = 0; j < Nmorph-1; j++)
        dMphLevelVolda[j] = MphLevelVoldash[j+1] - MphLevelVoldash[j];

    dMphLevelVolda[Nmorph-1] = dMphLevelVolda[Nmorph-2];

    return TRUE;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Compute the volume in inflow stacks which lies between depths d1 and d2.   *
 ******************************************************************************/
static REALTYPE extra_volume(REALTYPE d1, REALTYPE d2,
                                  REALTYPE SurfaceHeight, InflowDataType *Inflow)
{
    REALTYPE DTOP, DBOT, SUM, TAG, ZB;
    int k;

    SUM = zero;
    for (k = 0; k < NumInf; k++) {
        ZB = Inflow[k].Dlwst;
        if (! ((d2 <= ZB) || (SurfaceHeight <= Inflow[k].Dlwst)) ) {
            TAG = tan(Inflow[k].Alpha)/tan(Inflow[k].Phi);
            DBOT = d1;
            if (d1 < ZB) DBOT = ZB;
            DTOP = d2;
            if (d2 > SurfaceHeight) DTOP=SurfaceHeight;

            if (DTOP < Inflow[k].Dlwst+Inflow[k].HFlow)
                SUM += TAG*(pow(DTOP-ZB,3)-pow(DBOT-ZB,3))/three;
            else if (DBOT > Inflow[k].Dlwst + Inflow[k].HFlow)
                SUM += TAG*pow(Inflow[k].HFlow,2)*(DTOP-DBOT);
            else
                SUM += TAG*(pow(Inflow[k].HFlow,3)-pow(DBOT-ZB,3))/three +
                                TAG*pow(Inflow[k].HFlow,2) *
                                   (DTOP-Inflow[k].Dlwst-Inflow[k].HFlow);
        }
    }
    return SUM / thsnd;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
