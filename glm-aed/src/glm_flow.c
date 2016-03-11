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
#include "glm_const.h"
#include "glm_globals.h"

#include "glm_util.h"
#include "glm_mixu.h"
#include "glm_layers.h"
#include "glm_output.h"

#define _WQ_VarsTmp(i,j,k)  WQ_VarsTmp[_IDX_3d(Num_WQ_Vars,NumInf,MaxPar,i,j,k)]

FILE *myfile = NULL;
FILE *myfile2 = NULL;

typedef AED_REAL wq_vars_t[MaxVars];
typedef wq_vars_t wq_partic_t[MaxPar];

/******************************************************************************/
static int flowing_depth(AED_REAL *HF, AED_REAL *DL, AED_REAL VOL,
                                  AED_REAL DEPTH, AED_REAL Phi, AED_REAL Alpha);
static AED_REAL extra_volume(AED_REAL d1, AED_REAL d2,
                                 AED_REAL SurfaceHeight, InflowDataType *Inflow);
static void do_single_outflow(AED_REAL HeightOfOutflow, AED_REAL flow, OutflowDataType *outf);
static int new_storage(int *iRiv);
/*----------------------------------------------------------------------------*/

static AED_REAL *WQ_VarsS = NULL;
static wq_partic_t *WQ_VarsTmp = NULL;

static AED_REAL *Delta_V = NULL; //# The delta V from each layer taken by outflow

static int checkjday = -1;

LOGICAL seepage = FALSE;
AED_REAL seepage_rate = 0.0;

/******************************************************************************
 * Removes the outflow at level Outflow_LayerNum                              *
 ******************************************************************************/
void do_single_outflow(AED_REAL HeightOfOutflow, AED_REAL flow, OutflowDataType *outf)
{
    const AED_REAL min_flow = 0.0001;

    AED_REAL LenAtOutflow;   //# Reservoir length at height of withdrawal
    AED_REAL AVDEL;          //#
    AED_REAL DASNK;          //# Level above W.L. from which fluid is drawn
    AED_REAL ThickOutflow;   //# Thickness of withdrawal layer
    AED_REAL DeltaBot;       //# Lower half withdrawal layer thickness
    AED_REAL DeltaTop;       //# Upper half withdrawal layer thickness
    AED_REAL Delta_rho;      //# Density difference between outflow layer i above or below
    AED_REAL DT;             //#
    AED_REAL Delta_z;        //# Width over which density gradient is calculated
    AED_REAL FlowRate;       //# Flow rate
    AED_REAL F2,F3;
    AED_REAL Grashof;        //# Grashof number
    AED_REAL QSTAR;
    AED_REAL R;              //# Outflow parameter
    AED_REAL TEL;
    AED_REAL Viscos;         //# Vertical diffusivity of momentum averaged over the withdrawal thickness
    AED_REAL WidthAtOutflow; //# Reservoir width at height of withdrawal
    AED_REAL Nsqrd_outflow;  //# Brunt-Vaisala frequency squared
    AED_REAL zBot;           //# Height of bottom of withdrawal layer
    AED_REAL zTop;           //# Height of top of withdrawal layer

    int i,iBot,ILP;
    int Outflow_half;        //# Which half of outflow calculating +1 = top half, -1 = bottom half
    int iTop;
    int Outflow_LayerNum;    //# Layer number of outflow height
    int Layer_i;             //# Layer count either above or below outflow height

    int flag;                //# Used to describe the nature of the withdraw

/*----------------------------------------------------------------------------*/
//BEGIN
    if (HeightOfOutflow < 0 && outf->Type != 0 ) {
        fprintf(stderr, "HeightOfOutflow < 0 Outflow type is %d\n", outf->Type);
        exit(1);
    }

    //# Find number of layer (Outflow_LayerNum) opposite offtake
    for (i = botmLayer; i <= surfLayer; i++)
        if (Lake[i].Height >=  HeightOfOutflow) break;

    //# Return if reservoir surface is below outlet level
    if (i > surfLayer) return;

    Outflow_LayerNum = i;

    LenAtOutflow = 0.;
    WidthAtOutflow = 0.;
    ThickOutflow = 0.0;
    DeltaTop = 0.;
    ILP = FALSE;
    Delta_z = 0.0;

    //# Reset delta V for all layers as zero
    for (i = botmLayer; i <= surfLayer; i++) Delta_V[i]=0.0;

    /***********************************************************************
     * Determine offtake level, length, width and flow                     *
     *                                                                     *
     * withdrawal height selection                                         *
     * switch control by FloatOff                                          *
     * Assume:                                                             *
     *      1) that lake approximates as an ellipse                        *
     *      2) area = pi/4 * Length * Width                                *
     *      3) ratio Length:Width at outflow = crest                       *
     *                                                                     *
     ***********************************************************************/
    if (outf == NULL) {
        if ( HeightOfOutflow > zero) { //# Is an overflow so use crest height and width
            LenAtOutflow = LenAtCrest;
            WidthAtOutflow = WidAtCrest;
        } else { //# Seepage use bottom layer (Eq. x & x in GLM manual)
            LenAtOutflow = sqrt(Lake[Outflow_LayerNum].LayerArea*4.0/Pi*(LenAtCrest/WidAtCrest));
            WidthAtOutflow = LenAtOutflow * WidAtCrest/LenAtCrest;
        }
    } else {
        if (outf->FloatOff) { // Floating offtake
            //# Assume offtake level close to surface
            LenAtOutflow = Lake[surfLayer].LayerArea / WidAtCrest;
            WidthAtOutflow = WidAtCrest;
        } else { //# Fixed Offtake (Eq. x & x in GLM manual)
            LenAtOutflow = sqrt(Lake[Outflow_LayerNum].LayerArea*4.0/Pi*(LenAtCrest/WidAtCrest));
            WidthAtOutflow = LenAtOutflow * WidAtCrest/LenAtCrest;
        }
    }

    FlowRate = flow / SecsPerDay;

    if (flow <= min_flow) return;

    //# Calculate withdrawal layer thickness and volume removed from each layer
    if ((outf == NULL && HeightOfOutflow == zero) || surfLayer == botmLayer) {
        //# Is a seepage or single layer lake take all from bottom layer
        Delta_V[0] = flow;
        //# Correction if bottom layer should be emptied. Note that for a single layer
        //# lake, less than the specified outflow may be removed and this will be noted
        //# in the lake.csv diagnostic file
        if (surfLayer == botmLayer) {
            if (Delta_V[0] >= Lake[0].LayerVol)
                Delta_V[0] = Lake[0].LayerVol - 1.0;
        } else {
            for (i = botmLayer; i < surfLayer; i++) {
                if (Delta_V[i] >= Lake[i].LayerVol) {
                    Delta_V[i+1] = Delta_V[i+1] + Delta_V[i] - Lake[i].LayerVol + 1.0;
                    Delta_V[i] = Lake[i].LayerVol - 1.0;
                }
             }
        }
    } else {
        Outflow_half = 1; //# Start with withdraw from layers above outlet
        Layer_i = Outflow_LayerNum + 1;
        while (1) {
            if (Layer_i > surfLayer) Layer_i = surfLayer;
            if (Layer_i < botmLayer) Layer_i = botmLayer;
            Delta_rho = Lake[Outflow_LayerNum].Density - Lake[Layer_i].Density;

            flag = FALSE;
            if (Delta_rho * Outflow_half <= zero)
                //# Density of outflow layer less than layer i above or greater than layer i below
                //# i.e. unstable density
                flag = TRUE;
            else {
                Delta_z = Lake[Layer_i].MeanHeight - HeightOfOutflow;
                if ((Delta_z * Outflow_half) <= zero)
                    //# Counting up and Delta_z is negative or counting down and Delta_z is positive
                    flag = TRUE;
                else { //# Calculate the Brunt-Vaisala frequency of outflow (Eqn x in GLM Manual)
                    Nsqrd_outflow = Delta_rho * g / (Lake[Outflow_LayerNum].Density * Delta_z);
                    if (Nsqrd_outflow <= zero) flag = TRUE;
                }
            }

            if (flag) {
                //# check for zero gradient
                if (Outflow_half == 1) //# Taking from layers above only
                    ThickOutflow = Lake[surfLayer].Height - HeightOfOutflow;
                else //# Taking from layers below only
                    ThickOutflow = HeightOfOutflow;
            } else {
                //# vertical diffusivity of momentum averaged over withdraw layer
                Viscos = Lake[Outflow_LayerNum].Epsilon * 20.0;
                //# limit to molecular diffusivity
                if (Viscos <= zero) Viscos = Visc;
                //# Grashof number (Eq. x in GLM manual)
                Grashof = Nsqrd_outflow * sqr(LenAtOutflow) * sqr(LenAtOutflow) / sqr(Viscos);
                flag = TRUE;

                if (!ILP) {
                    //# Point sink case
                    F3 = FlowRate/(sqrt(Nsqrd_outflow)*LenAtOutflow*sqr(LenAtOutflow));
                    ThickOutflow = LenAtOutflow* pow(F3, (1.0/3.0));
                    if ((2.0 * ThickOutflow <= WidthAtOutflow) || (Outflow_half != 1)) {
                        R = F3*sqrt(Grashof);
                        if (R <= 1.0)
                            ThickOutflow = 2.9 * (pow((Viscos*Visc), (1.0/6.0)) *
                                             (pow((LenAtOutflow/sqrt(Nsqrd_outflow)), (1.0/3.0))));
                        flag = FALSE;
                    }
                }

                if ( flag ) {
                    //# Line sink case
                    //# Froude number (Eq. x in GLM Manual)
                    F2 = FlowRate/WidthAtOutflow/sqrt(Nsqrd_outflow)/sqr(LenAtOutflow);
                    ILP = TRUE;
                    ThickOutflow = 2.0 * LenAtOutflow * sqrt(F2);
                    //# R parameter (Eq. x in GLM Manual)
                    R = F2 * pow(Grashof, (1.0/3.0));
                    //# Thickness of outflow (Eq. x in GLM manual)
                    if (R <= 1.0) ThickOutflow = 2.0*LenAtOutflow/pow(Grashof, (1.0/6.0));
                }
            }

            /******************************************************************
             * Check that the withdrawal layer does not intersect the top or  *
             * bottom of the reservoir, and that ThickOutflow is less than    *
             * the range over which the density gradient is calculated.       *
             ******************************************************************/
            if (Outflow_half == 1) {
                if (ThickOutflow <= fabs(Delta_z) || Layer_i == surfLayer) {
                    DeltaTop = ThickOutflow;
                    if (DeltaTop > (Lake[surfLayer].Height-HeightOfOutflow))
                        DeltaTop = Lake[surfLayer].Height - HeightOfOutflow;
                    Outflow_half = -1;
                    Layer_i = Outflow_LayerNum-1;
                } else {
                    ILP = FALSE;
                    Layer_i++;
                }
            } else if (ThickOutflow <= fabs(Delta_z) || Layer_i == botmLayer) {
                DeltaBot = ThickOutflow;
                if (DeltaBot > HeightOfOutflow) DeltaBot = HeightOfOutflow;
                break;
            } else
                Layer_i--;
        }

        /**********************************************************************
         * Calculate top and bottom of w.l., and distance above               *
         * W.L. from which fluid is drawn.                                    *
         **********************************************************************/
        zTop = HeightOfOutflow + DeltaTop;
        zBot = HeightOfOutflow - DeltaBot;
        TEL = DeltaTop + DeltaBot;
        DASNK = flow / WidthAtOutflow / LenAtOutflow;
        AVDEL = TEL / 2.0;
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
            Delta_V[Outflow_LayerNum] = flow;
        else {
            //# Calculate the Delta_V[i), the portion of fluid drawn from the ith layer.
            for (i = iBot; i <= iTop; i++) {
                AED_REAL db = zero;
                if (i != botmLayer) db = Lake[i-1].Height;
                if (i == iBot) db = zBot;
                DT = Lake[i].Height;
                if (i == iTop) DT = zTop;
                Delta_V[i] = delta_volume(db, DT, DASNK, AVDEL, HeightOfOutflow, DeltaTop, DeltaBot);
            }

            //# Proportion drawn from each layer is known. match the
            //# total volume to the actual volume drawn out.

            QSTAR = zero;
            for (i = botmLayer; i <= surfLayer; i++)
                QSTAR += Delta_V[i];

            for (i = botmLayer; i <= surfLayer; i++)
                Delta_V[i] = (flow / QSTAR) * Delta_V[i];
        }

        //# Correction if any layer should be emptied.
        for (i = botmLayer; i < surfLayer; i++) {
            if (Delta_V[i] >= Lake[i].LayerVol) {
                Delta_V[i+1] = Delta_V[i+1] + Delta_V[i] - Lake[i].LayerVol + 1.0;
                Delta_V[i] = Lake[i].LayerVol - 1.0;
            }
        }
    }

    //# Now have Delta_V[i] for all layers and can remove it.
    for (i = botmLayer; i <= surfLayer; i++)
         if (Delta_V[i] >= 1E-5) Lake[i].LayerVol -= Delta_V[i];

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
AED_REAL do_outflows(int jday)
{
    int i, j, k;
    AED_REAL DrawHeight = -1; //# Height of withdraw [m]
    AED_REAL maxtemp = -1, mintemp = 100, mindifftemp = 100;
    static AED_REAL *laketemp = NULL;
    int targetlyr = 0; //# layer where modelled temperature is similar to the target temperature (isotherm)
    AED_REAL VolSum = Lake[surfLayer].Vol1; //# Lake volume prior to outflows [Ml]
    AED_REAL SeepDraw = 0.0; //# Seepage volume [Ml]

    if ( Delta_V == NULL ) Delta_V = malloc(sizeof(AED_REAL) * MaxLayers);
    if ( laketemp == NULL ) laketemp = malloc(sizeof(AED_REAL) * MaxLayers);

    /**************************************************************************
     * Do withdrawal for each offtake                                         *
     * Include water quality and particles                                    *
     * offtake type is switch controlled by FloatOff                          *
     **************************************************************************/
    for (i = 0; i < NumOut; i++) {
        AED_REAL tVolSum = Lake[surfLayer].Vol1;

        /**********************************************************************
         * Floating offtake.                                                  *
         * OLev(i) is distance below surface level for the offtake            *
         **********************************************************************/
        if ( Outflows[i].FloatOff || Outflows[i].Type == 2 ) {
            DrawHeight = Lake[surfLayer].Height - Outflows[i].OLev;
            //# Let it sit on the bottom if the lake has drained
            if (DrawHeight < 0.) DrawHeight = 0.;
        /**********************************************************************
         * Outlet type     .                                                  *
         * Type 1 is fixed outlet heights                                     *
         * Type 2 is floating offtake                                         *
         * Type 3 is fixed outlet heights + check for crit. hypol. oxygen     *
         * Type 4 is variable outlet heights for ISOTHERM                     *
         * Type 5 is variable outlet heights for TEMPERATURE TIME SERIES      *
         **********************************************************************/


        /**********************************************************************
         * Outlet type     .                                                  *
         * Type 1 is fixed outlet heights                                     *
         **********************************************************************/
        } else if (Outflows[i].Type == 1) {
            DrawHeight = Outflows[i].OLev; //# Fixed height offtake
        /**********************************************************************
         * Type 3 is fixed outlet heights + check for crit. hypol. oxygen     *
         **********************************************************************/
        } else if (Outflows[i].Type == 3) {
            int idx_dep = -1;
            if (myfile == NULL) {
                myfile = fopen("outlet_values_type_3.txt","w");
                fprintf(myfile,"JDay,OutletType,CritOXY,DrawHeight,ActOXY\n");
            }
            idx_dep = -1;
            for (k = 0; k < NumLayers; k++) {
                if (Lake[k].Height >=  (O2critdep-Base)) {
                    if (idx_dep < 0) {
                        idx_dep = k;
                    }
                }
            }
            if (jday < checkjday) {
               DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                if (DrawHeight > Lake[NumLayers-1].Height)   //get TargetLayerTemp
                    DrawHeight = Lake[NumLayers-1].Height;
                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
            } else {
                checkjday = -1;
                if (_WQ_Vars(Outflows[i].O2idx,idx_dep) <= O2crit) {  //get modelled O2 conc. via _WQ_Vars(var,lyr)
                    if (checkjday < 0)
                        checkjday = jday+O2critdays;
                    DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                    if (DrawHeight > Lake[NumLayers-1].Height)
                        DrawHeight = Lake[NumLayers-1].Height;
                    else if (Outflows[i].Hcrit < Base) {
                        DrawHeight = 1; //1 m above bottom
                        fprintf(stderr,"outlet_crit %12.4lf < base_elev %12.4lf - set to %12.4lf\n",Outflows[i].Hcrit,Base,Base+1);
                    }
                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                } else {
                    DrawHeight = Outflows[i].OLev;            //# Fixed height offtake
                    if (DrawHeight > Lake[NumLayers-1].Height)   //get TargetLayerTemp
                        DrawHeight = Lake[NumLayers-1].Height;
                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                }
            }
        /**********************************************************************
         * Type 4 is variable outlet heights for ISOTHERM                     *
         **********************************************************************/
        } else if (Outflows[i].Type == 4) {
            int idx_dep = -1;
            int idx_lay = -1;
            AED_REAL TARGETtemp = -1, MASSdepvarwith = 0, MASSbotout = 0, TEMPbotout = 0, TEMPdepvarwith = 0;
            if (myfile == NULL) {
                myfile = fopen("outlet_values_type_4.txt","w");
                fprintf(myfile,"JDay,OutletType,CritOXY,TargetTemp,LakeMAXTemp,LakeMINTemp,within_temp_range,within_facility_range,upper_bound,lower_bound,DrawHeight,ActOXY,mix_withdraw,DISdepvarwith,DISbotout,Tmix\n");
            }
            idx_dep = -1;
            for (k = 0; k < NumLayers; k++) {
                if (Lake[k].Height >=  (O2critdep-Base)) {
                    if (idx_dep < 0) {
                        idx_dep = k;
                    }
                }
            }
            if (jday < checkjday) {
                //AED_REAL outtemp = -1;
                //int idx = -1;
                DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                if (DrawHeight > Lake[NumLayers-1].Height)   //get TargetLayerTemp
                    DrawHeight = Lake[NumLayers-1].Height;
                fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
            }
            else {
                checkjday = -1;
                if (_WQ_Vars(Outflows[i].O2idx,idx_dep) <= O2crit) {  //get modelled O2 conc. via _WQ_Vars(var,lyr)
                    if (checkjday < 0)
                        checkjday = jday+O2critdays;
                    DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                    if (DrawHeight > Lake[NumLayers-1].Height)
                        DrawHeight = Lake[NumLayers-1].Height;
                    else if (Outflows[i].Hcrit < Base) {
                        DrawHeight = 1; //1 m above bottom
                        fprintf(stderr,"outlet_crit %12.4lf < base_elev %12.4lf - set to %12.4lf\n",Outflows[i].Hcrit,Base,Base+1);
                    }
                    fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                } else {
                    //find maximum of Lake.Temp
                    for (j = 0; j < NumLayers; j++) {
                        if (maxtemp < Lake[j].Temp) {
                            //idxmax = j;
                            maxtemp = Lake[j].Temp;
                        }
                    }
                    //find minimum of Lake.Temp
                    for (j = 0; j < NumLayers; j++) {
                        if (mintemp > Lake[j].Temp) {
                            //idxmin = j;
                            mintemp = Lake[j].Temp;
                        }
                    }
                    //restriction: maximum 800 l/s for depth-variable withdrawal
                    if (MIXwithdraw && i != NumOut && (Outflows[i+1].Draw * Outflows[i+1].Factor) > 0) {
                        AED_REAL TEMPdepvarwith_real = -1;
                        AED_REAL Tmix = -1;
                        //if (myfile2 == NULL) {
                        //  myfile2 = fopen("outlet_values_type_4_mix_withdraw.txt","w");
                        //  fprintf(myfile2,"JDay,OutletType,CritOXY,TargetTemp_from_file_or_isotherm,MASSdepvarwith,DISdepvarwith,MASSbotout,DISbotout,idx_lay,TEMPbotout,TEMPdepvarwith\n");
                        //}
                        //Mixing withdrawals: due to constructional limitation of depth-variable withdrawal a maximum discharge of X l/s can be
                        // withdrawn from it - rest of water for downstream river should be withdrawn from bottom outlet
                        //BUT: this means that the water is mixed and the target temperature for the downstream river is not the same like the water from the
                        // depth-variable withdrawal anymore
                        //THEREFORE: calculating water temperature for the depth-variable withdrawal on basis of a mixing temperature == target temperature!
                        //RESTRICTION: the outflowX.csv file with the discharges for the depth-variable withdrawal must be the last but one outflow (NumOut-1)

                        MASSdepvarwith = (Outflows[i].Draw * Outflows[i].Factor); //mass of water for depth-variable withdrawal
                        MASSbotout = (Outflows[i+1].Draw * Outflows[i+1].Factor); //mass of water for bottom outlet
                        //if (MASSbotout > 0) {//mix only if more than 800 l/s must be withdrawn to downstream river
                        //find the layer where bottom outlet lies
                        idx_lay = -1;
                        for (k = 0; k < NumLayers; k++) {
                            if (Lake[k].Height >=  Outflows[i+1].OLev) {
                                if (idx_lay < 0) {
                                    idx_lay = k;
                                }
                            }
                        }
                        TEMPbotout = Lake[idx_lay].Temp; //temperature of bottom outlet layer
                        TEMPdepvarwith = (Outflows[i].TARGETtemp*(MASSdepvarwith+MASSbotout)-(MASSbotout*TEMPbotout)) / MASSdepvarwith; //calculate temperature for depth-variable withdrawal from mixing temperature
                        TARGETtemp = TEMPdepvarwith;
                        //fprintf(myfile2,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,MASSdepvarwith,MASSbotout,MASSdepvarwith/86400,MASSbotout/86400,idx_lay,TEMPbotout,TEMPdepvarwith);
                        if (TARGETtemp > maxtemp) {// check if lake temperature is lower than target temperature
                            DrawHeight = (167.1-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            idx_lay = -1;
                            for (k = 0; k < NumLayers; k++) {
                                if (Lake[k].Height >=  DrawHeight) {
                                    if (idx_lay < 0) {
                                        idx_lay = k;
                                    }
                                }
                            }
                            TEMPdepvarwith_real = Lake[idx_lay].Temp;
                            Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                            //fprintf(stderr,"TARGETtemp > maxtemp\n");
                            //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,0,999,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                        } else if (TARGETtemp < mintemp) {// check if lake temperature is higher than target temperature
                            DrawHeight = (150.9-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            idx_lay = -1;
                            for (k = 0; k < NumLayers; k++) {
                                if (Lake[k].Height >=  DrawHeight) {
                                    if (idx_lay < 0) {
                                        idx_lay = k;
                                    }
                                }
                            }
                            TEMPdepvarwith_real = Lake[idx_lay].Temp;
                            Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                            //fprintf(stderr,"TARGETtemp < maxtemp\n");
                            //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,0,999,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                        } else {
                            AED_REAL TEMPdepvarwith_real = -1;
                            AED_REAL Tmix = -1;
                            //search for layer where target temperature (isotherm temperature) can be found
                            for (j = 0; j < NumLayers; j++)
                                laketemp[j] = fabs(Lake[j].Temp-TARGETtemp);
                            //find minimum of laketemp
                            mindifftemp = 100;
                            for (j = 0; j < NumLayers; j++) {
                                if (mindifftemp > laketemp[j]) {
                                    //targetlyr = j;
                                    mindifftemp = laketemp[j];
                                }
                            }
                            for (j = 0; j < NumLayers; j++) {
                                if (laketemp[j] == mindifftemp)
                                    targetlyr = j;
                            }
                            // check if layer is in range of facility!
                            // old way: if (Lake[i].Height(targetlyr) > 167.1)
                            if (Lake[targetlyr].Height >= (167.1-Base)) {
                                DrawHeight = (167.1-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                idx_lay = -1;
                                for (k = 0; k < NumLayers; k++) {
                                    if (Lake[k].Height >=  DrawHeight) {
                                        if (idx_lay < 0) {
                                            idx_lay = k;
                                        }
                                    }
                                }
                                TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                //fprintf(stderr,"TARGETlayer > 167.1\n");
                                //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                            } else if (Lake[targetlyr].Height <= (150.9-Base)) {
                                DrawHeight = (150.9-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                idx_lay = -1;
                                for (k = 0; k < NumLayers; k++) {
                                    if (Lake[k].Height >=  DrawHeight) {
                                        if (idx_lay < 0) {
                                        idx_lay = k;
                                        }
                                    }
                                }
                                TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                //fprintf(stderr,"TARGETlayer < 150.9\n");
                                //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                            } else {
                                DrawHeight = Lake[targetlyr].MeanHeight; // combine layer index with mean height
                                if (DrawHeight <= (150.9-Base)) {
                                    DrawHeight = (150.9-Base);
                                    if (DrawHeight > Lake[NumLayers-1].Height)
                                        DrawHeight = Lake[NumLayers-1].Height;
                                    idx_lay = -1;
                                    for (k = 0; k < NumLayers; k++) {
                                        if (Lake[k].Height >=  DrawHeight) {
                                            if (idx_lay < 0) {
                                            idx_lay = k;
                                            }
                                        }
                                    }
                                    TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                    Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                    //fprintf(stderr,"TARGETlayer > 150.9 & < 167.1 but lower limit\n");
                                    //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                                } else {
                                    idx_lay = -1;
                                    for (k = 0; k < NumLayers; k++) {
                                        if (Lake[k].Height >=  DrawHeight) {
                                            if (idx_lay < 0) {
                                            idx_lay = k;
                                            }
                                        }
                                    }
                                    TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                    Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                    //fprintf(stderr,"TARGETlayer in range\n");
                                    //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,1,0,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                                }
                            }
                        }
                    } else {
                        TARGETtemp = Outflows[i].TARGETtemp;

                        if (TARGETtemp > maxtemp) {// check if lake temperature is lower than target temperature
                            DrawHeight = (167.1-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,0,999,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                        } else if (TARGETtemp < mintemp) {// check if lake temperature is higher than target temperature
                            DrawHeight = (150.9-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,0,999,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                        } else {
                            //search for layer where target temperature (isotherm temperature) can be found
                            for (j = 0; j < NumLayers; j++)
                                laketemp[j] = fabs(Lake[j].Temp-TARGETtemp);

                            //find minimum of laketemp
                            mindifftemp = 100;
                            for (j = 0; j < NumLayers; j++) {
                                if (mindifftemp > laketemp[j]) {
                                    //targetlyr = j;
                                    mindifftemp = laketemp[j];
                                }
                            }
                            for (j = 0; j < NumLayers; j++) {
                                if (laketemp[j] == mindifftemp)
                                    targetlyr = j;
                            }
                            // check if layer is in range of facility!
                            // old way: if (Lake[i].Height(targetlyr) > 167.1)
                            if (Lake[targetlyr].Height >= (167.1-Base)) {
                                DrawHeight = (167.1-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            } else if (Lake[targetlyr].Height <= (150.9-Base)) {
                                DrawHeight = (150.9-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            } else {
                                DrawHeight = Lake[targetlyr].MeanHeight; // combine layer index with mean height
                                if (DrawHeight <= (150.9-Base)) {
                                    DrawHeight = (150.9-Base);
                                    if (DrawHeight > Lake[NumLayers-1].Height)
                                        DrawHeight = Lake[NumLayers-1].Height;
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                                } else
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,maxtemp,mintemp,1,1,0,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            }
                        }
                    }
                }
            }
        /**********************************************************************
         * Type 5 is variable outlet heights for TEMPERATURE TIME SERIES      *
         **********************************************************************/
        } else if (Outflows[i].Type == 5) {
            int idx_dep = -1;
            int idx_lay = -1;
            AED_REAL TARGETtemp = -1, MASSdepvarwith = 0, MASSbotout = 0, TEMPbotout = 0, TEMPdepvarwith = 0;
            //fprintf(stderr,"WithdrawalTemp %12.4lf at day %8d\n",WithdrawalTemp,jday);
            if (myfile == NULL) {
                    myfile = fopen("outlet_values_type_5.txt","w");
                    //fprintf(myfile,"JDay,OutletType,CritOXY,TargetTemp,LakeMAXTemp,LakeMINTemp,within_temp_range,within_facility_range,upper_bound,lower_bound,DrawHeight,ActOXY\n");
                    fprintf(myfile,"JDay,OutletType,CritOXY,TargetTemp,LakeMAXTemp,LakeMINTemp,within_temp_range,within_facility_range,upper_bound,lower_bound,DrawHeight,ActOXY,mix_withdraw,DISdepvarwith,DISbotout,Tmix\n");
            }
            idx_dep = -1;
            for (k = 0; k < NumLayers; k++) {
                if (Lake[k].Height >=  (O2critdep-Base)) {
                    if (idx_dep < 0) {
                        idx_dep = k;
                    }
                }
            }
            if (jday < checkjday) {
                DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                if (DrawHeight > Lake[NumLayers-1].Height)   //get TargetLayerTemp
                    DrawHeight = Lake[NumLayers-1].Height;
                //fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
            } else {
                checkjday = -1;
                if (_WQ_Vars(Outflows[i].O2idx,idx_dep) <= O2crit) {  //get modelled O2 conc. via _WQ_Vars(var,lyr)
                    if (checkjday < 0)
                        checkjday = jday+O2critdays;
                    DrawHeight = Outflows[i].Hcrit-Base;               //# withdraw at another fixed height (e.g. bottom outlet)
                    if (DrawHeight > Lake[NumLayers-1].Height)
                        DrawHeight = Lake[NumLayers-1].Height;
                    else if (Outflows[i].Hcrit < Base) {
                        DrawHeight = 1; //1 m above bottom
                        fprintf(stderr,"outlet_crit %12.4lf < base_elev %12.4lf - set to %12.4lf\n",Outflows[i].Hcrit,Base,Base+1);
                    }
                    //fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                    fprintf(myfile,"%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,1,999,999,999,999,999,999,999,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                } else {
                    //find maximum of Lake.Temp
                    for (j = 0; j < NumLayers; j++) {
                        if (maxtemp < Lake[j].Temp) {
                            maxtemp = Lake[j].Temp;
                        }
                    }
                    //find minimum of Lake.Temp
                    for (j = 0; j < NumLayers; j++) {
                        if (mintemp > Lake[j].Temp) {
                            mintemp = Lake[j].Temp;
                        }
                    }


                    //restriction: maximum 800 l/s for depth-variable withdrawal
                    if (MIXwithdraw && i != NumOut && (Outflows[i+1].Draw * Outflows[i+1].Factor) > 0) {
                        AED_REAL TEMPdepvarwith_real = -1;
                        AED_REAL Tmix = -1;
                        //if (myfile2 == NULL) {
                        //  myfile2 = fopen("outlet_values_type_4_mix_withdraw.txt","w");
                        //  fprintf(myfile2,"JDay,OutletType,CritOXY,TargetTemp_from_file_or_isotherm,MASSdepvarwith,DISdepvarwith,MASSbotout,DISbotout,idx_lay,TEMPbotout,TEMPdepvarwith\n");
                        //}
                        //Mixing withdrawals: due to constructional limitation of depth-variable withdrawal a maximum discharge of X l/s can be
                        // withdrawn from it - rest of water for downstream river should be withdrawn from bottom outlet
                        //BUT: this means that the water is mixed and the target temperature for the downstream river is not the same like the water from the
                        // depth-variable withdrawal anymore
                        //THEREFORE: calculating water temperature for the depth-variable withdrawal on basis of a mixing temperature == target temperature!
                        //RESTRICTION: the outflowX.csv file with the discharges for the depth-variable withdrawal must be the last but one outflow (NumOut-1)

                        MASSdepvarwith = (Outflows[i].Draw * Outflows[i].Factor); //mass of water for depth-variable withdrawal
                        MASSbotout = (Outflows[i+1].Draw * Outflows[i+1].Factor); //mass of water for bottom outlet
                        //if (MASSbotout > 0) {//mix only if more than 800 l/s must be withdrawn to downstream river
                        //find the layer where bottom outlet lies
                        idx_lay = -1;
                        for (k = 0; k < NumLayers; k++) {
                            if (Lake[k].Height >=  Outflows[i+1].OLev) {
                                if (idx_lay < 0) {
                                    idx_lay = k;
                                }
                            }
                        }
                        TEMPbotout = Lake[idx_lay].Temp; //temperature of bottom outlet layer
                        TEMPdepvarwith = (WithdrawalTemp*(MASSdepvarwith+MASSbotout)-(MASSbotout*TEMPbotout)) / MASSdepvarwith; //calculate temperature for depth-variable withdrawal from mixing temperature
                        TARGETtemp = TEMPdepvarwith;
                        //fprintf(myfile2,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,Outflows[i].TARGETtemp,MASSdepvarwith,MASSbotout,MASSdepvarwith/86400,MASSbotout/86400,idx_lay,TEMPbotout,TEMPdepvarwith);
                        if (TARGETtemp > maxtemp) {// check if lake temperature is lower than target temperature
                            DrawHeight = (167.1-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            idx_lay = -1;
                            for (k = 0; k < NumLayers; k++) {
                                if (Lake[k].Height >=  DrawHeight) {
                                    if (idx_lay < 0) {
                                        idx_lay = k;
                                    }
                                }
                            }
                            TEMPdepvarwith_real = Lake[idx_lay].Temp;
                            Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                            //fprintf(stderr,"TARGETtemp > maxtemp\n");
                            //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                        } else if (TARGETtemp < mintemp) {// check if lake temperature is higher than target temperature
                            DrawHeight = (150.9-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            idx_lay = -1;
                            for (k = 0; k < NumLayers; k++) {
                                if (Lake[k].Height >=  DrawHeight) {
                                    if (idx_lay < 0) {
                                        idx_lay = k;
                                    }
                                }
                            }
                            TEMPdepvarwith_real = Lake[idx_lay].Temp;
                            Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                            //fprintf(stderr,"TARGETtemp < maxtemp\n");
                            //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                        } else {
                            AED_REAL TEMPdepvarwith_real = -1;
                            AED_REAL Tmix = -1;
                            //search for layer where target temperature (isotherm temperature) can be found
                            for (j = 0; j < NumLayers; j++)
                                laketemp[j] = fabs(Lake[j].Temp-TARGETtemp);
                            //find minimum of laketemp
                            mindifftemp = 100;
                            for (j = 0; j < NumLayers; j++) {
                                if (mindifftemp > laketemp[j]) {
                                    //targetlyr = j;
                                    mindifftemp = laketemp[j];
                                }
                            }
                            for (j = 0; j < NumLayers; j++) {
                                if (laketemp[j] == mindifftemp)
                                    targetlyr = j;
                            }
                            // check if layer is in range of facility!
                            // old way: if (Lake[i].Height(targetlyr) > 167.1)
                            if (Lake[targetlyr].Height >= (167.1-Base)) {
                                DrawHeight = (167.1-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                idx_lay = -1;
                                for (k = 0; k < NumLayers; k++) {
                                    if (Lake[k].Height >=  DrawHeight) {
                                        if (idx_lay < 0) {
                                            idx_lay = k;
                                        }
                                    }
                                }
                                TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                //fprintf(stderr,"TARGETlayer > 167.1\n");
                                //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                            } else if (Lake[targetlyr].Height <= (150.9-Base)) {
                                DrawHeight = (150.9-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                idx_lay = -1;
                                for (k = 0; k < NumLayers; k++) {
                                    if (Lake[k].Height >=  DrawHeight) {
                                        if (idx_lay < 0) {
                                        idx_lay = k;
                                        }
                                    }
                                }
                                TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                //fprintf(stderr,"TARGETlayer < 150.9\n");
                                //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                            } else {
                                DrawHeight = Lake[targetlyr].MeanHeight; // combine layer index with mean height
                                if (DrawHeight <= (150.9-Base)) {
                                    DrawHeight = (150.9-Base);
                                    if (DrawHeight > Lake[NumLayers-1].Height)
                                        DrawHeight = Lake[NumLayers-1].Height;
                                    idx_lay = -1;
                                    for (k = 0; k < NumLayers; k++) {
                                        if (Lake[k].Height >=  DrawHeight) {
                                            if (idx_lay < 0) {
                                            idx_lay = k;
                                            }
                                        }
                                    }
                                    TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                    Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                    //fprintf(stderr,"TARGETlayer > 150.9 & < 167.1 but lower limit\n");
                                    //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                                } else {
                                    idx_lay = -1;
                                    for (k = 0; k < NumLayers; k++) {
                                        if (Lake[k].Height >=  DrawHeight) {
                                            if (idx_lay < 0) {
                                            idx_lay = k;
                                            }
                                        }
                                    }
                                    TEMPdepvarwith_real = Lake[idx_lay].Temp;
                                    Tmix = ((TEMPdepvarwith_real*MASSdepvarwith)+(TEMPbotout*MASSbotout))/(MASSdepvarwith+MASSbotout);
                                    //fprintf(stderr,"TARGETlayer in range\n");
                                    //fprintf(stderr,"%12.4lf,%12.4lf\n",TEMPdepvarwith_real,Tmix);
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%12.4lf,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,1,0,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),1,(Outflows[i].Draw * Outflows[i].Factor),(Outflows[i+1].Draw * Outflows[i+1].Factor),Tmix);
                                }
                            }
                        }
                    } else {
                        TARGETtemp = WithdrawalTemp;

                        if (TARGETtemp > maxtemp) {// check if lake temperature is lower than target temperature
                            DrawHeight = (167.1-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                        } else if (TARGETtemp < mintemp) {// check if lake temperature is higher than target temperature
                            DrawHeight = (150.9-Base);
                            if (DrawHeight > Lake[NumLayers-1].Height)
                                DrawHeight = Lake[NumLayers-1].Height;
                            //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                            fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,0,999,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                        } else {
                            //search for layer where target temperature (isotherm temperature) can be found
                            for (j = 0; j < NumLayers; j++)
                                laketemp[j] = fabs(Lake[j].Temp-TARGETtemp);

                            //find minimum of laketemp
                            mindifftemp = 100;
                            for (j = 0; j < NumLayers; j++) {
                                if (mindifftemp > laketemp[j]) {
                                    mindifftemp = laketemp[j];
                                }
                            }
                            for (j = 0; j < NumLayers; j++) {
                                if (laketemp[j] == mindifftemp)
                                    targetlyr = j;
                            }
                            // check if layer is in range of facility!
                            // old way: if (Lake[i].Height(targetlyr) > 167.1)
                            if (Lake[targetlyr].Height >= (167.1-Base)) {
                                DrawHeight = (167.1-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,1,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            } else if (Lake[targetlyr].Height <= (150.9-Base)) {
                                DrawHeight = (150.9-Base);
                                if (DrawHeight > Lake[NumLayers-1].Height)
                                    DrawHeight = Lake[NumLayers-1].Height;
                                //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                                fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            } else {
                                DrawHeight = Lake[targetlyr].MeanHeight; // combine layer index with mean height
                                if (DrawHeight <= (150.9-Base)) {
                                    DrawHeight = (150.9-Base);
                                    if (DrawHeight > Lake[NumLayers-1].Height)
                                        DrawHeight = Lake[NumLayers-1].Height;
                                    //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,0,0,1,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                                } else
                                    //fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,1,0,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep));
                                    fprintf(myfile,"%8d,%8d,%8d,%12.4lf,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d,%12.4lf,%12.4lf,%8d,%8d,%8d,%8d\n",jday,Outflows[i].Type,0,WithdrawalTemp,maxtemp,mintemp,1,1,0,0,DrawHeight,_WQ_Vars(Outflows[i].O2idx,idx_dep),999,999,999,999);
                            }
                        }
                    }
                }
            }
        }

        Outflows[i].Draw *= Outflows[i].Factor;

        do_single_outflow(DrawHeight, Outflows[i].Draw, &Outflows[i]);

        write_outflow(i, jday, DrawHeight, tVolSum - Lake[surfLayer].Vol1);
    }
    if (seepage) {
        SeepDraw = (seepage_rate / Lake[botmLayer].Height) * Lake[botmLayer].LayerVol;
        do_single_outflow(0., SeepDraw, NULL);
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
AED_REAL do_overflow(int jday)
{
    AED_REAL VolSum = Lake[surfLayer].Vol1;
    AED_REAL DrawHeight = 0.;

    if (VolSum > VolAtCrest)
        do_single_outflow(CrestLevel, (VolSum - VolAtCrest), NULL);

    write_outflow(MaxOut, jday, DrawHeight, VolSum - Lake[surfLayer].Vol1);

    return VolSum - Lake[surfLayer].Vol1;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static int insert_inflow(int k, //#Inflow element count
    int iRiver,                 //# River number
    AED_REAL Depth_t0,          //# Depth of lake before inflows [m]
    AED_REAL Alpha,             //# Stream half angle in radians
    AED_REAL Phi,               //# Stream slope in radians
    AED_REAL EntrainmentCoeff,  //# Entrainment coefficient [-]
    AED_REAL Ri)                //# Bulk Richardson number of the inflow [-]
{
    AED_REAL Inflow_Energy;    //# Energy of inflow
    AED_REAL Inflow_time;      //# Time taken for inflow to reach neutral buoyancy or bottom of lake [days]
    int Layer;                 //# Layer number adjacent to underflow, entrainment layer
    int Layer_t0;              //# Layer number at which inflow starts the day
    int kk;
    int flag;
    int wqidx;

    AED_REAL Delta_Q;          //# Delta Q the rate of entrainment of inflow aliquot [ML/day]
    AED_REAL Delta_t;          //# Delta t the time taken for downflow [days]
    AED_REAL Downflow_Depth;   //# Depth of downflow [m]
    AED_REAL Inflow_Density;   //# Density of the inflow
    AED_REAL Inflow_dx;        //# Distance travelled by inflow [m]
    AED_REAL Inflow_gprime;    //# Reduced gravity (gprime) between inflow and layer densities [-]
    AED_REAL Inflow_height_prev; //# Thickness of inflow from previous layer [m]
    AED_REAL Inflow_height;    //# Thickness of inflow post entrainment [m]
    AED_REAL Inflow_Flowrate;  //# Inflow flowrate [ML/day]
    AED_REAL Inflow_Salinity;  //# Inflow salinity [psu]
    AED_REAL Inflow_Temp;      //# Inflow temperature [oC]
    AED_REAL Inflow_Velocity;  //# Inflow velocity [m/s]

/*----------------------------------------------------------------------------*/
//BEGIN

    //# Check to see if have reached the number of inflows for this day.
    if (k >= Inflows[iRiver].iCnt) return FALSE;

    Inflow_Energy = zero;

    //# If fresh inflow for that day set depth as lake height
    if (Inflows[iRiver].DDown[k] == MISVAL) {
        Inflows[iRiver].TotIn += Inflows[iRiver].QDown[k];
        Inflows[iRiver].DDown[k] = Depth_t0;
    }

    //# Initialise inflow time
    Inflow_time = zero;
    //# Set depth inflow aliquot reached on previous day (DOld)
    Inflows[iRiver].DOld[k] = Inflows[iRiver].DDown[k];
    //# Calculate inflow density for this inflow aliquot
    Inflow_Density = calculate_density(Inflows[iRiver].TDown[k], Inflows[iRiver].SDown[k]);

    //# Calculate the layer in which this element starts the day.
    for (Layer_t0 = botmLayer; Layer_t0 <= surfLayer; Layer_t0++)
        if (Lake[Layer_t0].Height >= Inflows[iRiver].DDown[k]) break;

    if (Layer_t0 > surfLayer) Layer_t0 = surfLayer; //# Limit to surface layer
    Layer = Layer_t0;

    //# Loop for progression of an element through the next layer down.
    Inflow_Flowrate = Inflows[iRiver].QDown[k];

    Inflow_Temp = Inflows[iRiver].TDown[k];
    Inflow_Salinity = Inflows[iRiver].SDown[k];

    for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
        WQ_VarsS[wqidx] = Inflows[iRiver].WQDown[k][wqidx];

    Downflow_Depth = Inflows[iRiver].DDown[k];

    //# Check if this element lies below level of neutral buoyancy.
    //# If it is then add to insertion queue and renumber the stack elements.
    if (Inflow_Density <= Lake[Layer].Density){
        Inflows[iRiver].InPar[Inflows[iRiver].NoIns] = k;
        Inflows[iRiver].TIns[Inflows[iRiver].NoIns]  = Inflow_Temp;
        Inflows[iRiver].SIns[Inflows[iRiver].NoIns]  = Inflow_Salinity;
        Inflows[iRiver].DIIns[Inflows[iRiver].NoIns] = Inflow_Density;
        Inflows[iRiver].QIns[Inflows[iRiver].NoIns]  = Inflow_Flowrate;

        for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
            Inflows[iRiver].WQIns[Inflows[iRiver].NoIns][wqidx] = WQ_VarsS[wqidx];

        Inflows[iRiver].NoIns++;

        return TRUE;
    }

    //# Calculate the velocity of the inflow and hence the entrainment.
    while(1) {
        //# Reduced gravity of downflow (Eq. x in GLM manual)
        Inflow_gprime = g*(Inflow_Density-Lake[Layer].Density)/Lake[Layer].Density;

        //# Initial height of inflow plunge (Eq. x in GLM manual)
        Inflow_height_prev = pow((2.0*Ri*pow((Inflow_Flowrate/SecsPerDay*cos(Alpha)/sin(Alpha)), 2) / Inflow_gprime), 0.2);

        //# Distance travelled by inflow aliquot (Eq. x in GLM manual)
        if (Layer == botmLayer) Inflow_dx = Downflow_Depth/sin(Phi);
        else                    Inflow_dx = (Downflow_Depth-Lake[Layer-1].Height)/sin(Phi);

        //# New inflow thickness due to entrainment (Eq. x in GLM manual)
        Inflow_height = 1.2 * EntrainmentCoeff * Inflow_dx  +  Inflow_height_prev;

        //# Inflow velocity in m/day (Eq. x in GLM manual)
        Inflow_Velocity = Inflow_Flowrate * cos(Alpha) / (pow(Inflow_height, 2) * sin(Alpha));

        //# Time taken for inflow aliquot to travel in this day
        Delta_t = Inflow_dx/Inflow_Velocity;

        //# If time is greater than one day then carry on to next day and only increment this day's entrainment
        //# to calculation of new inflow layer thickness
        if ((Inflow_time + Delta_t) > 1.0) {
            Inflow_dx = Inflow_dx * (1.0-Inflow_time) / Delta_t;
            Delta_t = 1.0-Inflow_time;
            Inflow_height = 1.2 * EntrainmentCoeff * Inflow_dx  +  Inflow_height_prev;
        }

        Inflow_time += Delta_t;

        //# Estimate increase in flow rate due to entrainment for this day (Eq. x in GLM manual)
        Delta_Q = 0.2 * Inflow_Flowrate * (pow((Inflow_height/Inflow_height_prev), (5.0/3.0)) - 1.0);

        //# Check for negative inflow layer volume
        if (Lake[Layer].LayerVol < 0.0)
            fprintf(stderr, "Vol[%d] is negative = %f, surfLayer = %d\n", Layer, Lake[Layer].LayerVol, surfLayer);

        //# Check that the entrainment is less than 90% of the layer volumne
        if (Delta_Q > 0.9*Lake[Layer].LayerVol) Delta_Q = 0.9 * Lake[Layer].LayerVol;

        //# Determine physical properties of inflow aliquot once water from adjacent layer has been entrained
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
            Inflow_Energy += (Inflow_Density - Lake[Layer].Density) * Downflow_Depth * g * Inflow_Flowrate / SecsPerDay;
        else
            Inflow_Energy += (Inflow_Density - Lake[Layer].Density) * (Downflow_Depth - Lake[Layer-1].Height) * g * Inflow_Flowrate / SecsPerDay;

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
        if (Inflow_time >= 1.0)  flag = TRUE ;

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
        else if (Inflow_time >= 1.0)
            flag = TRUE;
        else if (Inflow_Density <= Lake[Layer-1].Density)
            flag = TRUE;

        if ( flag ) { //#Insert inflow parcel and re-calculate cumulative volumes return to do_inflows
            einff += Inflow_Energy/Inflow_time;
            if (Layer == botmLayer) Lake[botmLayer].Vol1 = Lake[botmLayer].LayerVol;
            if (Layer != botmLayer) Lake[Layer].Vol1 = Lake[Layer-1].Vol1 + Lake[Layer].LayerVol;
            for (kk = Layer+1; kk <= surfLayer; kk++)
                 Lake[kk].Vol1 = Lake[kk-1].Vol1 + Lake[kk].LayerVol;

            return TRUE;;
        }
        Layer--;
    }
    return FALSE;

}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Inserts the inflow at the level of neutral buoyancy, after calculating the *
 * entrainment                                                                *
 * Return the difference between total volume before and after.               *
 ******************************************************************************/
AED_REAL do_inflows()
{
    const AED_REAL cwnsq2 = 39.48;

    AED_REAL Alpha;             //# Stream half angle in radians
    AED_REAL Phi;               //# Stream slope in radians
    AED_REAL DragCoeff;         //# Stream drag coefficient
    AED_REAL EntrainmentCoeff;  //# Entrainment coefficient
    AED_REAL Ri;                //# Bulk Richardson number of the inflow [-]
    AED_REAL Inflow_Density;    //# Density of the inflow
    AED_REAL Inflow_Height_t0;  //# Initial inflow plunge point [m]
    AED_REAL Inflow_gprime_t0;  //# Reduced gravity (gprime) given surface and inflow densities

    int j, k, ll;
    int iRiv;
    int jk;
    int iRiver;
    int Layer_subm;             //# Layer number to insert submerged inflow

    int wqidx;

    AED_REAL Inflow_width;    //# Width of inflow [m]
    AED_REAL VolSum = Lake[surfLayer].Vol1; //# Total lake volume before inflows

/*----------------------------------------------------------------------------*/
//BEGIN
    if ( ! WQ_VarsS )   WQ_VarsS = malloc(Num_WQ_Vars * sizeof(AED_REAL));
    if ( ! WQ_VarsTmp ) WQ_VarsTmp = malloc(NumInf*sizeof(wq_partic_t));
    memset(WQ_VarsS, 0, (Num_WQ_Vars * sizeof(AED_REAL)));

    /**************************************************************************
     * Initially:                                                             *
     * 1) Calculate initial reduced gravity and plunge point of the inflow.   *
     * 2) Calculate WaveNumSquared and vel for use in the calculation of deep *
     * water mixing. See do_deep_mixing where they're used to calculate the   *
     * dispersion coefficient, Epsilon.                                       *
     * Count down from smallest river, overwriting each time, in case river   *
     * has zero inflow.                                                       *
     **************************************************************************/
    WaveNumSquared = zero;
    vel = zero;
    for (iRiver = NumInf-1; iRiver >= 0; iRiver--) {
        if (Inflows[iRiver].FlowRate*Inflows[iRiver].Factor != zero && !Inflows[iRiver].SubmFlag) {
            Alpha = Inflows[iRiver].Alpha;
            Phi = Inflows[iRiver].Phi;
            Inflow_Density = calculate_density(Inflows[iRiver].TemInf,Inflows[iRiver].SalInf);
            //# Reduced gravity of inflow (Eq. X in GLM manual)
            Inflow_gprime_t0 = gprime(Lake[surfLayer].Density,Inflow_Density);

            if (Inflow_gprime_t0 > zero) //#inflow density > surface
                //# Initial plunge point (Eq. x in GLM manual)
                Inflow_Height_t0 = pow(Inflows[iRiver].FlowRate * Inflows[iRiver].Factor / SecsPerDay, 0.4)/
                                   pow(Inflow_gprime_t0, 0.2);
            else //#inflow density < surface inflow thickness so plunge to depth of surface layer
                Inflow_Height_t0 = Lake[surfLayer].Height-Lake[surfLayer-1].Height;

            if (Inflow_Height_t0 > Lake[surfLayer].Height) //#Minimum limit = depth of the surface layer
                Inflow_Height_t0 = Lake[surfLayer].Height;

            //# Wave number squared (Eq. y in GLM manual)
            WaveNumSquared = cwnsq2/sqr(Inflow_Height_t0);
            //# Velocity of the plunging inflow (Eq. y in GLM manual)
            vel = 0.1 * Inflows[iRiver].FlowRate*Inflows[iRiver].Factor / SecsPerDay /
                                          (sqr(Inflow_Height_t0)*sin(Alpha)/cos(Alpha));
        }
    }

    /**************************************************************************
     * Here the integer iCnt is an inflow count of the number of inflows      *
     * associated with iRiver for that day including any inflows from         *
     * previous days.                                                         *
     * iCnt is initialised at the beginning of the simulation as zero and     *
     * incremented  each day that the inflow > 0.                             *
     * Once inflow has been inserted, iCnt = iCnt - 1.                        *
     **************************************************************************/

    for (iRiver = 0; iRiver < NumInf; iRiver++) {
        if (Inflows[iRiver].FlowRate * Inflows[iRiver].Factor > zero && !Inflows[iRiver].SubmFlag) {
            Inflows[iRiver].QDown[Inflows[iRiver].iCnt] = Inflows[iRiver].FlowRate * Inflows[iRiver].Factor;
            Inflows[iRiver].TDown[Inflows[iRiver].iCnt] = Inflows[iRiver].TemInf;
            Inflows[iRiver].SDown[Inflows[iRiver].iCnt] = Inflows[iRiver].SalInf;
            Inflows[iRiver].DDown[Inflows[iRiver].iCnt] = MISVAL;

            for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                Inflows[iRiver].WQDown[Inflows[iRiver].iCnt][wqidx] = Inflows[iRiver].WQInf[wqidx];

            Inflows[iRiver].iCnt++;
        }
    }

    /**************************************************************************
     * Work through each element in the downflow stacks and calculate the     *
     * travel distance and entrainment for the present day, and whether or    *
     * not it reaches its level of neutral buoyancy and hence can be inserted.*
     **************************************************************************/
    einff = zero; //# At the start of the day initialise the deltaPE to zero
    iRiver = 0;   //# Start with first inflow
    while(1) {
        while (iRiver < NumInf) {
            if  (!Inflows[iRiver].SubmFlag) {
               Alpha = Inflows[iRiver].Alpha;
               Phi = Inflows[iRiver].Phi;
               DragCoeff = Inflows[iRiver].DragCoeff;
               //# Bulk Richardson's number of the inflow (Eq. x in GLM manual)
               Ri = DragCoeff * ( 1.0 + 0.21 * sqrt(DragCoeff) * sin(Alpha) ) / (sin(Alpha) * sin(Phi) / cos(Phi));

               //# Entrainment coefficient
               EntrainmentCoeff = 1.6 * pow(DragCoeff, 1.5) / Ri;

               Inflows[iRiver].NoIns = 0;  //# Initialise number of insertions as zero

               k = -1;
               //# Loop for calculation of each separate inflow element.
               do  {
                   k++;
               } while (insert_inflow(k, iRiver, Lake[surfLayer].Height,
                                           Alpha, Phi, EntrainmentCoeff, Ri));
            }
            iRiver++;
         }

        /**********************************************************************
         * Insert all of the parcels which reached their level of NB on this  *
         * day.  Adjust the stacking to note the removal.                     *
         **********************************************************************/

        Inflow_width = 0.0;
        for (iRiver = 0; iRiver < NumInf; iRiver++) {
            if  (Inflows[iRiver].SubmFlag) {
               //# Submerged inflows have no momentum, simply insert at specified level and
               //# adjust layer properties including water quality variables
               //# Get layer number at submerged inflow level
               for (Layer_subm = botmLayer; Layer_subm <= surfLayer; Layer_subm++)
                  if (Lake[Layer_subm].Height >=  Inflows[iRiver].DragCoeff) break;
               Lake[Layer_subm].Temp = combine(Lake[Layer_subm].Temp,Lake[Layer_subm].LayerVol,Lake[Layer_subm].Density,
                                               Inflows[iRiver].TemInf,Inflows[iRiver].FlowRate,calculate_density(Inflows[iRiver].TemInf,Inflows[iRiver].SalInf));
               Lake[Layer_subm].Salinity = combine(Lake[Layer_subm].Salinity,Lake[Layer_subm].LayerVol,Lake[Layer_subm].Density,
                                               Inflows[iRiver].SalInf,Inflows[iRiver].FlowRate,calculate_density(Inflows[iRiver].TemInf,Inflows[iRiver].SalInf));

               for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                   _WQ_Vars(wqidx,Layer_subm) = combine_vol(_WQ_Vars(wqidx,Layer_subm),Lake[Layer_subm].LayerVol,Inflows[iRiver].WQInf[wqidx],Inflows[iRiver].FlowRate);

               Lake[Layer_subm].Density=calculate_density(Lake[Layer_subm].Temp,Lake[Layer_subm].Salinity);
               Lake[Layer_subm].LayerVol=Lake[Layer_subm].LayerVol+Inflows[iRiver].FlowRate;

               Lake[botmLayer].Vol1 = Lake[botmLayer].LayerVol;
               if (surfLayer != botmLayer) {
                    for (j = (botmLayer+1); j <= surfLayer; j++)
                        Lake[j].Vol1 = Lake[j-1].Vol1 + Lake[j].LayerVol;
                }
            }
            else {
               Phi = Inflows[iRiver].Phi;
               iRiv = iRiver;

               for (j = 0; j < Inflows[iRiver].NoIns; j++) {
                   for (wqidx = 0; wqidx < Num_WQ_Vars; wqidx++)
                       WQ_VarsTmp[iRiv][j][wqidx] = Inflows[iRiv].WQIns[j][wqidx];

                   insert(Inflows[iRiv].QIns[j], Inflows[iRiv].DIIns[j], Phi,
                              Inflows[iRiv].TIns[j], Inflows[iRiv].SIns[j],
                                         WQ_VarsTmp[iRiv][j], SecsPerDay, &Inflow_width, &ll);

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
                       Inflows[iRiv].Dlwst = MISVAL;
                   }
                   for (k = j; k < Inflows[iRiver].NoIns; k++)
                       Inflows[iRiver].InPar[k]--;
                  }
            }
        }

        //# Reset the number of insertions per river to be zero.
        for (k = 0; k < NumInf; k++) Inflows[k].NoIns = 0;

        //# Calculate the front of the downflow for each river.
        for (iRiver = 0; iRiver < NumInf; iRiver++) {
            Inflows[iRiver].Dlwst = MISVAL;
            for (j = 0; j < Inflows[iRiver].iCnt; j++) {
                if (Inflows[iRiver].DDown[j] < Inflows[iRiver].Dlwst)
                    Inflows[iRiver].Dlwst = Inflows[iRiver].DDown[j];
            }
        }

        iRiv = -1;

        /**********************************************************************
         * If a flow fit error has occured, go back and reroute first inflow  *
         * for the river of concern - iRiv.                                   *
         **********************************************************************/
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
static int flowing_depth(AED_REAL *HF, AED_REAL *DL, AED_REAL Volume,
                                   AED_REAL DEPTH, AED_REAL Phi, AED_REAL Alpha)
{
    AED_REAL Theta, H, TAG, CON;
    int all_ok = TRUE;

/*----------------------------------------------------------------------------*/

    //# Set the coefficients of the cubic.
    H = zero;
    if (Volume > zero) {
        TAG = tan(Alpha) / tan(Phi);

        while(1) {
            CON = DEPTH - *DL;
            if (fabs(1.0 - 6.0 * Volume  / TAG / pow(CON, 3)) <= 1.0) {
                Theta = acos(1.0 - 6.0 * Volume / TAG / pow(CON, 3));
                H = (2.0 * cos(Theta / 3.0) + 1.0) * CON / 2.0;
                if (H > zero && H < CON) break;
                H = (2.0 * cos(Theta / 3.0 + 2.0 * Pi / 3.0) + 1.0) * CON / 2.0;
                if (H > zero && H < CON) break;
                H = (2.0 * cos(Theta / 3.0 + 4.0 * Pi / 3.0) + 1.0) * CON / 2.0;
                if (H > zero && H < CON) break;
            }
            *DL -= 0.25;

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
   int iTop,j,k;

   AED_REAL D;
   AED_REAL DLOW;
   AED_REAL DOL;
   AED_REAL EXTRA;
   AED_REAL LAYPRO;
   AED_REAL LayerVol;
   AED_REAL VlSum;
   AED_REAL VOLSUM;
   AED_REAL MphLevelVoltemp;

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
    Lake[surfLayer].Height = ((j+1) + (VOLSUM - MphLevelVol[j])/dMphLevelVol[j]) * 0.1;

    //# Calculate the flowing depths of the inflowing streams.
    //#
    DLOW = MISVAL;
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
    iTop = round(Lake[surfLayer].Height * 10.0) - 1;
    if ( iTop >= Nmorph ) iTop = Nmorph - 1;
    EXTRA = zero;
    DOL = zero;

    for (j = botmLayer; j <= iTop; j++) {
        D = (j+1) / 10.0;
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
        if (MphLevelVoldash[j] < (MphLevelVoltemp + (1.0 - LAYPRO) * LayerVol))
            MphLevelVoldash[j] =  MphLevelVoltemp + (1.0 - LAYPRO) * LayerVol;
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
static AED_REAL extra_volume(AED_REAL d1, AED_REAL d2,
                                  AED_REAL SurfaceHeight, InflowDataType *Inflow)
{
    AED_REAL DTOP, DBOT, SUM, TAG, ZB;
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
                SUM += TAG*(pow(DTOP-ZB,3)-pow(DBOT-ZB,3))/3.0;
            else if (DBOT > Inflow[k].Dlwst + Inflow[k].HFlow)
                SUM += TAG*pow(Inflow[k].HFlow,2)*(DTOP-DBOT);
            else
                SUM += TAG*(pow(Inflow[k].HFlow,3)-pow(DBOT-ZB,3))/3.0 +
                                TAG*pow(Inflow[k].HFlow,2) *
                                   (DTOP-Inflow[k].Dlwst-Inflow[k].HFlow);
        }
    }
    return SUM;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
