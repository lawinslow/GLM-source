/******************************************************************************
 *                                                                            *
 * glm_mixer.c                                                                *
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

//#define dbgprt(...) fprintf(stderr, __VA_ARGS__)
#define dbgprt(...) /* __VA_ARGS__ */

/******************************************************************************/
#include "glm_types.h"
#include "glm_globals.h"
#include "glm_mixu.h"
#include "glm_util.h"
#include "aed_time.h"

/*============================================================================*/

REALTYPE DepMX   = 0.;

static   REALTYPE Epi_dz;     //# Thickness of epilimnion [m]
static   REALTYPE MeanSalt;   //# MeanSalt ... mean salinity
static   REALTYPE MeanTemp;   //# MeanTemp ... mass averaged mean temperature of epilimnion

static   REALTYPE htsave  = 0.;   //# mixed layer thickness from previous time step

static   REALTYPE GPEFF   = 0.;

static   REALTYPE Energy_RequiredMix = 0.; //# Energy required to entrain next layer into the epilimnion
static   REALTYPE Vol_Epi = 0.;  //# Volume of Epilimnion (surface layer after Kelvin-Helmholtz) 1000m3
static   REALTYPE Mass_Epi = 0.; //# Signma mass of Epilimnion (surface layer after Kelvin-Helmholtz) kg

static   REALTYPE Energy_AvailableMix = 0.;  //# Total available energy to mix
static   REALTYPE OLDSL   = 0.;
static   REALTYPE Time_end_shear   = 0.;  //# Time left before shear cut off [hours]
static   REALTYPE Time_start_shear = 0.;  //# Time count since start of sim for shear period start [hours]
static   REALTYPE Time_count_end_shear = 0.;  //# Time count since start of sim for shear period end [hours]

static   REALTYPE FO      = 0.;
static   REALTYPE FSUM    = 0.;
static   REALTYPE Time_count_sim     = 0.;  //# Time count since start of simulation [hours]
static   REALTYPE UF      = 0.;
static   REALTYPE UI      = 0.;
static   REALTYPE UAV     = 0.;
static   REALTYPE Half_Seiche_Period = 0.; //# One half the seiche period
static   REALTYPE Thermocline_Height = 0.; //# Height at the top of the metalimnion [m]


extern REALTYPE coef_mix_KH, CrestLevel, WidAtCrest, LenAtCrest;
extern REALTYPE coef_mix_conv,coef_wind_stir,coef_mix_shear,coef_mix_turb;

/*============================================================================*/

static REALTYPE kelvin_helmholtz(int *Meta_topLayer, int *Epi_botmLayer, REALTYPE Dens, REALTYPE *WQ_VarsM);


/*############################################################################*
 * Performs the surface mixing due to wind forcing                            *
 *----------------------------------------------------------------------------*/
void do_mixing()
{
    REALTYPE twelve = 12.0,
             twfour = 24.0,
             pt25 = 0.25,
             pt3 = 0.3,
             pt587 = 0.587,
             pt6 = 0.6,
             secshr = 3600.0,
             tenM10 = 1.0E-10,
             tenM5 = 1.0E-5,
             tifac = 1.587,
             tdfac = 8.33E-4,
             volfac = 1.0E+3,
             arfac = 1.0E+6;

    REALTYPE Energy_Conv;     //# Energy released by convective overturn
    REALTYPE Energy_WindStir; //# Energy available from wind stirring
    REALTYPE Energy_Deepen;   //# Energy available from the rate of deepening due to shear?
    REALTYPE Energy_Shear;    //# Energy available from shear production
    REALTYPE Energy_TotStir;  //#Total energy available for stirring
    REALTYPE CI;      //#wave speed along the thermocline for a two layer fluid
    REALTYPE CMsml;   //# Metalimnion 1st momement of density above the bottom (layer dz * layer ht * sigma-T)
    REALTYPE Epilimnion_Mid_Ht; //#Epilimnion height measured to middle of epilimnion
    REALTYPE DELTCM;
    REALTYPE DELTAX;
    REALTYPE DELTSQ;
    REALTYPE DELU;
    REALTYPE Delta_h;   //# Delta heigtht (layer thickness)
    REALTYPE Dens_Hypl; //# mean hypolimnion density
    REALTYPE GPEFFC;
    REALTYPE DZ;
    REALTYPE Thermo_L;  //# Effective length of lake at thermocline
    REALTYPE FN;
    REALTYPE GDASH;  //# g' or g prime
    REALTYPE Hypl_Thick; //# Effective thickness of the hypolimnion (Volume/Area)
    REALTYPE Epi_Thick;  //# Effective thickness of the epilimnion (Volume/Area)
    REALTYPE HTB;
    REALTYPE HTILDA;
    REALTYPE Q_cub;  //# Q^3
    REALTYPE Q_sqr;  //# Q^2
    REALTYPE Sbig;
    REALTYPE Ssml;
    REALTYPE SLOPE;
    REALTYPE Tbig;
    REALTYPE Tsml;
    REALTYPE TD;
    REALTYPE TIEFF;
//  REALTYPE top;
    REALTYPE UASAVE;
    REALTYPE UAVSQ;
    REALTYPE UEFF;
    REALTYPE UISAVE;
    REALTYPE U_star;     //# U*, wind induced surface water shear speed [m s-1]
    REALTYPE U_star_sqr; //# U*^2 [m2 s-2]
    REALTYPE U_star_cub; //# U*^3 [m3 s-3]
    REALTYPE WindSpeedX; //# Actual wind speed, accounting for wind factor or ice [m s-1]
    REALTYPE Vol_Hypl;   //# Volume of hypolimnion [m^3]
    REALTYPE VMbig;
    REALTYPE VMsml;
//  REALTYPE wBot;
    REALTYPE WTH;
//  REALTYPE wTop;
    REALTYPE XMsml;  //# Metalimnion 0th momement of density above the bottom (layer dz * sigma-T)


#ifndef _VISUAL_C_
    // The dumb compiler on windows doesn't like this so must malloc manually
    REALTYPE WQ_VarsM[Num_WQ_Vars];
#else
    REALTYPE *WQ_VarsM;
#endif

    REALTYPE Dens_Epil;  //# Mean epilimnion sigma_T (density - 1000) [kg/m3]

    int i, ij;
    int ix, wqvidx;  //# water quality variable index
    int Meta_topLayer, Epi_botmLayer;  //# Index for top layer of hypolimnion and bottom layer of epilimnion
    static int Mixer_Count = 0;  //# Mixer model step counter

/*----------------------------------------------------------------------------*/

#ifdef _VISUAL_C_
    WQ_VarsM = malloc(sizeof(REALTYPE) * Num_WQ_Vars);
#endif

    Mixer_Count++; //# Increment mixing step counter

    //# Determine actual surface wind accounting for wind factor or ice
    if (ice) WindSpeedX = 0.00001;
    else     WindSpeedX = MetData.WindSpeed;

    //# Calculate shear velocity U*, U*^2 and U*^3
    U_star = coef_wind_drag * sqrt(WindSpeedX*WindSpeedX);
    U_star_sqr = U_star*U_star;        //# U*^2 handy in mixing calcs
    U_star_cub = U_star*U_star*U_star; //# U*^3 handy in mixing calcs

    //# Perform mixing due to surface heat transfers, calculate
    //# P.E. released by bouyancy flux and surface wind stress
    Tbig  = zero; //# Mean Temperature of cumulative volume of mixed layer (1000th fraction)
    Tsml  = zero; //# Mean Temperature of cumulative volume of mixed layer (sigma-T fraction)
    Sbig  = zero; //# Mean Salinity of cumulative volume of mixed layer (1000th fraction)
    Ssml  = zero; //# Mean Salinity of cumulative volume of mixed layer (sigma-T fraction)
    VMbig = zero; //# Cumulative volumetric mass of mixed layer (1000th fraction)
    VMsml = zero; //# Cumulative volumetric mass of mixed layer (sigma-T fraction)
    XMsml = zero; //# Metalimnion 0th momement of density above the bottom (layer dz * sigma-T)
    CMsml = zero; //# Metalimnion 1st momement of density above the bottom (layer dz * layer ht * sigma-T)

    //# Loop from surface to bottom layer determine the density of all total volume from
    //# layer Epi_botmLayer and above and check if greater than density of next layer down
    //# if it is then have reached bottom of epilimnion and break from loop
    for (i = botmLayer; i <= surfLayer; i++) {
        Epi_botmLayer = surfLayer - i;

        //# Add layers to the upper mixed layers, Dens_Epi is returned from add_this_layer
        add_this_layer(&VMbig, &VMsml, &Tbig, &Tsml, &Sbig, &Ssml, &Mass_Epi,
                                                &MeanTemp, &MeanSalt, &Dens_Epil, Epi_botmLayer);

        //# Calculate 0th and 1st moments of density about the bottom
        if (Epi_botmLayer != botmLayer) {
            DZ    = Lake[Epi_botmLayer].Height - Lake[Epi_botmLayer-1].Height;
            XMsml = XMsml + Lake[Epi_botmLayer].Density * DZ;
            CMsml = CMsml + Lake[Epi_botmLayer].Density * DZ * Lake[Epi_botmLayer].MeanHeight;
            if (Dens_Epil < Lake[Epi_botmLayer-1].Density) break;
        } else {
            XMsml = XMsml + Lake[botmLayer].Density * Lake[botmLayer].Height;
            CMsml = CMsml + Lake[botmLayer].Density * Lake[botmLayer].Height * Lake[botmLayer].MeanHeight;
        }
    }

    //# Epi_botmLayer is now the bottom layer of the epilimnion or mixed layer,
    //# Dens_Epil is the density of the epilimnion or mixed layer

    if ( Epi_botmLayer == botmLayer ) {
        //# This means lake is fully mixed so set all layers to mean properties
        for (ij = Epi_botmLayer; ij < surfLayer; ij++) {
            Lake[ij].Temp = MeanTemp;
            Lake[ij].Salinity = MeanSalt;
            Lake[ij].Density = Dens_Epil;
        }
    }

    if (Epi_botmLayer != botmLayer)
        Epilimnion_Mid_Ht = (Lake[surfLayer].Height + Lake[Epi_botmLayer-1].Height) / two;  //# Epilimnion mean height measured to middle of epilimnion
    else //# fully mixed epilimnion height == mid lake height
        Epilimnion_Mid_Ht = (Lake[surfLayer].Height) / two;

    //# Check for bottom, update time index if necessary

    //# Meta_topLayer is now the top layer of the metalimnion
    //# Epi_botmLayer is the bottom layer of the epilimnion
    //# Meta_topLayer+1 == Epi_botmLayer
    Meta_topLayer = Epi_botmLayer - 1;
    if (Epi_botmLayer == botmLayer) {  //# This means that lake fully mixed exit to labl1800
        Time_count_sim += noSecs / 3600.0; //# Add num_hours to sim time counter (in hours since sim)
        goto labl800;
    }
    DELTCM = (CMsml-Epilimnion_Mid_Ht*XMsml);  //# Delta mass vertical gradient kg/m

    //# Kraus Turner, Energy_Conv measures energy released by convective
    //# overturn (ie cooled dense water falling)

    Energy_Conv = coef_mix_conv*g*DELTCM/((Dens_Epil+thsnd)*noSecs)*noSecs/two;
    if (Energy_Conv < zero) Energy_Conv = zero;

    //# Calculate total energy available for stirring and add to
    //# amount stored from last time step.

    Energy_WindStir = coef_wind_stir * U_star_cub * noSecs/two;

    Energy_TotStir = Energy_Conv + Energy_WindStir;
    Q_cub = Energy_TotStir / (coef_mix_conv * noSecs) * two;

    if (Q_cub <= zero) Q_cub = tenM10;
    Q_sqr = pow(Q_cub, (two/three));
    Energy_AvailableMix += Energy_TotStir; //# Add stirring energy to available mixing energy

    //# Loop for stirring. Check for bottom. Compute energy required to
    //# mix next layer and compare with available energy.
    while(1) {
        Epi_dz = Lake[surfLayer].Height - Lake[Meta_topLayer].Height;
        Delta_h = Lake[Meta_topLayer].Height;
        if (Meta_topLayer > botmLayer) Delta_h = Lake[Meta_topLayer].Height - Lake[Meta_topLayer-1].Height;
        GDASH = gprime(Dens_Epil,Lake[Meta_topLayer].Density);
        Energy_RequiredMix = (GDASH * Epi_dz + coef_mix_turb * Q_sqr) * Delta_h / two;
        if (Energy_AvailableMix < Energy_RequiredMix) break; //# Not enough energy to entrain any more layers into the mixed layer

        //# Entrain layer Meta_topLayer
        add_this_layer(&VMbig,&VMsml,&Tbig,&Tsml,&Sbig,&Ssml,&Mass_Epi,&MeanTemp,&MeanSalt, &Dens_Epil,Meta_topLayer);
        Energy_AvailableMix -= Energy_RequiredMix; //# Just used energy to entrain another layer into the mixed layer so no longer available
        average_layer(&Meta_topLayer,&Epi_botmLayer,MeanTemp,MeanSalt,Dens_Epil);
        if (Meta_topLayer < botmLayer) {
            //# Here if Kraus-Turner mixes to bottom
            Time_count_sim += noSecs/3600.0;  //#Add num_hours to sim time counter (in hours since sim)
            goto labl800;
        }
    }

    //# PRT - Shear production
    //# Cutoff shear production if thermocline occurs within the hole
    if (Lake[Meta_topLayer].Height <= 0.) goto labl900;

    //# Calculate Kraus-Turner depth
    HTILDA = DepMX - Lake[Meta_topLayer].Height;
    if (HTILDA <= zero) HTILDA = zero;

    /**************************************************************************
     * Calculate parameters needed for momentum computation                   *
     * CI is the wave speed along the thermocline for a two layer fluid       *
     * htsave is the mixed layer thickness from previous time step            *
     * Dens_Hypl is the mean hypolimnion density                              *
     * Dens_Epi is the epilimnion density                                     *
     * Epi_Thick,Hypl_Thick are the thicknesses of the epilimnion and hypolimnion *
     *  respectively calculated as volume/mean area                           *
     * CI = sqrt((GPEFF*Epi_Thick*Hypl_Thick)/(Epi_Thick+Hypl_Thick))         *
     **************************************************************************/
    WTH = zero;
    for (i = botmLayer; i <= Meta_topLayer; i++)
         WTH = Lake[i].LayerVol*Lake[i].Density + WTH;

    Dens_Hypl = WTH/Lake[Meta_topLayer].Vol1;
    GPEFF = gprime(Dens_Epil,Dens_Hypl); //# gprime_effective
    Vol_Epi = Lake[surfLayer].Vol1 - Lake[Meta_topLayer].Vol1;
    Vol_Hypl = Lake[Meta_topLayer].Vol1;
    Epi_Thick = (Vol_Epi/(Lake[Meta_topLayer].LayerArea+Lake[surfLayer].LayerArea))*(volfac/arfac)*two;
    Hypl_Thick = (Vol_Hypl/Lake[Meta_topLayer].LayerArea)*(volfac/arfac)*two;
    if (GPEFF <= 0.1E-6) GPEFF = 0.1E-6;
    CI = sqrt((fabs(GPEFF)*Epi_Thick*Hypl_Thick)/(Epi_Thick+Hypl_Thick));

    //# Adjust momentum for fluid entrained by Kraus Turner deepening
    //# if Epi_Thick > htsave

    UASAVE=UAV;
    if (UAV > zero && htsave < Epi_Thick) {
         UF = UF*htsave/Epi_Thick;
         UI=UF;
    }

    UISAVE=UI;

    //# Compute effective length at thermocline level
    /* Assume:
            1) that lake approximates as an ellipse
            2) area = pi/4 * Length * Width
            3) ratio Length:Width at thermocline = crest
    */
    Thermo_L = sqrt(Lake[Meta_topLayer].LayerArea*arfac*four/Pi*(LenAtCrest/WidAtCrest));
    /*for (i = NumOut; i >= 0; i--) {
         top = CrestLevel;
         wTop = WidAtCrest;
         if (i != NumOut) top = Outflows[i].OLev;
         if (i != NumOut) wTop = Outflows[i].OWid;
         bot = zero;
         wBot = zero;
         for (j = 0; j < NumOut; j++) {
            if (Outflows[j].OLev > bot && Outflows[j].OLev < top) {
               bot = Outflows[j].OLev;
               wBot = Outflows[j].OWid;
            }
         }
         if (db > bot && db <= top)
            EW = (Lake[j1].Height-bot)/(top-bot)*(wTop-wBot)+wBot;
    }
    EL = Lake[j1].LayerArea*(arfac)/EW;*/

    /**************************************************************************
     * Check momentum time counters                                           *
     * Half_Seiche_Period > 0 indicates a current shear event                 *
     * TIEFF is the effective forcing time                                    *
     * Half_Seiche_Period is one half the seiche period                       *
     * Time_start_shear is the start of shear forcing (hours from sim start)  *
     * Time_count_end_shear is the end of shear forcing (hours from sim start)*
     * TD is the damping time                                                 *
     **************************************************************************/
    if (Half_Seiche_Period <= zero) {
         Half_Seiche_Period = Thermo_L/(two*CI*secshr);
         TIEFF = Half_Seiche_Period;
         if (U_star <= zero)
            TD=zero;
         else {
            HTB=Epi_Thick+Hypl_Thick;
            TD=two*(HTB/tdfac)*(HTB/U_star_sqr)*(Hypl_Thick/Epi_Thick)*pow((GPEFF*Epi_Thick*Hypl_Thick/HTB), pt25) /
                                                      (sqrt(two*Thermo_L)*secshr)*sqrt(Visc);
            TIEFF = tifac*Half_Seiche_Period;
            if (TD/Half_Seiche_Period < ten) TIEFF = (one+pt587*(one-(1/cosh(TD/Half_Seiche_Period-one))))*Half_Seiche_Period;
         }
         Time_start_shear = Time_count_sim;
         Time_count_end_shear = Time_start_shear + TIEFF;

    }

    //# Calculate momentum forcing parameters for current time step
    //# FN is the acceleration (m/s**2) of UML by wind stress
    FN = U_star_sqr/Epi_Thick;
    FSUM = FSUM + FN;
    SLOPE = (FN-FO) + OLDSL;
    if (FN == zero) SLOPE = zero;
    else if (FN<=zero || fabs(SLOPE/FN)<=tenM5) SLOPE=zero;

    if (SLOPE < zero) SLOPE = zero;

    //# Check for momentum cutoff within current time step. Calculate time
    //# step for forcing, Time_end_shear, and reset parameters for next time step
    Time_count_sim += noSecs/3600.0;  //# Add num_hours to sim time counter (in hours since sim start)
    if (Time_count_sim >= Time_count_end_shear) {
         //# Here if cutoff within current time step
         Time_end_shear = Time_count_end_shear - Time_count_sim + noSecs/3600.0;
         OLDSL = FN - (FSUM / (Mixer_Count));
         if (OLDSL < zero) OLDSL = zero;
    } else {
         Time_end_shear = noSecs/3600.0;
         OLDSL = SLOPE;
    }
    FO = FN;

    //# Compute momentum increment
    if (UI < 1E-7) UI = zero;
    if (SLOPE < 1E-7) SLOPE = zero;
    UF = UI + SLOPE * Time_end_shear * secshr;
    UAVSQ = (UF * UF + UF * UI + UI * UI) / three;
    if (UAVSQ < 1E-7) UAVSQ = 1E-7;
    UAV = sqrt(UAVSQ);
    UI = UF;
    DELU = UAV - UASAVE;
    DELTSQ = pt6 * UAV * DELU / GPEFF;
    DELTAX = pt3 * UAV * UAV / GPEFF;


    if (DELTAX < 1.0E-10) DELTAX = 0.0;
    if (DELTSQ < 1.0E-10) DELTSQ = 0.0;

    Energy_Shear = coef_mix_shear * (UAV * UAV * (HTILDA + DELTSQ / six) + UAV * DELTAX * DELU / three) / two +
                GDASH * DELTAX *
                (DELTAX * HTILDA / (twfour * (Lake[surfLayer].Height - Lake[Meta_topLayer].Height)) - DELTSQ / twelve);
    if (Energy_Shear < zero) Energy_Shear = zero;


    Energy_AvailableMix += Energy_Shear;  //# Add available kinetic energy
    GPEFFC = GPEFF * Epi_Thick;

    //# Deepening loop for shear production begins here
    DELU = zero;
    while(1) {
         //# Save current values of Epi_Thick and UAV in case of mixing
         htsave = Epi_Thick;
         UEFF = UAV;

         //# Compute energy available for mixing next layer
         Delta_h = Lake[Meta_topLayer].Height;
         if (Meta_topLayer > botmLayer) Delta_h = Lake[Meta_topLayer].Height - Lake[Meta_topLayer-1].Height;
         Epi_dz = Lake[surfLayer].Height - Lake[Meta_topLayer].Height;
         Energy_Deepen = coef_mix_shear * (UEFF * UEFF * (Delta_h + DELTSQ / six) + UEFF * DELTAX * DELU / three) / two +
                 GDASH * DELTAX * (DELTAX * Delta_h / (twfour * Epi_dz) - DELTSQ / twelve);

         if (Energy_Deepen < zero) Energy_Deepen = zero;
         Energy_AvailableMix += Energy_Deepen;   //# Add available kinetic energy

         //# Compute energy required to entrain next layer
         Energy_RequiredMix = (GDASH * Epi_dz + coef_mix_turb * Q_sqr) * Delta_h / two;

         //# Compare energy available with energy required
         //printf("Energy_AvailableMix = %10.5f\n",Energy_AvailableMix*1000);
         //printf("Energy_RequiredMix = %10.5f\n",Energy_RequiredMix*1000);
         //if (Energy_AvailableMix < Energy_RequiredMix) printf("About to break\n");
         if (Energy_AvailableMix < Energy_RequiredMix) break; //# Insufficient energy to deepen so break

         //# Entrain layer Meta_topLayer
         //printf("NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer = %d,%d,%d,%d,%d\n",NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer);
         //printf("Epilimnion Mass, Temp, Salt, Dens = %10.5f,%10.5f,%10.5f,%10.5f\n",Vol_Epi/1000,MeanTemp,MeanSalt, 1000.+Dens_Epil);
         //printf("Meta_topLayer Vol, Temp, Salt, Dens = %10.5f,%10.5f,%10.5f,%10.5f\n",Lake[Meta_topLayer].LayerVol/1000.,Lake[Meta_topLayer].Temp,Lake[Meta_topLayer].Salinity,1000+Lake[Meta_topLayer].Density);
         add_this_layer(&VMbig,&VMsml,&Tbig,&Tsml,&Sbig,&Ssml,&Mass_Epi,&MeanTemp,&MeanSalt, &Dens_Epil,Meta_topLayer);
         average_layer(&Meta_topLayer,&Epi_botmLayer,MeanTemp,MeanSalt,Dens_Epil);
         //printf("New Epilimnion Mass, Temp, Salt, Dens = %10.5f,%10.5f,%10.5f,%10.5f\n",Vol_Epi/1000,MeanTemp,MeanSalt, 1000.+Dens_Epil);
         //printf("NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer = %d,%d,%d,%d,%d\n",NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer);

         Energy_AvailableMix -= Energy_RequiredMix; //# Just used energy to entrain another layer so no longer available

         if (Meta_topLayer < botmLayer) goto labl800;

         //# Adjust UF, UAV for entrained mass
         WTH = zero;

         for (i = botmLayer; i <= Meta_topLayer; i++)
             WTH = Lake[i].LayerVol*Lake[i].Density+WTH;

         Dens_Hypl = WTH/Lake[Meta_topLayer].Vol1;
         GDASH = gprime(Dens_Epil,Lake[Meta_topLayer].Density);

         Vol_Hypl = Lake[Meta_topLayer].Vol1;
         Vol_Epi = Vol_Epi + Lake[Meta_topLayer+1].LayerVol;
         Epi_Thick = (Vol_Epi / (Lake[Meta_topLayer].LayerArea + Lake[surfLayer].LayerArea)) * (volfac / arfac)*two;
         UF = UF * htsave / Epi_Thick;
         UAV = sqrt((UISAVE*UISAVE + UISAVE*UF + UF*UF)/three);
         DELU = UAV-UEFF;
         DELTAX = pt3*UAV*UAV/(GDASH);
         DELTSQ = pt6*UAV*DELU/(GDASH);
         UI = UF;
    }

    //# Here if insufficient energy to entrain next layer

    //# Check momentum time counters for cutoff
    Thermocline_Height = Lake[Meta_topLayer].Height;
    GPEFF = GPEFFC / Epi_Thick;

    //# Average water quality
    for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++) {
         WQ_VarsM[wqvidx] = 0.0;
         // sum over the layers
         for (ix = Meta_topLayer+1; ix <= surfLayer; ix++)
            WQ_VarsM[wqvidx] = _WQ_Vars(wqvidx,ix) * Lake[ix].LayerVol + WQ_VarsM[wqvidx];
         // divide by the volume
         WQ_VarsM[wqvidx] = WQ_VarsM[wqvidx] / (Lake[surfLayer].Vol1-Lake[Meta_topLayer].Vol1);
    }

    Dens_Epil = kelvin_helmholtz(&Meta_topLayer,&Epi_botmLayer,Dens_Epil, WQ_VarsM);
    DepMX = Lake[Meta_topLayer].Height;

    if (Time_count_sim < Time_count_end_shear)  goto labl1000; //# Insufficient energy to mix this time step so keep count of mixing model time count
    goto labl900;  //# All available energy used reset mixing model step count

labl800:
//printf("labl800\n");
    //# Here if deepened to bottom

    OLDSL = zero; //# Old slope = zero as fully mixed
    Energy_AvailableMix = zero;   //# Total available energy to mix reset to zero as lake fully mixed
    FO = zero;

    //# mix all water quality variables to average value
    for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++) {
         WQ_VarsM[wqvidx] = 0.0;
         for (ix = botmLayer; ix <= surfLayer; ix++)
            WQ_VarsM[wqvidx] = _WQ_Vars(wqvidx,ix)*Lake[ix].LayerVol + WQ_VarsM[wqvidx];
         WQ_VarsM[wqvidx] = WQ_VarsM[wqvidx] / Lake[surfLayer].Vol1;
    }

labl900:
//printf("labl900\n");

    //# Here if momentum cutoff

    Mixer_Count = 0;  //# Reset mixing model step count (note not reset if goto labl1000)
    FSUM = zero;
    Half_Seiche_Period = zero;  //# Reset one half the seiche period
    UF = zero;
    UI = zero;
    UAV = zero;
    Time_start_shear = Time_count_sim; //# Reset start time of shear forcing to hours from sim start
    Time_count_end_shear = Time_count_sim; //# Reset finish time of shear forcing to hours from sim start

    //# Mark 2 ends here. At this stage layers Meta_topLayer+1,Meta_topLayer+2,---Epi_botmLayer-1 are also mixed
    //# So make epilimnion into one big layer adding Meta_topLayer+1 ... surfLayer

    //# Renumber mixed layers

labl1000:
//printf("labl1000\n");

    //# Meta_topLayer+1 becomes the surface layer == mixed epilimnion layers
    Lake[Meta_topLayer+1].Height = Lake[surfLayer].Height;
    Lake[Meta_topLayer+1].Temp = MeanTemp;
    Lake[Meta_topLayer+1].Salinity = MeanSalt;

    //# water quality and particles
    for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
        _WQ_Vars(wqvidx,Meta_topLayer+1) = WQ_VarsM[wqvidx];

    //# reset the layer volume, density and area for the surface layer
    Lake[Meta_topLayer+1].Vol1 = Lake[surfLayer].Vol1;

    //# calculate cumulative volume of layers bottom .. Meta_topLayer
    //# technically volume of hypolimnion + metalimnion
    Vol_Hypl = zero; //# The case when fully mixed Meta_topLayer<bottom layer
    if (Meta_topLayer >= botmLayer) Vol_Hypl = Lake[Meta_topLayer].Vol1;

    //# volume of new mixed mega epilimnion layer
    Lake[Meta_topLayer+1].LayerVol = Lake[surfLayer].Vol1 - Vol_Hypl;
    Lake[Meta_topLayer+1].Density = Dens_Epil;
    Lake[Meta_topLayer+1].LayerArea = Lake[surfLayer].LayerArea;

    NumLayers = Meta_topLayer + 2; //# add 2 as count from 0 (ie bottom layer == 0)
    //printf("Time_count_sim = %10.5f\n",Time_count_sim);
    //printf("Time_start_shear = %10.5f\n",Time_start_shear);
    //printf("Time_count_end_shear = %10.5f\n",Time_count_end_shear);
        //printf("End mix KH NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer = %d,%d,%d,%d,%d\n",NumLayers, surfLayer, botmLayer, Meta_topLayer, Epi_botmLayer);

#ifdef _VISUAL_C_
    free(WQ_VarsM);
#endif
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 *----------------------------------------------------------------------------*/
static REALTYPE kelvin_helmholtz(int *Meta_topLayer, int *Epi_botmLayer, REALTYPE Dens, REALTYPE *WQ_VarsM)
{
    REALTYPE secshr = 3600.0;

    REALTYPE Delta_Mix; //Thickness of mixing layer [m]
    REALTYPE DepthB;
    REALTYPE DNL;
    REALTYPE Surface_Height; //# Height of lake surface
    REALTYPE D3;
    REALTYPE E = 0.02; //# Minimum tolerance
    REALTYPE EE;  //# Six times minimum tolerance (0.12)
    REALTYPE eBot;
    REALTYPE eTop;
    REALTYPE HMIN;
    REALTYPE T;
    REALTYPE TBIL;
    REALTYPE TBILF;
    REALTYPE THB;
    REALTYPE THT;

    int i;
    int ir;
    int kl1;
    int lbi;
    int Meta_botmLayer; //# Layer index for the layer intersecting the bottom of the metalimnion
    int n;
    int nl;
    int j1, k1; //# j1 = top layer of the metalimnion, k1 = bottom layer of the epilimnion

    int top, up;
    int wqvidx;

/*----------------------------------------------------------------------------*/

    j1 = *Meta_topLayer; k1 = *Epi_botmLayer;
    //printf("Start KH NumLayers, surfLayer, botmLayer, j1, k1 = %d,%d,%d,%d,%d\n",NumLayers, surfLayer, botmLayer, j1, k1);

    //# Set Tolerances
    EE = 6. * E;

    //# Compute Delta_Mix

    TBIL = UAV / (GPEFF * secshr); //# Time period for billowing effects
    TBILF = TBIL / Time_end_shear; //# Ratio time period for billowing to shear
    Delta_Mix = 0.;

    if (surfLayer <= botmLayer || TBILF > ten) return Dens;

    Delta_Mix = (coef_mix_KH*UAV*UAV)/(GPEFF*two*cosh(TBILF));
    //# Limit the thickness of the mixing layer to less than either the hypolimnion or epilimnion
    HMIN = MIN(Epi_dz, Thermocline_Height);
    if (Delta_Mix > HMIN) Delta_Mix = HMIN;
    eTop = Thermocline_Height + Delta_Mix;
    eBot = Thermocline_Height - Delta_Mix;
    top = FALSE;
    if (eTop > Lake[surfLayer-1].Height) eTop = Lake[surfLayer-1].Height;
    if ((eTop-Thermocline_Height) < EE/two) return Dens;
    if (eBot < Lake[botmLayer].Height) eBot = Lake[botmLayer].Height;
    if ((Thermocline_Height-eBot) < EE/two) return Dens;
    Surface_Height = Lake[surfLayer].Height;

    //# Find layer intersecting ebot
    for (Meta_botmLayer = botmLayer; Meta_botmLayer <= (k1-1); Meta_botmLayer++)
        if (Lake[Meta_botmLayer].Height > eBot) break;

    DepthB = zero;
    if (Meta_botmLayer > botmLayer) DepthB = Lake[Meta_botmLayer-1].Height;

    //# Check to see if ebot coincides with existing depth value
    T = fabs(eBot-DepthB);
    if (T <= E) {
        eBot = DepthB;
        Meta_botmLayer--;
    } else {
        T = fabs(Lake[Meta_botmLayer].Height - eBot);
        if (T <= E) {
            eBot = Lake[Meta_botmLayer].Height;
            if (Meta_botmLayer == k1-1) return Dens;
        } else {
            //# Here if new layer must be added below mixed region
            *Epi_botmLayer = (++k1);
            kl1 = k1-Meta_botmLayer-1; /* = number of layers */

            for (i = botmLayer; i < kl1; i++) {
                *Meta_topLayer = (j1 = k1-i-1);
                Lake[j1].Height = Lake[j1-1].Height;
                Lake[j1].Density = Lake[j1-1].Density;
                Lake[j1].Temp = Lake[j1-1].Temp;
                Lake[j1].Salinity = Lake[j1-1].Salinity;

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                    _WQ_Vars(wqvidx,j1) = _WQ_Vars(wqvidx,j1-1);
            }
            Lake[Meta_botmLayer].Height = eBot;
        }
    }

    //# Here after position (ebot) of bottom of shear zone has been
    //# determined and extra layer added if necessary
    T = fabs(eTop-eBot);
    if (T < EE) return Dens;

    //# Check number of layers in bottom half of shear layer - there must
    //# be at least three
    nl = k1-Meta_botmLayer-1;
    if (nl < 3) {
        if (nl != 2) {
            //# Here if nl=1
            nl = 3;
            D3 = (Lake[k1-1].Height - eBot)/three;
            Lake[Meta_botmLayer+3].Height = Lake[k1-1].Height;
            Lake[Meta_botmLayer+2].Height = eBot + D3 + D3;
            Lake[Meta_botmLayer+1].Height = eBot + D3;
            for (i = 2; i <= 3; i++) {
                lbi = Meta_botmLayer + i;
                Lake[lbi].Density = Lake[k1-1].Density;
                Lake[lbi].Temp = Lake[k1-1].Temp;
                Lake[lbi].Salinity = Lake[k1-1].Salinity;
                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                    _WQ_Vars(wqvidx,lbi)=_WQ_Vars(wqvidx,k1-1);
            }
            *Epi_botmLayer = (k1 = Meta_botmLayer+4);
        } else {
            //# Here if nl=two
            nl = 3;

            //# Find thicker layer
            THT = Lake[k1-1].Height - Lake[k1-2].Height;
            THB = Lake[k1-2].Height - eBot;
            if (THT <= THB) {
                //# Here if tht .le. thb - divide layer k1-2
                Lake[Meta_botmLayer+1].Height = eBot + THB/two;
                Lake[Meta_botmLayer+2].Height = eBot + THB;
                Lake[Meta_botmLayer+3].Height = eBot + THT + THB;
                *Epi_botmLayer = (++k1);
                Lake[k1-1].Density = Lake[k1-2].Density;
                Lake[k1-1].Temp = Lake[k1-2].Temp;
                Lake[k1-1].Salinity = Lake[k1-2].Salinity;

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                    _WQ_Vars(wqvidx,k1-1)=_WQ_Vars(wqvidx,k1-2);

                Lake[k1-2].Density  = Lake[k1-3].Density;
                Lake[k1-2].Temp     = Lake[k1-3].Temp;
                Lake[k1-2].Salinity = Lake[k1-3].Salinity;

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                    _WQ_Vars(wqvidx,k1-2) = _WQ_Vars(wqvidx,k1-3);
            } else {
                //# Here if tht > thb - split layer k1-1
                *Epi_botmLayer = (++k1);
                Lake[k1-1].Height = Lake[k1-2].Height;
                Lake[k1-1].Temp = Lake[k1-2].Temp;
                Lake[k1-1].Salinity = Lake[k1-2].Salinity;

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                    _WQ_Vars(wqvidx,k1-1) = _WQ_Vars(wqvidx,k1-2);

                Lake[k1-1].Density = Lake[k1-2].Density;
                Lake[k1-2].Height = Lake[k1-3].Height + THT/two;
            }
        }
    }

    //# Here after bottom half of shear zone has at least three layers
    //# divide top half of shear zone into nl layers
    DNL = (eTop - Lake[k1-1].Height)/(nl);
    for (i = 1; i <= nl; i++) {
        *Meta_topLayer = (j1 = i + k1 - 1);
        Lake[j1].Height = Lake[k1-1].Height + (i)*DNL;
        Lake[j1].Density = Dens;
        Lake[j1].Temp = MeanTemp;
        Lake[j1].Salinity = MeanSalt;

        for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
            _WQ_Vars(wqvidx,j1) = WQ_VarsM[wqvidx];
    }

    //# The number of the layer just below the mixed region is now j1=k1+nl-1
    //# unless top = .t.  if top=.t.,then layer j1 is the mixed region
    if (!top) {
        NumLayers = k1 + nl + 1;
        Lake[surfLayer].Temp = MeanTemp;
        Lake[surfLayer].Density = Dens;
        Lake[surfLayer].Salinity = MeanSalt;

        for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
            _WQ_Vars(wqvidx,surfLayer) = WQ_VarsM[wqvidx];
    } else {
        *Meta_topLayer = (--j1);
        NumLayers = k1+nl;
    }
    Lake[surfLayer].Height = Surface_Height;
    Epi_dz = Lake[surfLayer].Height - Lake[surfLayer-1].Height;


    //# Calculate volumes
    resize_internals(1,botmLayer);

    //# relax density structure within shear zone

    up = TRUE;
    ir = nl-2;
    while(1) {
        //# Mix middle two layers k1, k1-1
        Lake[k1].Temp = combine(Lake[k1].Temp,   Lake[k1].LayerVol,   Lake[k1].Density,
                                Lake[k1-1].Temp, Lake[k1-1].LayerVol, Lake[k1-1].Density);
        Lake[k1].Salinity = combine(Lake[k1].Salinity,   Lake[k1].LayerVol,   Lake[k1].Density,
                                    Lake[k1-1].Salinity, Lake[k1-1].LayerVol, Lake[k1-1].Density);
        Lake[k1].Density = calculate_density(Lake[k1].Temp,Lake[k1].Salinity);

        for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
            _WQ_Vars(wqvidx,k1) = combine_vol(_WQ_Vars(wqvidx,k1),   Lake[k1].LayerVol,
                                     _WQ_Vars(wqvidx,k1-1), Lake[k1-1].LayerVol);

        Lake[k1-1].Density = Lake[k1].Density;
        Lake[k1-1].Temp = Lake[k1].Temp;
        Lake[k1-1].Salinity = Lake[k1].Salinity;

        for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
            _WQ_Vars(wqvidx,k1-1)=_WQ_Vars(wqvidx,k1);

        if (ir == botmLayer) break;

        while(1) {
            //# do loop mixes up (up = .true.) or down (up = .false.)
            for (i = botmLayer; i <= ir; i++) {
                if (up) n = k1 + i - 1;
                else    n = k1 - i - 1;
                Lake[n].Temp = combine(Lake[n].Temp,Lake[n].LayerVol,Lake[n].Density,
                                       Lake[n+1].Temp,Lake[n+1].LayerVol,Lake[n+1].Density);
                Lake[n].Salinity = combine(Lake[n].Salinity,Lake[n].LayerVol,
                                           Lake[n].Density,Lake[n+1].Salinity,
                                           Lake[n+1].LayerVol,Lake[n+1].Density);
                Lake[n].Density = calculate_density(Lake[n].Temp,Lake[n].Salinity);

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                     _WQ_Vars(wqvidx,n) = combine_vol(_WQ_Vars(wqvidx,n),   Lake[n].LayerVol,
                                             _WQ_Vars(wqvidx,n+1), Lake[n+1].LayerVol);
                Lake[n+1].Density = Lake[n].Density;
                Lake[n+1].Temp = Lake[n].Temp;
                Lake[n+1].Salinity = Lake[n].Salinity;

                for (wqvidx=0; wqvidx < Num_WQ_Vars; wqvidx++)
                     _WQ_Vars(wqvidx,n+1) = _WQ_Vars(wqvidx,n);
            }

            up = !up;
            if (up) {
                ir--;
                break;
            }
        }
    }

    //# Here after relaxation complete. reset mixed region variables.
    Dens = Lake[surfLayer].Density;
    MeanTemp = Lake[surfLayer].Temp;
    MeanSalt = Lake[surfLayer].Salinity;

    //Vol_Epi = Lake[surfLayer].LayerVol;
    //Mass_Epi = Vol_Epi*Dens;


    for (wqvidx = 0; wqvidx < Num_WQ_Vars; wqvidx++)
        WQ_VarsM[wqvidx] = _WQ_Vars(wqvidx, surfLayer);
    //printf("End KH NumLayers, surfLayer, botmLayer, j1, k1 = %d,%d,%d,%d,%d\n",NumLayers, surfLayer, botmLayer, j1, k1);

    return Dens;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


//==============================================================================
