/******************************************************************************
 *                                                                            *
 * glm_surface.c                                                              *
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
#include "aed_time.h"
#include "glm_mixu.h"

//#define dbgprt(...) fprintf(stderr, __VA_ARGS__)
#define dbgprt(...) /* __VA_ARGS__ */


/******************************************************************************
 * Variables for the ice cover components                                     *
 * density_s = density of snow                                                *
 * Tf = temperature at which the water freezes                                *
 * K_S = thermal conductivity of snow                                         *
 * K_I = thermal conductivity of ice                                          *
 * K_w = molecular thermal conductivity of water                              *
 * Tmelt = temperature of ice melt                                            *
 * L_i = the latent heat of fusion for ice                                    *
 * L_s = the latent heat of fusion for snow                                   *
 * L_fusion = a switch between the snow and ice latent heats of fusion        *
 ******************************************************************************/

int ice;         // flag that tells if there is ice formation

extern int stepnum;

// These are made available for the lake.csv output
REALTYPE Q_shortwave;    // Solar radiation at water surface
REALTYPE Q_sensibleheat;
REALTYPE Q_latentheat;
REALTYPE Q_longwave;

static REALTYPE  Q01;     // heat flux out of snow/ice surface
static REALTYPE  QF1;     // heat flux across water/ice interface
static REALTYPE  QW1;     // heat flux through the water surface
static REALTYPE  QI1;     // heat flux through water due to molecular conductivity
static REALTYPE  QS1;     // heat flux through water due to flow under the ice
static REALTYPE  H_FLUX;  // the heat flux at the surface due to meteorological forcing
static REALTYPE  U_FLOW;  // the velocity of the underflow

void recalc_surface_salt(void);
REALTYPE calculate_qsw(int kDays, int mDays, int iclock, REALTYPE Latitude, REALTYPE SWOld, REALTYPE ShortWave);

/******************************************************************************
 * Performs the thermal transfers at the surface                              *
 * ice cover version, Brett Wallace 1996, David Hamilton 1998                 *
 * time step fixed to one hour, David Hamilton, 1998                          *
 * Light modified to sinusoidal distribution, David Hamilton 1993             *
 * The day commences at midday, variable daylength (Time of year and latitude)*
 ******************************************************************************/
void do_surface_thermodynamics(int jday, int iclock, int LWModel,
                              REALTYPE Latitude, REALTYPE SWOld, REALTYPE ShortWave)
// LWModel   // type of longwave radiation
// SWOld     // Total solar radiation at the surface for yesterday
// ShortWave // Total solar radiation at the surface for today
{
/*----------------------------------------------------------------------------*/
    REALTYPE  AreaFactor = 1.0E+6;
    REALTYPE  Stefan_Boltzman = 5.67E-8;  //#Stefan-Boltzman constant
    REALTYPE  Kelvin = 273.15,HTEVAP = 2.453E+9,SPHEAT = 4.186;
    REALTYPE  K_I1 = 2.3,K_I2 = 2.0,K_W = 0.57,CSEN = 0.0014;
    REALTYPE  L_I = 334000.0,L_S = 334000.0,TF = 0.0,TMELT = 0.0;
    REALTYPE  A_ONE = 0.7;        //# fraction of short wave radiation in first wavelength band
    REALTYPE  A_TWO = 0.3;        //# fraction of short wave radiation in second wavelength band
    REALTYPE  ET_ICE_ONE = 1.5;   //# attenuation coefficient of the ice in the first spectral band
    REALTYPE  ET_ICE_TWO = 20.;   //# attenuation coefficient of the ice in the second spectral band
    REALTYPE  ET_WICE_ONE = 6.0;  //# attenuation coefficient of the white ice in the first spectral band
    REALTYPE  ET_WICE_TWO = 20.;  //# attenuation coefficient of the white ice in the second spectral band
    REALTYPE  ET_SNOW_ONE = 6.0;  //# attenuation coefficient of the snow in the first spectral band
    REALTYPE  ET_SNOW_TWO = 20.;  //# attenuation coefficient of the snow in the second spectral band
    REALTYPE  DENSITY_I1 = 917.0; //# density of ice
    REALTYPE  DENSITY_I2 = 890.0;
    REALTYPE  RHOMXSNO = 300.0,RHOMNSNO = 50.;

/*----------------------------------------------------------------------------*/

    REALTYPE CLOUD;
    REALTYPE DT;

#ifndef _VISUAL_C_
    // The dumb compiler on windows doesn't like this so must malloc manually
    REALTYPE LayerThickness[MaxLayers],     //# Layer thickness (m)
             heat[MaxLayers];
#else
    REALTYPE *LayerThickness;
    REALTYPE *heat;
#endif
    REALTYPE HEN;
    REALTYPE Q_lw_in;       //# Long wave radiation from atmosphere W/m2
    REALTYPE QSI,QSI1;
    REALTYPE QRL;
    REALTYPE Q_lw_out;  //# Long wave radiation emission W/m2
    REALTYPE SVP0;
    REALTYPE WindSp;      //# Wind speed corrected by multiplicative factor

    REALTYPE AAA, BBB, CCC, DDD, EEE, FFF, GGG;
    REALTYPE T001;
    REALTYPE T01_OLD,T01_NEW;
    REALTYPE RHOSNO, RHOLDSNO, KCOMSNO, K_S;
    REALTYPE SUMPO4, SUMTP, SUMNO3, SUMNH4, SUMTN, SUMSI;
    REALTYPE QLATENT;

   //# New parameters for heat flux estimate
//  REALTYPE KSED;
//  REALTYPE TYEAR;
//  REALTYPE ZSED;

    int i;
    int kDays;    //# Days since start of year for yesterday
    int mDays;    //# Days since start of year for today
    int underFlow;

/*----------------------------------------------------------------------------*/

#ifdef _VISUAL_C_
    LayerThickness = malloc(sizeof(REALTYPE) * MaxLayers);
    heat = malloc(sizeof(REALTYPE) * MaxLayers);
#endif

    Q_longwave = 0.;
    SurfData.Evap = 0.;
    memset(heat, 0, sizeof(REALTYPE)*MaxLayers);

    T01_NEW = 0.;
    T01_OLD = 0.;
    QLATENT = 0.0;
    SUMPO4 = 0.0;
    SUMTP = 0.0;
    SUMNO3 = 0.0;
    SUMNH4 = 0.0;
    SUMTN = 0.0;
    SUMSI = 0.0;
    QSI = 0.0;
    QSI1 = 0.0;
    QRL = 0.0;
    RHOSNO = 0.0;
    underFlow = FALSE;

    //# Snow conductivity and avoid division by zero
    if (SurfData.HeightSnow  ==  0.)
        K_S = 0.31;
    else
        K_S = 0.021+(0.0042*RHOSNO)+(2.2E-09*pow(RHOSNO, 3));

    // Initialize T001 to the Air Temperature
    T001 = MetData.AirTemp;

    if (ice) WindSp = 0.00001;
    else     WindSp = MetData.WindSpeed;

    //# Convert julian days to days since the start of the year
    kDays = day_of_year(jday - 1);
    mDays = day_of_year(jday);

    Q_shortwave = calculate_qsw(kDays,mDays,iclock,Latitude,SWOld,ShortWave);

    // Into layer surfLayer goes qsw(surfLayer)-qsw(surfLayer-1) over the
    // area common to layers surfLayer and surfLayer-1, and qsw(surfLayer)
    // over the rest of area(surfLayer) units of heat[surfLayer] are
    // joules/sec; units of area(surfLayer) are 10**6 m**2
    for (i = (botmLayer+1); i <= surfLayer; i++)
        LayerThickness[i] = Lake[i].Height-Lake[i-1].Height;
    LayerThickness[botmLayer] = Lake[botmLayer].Height;


    if (!ice)
        // Assume PAR only (45% of the incident short wave radiation penetrates beyond the surface layer)
        Lake[surfLayer].Light = 0.45 * Q_shortwave;
    else
        // If there is ice cover the surface layer becomes the ICE/ICE+Snow Layer
        // The surface layer is now linked that of the overlying ice sheet
        Lake[surfLayer].Light =
             (0.45 * Q_shortwave * ((A_ONE * exp(-ET_SNOW_ONE * SurfData.HeightSnow)) +
                     (A_TWO * exp(-ET_SNOW_TWO * SurfData.HeightSnow)))) *
                    ((A_ONE * exp(-ET_WICE_ONE * SurfData.HeightWhiteIce)) +
                     (A_TWO * exp(-ET_WICE_TWO * SurfData.HeightWhiteIce))) *
                    ((A_ONE * exp(-ET_ICE_ONE  * SurfData.HeightBlackIce)) +
                     (A_TWO * exp(-ET_ICE_TWO  * SurfData.HeightBlackIce)));

    Lake[surfLayer-1].Light = Lake[surfLayer].Light * exp(-Lake[surfLayer].ExtcCoefSW*LayerThickness[surfLayer]);

    // Heating due to PAR
    heat[surfLayer] = ( Lake[surfLayer-1].LayerArea * (Lake[surfLayer].Light - Lake[surfLayer-1].Light) +
                       (Lake[surfLayer].LayerArea - Lake[surfLayer-1].LayerArea) * Lake[surfLayer].Light * 0.1) * AreaFactor;
    // Heating due to NIR/UV (all absorbed in surf layer only)
    heat[surfLayer] = heat[surfLayer] + 0.55 * Q_shortwave * Lake[surfLayer].LayerArea * AreaFactor;

    //Total daily short wave radiation (J/day)
    SurfData.dailyQsw += Lake[surfLayer].LayerArea * Q_shortwave * noSecs;


    //  Evaporative heat flux affects top layer only
    //  See tva report p4.11. also p 4.17.  units are joules
    if (!ice) {
        SVP0 = saturated_vapour(Lake[surfLayer].Temp);
  //    Q_latentheat [W/m2] = CE * rho_air * latent heat * psychro const / air_presssure * windspeed * VPD
        Q_latentheat = -CE * 1.200 * (HTEVAP*1e-3) * (0.622/1013.) * WindSp * (SVP0 - MetData.SatVapDef);

        if (Q_latentheat > 0.0) Q_latentheat = 0.0;

        if ( no_evap )
            SurfData.Evap = 0.0;
        else
            SurfData.Evap = Q_latentheat/(HTEVAP);

        // Conductive Heat Gain only affects top layer.
        // Units are Joules/M**2/S
  //    Q_sensibleheat [W/m2] = CH * rho_air * specific heat * windspeed * temp diff
        Q_sensibleheat = -CH * (1.200*1003.) * WindSp * (Lake[surfLayer].Temp - MetData.AirTemp);

        // Long Wave emission (ie. longwave out) affects only top layer.
        Q_lw_out = -Stefan_Boltzman*0.97*pow((Kelvin+Lake[surfLayer].Temp), 4.0);

        // Long Wave absorption (ie. longwave in) affects only top layer
        if (LWModel  ==  LW_CC){
            CLOUD = MetData.LongWave;
            Q_lw_in = (1.0 + 0.275 * CLOUD) * Stefan_Boltzman * pow((Kelvin+MetData.AirTemp), 4.0) *
                  (1.0 - 0.261 * exp(-0.000777E-4 * pow(MetData.AirTemp, 2.0)));
            Q_longwave = Q_lw_out + Q_lw_in;
        } else if (LWModel  ==  LW_IN){
            Q_lw_in = MetData.LongWave;
            Q_longwave = Q_lw_out+Q_lw_in;
        } else if (LWModel  ==  LW_NET)
            Q_longwave = MetData.LongWave;

        heat[surfLayer] = heat[surfLayer]+(Q_latentheat+Q_sensibleheat+Q_longwave)*Lake[surfLayer].LayerArea*AreaFactor;

        // Daily heat budget (J/day)
        SurfData.dailyQe += Q_latentheat * Lake[surfLayer].LayerArea * noSecs;
        SurfData.dailyQh += Q_sensibleheat * Lake[surfLayer].LayerArea * noSecs;
        SurfData.dailyQlw += Q_longwave * Lake[surfLayer].LayerArea * noSecs;
    } else {
        // The various atmospheric fluxes - evaporative, sensible heat, longwave
        // for ice cover all depend on the ice surface temperature.  note that wind
        // here is not set to zero (USE MetData.WindSpeed, NOT WindSp].
        // Emissivity for LW OUT IS 0.95
        T01_NEW = 50.;
        T01_OLD = -50.;
        T001 = 0.;
        while (1) {
            SVP0 = (1+(0.00972*T001)+(0.000042*pow(T001, 2)))*saturated_vapour(T001);
            Q_latentheat = CE * MetData.WindSpeed * (SVP0 - MetData.SatVapDef);
            if (Q_latentheat > 0.0) Q_latentheat = 0.0;
            Q_sensibleheat = CH * MetData.WindSpeed * (T001 - MetData.AirTemp);
            Q_lw_out = -Stefan_Boltzman * 0.95 * pow((Kelvin+T001), 4.0);

            if (LWModel  ==  LW_CC) {
                CLOUD = MetData.LongWave;
                Q_lw_in = (1.0 + 0.275 * CLOUD) * Stefan_Boltzman * pow((Kelvin+MetData.AirTemp), 4.0) *
                      (1.0 - 0.261 * exp(-0.000777E-4 * pow(MetData.AirTemp, 2.0)));
                Q_longwave = Q_lw_out+Q_lw_in;
            } else if (LWModel  ==  LW_IN) {
                Q_lw_in = MetData.LongWave;
                Q_longwave = Q_lw_out+Q_lw_in;
            } else if (LWModel  ==  LW_NET)
                Q_longwave = MetData.LongWave;

            // This is the net meteorological flux that drives the ice algorithm
            // flux due to precipitation on ice or Snow
            H_FLUX = (Q_latentheat+Q_sensibleheat+Q_longwave+QRL);

            // Now determine the new ice/Snow surface temperature based on the balance
            // fluxes h_ice and the expression for the upward heat flux as given by p

            AAA = (1. - exp(-ET_SNOW_ONE * SurfData.HeightSnow)) / (K_S * ET_SNOW_ONE);
            BBB = exp(-ET_SNOW_ONE * SurfData.HeightSnow) * (1.-exp(-ET_WICE_ONE * SurfData.HeightWhiteIce))/
                     (ET_WICE_ONE * K_I2);
            FFF = exp((-ET_SNOW_ONE*SurfData.HeightSnow)-(ET_WICE_ONE * SurfData.HeightWhiteIce))*
                     (1.-exp(-ET_ICE_ONE*SurfData.HeightBlackIce))/(ET_ICE_ONE*K_I1);
            CCC = (1.-exp(-ET_SNOW_TWO*SurfData.HeightSnow))/(K_S*ET_SNOW_TWO);
            DDD = exp(-ET_SNOW_TWO*SurfData.HeightSnow)*(1.-exp(-ET_WICE_TWO*SurfData.HeightWhiteIce))/
                     (ET_WICE_TWO*K_I2);
            GGG = exp((-ET_SNOW_TWO*SurfData.HeightSnow)-(ET_WICE_TWO*SurfData.HeightWhiteIce))*
                     (1.-exp(-ET_ICE_TWO*SurfData.HeightBlackIce))/(ET_ICE_TWO*K_I1);
            EEE = (K_S*K_I1*K_I2)/((SurfData.HeightSnow*K_I1*K_I2)+(SurfData.HeightBlackIce*K_S*K_I2)+
                     (SurfData.HeightWhiteIce*K_S*K_I1));

            Q01 = ((TF-T001-(Q_shortwave*(A_ONE*(AAA+BBB+FFF)+A_TWO*(CCC+DDD+GGG)))+QSI1)*EEE)+Q_shortwave+QSI;

            // Now compare the balance between the surface fluxes - and iterate
            // by bisection - NOTE the loop back to 200 in the iteration
            if (fabs(Q01+H_FLUX) > 1.0 && fabs(T01_NEW-T01_OLD) > 0.001) {
                if ((Q01+H_FLUX) < 0.0) T01_NEW = T001;
                if ((Q01+H_FLUX) > 0.0) T01_OLD = T001;
                T001 = (T01_NEW+T01_OLD)/2.0;
            } else
                break;
        }

        T01_NEW = 50.0;
        T01_OLD = -50.0;
        if (SurfData.HeightSnow > 0.)
            SurfData.HeightSnow = SurfData.HeightSnow+Q_latentheat/(HTEVAP)*noSecs;
        else {
            if (SurfData.HeightWhiteIce > 0.)
                SurfData.HeightWhiteIce = SurfData.HeightWhiteIce+Q_latentheat/(HTEVAP)*noSecs;
            else
                SurfData.HeightBlackIce = SurfData.HeightBlackIce+Q_latentheat/(HTEVAP)*noSecs;
        }

        //--------------------------------------------------------------------+
        // Now compare the ice/Snow surface temperature with the melting      |
        // temperature - if it is above the melting temp. then adjust         |
        // the thickness of the surface ice/Snow layer accordingly            |
        //--------------------------------------------------------------------+
        if (T001 >= TMELT) {
            T001 = TMELT;
            SVP0 = (1+(0.00972*T001)+(0.000042*pow((T001), 2)))*saturated_vapour(T001);
            Q_latentheat = CE * MetData.WindSpeed * (SVP0 - MetData.SatVapDef);
            if (Q_latentheat > 0.0)Q_latentheat = 0.0;
            Q_sensibleheat = CH * MetData.WindSpeed * (T001 - MetData.AirTemp);
            Q_lw_out = -Stefan_Boltzman * 0.95 * pow((Kelvin+T001), 4.0);

            if (LWModel  ==  LW_CC) {
                CLOUD = MetData.LongWave;
                Q_lw_in = (1.0 + 0.275 * CLOUD) * Stefan_Boltzman * pow((Kelvin+MetData.AirTemp), 4.0) *
                      (1.0 - 0.261 * exp(-0.000777E-4 * pow(MetData.AirTemp, 2.0)));
                Q_longwave = Q_lw_out + Q_lw_in;
            } else if (LWModel  ==  LW_IN) {
                Q_lw_in = MetData.LongWave;
                Q_longwave = Q_lw_out + Q_lw_in;
            } else if (LWModel  ==  LW_NET)
                Q_longwave = MetData.LongWave;

            H_FLUX = (Q_latentheat+Q_sensibleheat+Q_longwave+QRL);
            Q01 = ((TF-T001-(Q_shortwave*(A_ONE*(AAA+BBB+FFF)+A_TWO*(CCC+DDD+GGG))) +QSI1)*EEE)+Q_shortwave+QSI;

            //------------------------------------------------------------------
            // Now determine the new ice/Snow/water surface temperature and height ad
            // based on the balance between the surface fluxes h_ice and the expression
            // the upward heat flux as given by Patterson and Hamblin (1988]. note on
            // melting can occur at the surface and therefore water level will increase
            // note assumption that ice thickness won't change more than 1 cm in a ti
            //------------------------------------------------------------------
            if (SurfData.HeightSnow == 0.0) {
                SurfData.dHt = (1/(L_I*DENSITY_I1))*(H_FLUX+Q01)*noSecs;
                if (SurfData.HeightWhiteIce > 0.){
                    if ((SurfData.HeightBlackIce+SurfData.HeightWhiteIce-SurfData.dHt) < 0.0)
                        SurfData.dHt = SurfData.HeightBlackIce+SurfData.HeightWhiteIce;
                    SurfData.HeightWhiteIce = SurfData.HeightWhiteIce-SurfData.dHt;

                    if (SurfData.HeightWhiteIce < 0.){
                        SurfData.HeightWhiteIce = 0.0;
                        SurfData.HeightBlackIce = SurfData.HeightBlackIce+SurfData.HeightWhiteIce-SurfData.dHt;
                    }
                } else {
                    if ((SurfData.HeightBlackIce-SurfData.dHt) < 0.)SurfData.dHt = SurfData.HeightBlackIce;
                    SurfData.HeightBlackIce = SurfData.HeightBlackIce-SurfData.dHt;
                }
                Lake[surfLayer].Height = Lake[surfLayer].Height+SurfData.dHt*(DENSITY_I1/(1000.+Lake[surfLayer].Density));
            } else {
                if (RHOSNO == 0.0) RHOSNO = RHOMXSNO;
                SurfData.dHt = (1/(L_S*RHOSNO))*(H_FLUX+Q01)*noSecs;
                if ((SurfData.HeightSnow-SurfData.dHt) < 0.0)SurfData.dHt = SurfData.HeightSnow;
                SurfData.HeightSnow = SurfData.HeightSnow-SurfData.dHt;
                Lake[surfLayer].Height = Lake[surfLayer].Height+SurfData.dHt*(RHOSNO/(1000.+Lake[surfLayer].Density));
            }

            recalc_surface_salt();
        }

       // Daily heat budget (MJ/day)
       SurfData.dailyQe += Q_latentheat * Lake[surfLayer].LayerArea * noSecs;
       SurfData.dailyQh += Q_sensibleheat * Lake[surfLayer].LayerArea * noSecs;
       SurfData.dailyQlw += Q_longwave * Lake[surfLayer].LayerArea * noSecs;

    }

    // Now look at the ice or water interface
    if (surfLayer > botmLayer) {
        for (i = surfLayer-1; i >= botmLayer; i-- )
            Lake[i].Light = Lake[i+1].Light * exp(-Lake[i+1].ExtcCoefSW*LayerThickness[i+1]);

        /*--------------------------------------------------------------------*
         * Into layer i goes QSW[i]-QSW(i-1) over the area common to layers   *
         * i and i-1 and QSW[i] over the rest of AREA[i]                      *
         * units of heat[i) are joules/sec; units of AREA[i] are 10**6 m**2   *
         *--------------------------------------------------------------------*/
        for (i = surfLayer-1; i >= (botmLayer+1); i--)
            heat[i] = (Lake[i-1].LayerArea * (Lake[i].Light - Lake[i-1].Light) +
                       Lake[i].Light * (Lake[i].LayerArea - Lake[i-1].LayerArea)) * 0.1 *AreaFactor;
        heat[botmLayer] = Lake[botmLayer].Light * Lake[botmLayer].LayerArea * AreaFactor;
    }

    // Compute the temperature increase in non-surface layers over noSecs
    for (i = botmLayer; i <= surfLayer; i++) {
        if (fabs(heat[i]) >= 1E-20 && Lake[i].Density != 0.0 && Lake[i].LayerVol != 0.0)
            DT = heat[i]*noSecs/(SPHEAT*(1000.+Lake[i].Density)*Lake[i].LayerVol*AreaFactor);
        else
           DT = 0.;

        Lake[i].Temp += DT;
    }

    // The change in ice thickness at the bottom can now be determined
    // with the temperature of the lake water readjusted for the surface
    // heat exchange influence of the Ice/Snow
    if (ice) {
        // Both ablation and accretion of the ice can occur at the ice-water interface
        // change dht will be governed by the flux coming down from the surface
        // The water below the ice , See Patterson + Hamblin (1988)
        // first calculate the flux through the ice
        AAA = (1. - exp(-ET_SNOW_ONE * SurfData.HeightSnow)) / (K_S * ET_SNOW_ONE);
        BBB = exp(-ET_SNOW_ONE*SurfData.HeightSnow)*(1.-exp(-ET_WICE_ONE*SurfData.HeightWhiteIce))/ (ET_WICE_ONE*K_I2);
        FFF = exp((-ET_SNOW_ONE*SurfData.HeightSnow)-(ET_WICE_ONE*SurfData.HeightWhiteIce))*
                                      (1.-exp(-ET_ICE_ONE*SurfData.HeightBlackIce))/(ET_ICE_ONE*K_I1);
        CCC = (1.-exp(-ET_SNOW_TWO*SurfData.HeightSnow))/(K_S*ET_SNOW_TWO);
        DDD = exp(-ET_SNOW_TWO*SurfData.HeightSnow)*(1.-exp(-ET_WICE_TWO*SurfData.HeightWhiteIce))/ (ET_WICE_TWO*K_I2);
        GGG = exp((-ET_SNOW_TWO*SurfData.HeightSnow)-(ET_WICE_TWO*SurfData.HeightWhiteIce))*
                                      (1.-exp(-ET_ICE_TWO*SurfData.HeightBlackIce))/(ET_ICE_TWO*K_I1);
        EEE = (K_S*K_I1*K_I2)/((SurfData.HeightSnow*K_I1*K_I2)+(SurfData.HeightBlackIce*K_S*K_I2)+
                                       (SurfData.HeightWhiteIce*K_S*K_I1));
        Q01 = ((TF-T001-(Q_shortwave*(A_ONE*(AAA+BBB+FFF)+A_TWO*(CCC+DDD+GGG))) +QSI1)*EEE)+Q_shortwave+QSI;
        QF1 = Q01-(Q_shortwave*A_ONE*(1.-exp(-(ET_SNOW_ONE*SurfData.HeightSnow+ET_ICE_ONE*SurfData.HeightBlackIce
                            +ET_WICE_ONE*SurfData.HeightWhiteIce))))-(Q_shortwave*A_TWO*(1.-exp(-(ET_SNOW_TWO*
                             SurfData.HeightSnow+ET_ICE_TWO*SurfData.HeightBlackIce+ET_WICE_TWO*SurfData.HeightWhiteIce))));

        // Now determine the flux through the lake water below the ice
        // for temperature flux at the ice water interface use an exponential dec
        // fickian diffusion so use a gaussian distribution and adjust the diffuse
        // Accordingly - see farmer (1978) from p and h (1988)
        // see eq 22 of rogers et al and note that DZ has been adjusted to ...
        QI1 = -K_W*(TF-Lake[surfLayer].Temp)/0.039;
        if (underFlow)
            QS1 = CSEN*Lake[surfLayer].Density*SPHEAT*1000.*U_FLOW*(Lake[surfLayer].Temp-TF);
        else
            QS1 = 0.0;

        QW1 = QI1+QS1+QLATENT;

        // Now we can determine the amount of ablation or accretion of ice at
        // the given by qf-qw.  once the ice has been melted or formed assume
        // fluxes are in equilibrium - thus determining the heat loss/gain from
        // the surf calorimetry].  correction for thermal contraction given by
        // ratios of d is constant and area can be considered as constant as
        // change in depth again because time step is small, error for complete
        // melting of ice wi

        SurfData.dHt = (QF1-QW1)*noSecs/(L_I*DENSITY_I1);
        if (SurfData.dHt < -1.*SurfData.HeightBlackIce) {
            if (SurfData.dHt < -1. * (SurfData.HeightBlackIce+SurfData.HeightWhiteIce) )
                SurfData.dHt = -1.*(SurfData.HeightBlackIce+SurfData.HeightWhiteIce);
            SurfData.HeightWhiteIce = SurfData.HeightWhiteIce+SurfData.HeightBlackIce+SurfData.dHt;
            SurfData.HeightBlackIce = 0.0;
        } else
            SurfData.HeightBlackIce = SurfData.HeightBlackIce+SurfData.dHt;

        // Adjust water temperature for ice/water exchange, determine latent heat
        Lake[surfLayer].Temp = Lake[surfLayer].Temp+(((K_W*((TF-Lake[surfLayer].Temp)/0.039))*Lake[surfLayer].LayerArea*
                                 AreaFactor*noSecs)+QLATENT)/(SPHEAT*(1000.+Lake[surfLayer].Density)*Lake[surfLayer].LayerVol*AreaFactor);
        QLATENT = L_I*DENSITY_I1*SurfData.dHt/noSecs;
        Lake[surfLayer].Height = Lake[surfLayer].Height-SurfData.dHt*(DENSITY_I1/(1000.+Lake[surfLayer].Density));

        recalc_surface_salt();
    }

/******************************************************************************
 * Hard coded sediment "heating" factor in glm_surface.c but since it is based
 * on Mendota ends up cooling Kinneret!  In the future we will code in a
 * sediment heating function but will need to be generic and parameterised so
 * best to remove for this version.
 *            -----------------------------------

    // Input of heat from the sediments - based on rogers et al heat flux
    // sediment temperature measurements for lake mendota (birge et al 1927)
    // and basically invariant at 5m at deep station, LayerThickness is required
    TYEAR = 9.7+2.7*sin(((kDays-151.3)*2.*Pi)/365.);
    ZSED = 6.;
    KSED = 1.2;
    for (i = botmLayer+1; i <= surfLayer; i++) {
        Lake[i].Temp += ((KSED*(TYEAR-Lake[i].Temp)/ZSED)*
                  (Lake[i].LayerArea-Lake[i-1].LayerArea)*
                   AreaFactor*LayerThickness[i]*noSecs)/(SPHEAT*(1000.+Lake[i].Density)*Lake[i].LayerVol*AreaFactor);
    }
    Lake[botmLayer].Temp += ((KSED*(TYEAR-Lake[botmLayer].Temp)/ZSED)*
                               Lake[botmLayer].LayerArea*AreaFactor*LayerThickness[botmLayer] *
                           noSecs)/(SPHEAT*(1000.+Lake[botmLayer].Density)*Lake[botmLayer].LayerVol*AreaFactor);
 *
 ******************************************************************************/

    // precipitation, evaporation in the absence of ice
    if (! ice) {
        SurfData.dailyEvap += SurfData.Evap * noSecs * Lake[surfLayer].LayerArea * 1000.0;
        SurfData.dailyRain += MetData.Rain * (noSecs / SecsPerDay) * Lake[surfLayer].LayerArea;

        // Rainfall composition.  NOTE that evaporation leaves salts (nutrients)
        // deposits them at a rate dependent on the input composition of rainfall
        // with changes in depth, not area, for the surface layer. therefore just
        // depths to get new composition. firstly evaporation
        Lake[surfLayer].Height += (SurfData.Evap * noSecs + (MetData.Rain / 1000.0) * (noSecs / SecsPerDay));

        recalc_surface_salt();
    }

    if (iclock == 0 && ice) {
        // Snow cover as well as ice cover
        if (SurfData.HeightSnow > 0.0) {
            if (MetData.Snow > 0.0 && MetData.Rain >= 0.0) {
                // SNOWFALL ON Snow
                if (MetData.Rain == 0.0) MetData.Rain = MetData.Snow*0.15;
                if (MetData.AirTemp > 0.0)
                    KCOMSNO = 0.166+0.834*(1.-exp(-1.*MetData.Rain));
                else
                    KCOMSNO = 0.088+0.912*(1.-exp(-1.*MetData.Rain));

                RHOLDSNO = RHOSNO+(RHOMXSNO-RHOSNO)*KCOMSNO;
                SurfData.HeightSnow = SurfData.HeightSnow*RHOSNO/RHOLDSNO;
                RHOSNO = ((RHOLDSNO*SurfData.HeightSnow)+MetData.Rain)/(SurfData.HeightSnow+(MetData.Snow/1000.));
                SurfData.HeightSnow = SurfData.HeightSnow+(MetData.Snow/1000.);
                if (RHOSNO > RHOMXSNO) RHOSNO = RHOMXSNO;
                if (RHOSNO < RHOMNSNO) RHOSNO = RHOMNSNO;
                QRL = 0.0;
            } else if (MetData.Snow == 0.0 && MetData.Rain == 0.0) {
                // No snowfall or rainfall, compaction only of snow
                if (MetData.AirTemp > 0.0) KCOMSNO = 0.166;
                else                        KCOMSNO = 0.088;

                RHOLDSNO = RHOSNO+(RHOMXSNO-RHOSNO)*KCOMSNO;
                SurfData.HeightSnow = SurfData.HeightSnow*RHOSNO/RHOLDSNO;
                RHOSNO = RHOLDSNO;
                QRL = 0.0;
            } else if (MetData.Snow == 0.0 && MetData.Rain > 0.0) {
                //------------------------------------------------------------------#
                // Rainfall on Snow. Check the air temperature. If AirTemp > 0 then #
                // add the Rain. If AirTemp < 0 then add the rainfall to Snow.      #
                //------------------------------------------------------------------#
                if (MetData.AirTemp > 0.0) {
                    KCOMSNO = 0.166+0.834*(1.-exp(-1.*MetData.Rain));
                    RHOLDSNO = RHOSNO+(RHOMXSNO-RHOSNO)*KCOMSNO;
                    SurfData.HeightSnow = SurfData.HeightSnow*RHOSNO/RHOLDSNO;
                    RHOSNO = RHOLDSNO;
                    Lake[surfLayer].Height = Lake[surfLayer].Height+MetData.Rain/1000.0;

                    recalc_surface_salt();

                    if (T001 == TMELT)
                        QRL = SPHEAT*AreaFactor*(MetData.AirTemp-T001)*(MetData.Rain/1000.)/noSecs;
                    else
                        QRL = L_I*1000.*(MetData.Rain/1000.)/noSecs;
                } else {
                    MetData.Snow = MetData.Rain/RHOMXSNO;
                    RHOLDSNO = RHOMXSNO;
                    SurfData.HeightSnow = SurfData.HeightSnow*RHOSNO/RHOLDSNO;
                    RHOSNO = RHOMXSNO;
                    SurfData.HeightSnow = SurfData.HeightSnow+MetData.Snow;
                    QRL = 0.0;
                }
            }
        } else {  // no snow
            if (MetData.Snow > 0.0 && MetData.Rain >= 0.0) {
                // Snowfall on ice
                if (MetData.Rain == 0.0)MetData.Rain = MetData.Snow*0.15;
                SurfData.HeightSnow = MetData.Snow/1000.;
                RHOSNO = 1000.*MetData.Rain/MetData.Snow;
                if (RHOSNO > RHOMXSNO)RHOSNO = RHOMXSNO;
                if (RHOSNO < RHOMNSNO)RHOSNO = RHOMNSNO;
                QRL = 0.0;
            } else if (MetData.Snow == 0.0 && MetData.Rain == 0.0) {
                // No snowfall or rainfall
                QRL = 0.0;
            } else if (MetData.Snow == 0.0 && MetData.Rain > 0.0) {
                // Rainfall on ice - need to know whether it will contribute to water (T
                if (MetData.AirTemp > 0.0) {
                    Lake[surfLayer].Height = Lake[surfLayer].Height+MetData.Rain/1000.0;

                    recalc_surface_salt();

                    if (T001 == TMELT)
                        QRL = SPHEAT*AreaFactor*(MetData.AirTemp-T001)*(MetData.Rain/1000.)/noSecs;
                    else
                        QRL = L_I*1000.*(MetData.Rain/1000.)/noSecs;
                } else {
                    MetData.Snow = MetData.Rain/RHOMXSNO;
                    RHOSNO = RHOMXSNO;
                    SurfData.HeightSnow = MetData.Snow;
                    QRL = 0.0;
                }
            }
        }

        // Archmides principle for weight of Snow on Ice, White Ice formation
        if (RHOSNO == 0.) RHOSNO = 0.00000000000000001; //CAB# fudge factor!

        if (SurfData.HeightSnow > ((SurfData.HeightBlackIce*(1000.-DENSITY_I1) +
                                    SurfData.HeightWhiteIce*(1000.-DENSITY_I2))/RHOSNO)) {
            HEN = SurfData.HeightSnow-(SurfData.HeightBlackIce*(1000.-DENSITY_I1) +
                      SurfData.HeightWhiteIce*(1000.-DENSITY_I2))/RHOSNO;
            QSI = ((Lake[surfLayer].Temp*SPHEAT*1000.+L_I)*(Lake[surfLayer].Density +
                 1000.)*HEN*(1.-(RHOSNO/(Lake[surfLayer].Density+1000.))))/SecsPerDay;
            SurfData.HeightWhiteIce = SurfData.HeightWhiteIce+SurfData.HeightSnow -
                 ((SurfData.HeightBlackIce*(1000.-DENSITY_I1) +
                   SurfData.HeightWhiteIce*(1000.-DENSITY_I2))/RHOSNO);
            QSI1 = (QSI*SurfData.HeightSnow)/(2.0*K_S);
            SurfData.HeightSnow =
                  (SurfData.HeightBlackIce*(1000.-DENSITY_I1) +
                   SurfData.HeightWhiteIce*(1000.-DENSITY_I2))/RHOSNO;
        } else {
            HEN = 0.0;
            QSI = 0.0;
            QSI1 = 0.0;
        }

        // Rainfall/snowfall and dry deposition effects on nutrients
        SUMPO4 = SUMPO4 + 0.145 + MetData.RainConcPO4 * MetData.Rain / 1000.0;
        SUMTP  = SUMTP  + 0.368 + MetData.RainConcTp  * MetData.Rain / 1000.0;
        SUMNO3 = SUMNO3 + 0.507 + MetData.RainConcNO3 * MetData.Rain / 1000.0;
        SUMNH4 = SUMNH4 + 0.507 + MetData.RainConcNH4 * MetData.Rain / 1000.0;
        SUMTN  = SUMTN  + 2.576 + MetData.RainConcTn  * MetData.Rain / 1000.0;
        SUMSI  = SUMSI  + 0.0   + MetData.RainConcSi  * MetData.Rain / 1000.0;
    } // end iclock == 0 && ice

    // Recalculate densities
    for (i = botmLayer; i <= surfLayer; i++)
        Lake[i].Density = calculate_density(Lake[i].Temp,Lake[i].Salinity);

    // Set ice cover flag
    if (Lake[surfLayer].Temp <= 0.0 && SurfData.HeightBlackIce == 0.) {
        ice = TRUE;
        SurfData.HeightBlackIce = 0.05;
        SurfData.HeightWhiteIce = 0.0;
        Lake[surfLayer].Height = Lake[surfLayer].Height-0.05*(DENSITY_I1/(1000.+Lake[surfLayer].Density));

        recalc_surface_salt();

        SurfData.HeightSnow = 0.0;
    }

    if ((SurfData.HeightBlackIce+SurfData.HeightWhiteIce) < 0.05  &&  ice) {
        Lake[surfLayer].Height = Lake[surfLayer].Height+SurfData.HeightBlackIce*
              (DENSITY_I1/(1000.+Lake[surfLayer].Density))+SurfData.HeightWhiteIce*
              (DENSITY_I2/(1000.+Lake[surfLayer].Density))+SurfData.HeightSnow*
              (RHOSNO/(1000.+Lake[surfLayer].Density));

        recalc_surface_salt();

        ice = FALSE;
        SurfData.HeightBlackIce = 0.0;
        SurfData.HeightWhiteIce = 0.0;
        SurfData.HeightSnow = 0.0;
    }
    //for (i = botmLayer; i <= surfLayer; i++)
        //printf("Light = %10.5f\n",Lake[surfLayer].Light);
        //printf("surfLayer = %d\n",surfLayer);

#ifdef _VISUAL_C_
    free(LayerThickness);  free(heat);
#endif

}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/*############################################################################*/
REALTYPE calculate_qsw(int kDays, int mDays, int iclock, REALTYPE Latitude, REALTYPE SWOld, REALTYPE ShortWave)
/*----------------------------------------------------------------------------*
 * ARGUMENTS
 * INTEGER,INTENT(in)   :: kDays     !# Days since start of year for yesterday
 * INTEGER,INTENT(in)   :: mDays     !# Days since start of year for today
 * INTEGER,INTENT(in)   :: iclock    !# #seconds into the day
 * REALTYPE,INTENT(in)  :: Latitude
 * REALTYPE,INTENT(in)  :: SWOld     !# Total solar radiation at the surface for yesterday
 * REALTYPE,INTENT(in)  :: ShortWave !# Total solar radiation at the surface for today
 *----------------------------------------------------------------------------*/
{
    REALTYPE RADTIM0;  //# Start of time step as fraction of day in radians
    REALTYPE RADTIM1;  //# End of time step as fraction of day in radians
    REALTYPE SOLAR0;   //# Solar angle yesterday
    REALTYPE SOLAR1;   //# Solar angle today
    REALTYPE SOLARW;   //# Sun's arc from sunrise to sunset for yesterday
    REALTYPE SOLARY;   //# Sun's arc from sunrise to sunset for today
    REALTYPE SW0;      //# Total solar radiation at the surface for yesterday after ALBEDO
    REALTYPE SW1;      //# Total solar radiation at the surface for today after ALBEDO
    REALTYPE TD0;      //# Day length for yesterday
    REALTYPE TD1;      //# Day length for yesterday
    int jClock;        //# End of the current time step

    REALTYPE Albedo0;  //# Photosynthetically active radiation scattered previous day
    REALTYPE Albedo1;  //# Photosynthetically active radiation scattered present day
    REALTYPE T001;
    REALTYPE lQSW;

/*----------------------------------------------------------------------------*/

    lQSW = 0.;

    RADTIM1 = 0.;
    RADTIM0 = 0.;

    Albedo0 = 0.;
    Albedo1 = 0.;

    //# Initialize T001 to the Air Temperature
    T001 = MetData.AirTemp;

    // Put in wind factor, set ice flag, determine resultant albedo
    if (ice) {
        if ((SurfData.HeightBlackIce+SurfData.HeightWhiteIce) > 0.55) {
            if (T001 <= -5.)                   Albedo0 = 0.6;
            else if (T001 > -5.0 && T001 < 0.) Albedo0 = 0.44 - 0.032 * T001;
            else if (T001 >= 0.)               Albedo0 = 0.44;
        } else
            Albedo0 = 0.08+0.44*(pow((SurfData.HeightBlackIce+SurfData.HeightWhiteIce-0.05), 0.28));

        if (SurfData.HeightSnow > 0.0) {
            if (T001 <= -5.)                   Albedo1 = 0.7;
            else if (T001 > -5.0 && T001 < 0.) Albedo1 = 0.5-0.04*T001;
            else if (T001 >= 0.)               Albedo1 = 0.5;

            if (SurfData.HeightSnow < 0.1)
                Albedo0 = Albedo1-(((0.1-SurfData.HeightSnow)/0.1)*(Albedo1-Albedo0));
            else
                Albedo0 = Albedo1;
        }

        Albedo1 = Albedo0;
    } else {
        if (Latitude > 2*Pi) { //# Lake is in the northern hemisphere
            Albedo0 = 0.08-0.02*sin(2*Pi*kDays/365-(Pi/2));
            Albedo1 = 0.08-0.02*sin(2*Pi*mDays/365-(Pi/2));
        } else { //# Lake is in the southern hemisphere
            Albedo0 = 0.08-0.02*sin(2*Pi*kDays/365+(Pi/2));
            Albedo1 = 0.08-0.02*sin(2*Pi*mDays/365+(Pi/2));
        }
    }


    if ( subdaily )
        lQSW = ShortWave * (1.0-Albedo1);
    else {
        //# Determine the daylength (hours) from the latitude (entered as -ve for
        //# and the number of days since the start of the year. firstly determine
        SOLAR0 = -23.45 * sin((284 + kDays) * 2.0 * Pi / 365.0);
        SOLAR1 = -23.45 * sin((284 + mDays) * 2.0 * Pi / 365.0);

        // Determine the angle of the sun's arc from sunrise to sunset after
        // converting to radians, first determine solar declination
        SOLAR0 = SOLAR0 * Pi / 180;
        SOLAR1 = SOLAR1 * Pi / 180;
        SOLARW = 2.0 * acos(-tan(Latitude) * tan(SOLAR0));
        SOLARY = 2.0 * acos(-tan(Latitude) * tan(SOLAR1));

        // Calculate the daylength (Secs) using angular velocity 15 Degrees/Hour
        TD0 = 3600 * SOLARW / (15 * Pi/180);
        TD1 = 3600 * SOLARY / (15 * Pi/180);

        // Set the end of the glm time step
        jClock = iclock + noSecs;

        // Convert the start and end of the day to radians
        if (iclock < 0.5*TD0)
            RADTIM0 = iclock*Pi/TD0+Pi/2;
        else if (iclock >= 0.5*TD0 && iclock <= SecsPerDay-0.5*TD0)
            RADTIM0 = Pi;
        else if (iclock > SecsPerDay-0.5*TD0)
            RADTIM0 = (0.5*TD0+iclock-SecsPerDay)*Pi/TD0;

        if (jClock < 0.5*TD1)
            RADTIM1 = jClock*Pi/TD1+Pi/2;
        else if (jClock >= 0.5*TD1 && jClock <= SecsPerDay-0.5*TD1)
            RADTIM1 = Pi;
        else if (jClock > SecsPerDay-0.5*TD1)
            RADTIM1 = (0.5*TD1+jClock-SecsPerDay)*Pi/TD1;

        SW0 = SWOld*(1.0-Albedo0) * 86.4;
        SW1 = ShortWave*(1.0-Albedo1) * 86.4;

        // Determine the area under a sinusoidal curve and convert to a rate
        if (iclock <= (SecsPerDay - 0.5 * TD0) && jClock > (SecsPerDay - 0.5 * TD1) )
            lQSW = (0.5*SW0*1000.0*(cos(-1.0*RADTIM0)+1.0) + 0.5*SW1*1000.0*(1.0-cos(-1.0*RADTIM1)))/noSecs;
        else if (iclock <= (SecsPerDay - 0.5 * TD0) && jClock <= (SecsPerDay - 0.5 * TD1))
            lQSW = (0.5*SW0*1000.0*(cos(-1.0*RADTIM0)-cos(-1.0*RADTIM1)))/noSecs;
        else if (iclock > (SecsPerDay - 0.5 * TD0) && jClock > (SecsPerDay - 0.5 * TD1))
            lQSW = (0.5*SW1*1000.0*(cos(-1.0*RADTIM0)-cos(-1.0*RADTIM1)))/noSecs;
    }

    if (lQSW < 0.1) lQSW = 0.0;

    return lQSW;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
void recalc_surface_salt()
{
    REALTYPE OldVol, AddDensity, WaterMass;

    OldVol = Lake[surfLayer].LayerVol;

    resize_internals(1, surfLayer);

    AddDensity = calculate_density(Lake[surfLayer].Temp, zero);

    WaterMass = OldVol * (Lake[surfLayer].Density - AddDensity) +
                Lake[surfLayer].LayerVol * (AddDensity + 1000.);

    Lake[surfLayer].Salinity = ((1000. + Lake[surfLayer].Density) / WaterMass) *
                                                   OldVol * Lake[surfLayer].Salinity;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


#define r_zero    0.0
#define r_one     1.0
#define r_two     2.0
#define r_three   3.0
#define r_five    5.0
#define r_ten    10.0
#define r_comp    0.5
#define c_gas     0.0
#define c_tbase   0.0
#define gamma_air 0.0
#define k_air     0.0
#define cp_air    0.0
#define mu_air    0.0
#define GRAV_TRUE 0.0
#define c_latent_heat 0.0
#define WIND_HEIGHT 100.
#define HUMIDITY_HEIGHT 100.
#define RHO_AIR 1.0

#define SIGN(a,b)  ( ((b) > 0.) ? abs(a) : -abs(a) )


static REALTYPE PSI_M(REALTYPE zL);
static REALTYPE PSI_HW(REALTYPE zL);

#if 0
/******************************************************************************/
void  Atmos_Stability(REALTYPE *evap_heat_flux,
                      REALTYPE *sens_heat_flux,
                      REALTYPE *wind_speed,
                      REALTYPE *temperature,
                      REALTYPE *t_air,
                      REALTYPE *p_atm,
                      REALTYPE *c_wind,
                      REALTYPE *humidity_surface,
                      REALTYPE *humidity_altitude,
                      REALTYPE *m_wet_point,
                      REALTYPE *m_surface,
                      int n_max, int n_surface)
{
    REALTYPE U10[n_surface+1], U_sensM[n_surface+1], CD4[n_surface+1];
    REALTYPE zL, L, zL0, z0, zS, G1, G2, G3, G5, G6;
    REALTYPE U_sensH, CDN10, CHWN10, CDN4, CDN3;
    REALTYPE CHW, CHWN, rCDN, P1, P2, P4;
    REALTYPE atm_density, T_virt, Ux, dT, dq;
    REALTYPE SH, LH, momn_flux;
    REALTYPE r_o, r_a, rho_a, rho_o, alpha_e;
    REALTYPE evap_heat_flux_still, sens_heat_flux_still;

    int NCOUNT, i;

    REALTYPE c_vonK = 0.41;      // Von Karmans constant
    REALTYPE c_z0 = 0.0001;      // Default roughness

    REALTYPE zL_MAX;

/*----------------------------------------------------------------------------*/

    // Bound the iteration (e.g. 15 for 10m, 3 for 2m)
    zL_MAX = -15.0;

/*
    if (time_step == 1) {
        printf("                       --oo0oo--                          \n");
        printf("  Correction for atmospheric stability is being calculated\n");
        printf("  Anemometer Height (m)                  = %f\n",WIND_HEIGHT);
        printf("  Humidity/Temperature Sensor Height (m) = %f\n",HUMIDITY_HEIGHT);
        printf("                       --oo0oo--                          \n");
    }
*/

    if (abs(WIND_HEIGHT-10.0) > r_comp) {
        for (i = 0; i <= n_surface; i++)
            U10[i] = wind_speed[i]*(log(r_ten/c_z0)/log(WIND_HEIGHT/c_z0));
    } else {
        for (i = 0; i <= n_surface; i++)
            U10[i] = wind_speed[i];
    }
    for (i = 0; i <= n_surface; i++)
        U_sensM[i] = wind_speed[i];

    CHWN10 = 0.0013;
    for (i = 0; i <= n_surface; i++) {
        if (m_wet_point(m_surface[i]) == 0)
            continue;  // back to start of the loop

        // Calculate still air approximations and use this as a minimum
        // Fluxes to sill air see TVA Section 5.311 and 5.314

        // mixing ratios
        r_o = humidity_surface[i]/(1-humidity_surface[i]/c_gas);
        r_a = humidity_altitude[i]/(1-humidity_altitude[i]/c_gas);

        // density
        // 0.01 is for conversion from pascal to millibars
        rho_a = 0.348*((1+r_a)/(1+1.61*r_a))*(p_atm[i]*0.01/(t_air[i]+c_tbase));
        rho_o = 0.348*((1+r_o)/(1+1.61*r_o))*(p_atm[i]*0.01/(temperature(m_surface[i])+c_tbase));

        dT = (temperature(m_surface[i]) - t_air[i]);
        dq = (humidity_surface[i] - humidity_altitude[i]);

        if (rho_a - rho_o > r_zero) {
            alpha_e = 0.137*0.5*(gamma_air/cp_air)* pow((GRAV_TRUE*(rho_a-rho_o)/(rho_a*mu_air*k_air)), (1/r_three));
            sens_heat_flux_still = -alpha_e * dT;
            evap_heat_flux_still = -alpha_e * dq * c_latent_heat;
        } else {
            sens_heat_flux_still = r_zero;
            evap_heat_flux_still = r_zero;
        }

        if (U_sensM[i]<0.01) {
            sens_heat_flux[i] = sens_heat_flux_still;
            evap_heat_flux[i] = evap_heat_flux_still;
        } else {
            // Neutral Drag Coefficient is a function of windspeed @ 10m
            CDN10 = 0.001;
            if (U10[i] > r_five) {
               CDN10 = (r_one + 0.07*(U10[i]-r_five))/1000.0;
            }

            // Estimate Surface Roughness Lengths
            z0 = r_ten/(exp(c_vonK/sqrt(CDN10)));
            zS = r_ten/(exp(c_vonK*c_vonK/(CHWN10*log(r_ten/z0))));

            // Height Correction Factors
            G1 = log(r_ten/z0);
            G2 = log(r_ten/zS);
            G3 = log(HUMIDITY_HEIGHT/zS);
            G5 = log(HUMIDITY_HEIGHT/z0);
            G6 = log(WIND_HEIGHT/z0);

            CDN4 = CDN10*(G1*G1)/(G6*G6);    // Scale down to sensor heights
            CDN3 = CDN10*(G1*G1)/(G5*G5);
            CHWN = CHWN10*(G1*G2)/(G5*G3);
            CD4[i] = CDN4;                   // Initialize
            CHW = CHWN;

            // Windspeed at Humidity Sensor
            U_sensH = U_sensM[i]*(G5/G6);

            // Atmospheric Density
            atm_density = RHO_AIR*c_tbase/(t_air[i]+c_tbase);

            // Virtual Air Temperature
            T_virt = (t_air[i] + c_tbase)*(r_one + 0.61*humidity_altitude[i]);

            // Heat Fluxes
            dT = (temperature(m_surface[i]) - t_air[i]);
            dq = (humidity_surface[i] - humidity_altitude[i]);
            SH = CHW * atm_density * cp_air * U_sensH  * dT;
            LH = CHW * atm_density * U_sensH * dq;

            // Friction Velocity
            momn_flux = CD4[i] * atm_density * U_sensM[i]*U_sensM[i];
            Ux = sqrt(momn_flux/atm_density);

            // Monin - Obukhov Length
            L = -atm_density *Ux*Ux*Ux * T_virt / (c_vonK * GRAV_TRUE
                            * ((SH/cp_air) + 0.61*(t_air[i]+c_tbase)*LH));
            if (abs(L) < r_comp) {
                L = SIGN(r_one*1.0e-20,dT);
            }
            zL = HUMIDITY_HEIGHT/L;

            // Start Iterative Sequence for Heat Flux Calculations
            NCOUNT = 1;
            zL0 = r_zero;
            while ((abs(zL - zL0)>= 0.0001*abs(zL)) && (abs(zL) <= abs(zL_MAX))) {
                zL0 = zL;
                NCOUNT = NCOUNT + 1;
                zL = WIND_HEIGHT/L;
                if (NCOUNT>=15) {
                    break;
                }
                // Calculate Drag Coefficient, CD
                P4 = PSI_M(zL);
                rCDN = sqrt(CDN4);
                CD4[i] = CDN4/(r_one+CDN4*(P4*P4 - r_two*c_vonK*P4/rCDN)/(c_vonK*c_vonK));
                // Calculate Humdity/Temp Coefficient, CHW
                zL = HUMIDITY_HEIGHT/L;

                P1 = PSI_M(zL);
                P2 = PSI_HW(zL);
                rCDN = sqrt(CDN3);
                CHW = CHWN/(r_one + CHWN*(P1*P2 - (c_vonK*P2/rCDN)
                            - c_vonK*P1*rCDN/CHWN)/(c_vonK*c_vonK));

                // Recalculate Heat Fluxes
                SH = CHW * atm_density * cp_air * U_sensH * dT;
                LH = CHW * atm_density * U_sensH  * dq;
                momn_flux = CD4[i] * atm_density * U_sensM[i]*U_sensM[i];
                // Recalculate Friction Velocity
                Ux = sqrt(momn_flux/atm_density);
                // Recalculate Monin - Obukhov Length
                L = -atm_density *Ux*Ux*Ux * T_virt / (c_vonK * GRAV_TRUE
                                * ((SH/cp_air) + 0.61*(t_air[i]+c_tbase)*LH));
                if (abs(L) < r_comp) {
                    L = SIGN(r_one*1.0e-20,dT);
                }
                zL = HUMIDITY_HEIGHT/L;
            } // enddo

            // Last Calculation - But 1st, check for high values
            if (abs(zL)>abs(zL_MAX))
                zL = SIGN(abs(zL_MAX),zL);
            else
                zL = WIND_HEIGHT/L;

            P4 = PSI_M(zL);
            rCDN = sqrt(CDN4);
            CD4[i] = CDN4/(r_one+CDN4*(P4*P4 - r_two*c_vonK*P4/rCDN)/(c_vonK*c_vonK));
            zL = zL*HUMIDITY_HEIGHT/WIND_HEIGHT;
            P1 = PSI_M(zL);
            P2 = PSI_HW(zL);
            rCDN = sqrt(CDN3);
            CHW = CHWN/(r_one + CHWN*(P1*P2 - (c_vonK*P2/rCDN)
                                - c_vonK*P1*rCDN/CHWN)/(c_vonK*c_vonK));

            sens_heat_flux[i] = -CHW * atm_density * cp_air * U_sensH * dT;
            evap_heat_flux[i] = -CHW * atm_density * U_sensH * dq * c_latent_heat;

            // Limit minimum to still air value
            if (sens_heat_flux_still < sens_heat_flux[i])
                sens_heat_flux[i] = sens_heat_flux_still;
            if (evap_heat_flux_still < evap_heat_flux[i])
                evap_heat_flux[i] = evap_heat_flux_still;

            // Link stability corrected drag to main code
            c_wind[i] = CD4[i];
        }
    } // enddo
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif

/******************************************************************************/
REALTYPE PSI_M(REALTYPE zL)
{
    REALTYPE X;

    REALTYPE AA = 5.0;
    REALTYPE PIE = 3.14159;

/*----------------------------------------------------------------------------*/

    if (zL < 0.0) {
        X = pow(r_one - 16.0*zL, 0.25);
        return r_two*log((r_one+X)/r_two)+log((r_one+X*X)/r_two) - r_two*atan(X) + PIE/r_two;
    } else if (zL > 0.0) {
        if (zL > 0.5) {
            if (zL > r_ten)
                return log(r_one*zL) - 0.76*zL - 12.093;
            return (0.5/(zL*zL)) -4.25/zL -7.0*log(zL)-0.852;
        }
        return -AA*zL;
    }
    return 0.0;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
REALTYPE PSI_HW(REALTYPE zL)
{
    REALTYPE X;

    REALTYPE AA = 5.0;

/*----------------------------------------------------------------------------*/

    if (zL < 0.0) {
        X = pow(r_one - 16.0*zL, 0.25);
        return r_two*log((r_one+X*X)/r_two);
    } else if (zL > 0.0) {
        if (zL > 0.5) {
            if (zL>r_ten)
                return log(zL) - 0.76*zL - 12.093;
            return (0.5/(zL*zL)) -4.25/zL -7.0*log(zL) -0.852;
        }
        return -AA*zL;
    }

    return 0.0;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
