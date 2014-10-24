/******************************************************************************
 *                                                                            *
 * glm_bird.c                                                                 *
 *                                                                            *
 * Bird and Hulstrom's Solar Irradiance Model                                 *
 * Richard E. Bird and Roland L. Hulstrom                                     *
 * A Simplified Clear Sky Model for Direct and Diffuse Insolation on          *
 *                                                       Horizontal Surfaces. *
 * SERI/TR-642-761                                                            *
 * Solar Energy Research Institute                                            *
 * Golden, Colorado, USA, February 1981.                                      *
 *                                                                            *
 ******************************************************************************/

#include <math.h>

#include "glm.h"
#include "glm_types.h"
#include "aed_time.h"


// #define Pi 3.14
// typedef double AED_REAL;

//# All calculations are hourly!
//#_____________________________________________________________________________
//#Inputs
 
//#lat = -31.77; % -ve for S & +ve for N
//#lon = 116.03; % +ve for E & -ve for W
//#TZ = 7.5; %Time Zone +ve for East & -ve for W
//# startdate = datenum(2010,06,01,00,00,00); %Specify Start Date
//# enddate   = datenum(2011,06,01,00,00,00); %Specify End Date

static AED_REAL E = 200;        //# Station Elevation in m
static AED_REAL AP = 973;       //# Atmospheric Pressure in milibars
static AED_REAL Oz = 0.279;     //# Ozone concentration in atm-cm
static AED_REAL WatVap = 1.1;   //# Total Precipitable water vapor in atm-cm
static AED_REAL AOD500 = 0.033; //# Dimensionless Aerosol Optical Depth at wavelength 500 nm
static AED_REAL AOD380 = 0.038; //# Dimensionless Aerosol Optical Depth at wavelength 380 nm
static AED_REAL Albedo = 0.2;   //# Albedo value kept at default
static AED_REAL deg2rad = Pi/180.;
static AED_REAL rad2deg = 180./Pi;

//# Mean (1 AU) solar constant [W/m2]
#define I_sc  1367



AED_REAL calc_bird(AED_REAL lon, AED_REAL lat, int jday, int iclock)
{
//   //#------------------------------------------------------------------------
//   //#Date Processing
//   
//   NewDate = [startdate:0.041666666666666666666666667:enddate];
//   Date_Vec = datevec(NewDate);
//   
//   
//   //#________________________________________________________________________
//   //#Calculate Day of Year
//   
//   n = datenum(Date_Vec(1,1),Date_Vec(1,2),Date_Vec(1,3)) - datenum(Date_Vec(1,1),01,01);
//   m = datenum(Date_Vec(end,1),Date_Vec(end,2),Date_Vec(end,3)) - datenum(Date_Vec(1,1),Date_Vec(1,2),Date_Vec(1,3));
//   
//   % non loop method
//   ni = NewDate' - (datenum(Date_Vec(:,1),01,01)-1);
//   Day = floor(ni);
//   mHour = ni - Day;
//   Hour = mHour*24;
//

    int Hour = iclock / 3600;
    int day = jday;
    AED_REAL TZ;
//______________________________________________________________________________
    // Extra Terrestrial Beam Intensity
    // Correction of Earth Sun Distance based on elliptical path of the sun
    AED_REAL ETR = I_sc * (1.00011+0.034221*cos(2*Pi*(day-1)/365) +
                                0.00128 *sin(   2*Pi*(day-1)/365) +
                                0.000719*cos(2*(2*Pi*(day-1)/365)) +
                                0.000077*sin(2*(2*Pi*(day-1)/365)) );

    // Day Angle :- Position of the earth in sun's orbit
    AED_REAL DayAngle = (6.283185*(day-1)/365);

    // Solar Declination
    AED_REAL SolarDeclination = ( 0.006918 - 0.399912*cos(DayAngle) +
                                             0.070257*sin(DayAngle) -
                                             0.006758*cos(2*DayAngle) +
                                             0.000907*sin(2*DayAngle) -
                                             0.002697*cos(3*DayAngle) +
                                              0.00148*sin(3*DayAngle) ) * rad2deg;

    // Equation of Time
    AED_REAL EquationOfTime = ( 0.0000075 + 0.001868*cos(DayAngle) -
                                            0.032077*sin(DayAngle) -
                                            0.014615*cos(2*DayAngle) -
                                            0.040849*sin(2*DayAngle) ) * 229.18;

    // Hour Angle
    AED_REAL HourAngle = 15.*(Hour-12.5) + lon - TZ * 15 + (EquationOfTime/4);

    // Zenith Angle
    AED_REAL ZenithAngle = acos( cos(SolarDeclination/rad2deg) *
                                 cos(lat/rad2deg) * cos(HourAngle/rad2deg) + 
                     sin(SolarDeclination/rad2deg) * sin(lat/rad2deg)) * rad2deg;

    // Air Mass
    AED_REAL AirMass = (ZenithAngle < 89) ? 
             1. / (cos(ZenithAngle/rad2deg) + 0.15 / pow(93.885-ZenithAngle, 1.25) )
           : 0.;

    // Rayleigh Scattering
    AED_REAL AMp = (AirMass*AP)/1013;
    AED_REAL TRayleigh = (AirMass > 0) ?
                 exp(-0.0903*pow(AMp,0.84) * (1 + AMp - pow(AMp,1.01)))
               : 0.0;

    // Ozone Scattering
    AED_REAL Ozm = Oz * AirMass;
    AED_REAL Toz = (AirMass > 0) ?
              1-0.1611*Ozm*pow(1.+139.48*Ozm,-0.3035)-0.002715*Ozm/(1.+0.044*Ozm+0.0003*pow(Ozm,2))
            : 0.0;

    // Scattering due to mixed gases
    AED_REAL Tm = (AirMass > 0) ? exp(-0.0127 * pow(AMp, 0.26)) : 0;

    // Scattering due to Water Vapor
    AED_REAL Wm = AirMass * WatVap;
    AED_REAL tWater = (AirMass > 0) ?
                         1-2.4959*Wm/((1.+pow(79.034*Wm,0.6828))+6.385*Wm)
                       : 0.0;

    // Scattering due to Aerosols
    AED_REAL TauA = 0.2758*AOD380 + 0.35*AOD500;
    AED_REAL Ta = (AirMass > 0) ?
                exp((-pow(TauA,0.873))*(1.+TauA-(pow(TauA,0.7088)))*pow(AirMass,0.9108))
              : 0.0;

    AED_REAL TAA = (AirMass > 0) ?
                      1. - 0.1*(1-AirMass+pow(AirMass,1.06))*(1-Ta)
                    : 0.0;

    AED_REAL TAs = Ta / TAA;
    AED_REAL sr = (AirMass > 0) ? 0.0685 + (1 - 0.84) * (1 - TAs) : 0.0;

    // Direct Beam Radiation (Extra-Terrestrial)
    AED_REAL DirectBeam = ETR * 0.9662 * TRayleigh * Toz * Tm * tWater * Ta;

    // Direct Beam Horizontal Radiation
    AED_REAL DirectBeamHoriz = (ZenithAngle < 90) ? DirectBeam * cos(ZenithAngle*deg2rad) : 0.0;

    AED_REAL Ias = (AirMass > 0) ?
               0.79 * ETR * cos(ZenithAngle * deg2rad) * Toz * Tm * tWater * TAA *
                   (0.5 * (1-TRayleigh) + 0.84*(1.-TAs)) / (1-AirMass + pow(AirMass,1.02))
             : 0.0;

    // Global Horizontal Irradiation 
    AED_REAL GHI = (AirMass > 0) ?  (DirectBeamHoriz + Ias)/(1 - Albedo * sr) : 0.0;

    // Diffused Radiation
    AED_REAL DiffuseRad = GHI - DirectBeamHoriz;

    return GHI;
}
