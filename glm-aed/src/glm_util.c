/******************************************************************************
 *                                                                            *
 * glm_util.c                                                                 *
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "glm.h"
#include "glm_types.h"


#ifndef sqr
/******************************************************************************/
// This function computes the square of the argument
REALTYPE sqr(REALTYPE x) { return x*x; }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif


#ifndef gprime
/******************************************************************************
 * This function calculates reduced gravity (gprime) given two densities      *
 * Note +2000.0 is since rho passed in is actually sigma (rho-1000)           *
 * and rho_ref = (rho1 + rho2)                                                *
 ******************************************************************************/
REALTYPE gprime(REALTYPE rho1, REALTYPE rho2)
{ return ((rho2 - rho1) * 9.81 * 2.0) / ((rho1 + rho2) + 2000.0); }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif


/******************************************************************************
 * Function to calculate the proportion of fluid withdrawn from any layer,    *
 * given the depth of its top and bottom, using a curve which fits the region *
 * of fluid drawn in a given time to decide which set of withdrawal curves to *
 * use. If large withdrawal use first set, otherwise the 2nd.                 *
 ******************************************************************************/
REALTYPE dv_1(REALTYPE z1, REALTYPE z2, REALTYPE da, REALTYPE avdel,
              REALTYPE hh, REALTYPE delt, REALTYPE delb)
{
    REALTYPE pt25=0.25,pt5=0.5,pt75=0.75,pt9=0.9;

    REALTYPE a, da4, da7, s1, s2, s3, temp1, temp2,
             tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, z3;
    REALTYPE ret;

/*----------------------------------------------------------------------------*/
    if (da >= pt9*avdel) {
        // Curves for large withdrawal.
        s1 = (z1 - hh) / avdel;
        s2 = (z2 - hh) / avdel;
        a = da / avdel;
        ret = 0.0;

        // If top and bottom of layer fall within lower curve.
        if (z1 <= pt25*a*avdel+hh) {
            tmp1 = s1+two/a/(one+pt5*a*(delb/avdel+s1));
            tmp2 = s2+two/a/(one+pt5*a*(delb/avdel+s2));
            ret  = tmp1-tmp2;
        } else if (z2 >= hh+pt25*avdel*a) {
            //  If layer falls within upper curve.
            tmp1 = exp(-1*sqrt(two)*(delt/avdel+pt75*a));
            tmp2 = one+pt5*a*delb/avdel+sqr(a)/eight;
            tmp3 = (1-one/sqr(tmp2))*sqr((1+tmp1)/(1-tmp1));
            tmp4 = sqrt(two)*(s1-delt/avdel-a);
            tmp5 = sqrt(two)*(s2-delt/avdel-a);
            tmp6 = four/(1+exp(tmp4))+tmp4;
            tmp7 = four/(1+exp(tmp5))+tmp5;
            ret = (tmp6-tmp7)/sqrt(two)*tmp3;
        } else {
            //  If join of curves lies within the layer.
            s3 = pt25*a;
            tmp2 = s2+two/a/(one+pt5*a*(delb/avdel+s2));
            tmp1 = s3+two/a/(one+pt5*a*(delb/avdel+s3));
            ret = tmp1-tmp2;
            tmp1 = exp(-1*sqrt(two)*(delt/avdel+pt75*a));
            tmp2 = one+pt5*a*delb/avdel+sqr(a)/eight;
            tmp3 = (1-one/sqr(tmp2))*sqr((1+tmp1)/(1-tmp1));
            tmp4 = sqrt(two)*(s1-delt/avdel-a);
            tmp5 = sqrt(two)*(s3-delt/avdel-a);
            tmp6 = four/(1+exp(tmp4))+tmp4;
            tmp7 = four/(1+exp(tmp5))+tmp5;
            ret = ret+(tmp6-tmp7)/sqrt(two)*tmp3;
        }
    } else {
        //  Curves for small withdrawal
        ret = zero;

        if (z1 <= da/four+hh) {
            //  If top and bottom fall within lower curve.;
            tmp1 = z1+(delb+da/four)/Pi*sin(Pi*(z1-hh-da/four)/(delb+da/four));
            tmp2 = z2+(delb+da/four)/Pi*sin(Pi*(z2-hh-da/four)/(delb+da/four));
            ret = tmp1-tmp2;
        } else if (z2 >= hh+da/four) {
            //  If layer falls within upper curve.
            temp1 = z1+(delt+da*pt75)/Pi*sin(Pi*(z1-hh-da/four)/(delt+da*pt75));
            temp2 = z2+(delt+da*pt75)/Pi*sin(Pi*(z2-hh-da/four)/(delt+da*pt75));
            ret = temp1-temp2;
        } else {
            // If join of curves falls within the layer.
            z3 = pt25*da+hh;
            da4 = da/four;
            da7 = da*pt75;
            tmp1 = z3+(delb+da4)/Pi*sin(Pi*(z3-hh-da4)/(delb+da4));
            tmp2 = z2+(delb+da4)/Pi*sin(Pi*(z2-hh-da4)/(delb+da4));
            ret = tmp1-tmp2;
            tmp3 = z3+(delt+da7)/Pi*sin(Pi*(z3-hh-da4)/(delt+da7));
            tmp4 = z1+(delt+da7)/Pi*sin(Pi*(z1-hh-da4)/(delt+da7));
            ret = tmp4-tmp3+ret;
        }
    }

    return ret;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * This function combines two layers and return the mean concentration        *
 ******************************************************************************/
REALTYPE combine(REALTYPE c1, REALTYPE v1, REALTYPE d1,
                 REALTYPE c2, REALTYPE v2, REALTYPE d2)
{
    REALTYPE M1big, M1sml;
    REALTYPE M2big, M2sml;
    REALTYPE MTotal;

/*----------------------------------------------------------------------------*/
    if (fabs(c1-c2) < 1e-5 && fabs(d1-d2) < 1e-5) return c1;

    M1big = v1*thsnd;
    M1sml = v1*d1;
    M2big = v2*thsnd;
    M2sml = v2*d2;

    MTotal = M1sml + M2sml + M1big + M2big;

    return ((c1*M1sml+c2*M2sml) + thsnd*(c1*v1+c2*v2)) / MTotal;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


#ifndef combine_vol
/******************************************************************************
 * Function to combine two layers and return the mean                         *
 * concentration taking into account volumes                                  *
 ******************************************************************************/
REALTYPE combine_vol(REALTYPE c1,REALTYPE v1,REALTYPE c2,REALTYPE v2)
{ return (c1 * v1 + c2 * v2)/(v1 + v2); }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif


/******************************************************************************
 *                                                                            *
 * Function to calculates the density (rho) of water at a given temperature   *
 * (deg C) and salinity (ppm) based on UNESCO (1981) polynomial               *
 *                                                                            *
 * Note: this estimate actually returns (density - 1000)                      *
 *       c1 accounts for this correction (=999.842594) is desired             *
 *                                                                            *
 ******************************************************************************/
REALTYPE calculate_density(REALTYPE temp, REALTYPE salt)
{
    REALTYPE dpure;
    REALTYPE t1,t2,t3,t4,t5,tm;
    REALTYPE s1,s32,s2;
    REALTYPE csal1;
    REALTYPE csal32;
    REALTYPE csal2;

    REALTYPE term[15];

    REALTYPE c1=0.157406,    c2=6.793952E-2, c3=9.095290E-3,
             c4=1.001685E-4, c5=1.120083E-6, c6=6.536332E-9,
             d1=8.24493E-1,  d2=4.0899E-3,   d3=7.6438E-5,
             d4=8.2467E-7,   d5=5.3875E-9,   d6=5.72466E-3,
             d7=1.0227E-4,   d8=1.6546E-6,   d9=4.8314E-4,
             onept5=1.5,     tenthou=10000.0;

    t1 = (temp);
    s1 = (salt);
    tm = round(t1*tenthou);
    t1 = (tm)/tenthou;

    t2 = sqr(t1);
    t3 = t2*t1;
    t4 = t3*t1;
    t5 = t4*t1;
    s2 = s1*s1;
    s32 = pow(s1,onept5);

    term[0]  = -c1;
    term[1]  =  c2 * t1;
    term[2]  = -c3 * t2;
    term[3]  =  c4 * t3;
    term[4]  = -c5 * t4;
    term[5]  =  c6 * t5;
    term[6]  =  d1;
    term[7]  = -d2 * t1;
    term[8]  =  d3 * t2;
    term[9]  = -d4 * t3;
    term[10] =  d5 * t4;
    term[11] = -d6;
    term[12] =  d7 * t1;
    term[13] = -d8 * t2;
    term[14] =  d9;

    dpure  =  term[5]  + term[4]  + term[3]  + term[1] + term[2] + term[0];
    csal1  = (term[10] + term[9]  + term[8]  + term[7] + term[6]) * s1;
    csal32 = (term[13] + term[12] + term[11]) * s32;
    csal2  =  term[14] * s2;

    return dpure + csal1 + csal32 + csal2;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Calculates the saturated vapour pressure (SatVap) corresponding to         *
 * temperature (temp, deg C)                                                  *
 ******************************************************************************/
REALTYPE saturated_vapour(REALTYPE temp)
{ return pow(10.0, 9.28603523 - (2322.37885/(temp+273.15))); }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
int internal_var(const char *name)
{
    if ( strncasecmp(name, "flow", 4) == 0 ||
         strncasecmp(name, "temp", 4) == 0 ||
         strncasecmp(name, "salt", 4) == 0 )
        return 1;
    return 0;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
