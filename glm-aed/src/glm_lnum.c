/******************************************************************************
 *                                                                            *
 * glm_lakenum.c                                                              *
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


int lkn = FALSE;

/*----------------------------------------------------------------------------*/
static int interpolate_layer_data(REALTYPE *iheight, REALTYPE *density);
static void lnpe3(int NLayers, REALTYPE *iheight, REALTYPE *density,
                             REALTYPE *xpp, REALTYPE *zcp, REALTYPE *xmasspe3);
static REALTYPE calc_xmoment(int NLayers, REALTYPE *iheight, REALTYPE *density);
/*----------------------------------------------------------------------------*/


/******************************************************************************
 *                                                                            *
 *    This routine calculates the lakenumber for the current day              *
 *    and sends the output to a file:'Ldddhhmm.NUM'                           *
 *                                                                            *
 *    algorithm from program "lake number project" (11/8/1991)                *
 *                                                                            *
 *    written by: Stephen Barry and John Snell                                *
 *                                                                            *
 *    NOTES:  nlayers = number of layers of interp data (step = 0.1)          *
 *      all arrays begin with subscript of zero                               *
 *      all arrays of data interp to 0.1m have subscript of zero              *
 *      referring to depth of 0.1m (Top of first layer)                       *
 *                                                                            *
 ******************************************************************************/
REALTYPE calculate_lake_number(void)
{
    int  NLayers;
    REALTYPE wind_speed;
#ifndef _VISUAL_C_
    REALTYPE density[Nmorph], iheight[Nmorph];
#else
    REALTYPE *density, *iheight;
#endif
    REALTYPE lake_top, lake_bottom;
    REALTYPE usquared, thermdepth;
    REALTYPE xp, zc, xmass;

/*----------------------------------------------------------------------------*/
#ifdef _VISUAL_C_
    density = malloc(sizeof(REALTYPE) * Nmorph);
    iheight = malloc(sizeof(REALTYPE) * Nmorph);
#endif

    NLayers = interpolate_layer_data(iheight, density);

    if (NLayers < 1) {
#ifdef _VISUAL_C_
        free(density); free(iheight);
#endif
        return missing;
    }

    wind_speed = MetData.WindSpeed;
    if (wind_speed < 0.1) wind_speed = 0.1;

    usquared = 1.612e-6 * wind_speed * wind_speed;
    XMoment1 = calc_xmoment(NLayers, iheight, density);

    if (XMoment1 <= 0.0) {
#ifdef _VISUAL_C_
        free(density); free(iheight);
#endif
        return 0.0;
    }

    thermdepth = iheight[NLayers-1] - XMoment1;
    xp = 0.0;
    zc = 0.0;
    xmass = 0.0;

    lnpe3(NLayers, iheight, density, &xp, &zc, &xmass);

    lake_top = -xp * (xmass * 1e6) * (1.0 - ((iheight[NLayers-1] - thermdepth) / iheight[NLayers-1]));
    lake_bottom = 1000 * usquared * pow((MphLevelArea[(NLayers-1)+1] * 1e6), 1.5) *
                                                      (1.0 - (zc / iheight[NLayers-1]));

#ifdef _VISUAL_C_
    free(density); free(iheight);
#endif

    if (lake_bottom == 0.0)
        return missing;

    return lake_top / lake_bottom;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 *    This subroutine interpolates the depth                                  *
 *    and density data onto a series of points of increment every             *
 *    0.1 m. The interpolation assumes that all 3 parameters are              *
 *    of constant value within each layer.(ZERO'th ORDER INTERPOL.)           *
 *                                                                            *
 ******************************************************************************/
static int interpolate_layer_data(REALTYPE *iheight, REALTYPE *density)
{
    REALTYPE this_height;
    int i,j;

    i = 0;
    this_height = 0.1;

    for (j = 0; j < NumLayers; j++) {
        while (this_height <= Lake[j].Height) {
            iheight[i] = this_height;
            density[i] = Lake[j].Density;
            this_height += 0.1;
            i++;
            if ( i > Nmorph ) {
                fprintf(stderr, "NLayers greater than Nmorph\n");
                exit(1);
            }
        }
    }
    return i;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/*############################################################################*/
void lnpe3(int NLayers, REALTYPE *iheight, REALTYPE *density, REALTYPE *xpp,
                                             REALTYPE *zcp, REALTYPE *xmasspe3)
{
    DOUBLETYPE zcvp, ab, da, ht, dens, xvol, xvolp;
    DOUBLETYPE xmassp, xpe, zcv, xmass;
    int        i_pe3, ij, il, l, ibdep;

/*----------------------------------------------------------------------------*/

    dens = 0.;
    ibdep = 0;

    // get the centre of volume and potential energy, total mass etc.
    // locate the surface in relation to the storage table.  ij is
    // the last storage entry before the surface, y is the distance
    // to the surface from there.

    ij = (iheight[NLayers-1] * 10.0);

    // initialize the loop. il is the current layer number, do the area
    // gradient starting at ibdep (here, ibdep always equals zero)

    xvol = 0.0;
    xmass = 0.0;
    xpe = 0.0;
    zcv = 0.0;

    da = (MphLevelArea[0]);
    ab = 0.0;

    il = ibdep;
    ht = 0.0;
    for (i_pe3 = 0; i_pe3 <= (ij-1); i_pe3++) {
        il = il + 1;

        dens = (density[il] + 1000.0);
        ht = ht + 0.1;
        if (i_pe3 != 0) da = (dMphLevelArea[(i_pe3-1)+1] * 10.0);
        zcvp = ab * ((0.1 * ht) - 0.005) + (da / 6) * ((0.03 * ht) - 0.001);
        zcv = zcv + zcvp;
        xpe = xpe + dens * zcvp;
        xvolp = 0.1 * (ab + da * 0.05);
        xvol = xvol + xvolp;
        xmassp = xvolp * dens;
        xmass = xmass + xmassp;
        ab = ab + 0.1 * da;
    }

    // go to the surface.
    if (il == (NLayers-1)) {
        ht = iheight[NLayers-1];
        zcvp = ab * ((0.1 * ht) - 0.005) + (da / 6) * ((0.03 * ht) - 0.001);
        zcv = zcv + zcvp;
        xpe = xpe + dens * zcvp;
        xvolp = 0.1 * (ab + da * 0.05);
        xvol = xvol + xvolp;
        xmassp = xvolp * dens;
        xmass = xmass + xmassp;
    } else {
        for (l = il; l <= (NLayers-1); l++) {
            dens = density[l];
            ht = iheight[l];
            zcvp = ab * ((0.1 * ht) - 0.005) + (da / 6) * ((0.03 * ht) - 0.001);
            zcv = zcv + zcvp;
            xpe = xpe + dens * zcvp;
            xvolp = 0.1 * (ab + da * 0.05);
            xvol = xvol + xvolp;
            xmassp = xvolp * dens;
            xmass = xmass + xmassp;
            ab = MphLevelArea[l+1];
        }
    }

    // NOTE that units of xp are millions of kg m**2/sec**2,
    // units of xmass are millions of kg

    *zcp = (zcv / xvol);
    *xpp = ((xpe / xmass - zcv / xvol) * 9.810);

    *xmasspe3 = xmass;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/*############################################################################*/
static REALTYPE calc_xmoment(int NLayers, REALTYPE *iheight, REALTYPE *density)
{
#ifndef _VISUAL_C_
    REALTYPE BFSQ[Nmorph], mid_depth[Nmorph];
#else
    REALTYPE *BFSQ, *mid_depth;
#endif
    REALTYPE func_XMOM, func_XMOMO;
    int i;

/*-----------------------------------------------------------------------------*/
#ifdef _VISUAL_C_
    BFSQ = malloc(sizeof(REALTYPE) * Nmorph);
    mid_depth = malloc(sizeof(REALTYPE) * Nmorph);
#endif

    func_XMOM = 0.0;
    func_XMOMO = 0.0;

    mid_depth[0] = iheight[0] / 2.0;

    for (i = 1; i < NLayers; i++) {
        mid_depth[i]=(iheight[i]+iheight[i-1])/2.0;

        BFSQ[i] = gprime(density[i], density[i-1]) / (mid_depth[i] - mid_depth[i-1]);

        func_XMOM  = func_XMOM + iheight[i-1] * BFSQ[i] * (dMphLevelVol[i+1]+dMphLevelVol[i]) / 2.0;
        func_XMOMO = func_XMOMO +               BFSQ[i] * (dMphLevelVol[i+1]+dMphLevelVol[i]) / 2.0;
    }

    if (func_XMOMO <= 0.0)
        func_XMOM = -1.0;
    else
        func_XMOM = func_XMOM / func_XMOMO;

#ifdef _VISUAL_C_
    free(BFSQ); free(mid_depth);
#endif

    return func_XMOM;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/