/******************************************************************************
 *                                                                            *
 * glm_output.c                                                               *
 *                                                                            *
 *  output for glm                                                            *
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
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _WIN32
  #include <unistd.h>
#else
  #include <direct.h>
  #define S_ISDIR(mode) (mode & _S_IFDIR)
  #define mkdir(path, mode) _mkdir(path)
#endif


#include "glm.h"

#include "glm_types.h"
#include "glm_globals.h"

#include "aed_time.h"
#include "aed_csv.h"
#include "glm_csv.h"
#include "glm_ncdf.h"
#include "glm_wqual.h"
#include "glm_plot.h"
#ifdef PLOTS
#include "libplot.h"
#endif
/* for WQ interface */
void write_glm_wq(int ncid, int wlev, int nlev, int *lvl, int point_nlevs)
{ write_glm_wq_(&ncid, &wlev, &nlev, lvl, &point_nlevs); }


extern REALTYPE XLW, XCO, XEV, QSW;

#ifdef PLOTS
extern int saveall;
extern int do_plots, theplots[10], nplots;
#ifdef XPLOTS
extern int xdisp;
#endif
#endif


/******************************************************************************
 * Initialise output streams                                                  *
 *----------------------------------------------------------------------------*/
void init_output(int jstart, const char *out_dir, const char *out_fn,
                   int oMaxLayers, REALTYPE Longitude, REALTYPE Latitude, int o_lkn)
{
    char ts[20];
    char path[1024];
    struct stat sb;

    if ( stat(out_dir, &sb) ) {
        fprintf(stderr, "Directory \"%s\" does not exist - attempting to create it\n", out_dir);
        if ( mkdir(out_dir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH) ) {
            fprintf(stderr, "mkdir failed\n");
            exit(1);
        }
    } else {
        if ( ! S_ISDIR(sb.st_mode) ) {
            fprintf(stderr, "Name given in out_dir (%s) is not a directory\n", out_dir);
            exit(1);
        }
    }

    lkn = o_lkn;
    MaxLayers = oMaxLayers;
    write_time_string(ts,jstart,0);
    snprintf(path, 1024, "%s/%s.nc", out_dir, out_fn);
    ncid = init_glm_ncdf(path, "glm run", Latitude, Longitude, MaxLayers, ts);

    init_csv_output(out_dir, lkn);

    //# Initialize WQ output (creates NetCDF variables)
    if (wq_calc) init_glm_wq_output(&ncid, &x_dim, &y_dim, &z_dim, &time_dim);

#ifdef PLOTS
# ifdef XPLOTS
    do_plots = xdisp || saveall;
# else
    do_plots = saveall;
# endif
    if ( do_plots ) init_plots(jstart,nDays,CrestLevel);
#endif
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
static REALTYPE min_temp(LakeDataType *Lake, int count)
{
    int i;
    REALTYPE min;

    min = Lake[0].Temp;
    for (i = 1; i < count; i++)
        if ( min > Lake[i].Temp ) min = Lake[i].Temp;
    return min;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
static REALTYPE max_temp(LakeDataType *Lake, int count)
{
    int i;
    REALTYPE max;

    max = Lake[0].Temp;
    for (i = 1; i < count; i++)
        if ( max < Lake[i].Temp ) max = Lake[i].Temp;
    return max;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
static REALTYPE sum_lake_layervol()
{
    REALTYPE sum = 0.;
    int i;
    for (i = 0; i < NumLayers; i++)
        sum += Lake[i].LayerVol;
    return sum;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
static REALTYPE max_dtdz_at(LakeDataType *Lake, int count)
{
    int  i, max_at;
    REALTYPE dtdz, max_dtdz;

    if (count < 2)
        return Lake[0].Height / 2;

    max_at = 1;
    max_dtdz = (Lake[1].Temp - Lake[0].Temp) / (Lake[1].Height - Lake[0].Height);
    for (i = 2; i < count; i++) {
        dtdz = (Lake[i].Temp - Lake[i-1].Temp) / (Lake[i].Height - Lake[i-1].Height);
        if ( max_dtdz < dtdz ) {
            max_dtdz = dtdz;
            max_at = i;
        }
    }
    return (Lake[max_at].Height - Lake[max_at - 1].Height) / 2;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Write the various output files and/or plots.                               *
 *----------------------------------------------------------------------------*/
void write_output(int jday, int iclock, int nsave, int stepnum)
{
    int i, n, lvl[MaxPointCSV];
    char ts[20];

    if ( csv_point_nlevs > 0 ) {
        for (i = 0; i < MaxPointCSV; i++) lvl[i] = -1;

        //# Output at end of time step so add noSecs to time string
        write_time_string(ts, jday, iclock + noSecs);
        for (i = 0; i < csv_point_nlevs; i++) {
            write_csv_point(i, "time", 0.0, ts, FALSE);

            //# find which level csv_at is in
            for (n = 0; n < NumLayers; n++) {
                if ( Lake[n].Height >= csv_point_at[i] ) {
                    lvl[i] = n;
                    break;
                }
            }

            write_csv_point(i, "temp", Lake[lvl[i]].Temp,     NULL, FALSE);
            write_csv_point(i, "salt", Lake[lvl[i]].Salinity, NULL, FALSE);
        }
    }

#ifdef PLOTS
    if ( do_plots ) {
        put_xplot_val("temp", NumLayers, NULL);
        put_xplot_val("salt", NumLayers, NULL);
        put_xplot_val("rad", NumLayers, NULL);
        put_xplot_val("extc", NumLayers, NULL);
        put_xplot_val("dens", NumLayers, NULL);
    }
#endif

    write_glm_ncdf(ncid, NumLayers, MaxLayers, stepnum, timestep);

    //# outputs WQ vars to NetCDF
    if (wq_calc)
        write_glm_wq(ncid, NumLayers, MaxLayers, lvl, csv_point_nlevs);

    if (csv_point_nlevs > 0) {
        for (i = 0; i < csv_point_nlevs; i++)
            write_csv_point(i, "", 0.0, NULL, TRUE);
    }

    //# if we are doing point output we already have the date
    if ( csv_point_nlevs <= 0 )
        write_time_string(ts, jday, iclock);

#ifdef PLOTS
# ifdef XPLOTS
    if ( xdisp ) {
        for (i = 0; i < nplots; i++)
            flush_plot(theplots[i]);
    }
# endif
#endif
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Write the various output files and/or plots.                               *
 *----------------------------------------------------------------------------*/
void write_diags(int jday, REALTYPE LakeNum)
{
    char ts[20];

    if ( csv_lake_file < 0 ) return;

    //# Output at end of day
    write_time_string(ts, jday, SecsPerDay);

    write_csv_lake("time",            0.0,                       ts,   FALSE);
    write_csv_lake("Volume",          sum_lake_layervol(),       NULL, FALSE);
    write_csv_lake("Tot Inflow Vol",  SurfData.dailyInflow,      NULL, FALSE);
    write_csv_lake("Tot Outflow Vol", SurfData.dailyOutflow,     NULL, FALSE);
    write_csv_lake("Overflow Vol",    SurfData.dailyOverflow,    NULL, FALSE);
    write_csv_lake("Evapouration",    SurfData.dailyEvap,        NULL, FALSE);
    write_csv_lake("Rain",            SurfData.dailyRain,        NULL, FALSE);
    write_csv_lake("Lake Level",      Lake[surfLayer].Height,    NULL, FALSE);
    write_csv_lake("Surface Area",    Lake[surfLayer].LayerArea, NULL, FALSE);
    write_csv_lake("Black Ice",       SurfData.HeightBlackIce,   NULL, FALSE);
    write_csv_lake("Snow",            SurfData.HeightSnow,       NULL, FALSE);
    write_csv_lake("White Ice",       SurfData.HeightWhiteIce,   NULL, FALSE);
    write_csv_lake("Max Temp",        max_temp(Lake, NumLayers), NULL, FALSE);
    write_csv_lake("Min Temp",        min_temp(Lake, NumLayers), NULL, FALSE);
    write_csv_lake("Surface Temp",    Lake[surfLayer].Temp,      NULL, FALSE);
    write_csv_lake("Daily Qsw",       SurfData.dailyQsw,         NULL, FALSE);
    write_csv_lake("Daily Qe",        SurfData.dailyQe,          NULL, FALSE);
    write_csv_lake("Daily Qh",        SurfData.dailyQh,          NULL, FALSE);
    write_csv_lake("Daily Qlw",       SurfData.dailyQlw,         NULL, FALSE);
    write_csv_lake("Light",           Lake[surfLayer].Light,     NULL, FALSE);
    write_csv_lake("Benthic Light",   Benthic_Light_pcArea,      NULL, FALSE);

    if (lkn) write_csv_lake("LakeNumber", LakeNum, NULL, FALSE);
    write_csv_lake("Max dT/dz",    max_dtdz_at(Lake, NumLayers), NULL, TRUE);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Write the outflow data file with WQ variables.                             *
 *----------------------------------------------------------------------------*/
void write_outflow(int of_idx, int jday, REALTYPE vol)
{
    char ts[64];
    int i, lvl;
    REALTYPE DrawHeight;
    extern int csv_outlet_allinone, csv_outfl_nvars;
    extern int ofl_wq_idx[];
    static REALTYPE vol_tot, state_of_v[MaxCSVOutVars];

    //# work out which level the outflow is at now.
    if (Outflows[of_idx].FloatOff) {
        DrawHeight = Lake[surfLayer].Height - Outflows[of_idx].OLev;
        //# Let it sit on the bottom if the lake has drained
        if (DrawHeight < 0.) DrawHeight = 0.;
    } else
        DrawHeight = Outflows[of_idx].OLev; //# Fixed height offtake

    for (lvl = botmLayer; lvl <= surfLayer; lvl++)
        if (Lake[lvl].Height >= DrawHeight) break;

    if ( csv_outlet_allinone ) {
        if ( of_idx == 0 ) // initialize
            vol_tot = 0;

        for (i = 0; i < csv_outfl_nvars; i++) {
            state_of_v[i] *= vol_tot;
            state_of_v[i] += (vol * _WQ_Vars(ofl_wq_idx[i], lvl));
        }
        vol_tot += vol;
        for (i = 0; i < csv_outfl_nvars; i++)
            state_of_v[i] /= vol_tot;

        if ( of_idx != MaxOut ) return;
        of_idx = 0;
    } else if (wq_calc) {
        for (i = 0; i < csv_outfl_nvars; i++)
            state_of_v[i] = _WQ_Vars(ofl_wq_idx[i], lvl);
    }

    write_time_string(ts, jday, 0);

    write_csv_outfl(of_idx, "time",       0.0,                     ts,   FALSE);
    write_csv_outfl(of_idx, "flow",       vol,                     NULL, FALSE);

    write_csv_outfl(of_idx, "Temp",       Lake[lvl].Temp,          NULL, FALSE);
    write_csv_outfl(of_idx, "Salt",       Lake[lvl].Salinity,      NULL, FALSE);

    if (wq_calc) {   //# must do each of the WQ vars
        for (i = 0; i < csv_outfl_nvars; i++)
            write_csv_outfl_idx(of_idx, i,    state_of_v[i],       NULL, FALSE);
    }

    //# force a newline
    write_csv_outfl(of_idx, "",           0.0,                     NULL, TRUE);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * Close files and plots                                                      *
 *----------------------------------------------------------------------------*/
void close_output()
{
    close_glm_ncdf(ncid);
    glm_close_csv_output();

#ifdef PLOTS
    if ( do_plots ) do_cleanup(saveall);
#endif
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/