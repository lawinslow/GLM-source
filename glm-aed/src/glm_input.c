/******************************************************************************
 *                                                                            *
 * glm_input.c                                                                *
 *                                                                            *
 * Routines to read some input files.  These will be replaced                 *
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
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "glm.h"
#include "glm_types.h"
#include "glm_globals.h"
#include "aed_time.h"
#include "glm_util.h"
#include "aed_csv.h"


//#define dbgprt(...) fprintf(stderr, __VA_ARGS__)
#define dbgprt(...) /* __VA_ARGS__ */

typedef struct _inf_data_ {
    int inf;  /* Inflow Number */
    int n_vars;   /* number of vars in this file */
    int flow_idx;
    int temp_idx;
    int salt_idx;
    int in_vars[MaxVars];
} InflowDataT;

typedef struct _out_data_ {
    int outf;
    int draw_idx;
} OutflowDataT;

static InflowDataT inf[MaxInf];
static OutflowDataT outf[MaxOut];

static int metf;
static int rain_idx, hum_idx, lwav_idx, sw_idx, atmp_idx, wind_idx, snow_idx,
           rpo4_idx, rtp_idx, rno3_idx, rnh4_idx, rtn_idx, rsi_idx;

static int time_idx = -1;

int lw_ind = 0;
static int have_snow = FALSE, have_rain_conc = FALSE;

static int n_steps;

static MetDataType *submet = NULL;
static REALTYPE loaded_day;

REALTYPE wind_factor = 1.0;   //# Windspeed scaling factor
REALTYPE sw_factor   = 1.0;
REALTYPE lw_factor   = 1.0;
REALTYPE at_factor   = 1.0;
REALTYPE rh_factor   = 1.0;
REALTYPE rain_factor = 1.0;


int *WQ_VarsIdx = NULL;


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void read_daily_inflow(int julian, int NumInf, REALTYPE *flow, REALTYPE *temp,
                                               REALTYPE *salt, REALTYPE *wq)
{
    int csv;
    int i,j,k;

    for (i = 0; i < NumInf; i++) {
        int n_invars = inf[i].n_vars;
        csv = inf[i].inf;
        find_day(csv, time_idx, julian);

        flow[i] = get_csv_val_r(csv,inf[i].flow_idx);
        temp[i] = get_csv_val_r(csv,inf[i].temp_idx);
        salt[i] = get_csv_val_r(csv,inf[i].salt_idx);

        for (j = 0; j < n_invars; j++) {
            if (WQ_VarsIdx[j]<0) k = j; else k = WQ_VarsIdx[j];
            if (inf[i].in_vars[k] == -1 )
                WQ_INF_(wq, i, k) = 0.;
            else
                WQ_INF_(wq, i, k) = get_csv_val_r(csv,inf[i].in_vars[j]);
        }
    }
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void read_daily_outflow(int julian, int NumOut, REALTYPE *draw)
{
    int csv, i;

    for (i = 0; i < NumOut; i++) {
        csv = outf[i].outf;
        find_day(csv, time_idx, julian);
        draw[i] = get_csv_val_r(csv,outf[i].draw_idx);
    }
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void read_daily_met(int julian, MetDataType *met)
{
    int csv, i;
    REALTYPE now, tomorrow;

    now = julian;
    tomorrow = now + 1.0;
    loaded_day = now;

    dbgprt("read_daily_met (SUBDAY_MET) in\n");

    csv = metf;
    find_day(csv, time_idx, julian);

    i = 0;
    while (get_csv_val_r(csv, time_idx) < tomorrow) {
        if ( i >= n_steps ) {
            int dd,mm,yy;
            calendar_date(now,&yy,&mm,&dd);
            fprintf(stderr, "Warning! Too many steps for %4d-%02d-%02d\n", yy,mm,dd);
            break;
        }

        // Rain is the exception - goes as is
        submet[i].Rain        = get_csv_val_r(csv, rain_idx) * rain_factor * 1000.;
        submet[i].RelHum      = get_csv_val_r(csv, hum_idx)  * rh_factor;
        if ( submet[i].RelHum > 100. ) submet[i].RelHum = 100.;
        submet[i].LongWave    = get_csv_val_r(csv, lwav_idx) * lw_factor;
        submet[i].ShortWave   = get_csv_val_r(csv, sw_idx)   * sw_factor;
        submet[i].AirTemp     = get_csv_val_r(csv, atmp_idx) * at_factor;
        submet[i].WindSpeed   = get_csv_val_r(csv, wind_idx) * wind_factor;

        // Read in rel humidity into svd (%), and convert to satvap
        submet[i].SatVapDef   =  (submet[i].RelHum/100.) * saturated_vapour(submet[i].AirTemp);

        if ( have_snow )
             submet[i].Snow = get_csv_val_r(csv,snow_idx) * 1000. ;
        else submet[i].Snow = 0. ;

        if ( have_rain_conc ) {
            submet[i].RainConcPO4 = get_csv_val_r(csv, rpo4_idx);
            submet[i].RainConcTp  = get_csv_val_r(csv, rtp_idx);
            submet[i].RainConcNO3 = get_csv_val_r(csv, rno3_idx);
            submet[i].RainConcNH4 = get_csv_val_r(csv, rnh4_idx);
            submet[i].RainConcTn  = get_csv_val_r(csv, rtn_idx);
            submet[i].RainConcSi  = get_csv_val_r(csv, rsi_idx);
        } else {
            submet[i].RainConcPO4 = 0.;
            submet[i].RainConcTp  = 0.;
            submet[i].RainConcNO3 = 0.;
            submet[i].RainConcNH4 = 0.;
            submet[i].RainConcTn  = 0.;
            submet[i].RainConcSi  = 0.;
        }
        i++;

        if (!load_csv_line(csv) ) break;
    }

    *met = submet[0];
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void read_sub_daily_met(int julian, int iclock, MetDataType *met)
{
    REALTYPE now ;
    int  idx = 0;

    now = julian;
    if ( now != loaded_day ) {
        fprintf(stderr, "Loaded day %12.6f Not equal to %12.4f\n", loaded_day, now);
        exit(1);
    }

    idx = iclock/3600;

    *met = submet[idx];
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static void locate_time_column(int csv, const char *which, const char *fname)
{
    int lt_idx = -1;

    if ( (lt_idx = find_csv_var(csv, "time")) < 0 )
        lt_idx = find_csv_var(csv, "date");

    if (lt_idx != 0) {
        fprintf(stderr,
                 "Error in %s file '%s': 'Time (Date)' is not first column!\n",
                                                                  which, fname);
        exit(1);
    }
    if (time_idx < 0) time_idx = lt_idx;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void open_met_file(const char *fname, int snow_sw, int rain_sw,
                                                            const char *timefmt)
{
    char *LWTypeName;

    have_snow = snow_sw;
    have_rain_conc = rain_sw;
    if ( (metf = open_csv_input(fname, timefmt)) < 0 ) {
        fprintf(stderr, "Failed to open '%s'\n", fname);
        exit(1);
    }

    locate_time_column(metf, "met", fname);

    if ( (rain_idx = find_csv_var(metf,"Rain")) < 0 ) {
        fprintf(stderr,"Error in met file, Rain not found!\n");
        exit(1);
    }
    if ( (hum_idx = find_csv_var(metf,"RelHum")) < 0 ) {
        fprintf(stderr,"Error in met file, RelHum not found!\n");
        exit(1);
    }
    if ( lw_ind == LW_CC )
        LWTypeName = "Cloud";
    else
        LWTypeName = "LongWave";

    if ((lwav_idx = find_csv_var(metf, LWTypeName)) < 0 ) {
        fprintf(stderr,"Error in met file, '%s' not found!\n", LWTypeName);
        exit(1);
    }
    sw_idx = find_csv_var(metf,"ShortWave");
    atmp_idx = find_csv_var(metf,"AirTemp");
    wind_idx = find_csv_var(metf,"WindSpeed");
    if ( have_snow ) {
        snow_idx = find_csv_var(metf,"Snow");
        if ( snow_idx < 0 ) have_snow = FALSE;
    }
    if (have_rain_conc) {
        rpo4_idx = find_csv_var(metf,"rainPO4");
        rtp_idx  = find_csv_var(metf,"rainTP");
        rno3_idx = find_csv_var(metf,"rainNO3");
        rnh4_idx = find_csv_var(metf,"rainNH4");
        rtn_idx  = find_csv_var(metf,"rainTN");
        rsi_idx  = find_csv_var(metf,"rainSi");
    }

    n_steps = 86400.0 / timestep;
    // Allocate sub daily met array with an element for each timestep
    submet = malloc(n_steps * sizeof(MetDataType));
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void open_inflow_file(int idx, const char *fname,
                                   int nvars, char *vars[], const char *timefmt)
{
    int j,k,l;

    if ( (inf[idx].inf = open_csv_input(fname, timefmt)) < 0 ) {
        fprintf(stderr, "Failed to open '%s'\n", fname);
        exit(1) ;
    }
    locate_time_column(inf[idx].inf, "inflow", fname);

    inf[idx].flow_idx = find_csv_var(inf[idx].inf,"flow");
    inf[idx].temp_idx = find_csv_var(inf[idx].inf,"temp");
    inf[idx].salt_idx = find_csv_var(inf[idx].inf,"salt");
    l = 0;
    for (j = 0; j < nvars; j++) {
        k = find_csv_var(inf[idx].inf, vars[j]);
        if (k == -1)
            fprintf(stderr,"No match for '%s' in file '%s'\n",vars[j],fname);
        else {
            if ( k != inf[idx].flow_idx && k != inf[idx].temp_idx &&
                                           k != inf[idx].salt_idx ) {
                inf[idx].in_vars[l++] = k;
            }
        }
    }
    inf[idx].n_vars = l;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void open_outflow_file(int idx, const char *fname, const char *timefmt)
{
    if ( (outf[idx].outf = open_csv_input(fname, timefmt)) < 0 ) {
        fprintf(stderr, "Failed to open '%s'\n", fname);
        exit(1) ;
    }

    locate_time_column(outf[idx].outf, "outflow", fname);

    outf[idx].draw_idx = find_csv_var(outf[idx].outf,"flow");
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void close_met_files()
{ close_csv_input(metf); }
/*----------------------------------------------------------------------------*/
void close_inflow_files()
{ int i; for (i = 0; i < NumInf; i++) close_csv_input(inf[i].inf); }
/*----------------------------------------------------------------------------*/
void close_outflow_files()
{ int i; for (i = 0; i < NumOut; i++) close_csv_input(outf[i].outf); }
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
