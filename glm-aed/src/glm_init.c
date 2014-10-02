/******************************************************************************
 *                                                                            *
 * glm_init.c                                                                 *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Earth & Environment                                          *
 *     University of Western Australia                                        *
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


#define _ONE_ 1.

#include "glm_types.h"
#include "glm_globals.h"
#include "glm_csv.h"
#include "glm_input.h"
#include "aed_time.h"
#include "glm_util.h"
#include "glm_layers.h"
#include "glm_wqual.h"
#include "glm_lnum.h"

#include "namelist.h"


extern int *WQ_VarsIdx;

static REALTYPE        base_elev;
static REALTYPE        crest_elev;
static int             crest_sto_idx;

char glm_nml_file[256] = "glm.nml";

static void create_lake(int namlst);
static void initialise_lake(int namlst);
static int init_time(char *start, char *stop, int timefmt, int *nDays);

/*############################################################################*/

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void init_glm(int *jstart, char *outp_dir, char *outp_fn, int *nsave)
{
    int jyear, jmonth, jday, julianday;

    /*---------------------------------------------
     * glm setup
     *-------------------------------------------*/
    char           *sim_name;
    int             max_layers;
    REALTYPE        min_layer_vol;
    REALTYPE        min_layer_thick;
    REALTYPE        max_layer_thick;
//  REALTYPE        Kw;
    extern REALTYPE Benthic_Imin;
    REALTYPE        coef_inf_entrain;
//  REALTYPE        coef_mix_conv;
//  REALTYPE        coef_mix_eta;
//  REALTYPE        coef_mix_ct;
//  REALTYPE        coef_mix_cs;
//  REALTYPE        coef_mix_kh;
//  REALTYPE        coef_mix_hyp;
//  CLOGICAL        mobility_off;
//  CLOGICAL        non_avg;
//  int             deep_mixing;
    /*-------------------------------------------*/

    /*---------------------------------------------
     * wq setup
     *-------------------------------------------*/
//  int             ode_method;
//  int             split_factor;
//  LOGICAL         bioshade_feedback;
//  LOGICAL         repair_state;
    char           *wq_nml_file = "fabm.nml";
//  LOGICAL         multi_ben;
    /*-------------------------------------------*/

    /*---------------------------------------------
     * time format
     *-------------------------------------------*/
    int             timefmt;
    char           *start = NULL;
    char           *stop  = NULL;
    REALTYPE        dt;        // timestep
    int             num_days;  // number of days to run the sim
    /*-------------------------------------------*/

    /*---------------------------------------------
     * output
     *-------------------------------------------*/
    char           *out_dir;
    char           *out_fn;
    LOGICAL         out_lkn;
//  int             nsave;
    int             csv_point_nlevs  = 0;
    char           *csv_point_fname  = NULL;
    REALTYPE       *csv_point_at     = NULL;
    int             csv_point_nvars  = 0;
    char          **csv_point_vars   = NULL;
    char           *csv_lake_fname   = NULL;
    LOGICAL         csv_outlet_allinone = FALSE;
    char           *csv_outlet_fname = NULL;
    int             csv_outlet_nvars = 0;
    char          **csv_outlet_vars  = NULL;
    char           *csv_ovrflw_fname = NULL;
    /*-------------------------------------------*/

    /*---------------------------------------------
     * meteorology
     *-------------------------------------------*/
    LOGICAL         met_sw;        // Include surface meteorological forcing
    char           *lw_type;       // Type LW measurement (LW_IN/LW_CC/LW_NET)
    LOGICAL         rain_sw;       // Rainfall composition
    LOGICAL         snow_sw;       // Snowfall
    LOGICAL         atm_stab;      // Account for non-neutral atmospheric stability
    char           *meteo_fl;      // Name of meteorology input file
//  int             lw_ind;        // type of longwave radiation - now in glm_input
//  LOGICAL         subdaily;      //
//  REALTYPE        coef_wind_drag;
//  REALTYPE        CE;
//  REALTYPE        CH;
    extern REALTYPE wind_factor;
    extern REALTYPE sw_factor;
    extern REALTYPE lw_factor;
    extern REALTYPE at_factor;
    extern REALTYPE rh_factor;
    extern REALTYPE rain_factor;
    char           *timefmt_m = NULL;
    /*-------------------------------------------*/

    /*---------------------------------------------
     * inflow
     *-------------------------------------------*/
    int             num_inflows;
    char          **names_of_strms = NULL;
    REALTYPE       *strm_hf_angle  = NULL;
    REALTYPE       *strmbd_slope   = NULL;
    REALTYPE       *strmbd_drag    = NULL;
    REALTYPE       *inflow_factor  = NULL;
    char          **inflow_fl      = NULL;
    int             inflow_varnum;
    char          **inflow_vars    = NULL;
    char           *timefmt_i      = NULL;
    /*-------------------------------------------*/

    /*---------------------------------------------
     * outflow
     *-------------------------------------------*/
    int             num_outlet;
    LOGICAL        *flt_off_sw   = NULL;
    REALTYPE       *outl_elvs    = NULL;
    REALTYPE       *bsn_len_outl = NULL;
    REALTYPE       *bsn_wid_outl = NULL;
    char          **outflow_fl   = NULL;
    REALTYPE       *outflow_factor;
    char           *timefmt_o    = NULL;
    /*-------------------------------------------*/

    int i, j, k;
    int namlst;


    //==========================================================================
    NAMELIST glm_setup[] = {
          { "glm_setup",         TYPE_START,            NULL               },
          { "sim_name",          TYPE_STR,              &sim_name          },
          { "max_layers",        TYPE_INT,              &max_layers        },
          { "min_layer_vol",     TYPE_DOUBLE,           &min_layer_vol     },
          { "min_layer_thick",   TYPE_DOUBLE,           &min_layer_thick   },
          { "max_layer_thick",   TYPE_DOUBLE,           &max_layer_thick   },
          { "Kw",                TYPE_DOUBLE,           &Kw                },
          { "Benthic_Imin",      TYPE_DOUBLE,           &Benthic_Imin      },
          { "coef_inf_entrain",  TYPE_DOUBLE,           &coef_inf_entrain  },
          { "coef_mix_conv",     TYPE_DOUBLE,           &coef_mix_conv     },
          { "coef_wind_stir",    TYPE_DOUBLE,           &coef_wind_stir    },
          { "coef_mix_turb",     TYPE_DOUBLE,           &coef_mix_turb     },
          { "coef_mix_shear",    TYPE_DOUBLE,           &coef_mix_shear    },
          { "coef_mix_KH",       TYPE_DOUBLE,           &coef_mix_KH       },
          { "coef_mix_hyp",      TYPE_DOUBLE,           &coef_mix_hyp      },
          { "mobility_off",      TYPE_BOOL,             &mobility_off      },
          { "non_avg",           TYPE_BOOL,             &non_avg           },
          { "deep_mixing",       TYPE_INT,              &deep_mixing       },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST wq_setup[] = {
          { "wq_setup",          TYPE_START,            NULL               },
          { "ode_method",        TYPE_INT,              &ode_method        },
          { "split_factor",      TYPE_INT,              &split_factor      },
          { "bioshade_feedback", TYPE_BOOL,             &bioshade_feedback },
          { "repair_state",      TYPE_BOOL,             &repair_state      },
          { "wq_nml_file",       TYPE_STR,              &wq_nml_file       },
          { "multi_ben",         TYPE_BOOL,             &multi_ben         },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST time[] = {
          { "time",              TYPE_START,            NULL               },
          { "timefmt",           TYPE_INT,              &timefmt           },
          { "start",             TYPE_STR,              &start             },
          { "stop",              TYPE_STR,              &stop              },
          { "dt",                TYPE_DOUBLE,           &dt                },
          { "num_days",          TYPE_INT,              &num_days          },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST output[] = {
          { "output",            TYPE_START,            NULL               },
          { "out_dir",           TYPE_STR,              &out_dir           },
          { "out_fn",            TYPE_STR,              &out_fn            },
          { "out_lkn",           TYPE_BOOL,             &out_lkn           },
          { "nsave",             TYPE_INT,               nsave             },
          { "csv_point_nlevs",   TYPE_INT,              &csv_point_nlevs   },
          { "csv_point_fname",   TYPE_STR,              &csv_point_fname   },
          { "csv_point_at",      TYPE_DOUBLE|MASK_LIST, &csv_point_at      },
          { "csv_point_nvars",   TYPE_INT,              &csv_point_nvars   },
          { "csv_point_vars",    TYPE_STR|MASK_LIST,    &csv_point_vars    },
          { "csv_lake_fname",    TYPE_STR,              &csv_lake_fname    },
          { "csv_outlet_allinone", TYPE_BOOL,           &csv_outlet_allinone},
          { "csv_outlet_fname",  TYPE_STR,              &csv_outlet_fname  },
          { "csv_outlet_nvars",  TYPE_INT,              &csv_outlet_nvars  },
          { "csv_outlet_vars",   TYPE_STR|MASK_LIST,    &csv_outlet_vars   },
          { "csv_ovrflw_fname",  TYPE_STR,              &csv_ovrflw_fname  },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST meteorology[] = {
          { "meteorology",       TYPE_START,            NULL               },
          { "met_sw",            TYPE_BOOL,             &met_sw            },
          { "lw_type",           TYPE_STR,              &lw_type           },
          { "rain_sw",           TYPE_BOOL,             &rain_sw           },
          { "snow_sw",           TYPE_BOOL,             &snow_sw           },
          { "atm_stab",          TYPE_BOOL,             &atm_stab          },
          { "meteo_fl",          TYPE_STR,              &meteo_fl          },
          { "subdaily",          TYPE_BOOL,             &subdaily          },
          { "wind_factor",       TYPE_DOUBLE,           &wind_factor       },
          { "sw_factor",         TYPE_DOUBLE,           &sw_factor         },
          { "lw_factor",         TYPE_DOUBLE,           &lw_factor         },
          { "at_factor",         TYPE_DOUBLE,           &at_factor         },
          { "rh_factor",         TYPE_DOUBLE,           &rh_factor         },
          { "rain_factor",       TYPE_DOUBLE,           &rain_factor       },
          { "coef_wind_drag",    TYPE_DOUBLE,           &coef_wind_drag    },
          { "CE",                TYPE_DOUBLE,           &CE                },
          { "CH",                TYPE_DOUBLE,           &CH                },
          { "time_fmt",          TYPE_STR,              &timefmt_m         },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST inflow[] = {
          { "inflow",            TYPE_START,            NULL               },
          { "num_inflows",       TYPE_INT,              &num_inflows       },
          { "names_of_strms",    TYPE_STR|MASK_LIST,    &names_of_strms    },
          { "strm_hf_angle",     TYPE_DOUBLE|MASK_LIST, &strm_hf_angle     },
          { "strmbd_slope",      TYPE_DOUBLE|MASK_LIST, &strmbd_slope      },
          { "strmbd_drag",       TYPE_DOUBLE|MASK_LIST, &strmbd_drag       },
          { "inflow_factor",     TYPE_DOUBLE|MASK_LIST, &inflow_factor     },
          { "inflow_fl",         TYPE_STR|MASK_LIST,    &inflow_fl         },
          { "inflow_varnum",     TYPE_INT,              &inflow_varnum     },
          { "inflow_vars",       TYPE_STR|MASK_LIST,    &inflow_vars       },
          { "time_fmt",          TYPE_STR,              &timefmt_i         },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST outflow[] = {
          { "outflow",           TYPE_START,            NULL               },
          { "num_outlet",        TYPE_INT,              &num_outlet        },
          { "flt_off_sw",        TYPE_BOOL|MASK_LIST,   &flt_off_sw        },
          { "outl_elvs",         TYPE_DOUBLE|MASK_LIST, &outl_elvs         },
          { "bsn_len_outl",      TYPE_DOUBLE|MASK_LIST, &bsn_len_outl      },
          { "bsn_wid_outl",      TYPE_DOUBLE|MASK_LIST, &bsn_wid_outl      },
          { "outflow_fl",        TYPE_STR|MASK_LIST,    &outflow_fl        },
          { "outflow_factor",    TYPE_DOUBLE|MASK_LIST, &outflow_factor    },
          { "time_fmt",          TYPE_STR,              &timefmt_o         },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST diffuser[] = {
          { "diffuser",          TYPE_START,            NULL               },
          { "NumDif",            TYPE_INT,              &NumDif            },
          { "diff",              TYPE_DOUBLE|MASK_LIST, &mol_diffusivity   },
          { NULL,                TYPE_END,              NULL               }
    };
    NAMELIST debugging[] = {
          { "debugging",         TYPE_START,            NULL               },
          { "disable_evap",      TYPE_BOOL,             &no_evap           },
          { NULL,                TYPE_END,              NULL               }
    };

/*----------------------------------------------------------------------------*/

    fprintf(stderr, "Reading config from %s\n",glm_nml_file);

    // Open the namelist file.
    if ( (namlst = open_namelist(glm_nml_file)) < 0 ) {
        fprintf(stderr,"Error opening namelist file %s\n", glm_nml_file);
        exit(1);
    }

    //-------------------------------------------------
    // Set some default values
    coef_inf_entrain = 0.;
    Kw = 0.2;

    if ( get_namelist(namlst, glm_setup) != 0 ) {
       fprintf(stderr,"Error reading the 'glm_setup' namelist from %s\n", glm_nml_file);
       exit(1);
    }

    MaxLayers = max_layers;
    VMin = min_layer_vol;
    DMin = min_layer_thick;
    DMax = max_layer_thick;

    einff = coef_inf_entrain;

    wq_calc   = TRUE;

    if ( get_namelist(namlst, wq_setup) ) {
        fprintf(stderr, "No WQ config\n");
        wq_calc           = FALSE;
        ode_method        = 1;
        split_factor      = 1;
        bioshade_feedback = TRUE;
        repair_state      = FALSE;
    }

    //-------------------------------------------------
    if ( get_namelist(namlst, time) ) {
        fprintf(stderr,"Error reading the 'time' namelist from %s\n", glm_nml_file);
        exit(1);
    }

    nDays = num_days;
    timestep = dt;

    printf("nDays %d timestep %f\n", nDays, timestep);

    //-------------------------------------------------
    create_lake(namlst);

    //-------------------------------------------------
    csv_point_nlevs = 0;
    csv_point_nvars = 0;
    csv_lake_fname = NULL;

    if ( get_namelist(namlst, output) ) {
        fprintf(stderr,"Error in output parameters specified");
        strcpy(outp_dir, ".");
        strcpy(outp_fn, "output");
        *nsave = 24;
    } else {
        strcpy(outp_dir, out_dir);
        strcpy(outp_fn, out_fn);
    }

    if ( csv_point_nlevs > MaxPointCSV ) { fprintf(stderr, "csv_point_nlevs must be < %d\n", MaxPointCSV); exit(1); }
    if ( csv_point_nvars > MaxCSVOutVars ) { fprintf(stderr, "csv_point_nlevs must be < %d\n", MaxCSVOutVars); exit(1); }
    if ( csv_outlet_nvars > MaxCSVOutVars ) { fprintf(stderr, "csv_outlet_nlevs must be < %d\n", MaxCSVOutVars); exit(1); }

    if ( wq_calc ) {
        for (i = 0; i < csv_point_nvars; i++)
            set_csv_point_varname(i, csv_point_vars[i]);
    } else {
        /**********************************************************************
         * If we are not doing water quality calculations, ignore vars that   *
         * belong to WQ states                                                *
         **********************************************************************/
        int tn = csv_point_nvars, j = 0;
        for (i = 0; i < csv_point_nvars; i++) {
            if ( internal_var(csv_point_vars[i]) )
                set_csv_point_varname(j++, csv_point_vars[i]);
            else tn--;
        }
        csv_point_nvars = tn;
    }
    configure_csv(csv_point_nlevs, csv_point_at, csv_point_fname, csv_point_nvars, csv_lake_fname);

    if ( wq_calc ) {
        for (i = 0; i < csv_outlet_nvars; i++)
            set_csv_outfl_varname(i, csv_outlet_vars[i]);
    } else {
        int tn = csv_outlet_nvars, j = 0;
        for (i = 0; i < csv_outlet_nvars; i++) {
            if ( internal_var(csv_outlet_vars[i]) )
                set_csv_outfl_varname(j++, csv_outlet_vars[i]);
            else tn--;
        }
        csv_outlet_nvars = tn;
    }
    configure_outfl_csv(csv_outlet_allinone, csv_outlet_fname, csv_outlet_nvars, csv_ovrflw_fname);

    //-------------------------------------------------
    wind_factor = 1.0;
    sw_factor = 1.0;
    lw_factor = 1.0;
    at_factor = 1.0;
    rh_factor = 1.0;
    rain_factor = 1.0;

    if ( get_namelist(namlst, meteorology) ) {
        fprintf(stderr,"Error reading 'meteorology' from namelist file %s\n", glm_nml_file);
        exit(1);
    }

    if ( strcmp(lw_type, "LW_CC") == 0 )
        lw_ind = LW_CC;
    else if ( strcmp(lw_type, "LW_IN") == 0 )
        lw_ind = LW_IN;
    else if ( strcmp(lw_type, "LW_NET") == 0 )
        lw_ind = LW_NET;
    else {
        fprintf(stderr," Error in long wave type : '%s' unknown\n", lw_type);
        exit(1);
    }

    open_met_file(meteo_fl, snow_sw, rain_sw, timefmt_m);

    //-------------------------------------------------
    for (i = 0; i < MaxInf; i++) {
        for (j = 0; j < MaxPar; j++ ) {
            Inflows[i].QDown[j] = 0.0;
            Inflows[i].QIns[j] = 0.0;
            Inflows[i].SDown[j] = 0.0;
            Inflows[i].SIns[j] = 0.0;
            Inflows[i].TDown[j] = 0.0;
            Inflows[i].TIns[j] = 0.0;
            Inflows[i].DDown[j] = 0.0;
            Inflows[i].DOld[j] = 0.0;
            Inflows[i].DIIns[j] = 0.0;
            Inflows[i].InPar[j] = 0;
            for (k = 0; k < MaxVars; k++) {
                Inflows[i].WQIns[j][k] = 0.0;
                Inflows[i].WQDown[j][k] = 0.0;
            }
            Inflows[i].WQInf[j] = 0.0;
        }
        Inflows[i].HFlow = 0.0;
        Inflows[i].TotIn = 0.0;
        Inflows[i].Dlwst = 0.0;
        Inflows[i].iCnt = 0;
        Inflows[i].NoIns = 0;
    }

    num_inflows = 0;
    if ( get_namelist(namlst, inflow) ) {
        NumInf = 0;
        if ( num_inflows == 0 )
            fprintf(stderr, "No inflow config, assuming no inflows\n");
        else {
            if ( num_inflows > MaxInf )
                fprintf(stderr, "Too many inflows specified in inflow config %d > %d\n", num_inflows, MaxInf);
            else
                fprintf(stderr, "Unknown error in inflow config\n");
            exit(1);
        }
    } else {
        if ( num_inflows > MaxInf ) {
            fprintf(stderr, "Too many inflows specified in inflow config %d > %d\n", num_inflows, MaxInf);
            exit(1);
        }
        NumInf = num_inflows;

        for (i = 0; i < NumInf; i++) {
            Inflows[i].Alpha = strm_hf_angle[i] * Pi/PiDeg;
            Inflows[i].Phi = strmbd_slope[i] * Pi/PiDeg;
            Inflows[i].DragCoeff = strmbd_drag[i];
            Inflows[i].Factor = inflow_factor[i];

            open_inflow_file(i, inflow_fl[i], inflow_varnum, (const char**)inflow_vars, timefmt_i);
        }
    }

    //-------------------------------------------------
    if ( get_namelist(namlst, outflow) ) {
        fprintf(stderr, "No outflow config, assuming no outflows\n");
        NumOut = 0;
    } else {
        if ( num_outlet > MaxOut) {
            fprintf(stderr, "Too many outlets specified in outflow config %d > %d\n", num_outlet, MaxOut);
            exit(1);
        }
        NumOut = num_outlet;
        if ( flt_off_sw == NULL ) {
            flt_off_sw = malloc(sizeof(LOGICAL)*num_outlet);
            for (i = 0; i < NumOut; i++) flt_off_sw[i] = FALSE;
        }

        for (i = 0; i < NumOut; i++) {
            if ( (Outflows[i].FloatOff = flt_off_sw[i]) ) {
                if ( (outl_elvs[i] > (crest_elev-base_elev)) || (outl_elvs[i] < 0.0) ) {
                    fprintf(stderr,
                    "Floating outflow (%124lf) above surface or deeper than lake depth (%12.4lf)\n",
                                    outl_elvs[i], crest_elev - base_elev);
                    exit(1);
                }
                Outflows[i].OLev = outl_elvs[i];  // if floating outlet make it is relative to surface
            } else {
                if ( (outl_elvs[i] > crest_elev) || (outl_elvs[i] < base_elev) ) {
                    fprintf(stderr,
                    "Outflow elevation (%124lf) above crest elevation (%12.4lf) or below base elevation (%12.4lf)\n",
                                    outl_elvs[i], crest_elev, base_elev);
                    exit(1);
                }
                Outflows[i].OLev = outl_elvs[i] - Base; // else make it relative to bottom
            }
//          Outflows[i].OLen = bsn_len_outl[i];
//          Outflows[i].OWid = bsn_wid_outl[i];
            Outflows[i].Factor = outflow_factor[i];

            if (outflow_fl[i] != NULL) open_outflow_file(i, outflow_fl[i], timefmt_o);
        }
    }

    //-------------------------------------------------
    for (i = 1; i < MaxDif; i++) mol_diffusivity[i] = 1.25E-09;
    mol_diffusivity[0] = 0.00000014;
    NumDif = 2;

    if ( get_namelist(namlst, diffuser) )
         fprintf(stderr,"No diffuser data, setting default values\n");

    //--------------------------------------------------------------------------

    if ( timefmt != 2 && timefmt != 3 ) {
        fprintf(stderr, "invalid time format \"%d\"\n", timefmt);
        exit(1);
    }
    if ( start == NULL ) {
        fprintf(stderr, "Start date is required\n"); exit(1);
    }
    if ( timefmt == 2 ) {
        if ( stop == NULL ) {
            fprintf(stderr, "Stop date is required for timefmt == 2\n"); exit(1);
        }
    }
    if ( stop == NULL ) { stop = malloc(40); *stop = 0; }
    else if ( strlen(stop) < 39 ) stop = realloc(stop, 40);
    if ( timefmt != 2 ) *stop = 0;

    julianday = init_time(start, stop, timefmt, &nDays);
    calendar_date(julianday, &jyear, &jmonth, &jday);
    //# Days since start of the year, jyear
    jday = julianday - julian_day(jyear, 1, 1) + 1;

    *jstart = julian_day(jyear, 1, jday);

    lkn  = out_lkn;

    //--------------------------------------------------------------------------

    Num_WQ_Vars = 0;

    if ( wq_calc ) {
        int l = strlen(wq_nml_file);
        init_glm_wq(wq_nml_file, &l, &MaxLayers, &Num_WQ_Vars, &Kw); // Reads WQ namelist file
    }
    NumDif = Num_WQ_Vars + 2;

    initialise_lake(namlst);

    // This is where we could map inflow, met and csv_output vars to wq vars

    if ( ! WQ_VarsIdx ) {
        WQ_VarsIdx = malloc(sizeof(int)*inflow_varnum);
        for (j = 0; j < inflow_varnum; j++) WQ_VarsIdx[j] = -1;
    }
    if ( wq_calc ) {
        /* The first 3 vars are flow, temp and salt */
        for (j = 3; j < inflow_varnum; j++) {
            int k =  strlen(inflow_vars[j]);
            WQ_VarsIdx[j-3] = wqvar_index_c(inflow_vars[j], &k);
        }

        set_glm_wq_data(Lake, &MaxLayers, &NumLayers, &MetData, &SurfData, &dt);
    }

    get_namelist(namlst, debugging);

    close_namelist(namlst);  // Close the glm.nml file

#if DEBUG
    debug_initialisation(0);
#endif
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * This routine sets up the lake morphometry                                  *
 ******************************************************************************/
void create_lake(int namlst)
{
    /*---------------------------------------------
     * morphometry
     *-------------------------------------------*/
    char           *lake_name;
//  REALTYPE        Latitude;      // global variable
//  REALTYPE        Longitude;     // global variable
//  REALTYPE        base_elev;     // module variable
//  REALTYPE        crest_elev;    // module variable
    REALTYPE        bsn_len;
    REALTYPE        bsn_wid;
    int             bsn_vals;
    REALTYPE       *H = NULL;
    REALTYPE       *A = NULL;
    REALTYPE       *V = NULL;
    /*-------------------------------------------*/


    int kar;                // first layer with a positive area
    int ksto;               // first layer with a positive storage
#ifndef _VISUAL_C_
    // The dumb compiler on windows doesn't like this so must malloc manually
    REALTYPE alpha_b[MaxLayers]; // interpolation coefficient for volume
    REALTYPE beta_b[MaxLayers];  // interpolation coefficient for area
#else
    REALTYPE *alpha_b;           // interpolation coefficient for volume
    REALTYPE *beta_b;            // interpolation coefficient for area
#endif
    int lanext;             // temporary variable for interpolating area
    int lvnext;             // temporary variable for interpolating volume
    REALTYPE x, y;
    int i,j;
    int ij;

    NAMELIST morphometry[] = {
          { "morphometry",       TYPE_START,            NULL               },
          { "lake_name",         TYPE_STR,              &lake_name         },
          { "Latitude",          TYPE_DOUBLE,           &Latitude          },
          { "Longitude",         TYPE_DOUBLE,           &Longitude         },
          { "base_elev",         TYPE_DOUBLE,           &base_elev         },
          { "crest_elev",        TYPE_DOUBLE,           &crest_elev        },
          { "bsn_len",           TYPE_DOUBLE,           &bsn_len           },
          { "bsn_wid",           TYPE_DOUBLE,           &bsn_wid           },
          { "bsn_vals",          TYPE_INT,              &bsn_vals          },
          { "H",                 TYPE_DOUBLE|MASK_LIST, &H                 },
          { "A",                 TYPE_DOUBLE|MASK_LIST, &A                 },
          { "V",                 TYPE_DOUBLE|MASK_LIST, &V                 },
          { NULL,                TYPE_END,              NULL               }
    };
    REALTYPE h_z = 0.;

/*----------------------------------------------------------------------------*/

    base_elev = MISVAL;
    crest_elev = MISVAL;
    //-------------------------------------------------
    if ( get_namelist(namlst, morphometry) ) {
        fprintf(stderr,"Error reading the 'morphometry' namelist from %s\n", glm_nml_file);
        exit(1);
    }

    if (base_elev != MISVAL || crest_elev != MISVAL) {
        fprintf(stderr, "values for base_elev and crest_elev are no longer used\n");
    }
    if ( V != NULL ) {
        fprintf(stderr, "values for V are no longer used\n");
        free(V); V = NULL;
    }

    base_elev = H[0]; crest_elev = H[bsn_vals-1];

    if ( (MaxLayers * DMax) < (crest_elev - base_elev) ) {
        fprintf(stderr, "Configuration Error. MaxLayers * max_layer_height < depth of the lake");
        exit(1);
    }

    Lake = malloc(sizeof(LakeDataType)*MaxLayers);
    memset(Lake, 0, sizeof(LakeDataType)*MaxLayers);
    for (i = 0; i < MaxLayers; i++) Lake[i].ExtcCoefSW = Kw;

    Base = H[0];
    ksto = 0;
    kar = 0;

    if ( V == NULL ) V = malloc(sizeof(REALTYPE)*bsn_vals);
    V[0] = 0.;
    for (i = 1; i < bsn_vals; i++) {
        if ( (A[i] < A[i-1]) || (H[i] < H[i-1]) ) {
            fprintf(stderr, "Error. H and A in morphometry must be monotonically increasing\n");
            fprintf(stderr, "A[%d] = %f; A[%d] = %f; H[%d] = %f; H[%d] = %f\n",
                             i-1, A[i-1], i, A[i], i-1, H[i-1], i, H[i]);
            exit(1);
        }
        V[i] = V[i-1] + (  (A[i-1]+(A[i]-A[i-1])/2.0) * (H[i] - H[i-1]));
    }

    for (i = 0; i < bsn_vals; i++) {
        A[i] /= 1000.0;
        H[i] -= Base;

        if (A[i] <= 0.0 ) kar++;
        if (H[i] <= 0.0 ) ksto++;
    }

    /**************************************************************************
     * The model creates a refined lookup-table of depth-area-volume for later*
     * use. The maximum number of elements in the internal lookup table is    *
     * defined as Nmorph and calculated based on the highest supplied lake    *
     * depth index into storage arrays may be calculated as 10* the maximum   *
     * lake depth. Since the surface layer height is calculated after inflows *
     * and outflows, the height may be temporarily above the crest level,     *
     * and therefore 10 additional layers are included                        *
     **************************************************************************/
    Nmorph = ( ( H[bsn_vals-1] * 10.0 ) + 1.0 / 1000.0 ) + 10;

    allocate_storage();

    CrestLevel = crest_elev - Base;
    LenAtCrest = bsn_len;
    WidAtCrest = bsn_wid;

#ifdef _VISUAL_C_
    alpha_b = malloc(sizeof(REALTYPE) * MaxLayers);
    beta_b = malloc(sizeof(REALTYPE) * MaxLayers);
#endif
    // Loop from the bottom to top of the provided depth points given in
    // &morphometry to calculate the bathymetric interpolation coefficients,
    // "a" and "b", at each level
    for (i = 1; i < (bsn_vals-1); i++) {
        if (V[i] > 0.0)
            alpha_b[i] = log10(V[i+1]/V[i]) / log10(H[i+1] / H[i]);

        dbgprt( " i = %2d V[i+1] = %24.18e V[i] = %24.18e ALOG10 = %24.18e\n", i, V[i+1], V[i], log10(V[i+1]/V[i]));
        dbgprt( " i = %2d H[i+1] = %24.18e H[i] = %24.18e ALOG10 = %24.18e\n", i, H[i+1], H[i], log10(H[i+1]/H[i]));
        dbgprt( " i = %2d  alpha_b[i] = %24.18e\n", i, alpha_b[i]);

        if (A[i] > 0.0)
            beta_b[i] = log10(A[i+1]/A[i]) / log10(H[i+1] / H[i]);

        dbgprt( " i = %2d A[i+1] = %24.18e A[i] = %24.18e ALOG10 = %24.18e\n", i, A[i+1], A[i], log10(A[i+1]/A[i]));
        dbgprt( " i = %2d H[i+1] = %24.18e H[i] = %24.18e ALOG10 = %24.18e\n", i, H[i+1], H[i], log10(H[i+1]/H[i]));
        dbgprt( " i = %2d  beta_b[i] = %24.18e\n", i, beta_b[i]);

    }
    // The values of a and b exponents for layer 0 are not used as the
    // area and volume are assumed to vary linearly from the
    // lake bottom to the top of the first layer
    alpha_b[0] = 1.0;
    beta_b[0] = 1.0;

    alpha_b[bsn_vals-1] = alpha_b[bsn_vals-2];
    beta_b[bsn_vals-1] = beta_b[bsn_vals-2];

    // Now prepare a refined depth-area-volume relationship using finer depth
    // increments to support layer interpolations later in the simulation
    // Note: kar is the first layer with a positive A
    // and  ksto is the first layer with a positive V
    j = -1;
    for (i = 0; i < Nmorph; i++) {
    	h_z = (i+1.0)/10.0;

        while (j != (bsn_vals-1)) {
            j++;
            if (h_z < H[j+1]) break;
        }

        // Interpolate A and V for all depths below the
        // first layer that is a non-zero table entry (ie blank)
        if (j == 0) {
            lvnext = MAX(1, ksto);
            lanext = MAX(1, kar);
            MphLevelVol[i] = V[lvnext] * h_z / H[lvnext];
            MphLevelArea[i] = A[lanext] * h_z / H[lanext];
        } else {
            if (j < ksto)
                MphLevelVol[i] = V[ksto] * h_z / H[ksto];
            else
                MphLevelVol[i] = V[j] * pow((h_z / H[j]), alpha_b[j]);

            dbgprt( " i=%2d j = %2d V = %24.18f H = %24.18f alpha_b = %24.18f\n", i, j, V[j], H[j], alpha_b[j]);
            dbgprt( " h_z = %24.18f and result is %24.18f pow = %24.18f\n", h_z, MphLevelVol[i], pow(h_z / H[j], alpha_b[j]));

            if (j < kar)
                MphLevelArea[i] = A[kar] * h_z / H[kar];
            else
                MphLevelArea[i] = A[j] * pow((h_z / H[j]), beta_b[j]);
        }
        j--;
    }

    // Calculate change between points for volume and area
    for (i = 0; i < Nmorph-1; i++) {
        dMphLevelVol[i] = (MphLevelVol[i+1] - MphLevelVol[i]);
        dMphLevelArea[i] = (MphLevelArea[i+1] - MphLevelArea[i]);
    }
    dMphLevelVol[Nmorph-1] = dMphLevelVol[Nmorph-2];
    dMphLevelArea[Nmorph-1] = dMphLevelArea[Nmorph-2];

    // Calculate the maximum and minimum volume and volume at crest
    VMin = MphLevelVol[Nmorph-1] * VMin;
    VMax = 10.0 * VMin;

    // Calculate storage at crest level, VolAtCrest
    x = CrestLevel * 10.0;
    y = AMOD(x,one);
    ij = x-y;
    if (ij >= Nmorph) {
        y =+ (ij - Nmorph);
        ij = Nmorph;
    }
    ij--;
    VolAtCrest = MphLevelVol[ij] + y * dMphLevelVol[ij];
    crest_sto_idx = ij;


    memcpy(MphLevelVoldash, MphLevelVol, sizeof(REALTYPE) * Nmorph);    // MphLevelVoldash = MphLevelVol;
    memcpy(dMphLevelVolda, dMphLevelVol, sizeof(REALTYPE) * Nmorph);    // dMphLevelVolda = dMphLevelVol;
    if ( V != NULL ) free(V);
    if ( A != NULL ) free(A);
    if ( H != NULL ) free(H);

#ifdef _VISUAL_C_
    free(alpha_b); free(beta_b);
#endif
}


/******************************************************************************
 * Routine to set the initial values for each layer                           *
 ******************************************************************************/
void initialise_lake(int namlst)
{
    /*---------------------------------------------
     * init_profiles
     *-------------------------------------------*/
    REALTYPE        lake_depth;
    int             num_heights; // support the old way
    REALTYPE       *the_heights; // support the old way
    int             num_depths;
    REALTYPE       *the_depths;
    REALTYPE       *the_temps;
    REALTYPE       *the_sals;
    int             num_wq_vars;
    char          **wq_names;
    REALTYPE       *wq_init_vals;
    /*-------------------------------------------*/

    NAMELIST init_profiles[] = {
          { "init_profiles",     TYPE_START,            NULL               },
          { "lake_depth",        TYPE_DOUBLE,           &lake_depth        },
          { "num_heights",       TYPE_INT,              &num_heights       },
          { "the_heights",       TYPE_DOUBLE|MASK_LIST, &the_heights       },
          { "num_depths",        TYPE_INT,              &num_depths        },
          { "the_depths",        TYPE_DOUBLE|MASK_LIST, &the_depths        },
          { "the_temps",         TYPE_DOUBLE|MASK_LIST, &the_temps         },
          { "the_sals",          TYPE_DOUBLE|MASK_LIST, &the_sals          },
          { "num_wq_vars",       TYPE_INT,              &num_wq_vars       },
          { "wq_names",          TYPE_STR|MASK_LIST,    &wq_names          },
          { "wq_init_vals",      TYPE_DOUBLE|MASK_LIST, &wq_init_vals      },
          { NULL,                TYPE_END,              NULL               }
    };

    int i, j;
    int nx, np, nz;
    int *idx = NULL;

/*----------------------------------------------------------------------------*/
    //-------------------------------------------------
    // Do the initial profiles
    //-------------------------------------------------
    num_heights = 0;
    num_depths = 0;
    lake_depth = MISVAL;
    num_wq_vars = 0;
    if ( get_namelist(namlst, init_profiles) ) {
        fprintf(stderr,"Error reading initial_profiles from namelist file %s\n", glm_nml_file);
        exit(1);
    }
    if (! wq_calc) num_wq_vars = 0;

    // Initial values for the number of levels specified in the glm.nml file
    if ( num_heights != 0 ) {
        NumLayers = num_heights;
        for (i = 0; i < num_heights; i++) {
            Lake[i].Height = the_heights[i];
            Lake[i].Temp = the_temps[i];
            Lake[i].Salinity = the_sals[i];
        }

        if (the_heights[num_depths-1] > CrestLevel) {
            fprintf(stderr, "maximum height is greater than crest level\n");
            exit(1);
        }
        num_depths = num_heights;
    } else {
        // Initial values for the number of levels specified in the glm.nml file
        NumLayers = num_depths;
        if ( lake_depth == MISVAL ) {
            fprintf(stderr, "the depths format requires a lake_depth value\n");
            exit(1);
        }
        for (i = 0, j = num_depths-1; i < num_depths; i++, j--) {
            Lake[i].Height = lake_depth - the_depths[j];
            Lake[i].Temp = the_temps[j];
            Lake[i].Salinity = the_sals[j];
        }

        if (the_depths[num_depths-1] > lake_depth ) {
            fprintf(stderr, "last depth is greater the specified lake depth\n");
            exit(1);
        }
    }

    // First map the wq state var names to their indices
    if ( num_wq_vars > 0 ) {
        idx = malloc(sizeof(int)*num_wq_vars);
        for (j = 0; j < num_wq_vars; j++) {
            int k =  strlen(wq_names[j]);
            if ((idx[j] = wqvar_index_c(wq_names[j], &k)) < 0)
                fprintf(stderr, "Cannot find \"%s\" for initial value\n", wq_names[j]);
        }
    }
    // Likewise for each of the listed wq vars
    for (j = 0; j < num_wq_vars; j++) {
        if ( num_heights != 0 ) {
            for (i = 0; i < num_depths; i++)
                if ( idx[j] >= 0 ) _WQ_Vars(idx[j],i) = wq_init_vals[j*num_depths+i];
        } else {
            int k;
            for (i = 0, k = num_depths-1; i < num_depths; i++, k--)
                if ( idx[j] >= 0 ) _WQ_Vars(idx[j],i) = wq_init_vals[j*num_depths+k];
        }
    }

    // Now interpolate into at least 50 layers
    while (NumLayers <= 50) {
        for (i = botmLayer; i < NumLayers; i++) {
            nx = 2 * (surfLayer - i);
            np = surfLayer - i;
            Lake[nx].Height = Lake[np].Height;
            Lake[nx].Temp = Lake[np].Temp;
            Lake[nx].Salinity = Lake[np].Salinity;
            for (j = 0; j < num_wq_vars; j++)
                if ( idx[j] >= 0 ) _WQ_Vars(idx[j],nx) = _WQ_Vars(idx[j],np);
        }
        for (i = botmLayer+1; i < NumLayers; i++) {
            nx = 2 * i - 1;
            np = 2 * i - 2;
            nz = 2 * i - 0;
            Lake[nx].Temp = (Lake[np].Temp + Lake[nz].Temp) / 2.0;
            Lake[nx].Height = (Lake[np].Height + Lake[nz].Height) / 2.0;
            Lake[nx].Salinity = (Lake[np].Salinity + Lake[nz].Salinity) / 2.0;
            for (j = 0; j < num_wq_vars; j++)
                if ( idx[j] >= 0 ) _WQ_Vars(idx[j],nx) = (_WQ_Vars(idx[j],np) + _WQ_Vars(idx[j],nz)) / 2.0;
        }
        NumLayers = 2*NumLayers - 1;
    }

    // And free the temporary index map
    if ( num_wq_vars > 0 ) free(idx);

    // calculate the density from the temp and salinity just read in
    for (i = botmLayer; i < NumLayers; i++)
        Lake[i].Density = calculate_density(Lake[i].Temp, Lake[i].Salinity);

}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************
 * The subroutine init_time() initialises the time module by reading          *
 * a namelist and take actions according to the specifications.               *
 ******************************************************************************/
#define INIT_T_STEP       1
#define INIT_T_BEGIN_END  2
#define INIT_T_BEGIN_STEP 3

static int init_time(char *start, char *stop, int timefmt, int *nDays)
{
    int jul1=0, secs1=0, jul2, secs2;
    int nsecs;

    switch (timefmt) {
        case INIT_T_STEP:
//          strcpy(start,"2000-01-01 00:00:00");
//          read_time_string(start, &jul1, &secs1);
            fprintf(stderr, "timefmt = 1 not supported\n");
            exit(1);
            break;
        case INIT_T_BEGIN_END:
            read_time_string(start, &jul1, &secs1);
            read_time_string(stop, &jul2, &secs2);

            nsecs = time_diff(jul2, secs2, jul1, secs1);

            *nDays = jul2-jul1;
            if (nsecs < 86400 && jul1 != jul2) nDays = nDays-1;
            nsecs = nsecs - 86400*(*nDays);
            break;
        case INIT_T_BEGIN_STEP:
            read_time_string(start, &jul1, &secs1);

            nsecs = (*nDays) * 86400;
            jul2  = jul1 + (*nDays);
            secs2 = (nsecs%86400);

            write_time_string(stop, jul2, secs2);
            break;
        default:
            fprintf(stderr, "Invalid time format specified\n");
            exit(1);
            break;
    }

    return jul1;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/