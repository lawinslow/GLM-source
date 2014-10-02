/******************************************************************************
 *                                                                            *
 * glm_plot.c                                                                 *
 *                                                                            *
 * plotting for glm                                                           *
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
#ifdef PLOTS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "glm.h"

#include "glm_types.h"
#include "glm_globals.h"
#include "glm_plot.h"

#include "namelist.h"
#include "libplot.h"

#ifdef XPLOTS
    int xdisp = 0;
#endif

int do_plots, saveall = 0;
int nplots, theplots[MAX_PLOTS+1];
char **vars = NULL;
int today = 0;
int plotstep = 0;
REALTYPE psubday = 1;
char * plots_nml_name = "plots.nml";


/******************************************************************************/
void init_plots(int jstart, int ndays, REALTYPE crest)
{
    int        maxx, maxy, width, height,i,w,h,acrs;
    int        plot_width, plot_height;
    int        namlst;
    char     **title;
    REALTYPE  *min_z;
    REALTYPE  *max_z;
    REALTYPE   min_x, max_x;
    REALTYPE   min_y, max_y;
    char      *glm_vers = NULL;
    char      *title_font = NULL;
    char      *label_font = NULL;
    int        tsz, lsz;

    NAMELIST plots_window[] = {
          { "plots_window",   TYPE_START,            NULL               },
          { "width",          TYPE_INT,              &width             },
          { "height",         TYPE_INT,              &height            },
          { NULL,             TYPE_END,              NULL               }
    };
    NAMELIST plots[] = {
          { "plots",          TYPE_START,            NULL               },
          { "nplots",         TYPE_INT,              &nplots            },
          { "plot_width",     TYPE_INT,              &plot_width        },
          { "plot_height",    TYPE_INT,              &plot_height       },
          { "title",          TYPE_STR|MASK_LIST,    &title             },
          { "title_font",     TYPE_STR,              &title_font        },
          { "title_size",     TYPE_INT,              &tsz               },
          { "label_font",     TYPE_STR,              &label_font        },
          { "label_size",     TYPE_INT,              &lsz               },
          { "vars",           TYPE_STR|MASK_LIST,    &vars              },
          { "min_z",          TYPE_DOUBLE|MASK_LIST, &min_z             },
          { "max_z",          TYPE_DOUBLE|MASK_LIST, &max_z             },
          { NULL,             TYPE_END,              NULL               }
    };

/*----------------------------------------------------------------------------*/

    if ( (namlst = open_namelist(plots_nml_name)) < 0 ) {
        fprintf(stderr,"Error opening namelist file plots.nml\n");
        return;
    }

    /*-----------------------------------------------*/
    if ( get_namelist(namlst, plots_window) != 0 ) {
       fprintf(stderr,"Error reading the 'plots_window' namelist from plots.nml\n");
       return;
    }

    if ( get_namelist(namlst, plots) != 0 ) {
       fprintf(stderr,"Error reading the 'plots' namelist from plots.nml\n");
       return;
    }

    close_namelist(namlst);

    if ( nplots > MAX_PLOTS ) {
        fprintf(stderr, "Built-in plotter can only handle %d plots\n", MAX_PLOTS);
        nplots = MAX_PLOTS;
    }

    glm_vers = malloc(strlen(GLM_VERSION) + 10);
    sprintf(glm_vers, "GLM-%s", GLM_VERSION);

    maxx = width;
    maxy = height;

    set_progname(glm_vers);
#ifdef XPLOTS
    if ( xdisp ) {
        if ( init_plotter(&maxx, &maxy) < 0 ) exit(1);
    }
#endif

    acrs = (maxx + 90) / (100 + plot_width);

    if (title_font != NULL) set_plot_font(PF_TITLE, tsz, title_font);
    if (label_font != NULL) set_plot_font(PF_LABEL, lsz, label_font);

    min_x = jstart;
    max_x = jstart + ndays + (1-psubday);
    min_y = 0;
    max_y = crest;
    w = 10;
    h = 10;
    for (i = 0; i < nplots; i++) {
        theplots[i] = create_plot(w, h, plot_width, plot_height, title[i]);
        w += plot_width + 100;
        if ( ((i+1) % acrs) == 0 ) {
            w = 10;
            h += plot_height + 100;
        }
        set_plot_x_label(theplots[i], "Time");
        set_plot_y_label(theplots[i], "Depth");
   //   set_plot_z_label(theplots[i], "m-mol/L");
        set_plot_x_limits(theplots[i], min_x, max_x);
        set_plot_y_limits(theplots[i], min_y, max_y);
        set_plot_z_limits(theplots[i], min_z[i], max_z[i]);
        set_plot_version(theplots[i], glm_vers);
    }
    free(glm_vers);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/******************************************************************************/
void put_xplot_val(char *name, int wlev, REALTYPE *val)
{
    int i, j, which = 0;
    REALTYPE todayish;

/*----------------------------------------------------------------------------*/
    if ( ! do_plots ) return;

    if ( today <= 0 ) return;

    if ( wlev != NumLayers ) fprintf(stderr,"wlev = %d sufLayer = %d\n",wlev,surfLayer);

    todayish = psubday;
    todayish *= plotstep;
    todayish += today;

//  fprintf(stderr, "plot %3d %d %f %f\n", plotstep, today, psubday, todayish);

    j = 0;
    while (j < nplots) {
        if ( strcasecmp(name, vars[j]) == 0 ) {
            if (strcasecmp(name, "temp") == 0) which = 1;
            else if (strcasecmp(name, "salt") == 0) which = 2;
            else if (strcasecmp(name, "rad") == 0) which = 3;
            else if (strcasecmp(name, "extc") == 0) which = 4;
            else if (strcasecmp(name, "dens") == 0) which = 5;

            for (i = 0; i < NumLayers; i++) {
                switch (which) {
                    case 0:
                        plot_value(theplots[j], todayish, Lake[i].Height, val[i]);
                        break;
                    case 1:
                        plot_value(theplots[j], todayish, Lake[i].Height, Lake[i].Temp);
                        break;
                    case 2:
                        plot_value(theplots[j], todayish, Lake[i].Height, Lake[i].Salinity);
                        break;
                    case 3:
                        plot_value(theplots[j], todayish, Lake[i].Height, Lake[i].Light);
                        break;
                    case 4:
                        plot_value(theplots[j], todayish, Lake[i].Height, Lake[i].ExtcCoefSW);
                        break;
                    case 5:
                        plot_value(theplots[j], todayish, Lake[i].Height, Lake[i].Density);
                        break;
                }
            }
            break;
        }
        j++;
    }
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/******************************************************************************/
/* for Fortran */
/******************************************************************************/
void put_xplot_val_(char *name, int *len, int *wlev, REALTYPE *val)
{
    char *n = malloc(*len + 1);
    strncpy(n, name, *len); n[*len] = 0;
    put_xplot_val(n, *wlev, val);
    free(n);
}

#endif
