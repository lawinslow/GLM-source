/******************************************************************************
 *                                                                            *
 * glm_test_bird.c                                                            *
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
#include <time.h>

#include "glm.h"
#include "glm_types.h"
#include "glm_const.h"
#include "glm_bird.h"

#include <aed_time.h>

#undef PLOTS

#ifdef PLOTS
   #include <libplot.h>
#ifdef XPLOTS
   extern int xdisp;
#endif
#endif

double Longitude = 0., Latitude = 0., TimeZone = 0.;

int startday = 0, iclock = 0;
int numdays = 1;
int stepsize = 900; // 15 minutes (3600 for 1 hour)

static char timestr[20];

/******************************************************************************/
int main(int argc, char *argv[])
{
    int all_ok = 1, show_options = 0;

#ifdef PLOTS
    saveall = 0;
#ifdef XPLOTS
    xdisp = 0;
#endif
#endif

    argv++; argc--;
    while (argc > 0) {
        if ( strcmp(*argv, "--lat") == 0) {
            argv++; argc--;
            Latitude = strtod(*argv, NULL);
        }
        else if ( strcmp(*argv, "--lon") == 0) {
            argv++; argc--;
            Longitude = strtod(*argv, NULL);
        }
        else if ( strcmp(*argv, "--tz") == 0) {
            argv++; argc--;
            TimeZone = strtod(*argv, NULL);
        }
        else if ( strcmp(*argv, "--startdate") == 0) {
            argv++; argc--;
            read_time_string(*argv, &startday, &iclock);
        }
        else if ( strcmp(*argv, "--numdays") == 0) {
            argv++; argc--;
            numdays = strtol(*argv, NULL, 10);
        }
#ifdef PLOTS
#ifdef XPLOTS
        else if (strcmp(*argv, "--xdisp") == 0) {
            xdisp = 1;
            if ( argc > 1 && strncmp(argv[1], "--", 2) != 0 ) {
                argv++; argc--;
                plots_nml_name = *argv;
            }
        }
#endif
#endif
        else {
            if (strcmp(*argv, "--help") != 0)
                printf("Unknown flag %s\n", *argv);
            show_options = 1;
            all_ok = 0;
        }
        argc--; argv++;
    }

#ifdef PLOTS
# ifdef XPLOTS
    do_plots = xdisp || saveall;
# else
    do_plots = saveall;
# endif
#endif

    if ( show_options ) {
       printf("--help  : show this blurb\n");
#ifdef PLOTS
#ifdef XPLOTS
       printf("--lat <latitude>  : use this latitude value\n");
       printf("--lon <longitude> : use this longitude value\n");
       printf("--tz <timezone>   : use this timezone value\n");

       printf("--startdate <startdate> : start at this date (YYYY-MM-DD hh:mm:ss)\n");
       printf("--numdays <numdays>     : run for this many days\n");

       printf("--xdisp : display temp/salt and selected others in x-window\n");
       printf("--xdisp <plotsfile> : like --xdisp, but use <plotsfile> instead of plots.nml\n");
#endif
#endif
    }
    else if ( all_ok ) {
        int julian;

        if ( startday == 0 ) {
            time_t now = time(NULL);
            struct tm *tm = localtime(&now);
            startday = julian_day(tm->tm_year+1900, tm->tm_mon+1, tm->tm_mday);
        }

        printf("Longitude %6.2f ; Latitude %6.2f ; TZ %6.1f\n", Longitude, Latitude, TimeZone);
        printf("Start at day %d time %d\n", startday, iclock);

        Latitude = 2*Pi+Latitude*Pi/180; //# Convert latitude from degrees to radians
        julian = startday;
        while (numdays > 0) {
            write_time_string(timestr, julian, iclock);

            printf("%s, %e\n", timestr,
                            calc_bird(Longitude, Latitude, julian, iclock, TimeZone));

            if ( (iclock += stepsize) >= 86400 ) {
                iclock = 0; julian++;
                numdays--;
            }
        }
    }

    exit(0);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
