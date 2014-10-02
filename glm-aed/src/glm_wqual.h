/******************************************************************************
 *                                                                            *
 * glm_wqual.h                                                                *
 *                                                                            *
 * The interface between glm and water quality code                           *
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
#ifndef _GLM_WQUAL_H_
#define _GLM_WQUAL_H_

#if !FORTRAN_VERSION

void init_glm_wq(char *fname, int *len, int *kk, int *NumFABMVars, REALTYPE *pKw);
void set_glm_wq_data(LakeDataType *Lake, int *MaxLayers, int *NumLayers,
                MetDataType *MetData, SurfaceDataType *SurfData, REALTYPE *dt);


void do_glm_wq(int *wlev, int *pIce);
// wlev is the number of levels used; nlev is the total num levels in the array

void clean_glm_wq(void);

void init_glm_wq_output(int *ncid, int *x_dim, int *y_dim, int *z_dim, int *time_dim);

void write_glm_wq_(int *ncid, int *wlev, int *nlev, int *lvl, int *point_nlevs);

int wqvar_index_c(const char*name, int *len);

extern int ode_method;
extern int split_factor;
extern int bioshade_feedback;
extern int repair_state;
extern int multi_ben;

#endif

#endif
