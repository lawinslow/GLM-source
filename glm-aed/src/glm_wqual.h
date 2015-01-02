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

#ifndef _FORTRAN_VERSION_

typedef void (*wq_init_glm_t)(char *fname, int *len, int *kk, int *NumFABMVars, AED_REAL *pKw);
typedef void (*wq_set_glm_data_t)(LakeDataType *Lake, int *MaxLayers,
                MetDataType *MetData, SurfaceDataType *SurfData, AED_REAL *dt);
typedef void (*wq_do_glm_t)(int *wlev, int *pIce);
typedef void (*wq_clean_glm_t)(void);
typedef void (*wq_init_glm_output_t)(int *ncid, int *x_dim, int *y_dim, int *z_dim, int *time_dim);
typedef void (*wq_write_glm_t)(int *ncid, int *wlev, int *nlev, int *lvl, int *point_nlevs);
typedef int  (*wq_var_index_c_t)(const char*name, int *len);
typedef void (*wq_set_flags_t)(int *split_factor, CLOGICAL *mobility, CLOGICAL *bioshade,
                 CLOGICAL *repair_state, int *ode_method, CLOGICAL *multi_ben, CLOGICAL *do_plots);


extern wq_init_glm_t        p_wq_init_glm;
extern wq_set_glm_data_t    p_wq_set_glm_data;
extern wq_do_glm_t          p_wq_do_glm;
extern wq_clean_glm_t       p_wq_clean_glm;
extern wq_init_glm_output_t p_wq_init_glm_output;
extern wq_write_glm_t       p_wq_write_glm;
extern wq_var_index_c_t     p_wq_var_index_c;
extern wq_set_flags_t       p_wq_set_flags;

#define wq_init_glm        (*p_wq_init_glm)
#define wq_set_glm_data    (*p_wq_set_glm_data)
#define wq_do_glm          (*p_wq_do_glm)
#define wq_clean_glm       (*p_wq_clean_glm)
#define wq_init_glm_output (*p_wq_init_glm_output)
#define wq_write_glm_      (*p_wq_write_glm)
#define wq_var_index_c     (*p_wq_var_index_c)
#define wq_set_flags       (*p_wq_set_flags)

int prime_glm_wq(const char *which);

#if USE_DL_LOADER
void wq_init_glm(char *fname, int *len, int *kk, int *NumFABMVars, AED_REAL *pKw);
void wq_set_glm_data(LakeDataType *Lake, int *MaxLayers,
                MetDataType *MetData, SurfaceDataType *SurfData, AED_REAL *dt);
void wq_do_glm(int *wlev, int *pIce);
void wq_clean_glm(void);
void wq_init_glm_output(int *ncid, int *x_dim, int *y_dim, int *z_dim, int *time_dim);
void wq_write_glm_(int *ncid, int *wlev, int *nlev, int *lvl, int *point_nlevs);
int  wq_var_index_c(const char*name, int *len);
void wq_set_flags(int *split_factor, CLOGICAL *mobility, CLOGICAL *bioshade, CLOGICAL *repair_state,
                                                       int *ode_method, CLOGICAL *multi_ben, CLOGICAL *do_plots);
#else
void fabm_init_glm(char *fname, int *len, int *kk, int *NumFABMVars, AED_REAL *pKw);
void fabm_set_glm_data(LakeDataType *Lake, int *MaxLayers,
                MetDataType *MetData, SurfaceDataType *SurfData, AED_REAL *dt);
void fabm_do_glm(int *wlev, int *pIce);
void fabm_clean_glm(void);
void fabm_init_glm_output(int *ncid, int *x_dim, int *y_dim, int *z_dim, int *time_dim);
void fabm_write_glm(int *ncid, int *wlev, int *nlev, int *lvl, int *point_nlevs);
int  fabm_var_index_c(const char*name, int *len);
void fabm_set_flags(int *split_factor, CLOGICAL *mobility, CLOGICAL *bioshade, CLOGICAL *repair_state,
                                                       int *ode_method, CLOGICAL *multi_ben, CLOGICAL *do_plots);

void aed2_init_glm(char *fname, int *len, int *kk, int *NumFABMVars, AED_REAL *pKw);
void aed2_set_glm_data(LakeDataType *Lake, int *MaxLayers,
                MetDataType *MetData, SurfaceDataType *SurfData, AED_REAL *dt);
void aed2_do_glm(int *wlev, int *pIce);
void aed2_clean_glm(void);
void aed2_init_glm_output(int *ncid, int *x_dim, int *y_dim, int *z_dim, int *time_dim);
void aed2_write_glm(int *ncid, int *wlev, int *nlev, int *lvl, int *point_nlevs);
int  aed2_var_index_c(const char*name, int *len);
void aed2_set_flags(int *split_factor, CLOGICAL *mobility, CLOGICAL *bioshade, CLOGICAL *repair_state,
                                                       int *ode_method, CLOGICAL *multi_ben, CLOGICAL *do_plots);
#endif

extern int ode_method;
extern int split_factor;
extern CLOGICAL bioshade_feedback;
extern CLOGICAL repair_state;
extern CLOGICAL multi_ben;
extern CLOGICAL do_plots;

#endif

#endif
