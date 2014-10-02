/******************************************************************************
 *                                                                            *
 * glm_init.h                                                                 *
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
#ifndef _GLM_INIT_H_
#define _GLM_INIT_H_

#ifdef _FORTRAN_VERSION_

  INTERFACE

     SUBROUTINE init_glm(jstart,outp_dir,dln,outp_fn,fln,nsave) BIND(C, name="init_glm_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(out)   :: jstart
        CCHARACTER,INTENT(out) :: outp_dir(*)
        CINTEGER,INTENT(out)   :: dln
        CCHARACTER,INTENT(out) :: outp_fn(*)
        CINTEGER,INTENT(out)   :: fln
        CINTEGER,INTENT(out)   :: nsave
     END SUBROUTINE init_glm

  END INTERFACE

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#else
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void init_glm(int *jstart, char *outp_dir, char *outp_fn, int *nsave);
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif

#endif
