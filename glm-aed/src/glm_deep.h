/******************************************************************************
 *                                                                            *
 * glm_deep.h                                                                 *
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
#ifndef _GLM_DEEP_MIX_H_
#define _GLM_DEEP_MIX_H_


#if _FORTRAN_VERSION_

INTERFACE

   SUBROUTINE do_diffusion() BIND(C, name="do_diffusion")
   END SUBROUTINE do_diffusion

   SUBROUTINE do_energy() BIND(C, name="do_dissipation_")
        USE ISO_C_BINDING
   END SUBROUTINE do_energy

   SUBROUTINE check_layer_stability() BIND(C, name="check_layer_stability")
   END SUBROUTINE check_layer_stability

END INTERFACE

#else

void do_diffusion(void);
void do_dissipation(void);
void do_dissipation_(void);
void check_layer_stability(void);

#endif


#endif
