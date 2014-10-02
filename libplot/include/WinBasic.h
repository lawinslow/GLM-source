/******************************************************************************
 *                                                                            *
 * WinBasic.h                                                                 *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Earth & Environment                                          *
 *     The University of Western Australia                                    *
 *                                                                            *
 * Copyright 2013, 2014 -  The University of Western Australia                *
 *                                                                            *
 *  This file is part of libplot - a plotting library for GLM                 *
 *                                                                            *
 *  libplot is free software: you can redistribute it and/or modify           *
 *  it under the terms of the GNU General Public License as published by      *
 *  the Free Software Foundation, either version 3 of the License, or         *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  libplot is distributed in the hope that it will be useful,                *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *
 *                                                                            *
 *                     -------------------------------                        *
 *                                                                            *
 *  Derived with permission from                                              *
 *                                                                            *
 * Copyright 2003 - Ambinet System                                            *
 *                                                                            *
 ******************************************************************************/

#ifndef _WIN_BASIC_H_
#define _WIN_BASIC_H_

#include <gd.h>

//#define INCLUDE_MENUS 1
//#define INCLUDE_SAVED 1

#ifndef INCLUDE_MENUS
#define INCLUDE_MENUS 0
#endif
#ifndef INCLUDE_SAVED
#define INCLUDE_SAVED 0
#endif

/******************************************************************************/
int InitX(int *width, int *height);
int CleanupX(void);

    /**********************************************************/
int CheckUI(void);
int DoUI(void);
void GetMouse(int *x, int *y);
#if INCLUDE_SAVED
char *DoSaveDialog(char *fname);
#endif

    /**********************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height);
void RenameControl(int itm_id, const char*title);
void DisableControl(int itm_id);
void EnableControl(int itm_id);

    /**********************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height);
void FlushPicture(gdImagePtr im, int item_id);

    /**********************************************************/
int NewEditTextItem(int left, int top, int width, int height, const char*text);
int NewTextItem(int left, int top, int width, int height, const char*text);

    /**********************************************************/
#if INCLUDE_MENUS
int NewMenu(const char*title);
#endif

#endif
