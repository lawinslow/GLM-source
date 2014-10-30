/******************************************************************************
 *                                                                            *
 * xbasic.c                                                                   *
 *                                                                            *
 *   A simple X Windows example.                                              *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <xbasic.h>

/******************************************************************************/

#ifndef CLICKY_BITS
#define CLICKY_BITS 0
#endif

/******************************************************************************/
#define FONT_R  "9x15"
#define FONT_B  "7x13bold"
#define FONT_F  "fixed"

#define CTL_ITEM    1
#define PIC_ITEM    2
#define TXT_ITEM    3
#define EDT_ITEM    4
#define WIN_MENU    5
#define MNU_ITEM    6

/******************************************************************************/
#define pushButton        0
#define checkBox          1
#define radioButton       2
#define scrollBar        16

#define inButton         10
#define inCheckBox       11
#define inUpButton       20
#define inDownButton     21
#define inPageUp         22
#define inPageDown       23
#define inThumb         129

/******************************************************************************/
typedef struct _win_item {
    struct _win_item *next;
    int               id;
    int               type;
    int               left;
    int               top;
    int               right;
    int               bottom;
    void             *data;
} WindowItem;

/******************************************************************************/
typedef struct _win_rec {
    struct _win_rec *next;
    WindowItem      *itm_lst;
    Window           win;
    GC               gc;
    int              curEdit;
    int              mbarItm;
} WindowRecord, *WindowPtr;

/******************************************************************************/
typedef int (*ProcPtr)(void*ctl);

typedef struct _ctl_item {
    Window            owner;
    int               left;
    int               top;
    int               width;
    int               height;
    char              visible;
    unsigned char     hilite;
    long int          value;
    long int          min;
    long int          max;
    int               variant;
    ProcPtr           action;
    char             *title;
} Control;

/******************************************************************************/
typedef struct _pic_item {
    XImage           *image;
    unsigned char    *img;
    int               true_colour;
    int               left;
    int               top;
    int               width;
    int               height;
    int               offset_left;
    int               offset_top;
} PictureItem;

/******************************************************************************/
#if INCLUDE_MENUS

#define MENU_ITEM_HEIGHT 20
#define MENU_BAR_HEIGHT 20

typedef struct _menu_info {
    int    menuID;
    int    nItems;
    int    h, tsize;
    int    last;
    int    width, height;
    char **items;
    char  *flags;
} Menu;

typedef struct _bar_item {
    int   nMenus;
    int   last;
    Menu *menus;
} MenuBar;

#endif

/******************************************************************************/
static Window _new_window(int left, int top,
                                          int width, int height, int transient);
static int _check_event(void);

static WindowPtr _find_window(Window win);

static void _dialog_key(Window win, char key);

static Control* _new_control(Window win,
                         int left, int top, int width, int height,
                         const char *title, char visible,
                         long int value, long int min, long int max,
                         int procID, long int refCon);
static int _point_in_ctl(Control * ctl, int h, int v);
static void _hilite_control(Control * ctl, int state);
static void _draw_control(Control * ctl);

static void _draw_picture(PictureItem *pic);

static void _draw_window_items(void);

#if INCLUDE_MENUS
static void _draw_mbar(MenuBar *mbar);
static void _draw_menu(Menu *menu);
#endif
/******************************************************************************/
static void _draw_string(int h, int v, Font font, const char *str);
static void _make_arcs(XArc *arcs, int left, int top, int right, int bottom,
                                                 int ovalWidth, int ovalHeight);

/******************************************************************************/
void InvertRect(int left, int top, int width, int height);
void FrameRoundRect(int left, int top, int width, int height,
                                                 int ovalWidth, int ovalHeight);
void InvertRoundRect(int left, int top, int width, int height,
                                                 int ovalWidth, int ovalHeight);

/******************************************************************************/

#if CLICKY_BITS
static int tracking = False;
#endif

/******************************************************************************/
extern char *progname;
static Display *display = NULL;
static Window  _window = 0L;
static GC      _gc = 0L;

unsigned long int Black, White, Grey;
unsigned long int Red, Green, Blue;

static int screen = -1;
static Visual *visual;
static Font font_r, font_b, font_f;

static unsigned int win_width, win_height;
static Colormap cmap;
static unsigned int display_width, display_height;

static int cur_x, cur_y;

static WindowPtr    _win_lst = NULL;

/******************************************************************************/
#define DEPTH 4
#if _MAC_PPC_
#define R_OFS 1
#define G_OFS 2
#define B_OFS 3
#else
#define R_OFS 2
#define G_OFS 1
#define B_OFS 0
#endif

#define IMG_TOP 6
#define IMG_LFT 6

/******************************************************************************/
static int idhint = 1;
static int _add_item(int type, void *data,
                                       int left, int top, int width, int height)
{
    WindowItem *item = malloc(sizeof(WindowItem));
    WindowItem *ti = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return -1;
    ti = wptr->itm_lst;

    item->next = NULL;
    if ( ti == NULL )
        wptr->itm_lst = item;
    else
        {
        while ( ti->next )
            ti = ti->next;
        ti->next = item;
        }

    item->type = type;
    item->data = data;
    item->left = left;
    item->top = top;
    item->right = left+width;
    item->bottom = top+height;
    item->id = idhint++;
    if ( wptr->mbarItm != -1 ) {
        item->top += MENU_BAR_HEIGHT;
        item->bottom += MENU_BAR_HEIGHT;
        if ( type == CTL_ITEM ) ((Control*)(data))->top += MENU_BAR_HEIGHT;
        if ( type == PIC_ITEM ) ((PictureItem*)(data))->top += MENU_BAR_HEIGHT;
    }

    return item->id;
}

/******************************************************************************/
static WindowItem *_find_item(int itm_id)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->id == itm_id )
            return item;
        item = item->next;
    }
    return NULL;
}

/******************************************************************************/
static WindowItem *_which_item(int x, int y)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( ( x >= item->left && x <= item->right ) &&
             ( y >= item->top  && y <= item->bottom ) )
            return item;
        item = item->next;
    }
    return NULL;
}

/******************************************************************************/
static WindowItem *_find_item_of_type(Window win, int type)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(win);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == type )
            return item;
        item = item->next;
    }

    return NULL;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static void _add_window(Window win, GC gc)
{
    WindowRecord *wrec = malloc(sizeof(WindowRecord));
    WindowRecord *tr = _win_lst;

    wrec->next = NULL;

    if ( tr == NULL )
        _win_lst = wrec;
    else
        {
        while ( tr->next )
            tr = tr->next;
        tr->next = wrec;
        }

    wrec->win = win;
    wrec->gc = gc;
    wrec->itm_lst = NULL;
    wrec->curEdit = -1;
    wrec->mbarItm = -1;
}

/******************************************************************************/
static WindowPtr _find_window(Window win)
{
    WindowPtr wptr = _win_lst;

    while ( wptr != NULL ) {
        if ( wptr->win == win )
            return wptr;
        wptr = wptr->next;
    }
    return NULL;
}

/******************************************************************************/
static void _set_window(Window win)
{
    WindowPtr wptr = _find_window(win);

    if ( wptr != NULL ) {
        _window  = wptr->win;
        _gc      = wptr->gc;
    }
}

/******************************************************************************/
static void _delete_window(Window win)
{
    WindowPtr wptr = _win_lst;
    WindowPtr tptr = NULL;
    WindowItem *item = NULL;

    XDestroyWindow(display, win);

    if ( wptr == NULL ) return; /* cant be in t' list */

    if ( wptr->win == win ) {
        tptr = wptr;
        if ( wptr->next != NULL ) {
            _window  = (wptr->next)->win;
            _gc      = (wptr->next)->gc;
        } else {
            _window  = 0;
            _gc      = 0;
        }
        _win_lst = wptr->next;
    } else {
        while ( wptr->next != NULL ) {
            if ( (wptr->next)->win == win ) {
                tptr = wptr->next;
                wptr->next = (wptr->next)->next;
                _window  = wptr->win;
                _gc      = wptr->gc;
                break;
            }
            wptr = wptr->next;
        }
    }

    if ( tptr == NULL ) return;

    item = tptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == PIC_ITEM ) {
            PictureItem *pic = item->data;
            if ( pic->image != NULL )
                XDestroyImage(pic->image);
            free(item->data);
        }
        else if (item->type == CTL_ITEM ) {
            free(((Control*)(item->data))->title);
            free(item->data);
        }
        else if (item->type == TXT_ITEM ||
                 item->type == EDT_ITEM ) {
            free(item->data);
        }
        item = item->next;
    }
    free(tptr);
}

/******************************************************************************/
#if CLICKY_BITS
static int last_x = -1, last_y = -1;
static int prev_x = -1, prev_y = -1;
static int link_x = -1, link_y = -1;
int n_clicks = 0, n_bends = 0;
static int second_bend = 0, third_bend = 0;
int click_x[1024];
int click_y[1024];
int bends[1024];
static int togline = 0;
#define TL_OFF 0
#define TL_ON  1

/******************************************************************************/
static void _toggle_line(int onoff)
{
    if ( last_x == -1 ) return;

    if (onoff == togline) return; /* already in the state we require */

    togline = onoff;

    XSetFunction(display, _gc, GXxor);
    XSetForeground(display, _gc, Grey);
    XDrawLine(display, _window, _gc, last_x, last_y, prev_x, prev_y);
    if ( link_x >= 0 )
        XDrawLine(display, _window, _gc, prev_x, prev_y, link_x, link_y);
    XSetForeground(display, _gc, Black);
    // XSetFunction(display, _gc, GXor);
}
#endif

/******************************************************************************/
void _draw_picture(PictureItem *pic)
{
#if CLICKY_BITS
    _toggle_line(TL_OFF); /* turn it off */
#endif

    XSetFunction(display, _gc, GXcopy);
    if ( pic->image != NULL )
        XPutImage(display, _window, _gc, pic->image, 0, 0,
                                    pic->left, pic->top, win_width, win_height);

    XDrawRectangle(display, _window, _gc, pic->left-1, pic->top-1,
                                                   pic->width+1, pic->height+1);

#if CLICKY_BITS
    if ( n_clicks > 0 ) {
        int i;

        XSetForeground(display, _gc, (bends[0])?Red:Blue);
        XFillRectangle(display, _window, _gc, click_x[0]-1, click_y[0]-1, 3, 3);
        XSetForeground(display, _gc, Black);

        for ( i = 1 ; i < n_clicks; i++ ) {
            XDrawLine(display, _window, _gc,
                            click_x[i-1], click_y[i-1], click_x[i], click_y[i]);
            XSetForeground(display, _gc, (bends[i])?Red:Blue);
            XFillRectangle(display, _window, _gc,
                                              click_x[i]-1, click_y[i]-1, 3, 3);
            XSetForeground(display, _gc, Black);
        }
    }

    _toggle_line(TL_ON); /* turn it on */
#endif
}

#if CLICKY_BITS
/******************************************************************************/
static void _draw_pictures()
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == PIC_ITEM )
            _draw_picture(item->data);
        item = item->next;
    }
}
#endif

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static Window _new_window(int left, int top,
                          int width, int height, int transient)
{
    XClassHint xch;
    XSizeHints xsh;
    XWMHints   xwmh;
    XSetWindowAttributes xswa;
    int mask;
    XGCValues gcv;
    Window win;
    GC gc = 0L;

    xswa.background_pixel = White;
    xswa.border_pixel = Black;
    xswa.backing_store = WhenMapped;
    mask = CWBackPixel | CWBorderPixel | CWBackingStore;

    win = XCreateWindow(display,
            (_window == 0L) ? RootWindow(display, screen) : _window,
                              left, top, width, height,
            (transient) ? 1 : 3,
                              CopyFromParent,
                              InputOutput, CopyFromParent, mask, &xswa);

    xsh.flags = PSize | PMinSize | PMaxSize;
    xsh.x = left;
    xsh.y = top;
    xsh.flags |= USPosition;
    xsh.width = xsh.min_width = xsh.max_width = width;
    xsh.height = xsh.min_height = xsh.max_height = height;
    xsh.max_height += MENU_BAR_HEIGHT;
    XSetNormalHints(display, win, &xsh);

    xch.res_name = progname;
    xch.res_class = "lock";
    XSetClassHint(display, win, &xch);

    XStoreName(display, win, progname);
    XSetIconName(display, win, progname);

    xwmh.flags = InputHint | StateHint ;
    xwmh.input = True;
    xwmh.initial_state = NormalState;
    XSetWMHints(display, win, &xwmh);

    if ( transient )
        XSetTransientForHint(display, win, win);

    gcv.foreground = Black;
    gcv.background = White;
    gcv.font = font_r;
    gc = XCreateGC(display, win, GCForeground|GCBackground|GCFont, &gcv);

    XSelectInput(display, win,
        KeyPressMask | KeyReleaseMask | ExposureMask | PointerMotionMask |
        ButtonPressMask | ButtonReleaseMask | StructureNotifyMask);

    XMapWindow(display, win);

    _add_window(win, gc);
    _set_window(win);

    return win;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void _show_position(int h, int v)
{
    char         buff[64];

    XClearArea(display, _window, h-10, v-15, 100, 20, False);
    XDrawRectangle(display, _window, _gc, h-10, v-15, 100, 20);
    sprintf(buff, "%4d,%-4d", cur_x, cur_y);
    _draw_string(h, v, font_r, buff);
}

#if CLICKY_BITS
static int esc_count = 0;
#endif

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static int _process_event(XEvent *ev)
{
    char         buf;
    KeySym       ks;
    XComposeStatus status;
    Window       twin;

    twin = _window;
    // remember to restore before leaving - at the moment the only "return"
    // is at the end of this routine.
    _set_window(ev->xany.window);
//  if ( _window != ev->xany.window )
//      return 0;

    switch (ev->type)
        {
        case KeyPress:
            break;
        case KeyRelease:
            XLookupString((XKeyEvent *)ev, &buf, 1, &ks, &status);
            // fprintf(stderr, "Key%s = \'%c\'\n",
            //                     (ev->type==KeyPress)?"Press":"Release", buf);
            cur_x = ev->xkey.x; cur_y = ev->xkey.y;
            _dialog_key(_window, buf);
#if CLICKY_BITS
            if ( buf == 0x1B ) {
                esc_count++;
                if ( esc_count == 3 ) {
                    esc_count = 0;
                    n_clicks = 0;
                    n_bends = 0;
                    _toggle_line(TL_OFF);
                    last_x = -1;
                    _draw_pictures();
                }
            }
            else if ( buf == 0x08 && n_clicks > 0 ) {
                n_clicks--;
                if ( bends[n_clicks] )
                    n_bends--;
                _toggle_line(TL_OFF); /* off */
                if ( n_clicks > 0 ) {
                    last_x = click_x[n_clicks-1];
                    last_y = click_y[n_clicks-1];
                }
                else
                    last_x = -1;
                _toggle_line(TL_ON); /* on */
                _draw_pictures();
            }
#endif
            break;

        case Expose:
            XClearArea(display, _window, 0, 0, win_width, win_height, False);
            _draw_window_items();
            break;

        case MotionNotify:
            while (XCheckTypedEvent(display, MotionNotify, ev))
                ;

            cur_x = ev->xmotion.x; cur_y = ev->xmotion.y;
//          _show_position(win_width-120, 30);

#if CLICKY_BITS
            if ( !tracking && last_x != -1 ) {
                _toggle_line(TL_OFF);
                prev_x = ev->xmotion.x; prev_y = ev->xmotion.y;
                _toggle_line(TL_ON);
            }
#endif
            XFlush(display);
            break;

        case MapNotify:
        case UnmapNotify:
            break;

        case ButtonPress:
            cur_x = ev->xbutton.x; cur_y = ev->xbutton.y;
            break;
        case ButtonRelease:
            cur_x = ev->xbutton.x; cur_y = ev->xbutton.y;
#if CLICKY_BITS
            if ( tracking )
                /* stop tracking */
                break;
            else if ( ev->xbutton.button != 3 ) {
                if ( last_x >= 0 ) {
                    _toggle_line(TL_OFF); /* turn it off */

                    prev_x = ev->xbutton.x; prev_y = ev->xbutton.y;

                    /* draw click line */
                    XSetFunction(display, _gc, GXcopy);
                    XDrawLine(display, _window, _gc,
                                                last_x, last_y, prev_x, prev_y);
                }
                last_x = ev->xbutton.x; last_y = ev->xbutton.y;
                prev_x = ev->xbutton.x; prev_y = ev->xbutton.y;

                click_x[n_clicks] = ev->xbutton.x;
                click_y[n_clicks] = ev->xbutton.y;
                if ( (bends[n_clicks] = (ev->xbutton.state & ShiftMask)) ) {
                    n_bends++;
                    if ( n_bends == 2 ) second_bend = n_clicks;
                    if ( n_bends == 3 ) third_bend  = n_clicks;
                }

                if ( n_bends == 3 ) {
                    link_x = click_x[second_bend - (n_clicks-third_bend) - 1];
                    link_y = click_y[second_bend - (n_clicks-third_bend) - 1];
                }

                n_clicks++;

                XSetForeground(display, _gc,
                                      (ev->xbutton.state & ShiftMask)?Red:Blue);
                XFillRectangle(display, _window, _gc, last_x-1, last_y-1, 3, 3);
                XSetForeground(display, _gc, Black);

                _toggle_line(TL_ON); /* turn it on */
            }
#endif
            break;
/*
        default:
            printf("Unprocessed event %d\n", ev->type);
            break;
*/
        }
    _set_window(twin);
    return ev->type;
}

/******************************************************************************/
static Control *_trking_ctl = NULL;
static int _trking_in = 0;
static int _trking_itmid = 0;

/******************************************************************************/
static void _start_track_control(Control * ctl, int h, int v, ProcPtr action)
{
    _trking_ctl = ctl;
    _trking_in = True;
    _hilite_control(ctl, 254);
}

/******************************************************************************/
static int _check_track_control()
{
    XEvent ev;

    ev.type = 0;
    while ( XCheckMaskEvent(display, -1L, &ev) == True ) {
        if ( _process_event(&ev) != ButtonRelease ) {
            if ( _point_in_ctl(_trking_ctl, cur_x, cur_y) ) {
                if ( !_trking_in ) {
                    _hilite_control(_trking_ctl, 254);
                    _trking_in = True;
                }
            } else {
                if ( _trking_in ) {
                    _hilite_control(_trking_ctl, 0);
                    _trking_in = False;
                }
            }
            return 0;
        }
        else
            return 1;
    }
    return -1;
}

/******************************************************************************/
static int _finish_track_control()
{
    if ( _trking_in )
        _hilite_control(_trking_ctl, 0);
    _trking_ctl = NULL;

    return _trking_in;
}

/******************************************************************************
 *                                                                            *
 *                                                                            *
 ******************************************************************************/


/******************************************************************************/
Control * _new_control(Window win,
                int left, int top, int width, int height,
                const char *title, char visible,
                long int value, long int min, long int max,
                int procID, long int refCon)
{
    Control * ctl = malloc(sizeof(Control));

    ctl->owner = win;
    ctl->left = left;
    ctl->top = top;
    ctl->width = width;
    ctl->height = height;
    ctl->visible = visible;
    ctl->hilite = 0;
    ctl->value = value;
    ctl->min = min;
    ctl->max = max;
    ctl->variant = procID;
    ctl->action = NULL;
    ctl->title = strdup(title);

    return ctl;
}

/******************************************************************************/
static void _set_ctl_state(int itm_id, unsigned char state)
{
    Control *ctl = NULL;
    WindowItem *item = _find_item(itm_id);

    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( (ctl = item->data) == NULL ) return;

    ctl->hilite = state;
    _draw_control(ctl);
}

/******************************************************************************/
int _point_in_ctl(Control * ctl, int h, int v)
{
    /* cant be in disabled controls */
    if ( ctl->hilite == 255 ) return False;

    if ( h < ctl->left || v < ctl->top ||
         h > ctl->left+ctl->width ||
         v > ctl->top+ctl->height )
        return False;
    return True;
}

/******************************************************************************/
void _draw_string(int h, int v, Font font, const char *str)
{
    XTextItem ti;

    ti.font = font;
    ti.chars = (char*)str;
    ti.nchars = strlen(str);
    ti.delta = 0;
    XDrawText(display, _window, _gc, h, v, &ti, 1);
}

/******************************************************************************/
void _hilite_control(Control *ctl, int state)
{
    ctl->hilite = state;
    _draw_control(ctl);
}

#define A_UP 0
#define A_DN 1
#define A_LF 2
#define A_RT 3

/******************************************************************************/
void _arrow_box(int left, int top, int right, int bottom, int dir, int hilite)
{
    int hcentre, vcentre;
    XPoint points[8];

    hcentre = (left + right)/2;
    vcentre = (top + bottom)/2;

    XDrawRectangle(display, _window, _gc, left, top, right-left, bottom-top);

    switch ( dir ) {
        case A_UP:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = right;   points[2].y = vcentre;
            points[3].x = right-5; points[3].y = vcentre;
            points[4].x = right-5; points[4].y = bottom;
            points[5].x = left+5;  points[5].y = bottom;
            points[6].x = left+5;  points[6].y = vcentre;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_DN:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = bottom;
            points[2].x = right;   points[2].y = vcentre;
            points[3].x = right-5; points[3].y = vcentre;
            points[4].x = right-5; points[4].y = top;
            points[5].x = left+5;  points[5].y = top;
            points[6].x = left+5;  points[6].y = vcentre;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_LF:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = hcentre; points[2].y = top+5;
            points[3].x = right;   points[3].y = top+5;
            points[4].x = right;   points[4].y = bottom-5;
            points[5].x = hcentre; points[5].y = bottom-5;
            points[6].x = hcentre; points[6].y = bottom;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_RT:
            points[0].x = right;   points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = hcentre; points[2].y = top+5;
            points[3].x = left;    points[3].y = top+5;
            points[4].x = left;    points[4].y = bottom-5;
            points[5].x = hcentre; points[5].y = bottom-5;
            points[6].x = hcentre; points[6].y = bottom;
            points[7].x = right;   points[7].y = vcentre;
            break;
        default:
            return;
    }

    XDrawLines(display, _window, _gc, points, 8, CoordModeOrigin);
    if ( hilite )
        XFillPolygon(display, _window, _gc, points, 8, Convex, CoordModeOrigin);
}

/******************************************************************************/
void _draw_control(Control * ctl)
{
    int str_length, title_length;
    int left, top, width, height, right, bottom;

    if ( ctl == NULL || ctl->visible == 0 )
        return;

    left = ctl->left; width = ctl->width;
    top = ctl->top; height = ctl->height;
    right = left + width;
    bottom = top + height;

    XClearArea(display, _window, left, top, width, height, False);

    switch (ctl->variant) {
        case pushButton :
            FrameRoundRect(left, top, width, height, 16, 16);

            str_length = strlen(ctl->title);
            title_length = XTextWidth(XQueryFont(display, font_b),
                                                        ctl->title, str_length);
            // this is how to grey out a control
            if ( ctl->hilite == 255 ) XSetForeground(display, _gc, Grey);
            _draw_string(left+((width-title_length)/2), bottom-5,
                                                            font_b, ctl->title);
            XSetForeground(display, _gc, Black);

            if (ctl->hilite && ctl->hilite != 255 )
                InvertRoundRect(left, top, width, height, 16, 16);
            break;
        case checkBox :
        case radioButton :
            if (ctl->variant == checkBox) {
                XDrawRectangle(display, _window, _gc, left, top, 15, 15);
                if (ctl->value) {
                   XDrawLine(display, _window, _gc, left, top, left+15, top+15);
                   XDrawLine(display, _window, _gc, left, top+15, left+15, top);
                }
            } else {
                XDrawArc(display, _window, _gc, left, top, width, height,
                                                                     0, 360*64);

                if (ctl->value) {
                    XSetFunction(display, _gc, GXnor);
                    XDrawArc(display, _window, _gc, left+3, top+3,
                                                  width-6, height-6, 0, 360*64);
                    XSetFunction(display, _gc, GXcopy);
                    }
                }
            if ( ctl->hilite == 255 ) XSetForeground(display, _gc, Grey);
            _draw_string(left+20, top+12, font_r, ctl->title);
            XSetForeground(display, _gc, Black);

            break;
        case scrollBar :
            XDrawRectangle(display, _window, _gc, left, top, width, height);
            if ( height > width ) {
                _arrow_box(left, top, right, top+width, A_UP, ctl->hilite);
                _arrow_box(left, bottom-width, right,bottom, A_DN, ctl->hilite);
            } else {
                _arrow_box(left, top, left+height, bottom, A_LF, ctl->hilite);
                _arrow_box(right-height, top, right, bottom, A_RT, ctl->hilite);
            }
            break;
    }
}

/******************************************************************************/
static void _draw_window_items()
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        switch ( item->type ) {
            case CTL_ITEM: _draw_control(item->data); break;
            case PIC_ITEM: _draw_picture(item->data); break;
            case TXT_ITEM:
            case EDT_ITEM:
                _draw_string(item->left+6, item->top+15,
                            (item->type == TXT_ITEM)?font_b:font_r, item->data);
                if ( item->type == EDT_ITEM )
                    XDrawRectangle(display, _window, _gc,
                            item->left, item->top,
                            item->right-item->left+1, item->bottom-item->top+1);
                break;
#if INCLUDE_MENUS
            case WIN_MENU: _draw_mbar(item->data); break;
            case MNU_ITEM: _draw_menu(item->data); break;
#endif
            default:
                break;
        }
        item = item->next;
    }
}

/******************************************************************************/
static void _make_arcs(XArc *arcs, int left, int top, int right, int bottom,
                                                  int ovalWidth, int ovalHeight)
{
    int i;
    for (i=0; i<4; i++) {
        arcs[i].width = ovalWidth;
        arcs[i].height = ovalHeight;
        arcs[i].angle1 = (90*64)*i;
        arcs[i].angle2 = 90*64;
    }

    arcs[0].x = right-ovalWidth; arcs[0].y = top,
    arcs[1].x = left;            arcs[1].y = top,
    arcs[2].x = left;            arcs[2].y = bottom-ovalHeight;
    arcs[3].x = right-ovalWidth; arcs[3].y = bottom-ovalHeight;
}

/******************************************************************************/
void InvertRect(int left, int top, int width, int height)
{
    XSetFunction(display, _gc, GXnor);
    XFillRectangle(display, _window, _gc, left, top, width, height);
}

/******************************************************************************/
void FrameRoundRect(int left, int top, int width, int height,
                                                  int ovalWidth, int ovalHeight)
{
    int right = left + width,
        bottom = top + height,
        harc = ovalHeight / 2,
        warc = ovalWidth / 2;
    XArc        arcs[4];
    XSegment    segs[4];

    _make_arcs(arcs, left, top, right, bottom, ovalWidth, ovalHeight);

    segs[0].x1 = right;          segs[0].y1 = top+harc;
    segs[0].x2 = right;          segs[0].y2 = bottom-harc;
    segs[1].x1 = right-warc;     segs[1].y1 = bottom;
    segs[1].x2 = left+warc;      segs[1].y2 = bottom;
    segs[2].x1 = left;           segs[2].y1 = top+harc;
    segs[2].x2 = left;           segs[2].y2 = bottom-harc;
    segs[3].x1 = left+warc;      segs[3].y1 = top;
    segs[3].x2 = right-warc;     segs[3].y2 = top;

    XSetFunction(display, _gc, GXcopy);
    XDrawArcs(display, _window, _gc, arcs, 4);
    XDrawSegments(display, _window, _gc, segs, 4);
}

/******************************************************************************/
void InvertRoundRect(int left, int top, int width, int height,
                                                  int ovalWidth, int ovalHeight)
{
    int right = left + width,
        bottom = top + height,
        harc = ovalHeight / 2,
        warc = ovalWidth / 2;
    XArc        arcs[4];
    XRectangle  rcts[4];

    _make_arcs(arcs, left, top, right, bottom, ovalWidth, ovalHeight);

    rcts[0].x      = left+warc;
    rcts[0].y      = top;
    rcts[0].width  = right-left-ovalWidth;
    rcts[0].height = bottom-top;

    rcts[1].x      = left;
    rcts[1].y      = top+harc;
    rcts[1].width  = warc;
    rcts[1].height = bottom-top-ovalHeight;

    rcts[2].x      = right-warc;
    rcts[2].y      = top+harc;
    rcts[2].width  = warc;
    rcts[2].height = bottom-top-ovalHeight;

    XSetFunction(display, _gc, GXnor);
    XFillArcs(display, _window, _gc, arcs, 4);
    XFillRectangles(display, _window, _gc, rcts, 3);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int CheckUI()
{
    if ( _window == 0L ) return -1;
    return _check_event();
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int DoUI()
{
    int ret;
    if ( _window == 0L ) return -1;
    while ( (ret = _check_event()) == 0 )
        ;
    return ret;
}

/******************************************************************************/
void GetMouse(int *x, int *y)
{
    *x = cur_x; *y = cur_y;
}

/******************************************************************************/
int NewTextItem(int left, int top, int width, int height, const char*text)
{
    return _add_item(TXT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static long int _next_tickle = 0;
static int      _carat_on    = 0;

/******************************************************************************/
static void _tickle_carat(Window win, int force)
{
    char        *name_buf;
    unsigned int len;
    long int     now = time(NULL);
    WindowItem  *item = NULL;
    int          left = 0, top = 0, right = 0, bottom = 0;

    item = _find_item_of_type(win, EDT_ITEM);
    if ( item == NULL ) return;

    if ( !force && now < _next_tickle ) return;

    top = item->top + 2;
    left = item->left + 6;
    bottom = item->bottom - 2;

    name_buf = item->data;
    len = strlen(name_buf);

    left += XTextWidth(XQueryFont(display, font_r), name_buf, len);
    right = left + 1;
    if ( _carat_on )
        XClearArea(display, win, left, top, right-left, bottom-top, False);
    else {
        /* basically, invert the rectangle */
        XSetFunction(display, _gc, GXnor);
        XFillRectangle(display, win, _gc, left, top, right-left, bottom-top);
    }

    _carat_on = !_carat_on;
    _next_tickle = now + 1;
}

/******************************************************************************/
static void _dialog_key(Window win, char key)
{
    char        *name_buf;
    char         ch;
    unsigned int len;
    WindowItem  *item = NULL;
    int          left = 0, top = 0, right = 0, bottom = 0;

    item = _find_item_of_type(win, EDT_ITEM);
    if ( item == NULL ) return;

    if (_carat_on) _tickle_carat(win, True);

    top = item->top + 1;
    left = item->left + 6;
    bottom = item->bottom - 1;

    name_buf = item->data;
    len = strlen(name_buf);

    left += XTextWidth(XQueryFont(display, font_r), name_buf, len);

    if (key == 0x08 || key == 0x7F) { /* backspace or delete */
        if ( len ) {
            ch = name_buf[len-1];
            name_buf[len-1] = 0;
            right = left;
            left -= XTextWidth(XQueryFont(display, font_r), &ch, 1);
            XClearArea(display, win, left, top, right-left, bottom-top, False);
        }
    } else {
        if ( (item->data = realloc(name_buf, len+10)) == NULL )
            item->data = name_buf;
        else
            name_buf = item->data;
        name_buf[len] = key; name_buf[len+1] = 0;
        _draw_string(left, bottom-5, font_r, &name_buf[len]);
    }
}

/******************************************************************************/
int NewEditTextItem(int left, int top, int width, int height, const char*text)
{
    return _add_item(EDT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************/
static void _copy_img(gdImagePtr im, PictureItem *pic)
{
    int x, y;

    if ( pic->true_colour ) {
        int *tt = (int*)pic->img;

        for (y = 0; y < gdImageSY(im); y++) {
            for (x = 0; x < gdImageSX(im); x++ ) {
                *tt++ = gdImageTrueColorPixel(im, x, y);
            }
        }
    }
    else {
        unsigned char *tt = pic->img;

        for (y = 0; y < gdImageSY(im); y++) {
            for (x = 0; x < gdImageSX(im); x++ ) {
                int c = gdImagePalettePixel(im, x, y);
                tt[R_OFS] = gdImageRed(im, c);
                tt[G_OFS] = gdImageGreen(im, c);
                tt[B_OFS] = gdImageBlue(im, c);
                tt+=DEPTH;
            }
        }
    }
}

/******************************************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height)
{
    PictureItem *pic = malloc(sizeof(PictureItem));

    pic->img = malloc(gdImageSX(im) * gdImageSY(im) * DEPTH);
    pic->true_colour = true_colour;
    pic->left = left; pic->top = top;
    pic->width = width; pic->height = height;

    _copy_img(im, pic);

    pic->image = XCreateImage(display, visual, 24, ZPixmap, 0,
                                    (char*)pic->img, width, height, 8, width*4);

    _draw_picture(pic);
    return _add_item(PIC_ITEM, pic, left, top, width, height);
}

/******************************************************************************/
void FlushPicture(gdImagePtr im, int itm_id)
{
    WindowItem *itm = _find_item(itm_id);
    PictureItem *pic;

    if ( itm == NULL ) return;
    if ( itm->type != PIC_ITEM ) return;
    pic = itm->data;
    if ( pic == NULL ) return;

    _copy_img(im, pic);

    _draw_picture(pic);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
#if 0
int _still_down()
{
    XEvent ev;
    XNextEvent(display, &ev);
    return (_process_event(&ev) != ButtonRelease);
}
#endif

/******************************************************************************/
#if INCLUDE_SAVED

char *DoSaveDialog(char *fname)
{
    Window dwin;

    dwin = _new_window(100, 100, 300, 200, True);
    NewControl(pushButton, "OK", 230, 170, 60, 20);
    _add_item(TXT_ITEM, strdup("Save as :"), 20, 20, 80, 20);
    _add_item(EDT_ITEM, strdup(fname), 20, 60, 200, 20);
    //_add_item(EDT_ITEM, strdup(fname), 20, 90, 200, 20);

    // while ( _still_down() ) {
    while ( !_check_event() ) {
        _tickle_carat(dwin, False);
        ;
    }

    _delete_window(dwin);

    return NULL;
}
#endif

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void DisableControl(int itm_id)
{
    _set_ctl_state(itm_id, 255);
}

/******************************************************************************/
void EnableControl(int itm_id)
{
    _set_ctl_state(itm_id, 0);
}

/******************************************************************************/
void RenameControl(int itm_id, const char*title)
{
    Control *ctl = NULL;
    WindowItem *item = _find_item(itm_id);

    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( (ctl = item->data) == NULL ) return;

    if ( ctl->title != NULL ) free(ctl->title);

    ctl->title = strdup(title);
    _draw_control(ctl);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height)
{
    Control * ctl = _new_control(_window,
                left, top, width, height,
                title, True,
                0, 0, 0,
                type, 0);

    _draw_control(ctl);
    return _add_item(CTL_ITEM, ctl, left, top, width, height);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
#if INCLUDE_MENUS
static MenuBar *_new_menu_bar()
{
    MenuBar *mbar = malloc(sizeof(MenuBar));
    mbar->nMenus = 0;
    mbar->menus = NULL;

    return mbar;
}

/******************************************************************************/
static void _draw_mbar(MenuBar *mbar)
{
    int i, h;

    XClearArea(display, _window, 0, 0, win_width, MENU_BAR_HEIGHT, False);
    XDrawLine(display, _window, _gc, 0,
                                   MENU_BAR_HEIGHT, win_width, MENU_BAR_HEIGHT);

    h = 0;
    for (i = 0; i < mbar->nMenus; i++) {
        char *str = mbar->menus[i].items[0];
        mbar->menus[i].h = h;
        _draw_string(h+6, 16, font_b, str);
        mbar->menus[i].tsize =
               (10 + XTextWidth(XQueryFont(display, font_b), str, strlen(str)));
        h += mbar->menus[i].tsize;
    }
}

/******************************************************************************/
static void _hilite_menu(Menu *menu, int state)
{
    int h, l;

    h = menu->h;
    l = menu->tsize;
    XClearArea(display, _window, h+1, 0, l-2, MENU_BAR_HEIGHT, False);
    _draw_string(h+6, 16, font_b, menu->items[0]);
    if ( state != 0 ) InvertRect(h+1, 1, l-2, MENU_BAR_HEIGHT-1);
}

/******************************************************************************/
static void _draw_menu(Menu *menu)
{
    int i, v;

    v = 16;
    /* start the count from 1 because 0 is the title */
    for (i = 1; i < menu->nItems; i++) {
        char *str = menu->items[i];
        if ( menu->flags[i] == 0 ) XSetForeground(display, _gc, Grey);
        if ( strcmp(str, "-") == 0 )
            XDrawLine(display, _window, _gc, 3, v-6, menu->width-6, v-6);
        else
            _draw_string(6, v, font_b, str);
        XSetForeground(display, _gc, Black);
        v += MENU_ITEM_HEIGHT;
    }
}

/******************************************************************************/
//static MenuBar *_trking_mbar = NULL;
static Menu *_trking_menu = NULL;
static int _trk_menu_in = 0;
static Window _mwin = 0;

/******************************************************************************/
static void _start_track_menu(MenuBar *mbar, int x, int y, ProcPtr action)
{
    Menu* menu = NULL;
    int i, h, l;

    for (i = 0; i < mbar->nMenus; i++) {
        h = mbar->menus[i].h;
        l = mbar->menus[i].tsize;
        if ( x >= h && x <= h+l ) {
            menu = &(mbar->menus)[i];
            break;
        }
    }

    if ( menu != NULL ) {
        Window twin = _window;
        _trking_menu = menu;
        _trk_menu_in = True;
        _hilite_menu(menu, 254);
        _mwin = _new_window(menu->h, MENU_BAR_HEIGHT,
                                               menu->width, menu->height, True);
        _add_item(MNU_ITEM, menu, 0, 0, menu->width, menu->height);
        _draw_menu(menu);
        _set_window(twin);
    }
}

/******************************************************************************/
static int _point_in_menu(Menu *menu, int x, int y)
{
    int h, l;
    if ( y > MENU_BAR_HEIGHT ) return 0;
    h = menu->h;
    l = menu->tsize;
    if ( x >= h && x <= h+l )
        return 1;
    return 0;
}

/******************************************************************************/
static int _check_track_menu()
{
    XEvent ev;

    ev.type = 0;
    while ( XCheckMaskEvent(display, -1L, &ev) == True ) {
        if ( _process_event(&ev) != ButtonRelease ) {
            if ( _point_in_menu(_trking_menu, cur_x, cur_y) ) {
                if ( !_trk_menu_in ) {
                    _hilite_menu(_trking_menu, 254);
                    _trk_menu_in = True;
                }
            } else {
                if ( _trk_menu_in ) {
                    _hilite_menu(_trking_menu, 0);
                    _trk_menu_in = False;
                }
            }
            return 0;
        } else
            return 1;
    }
    return -1;
}

/******************************************************************************/
static int _finish_track_menu()
{
    if ( _trk_menu_in )
        _hilite_menu(_trking_menu, 0);
    _delete_window(_mwin);
    _trking_menu = NULL;
    _mwin = 0;

    return _trk_menu_in;
}

/******************************************************************************/
int NewMenu(const char*title)
{
    Menu * menu = NULL;
    MenuBar *mbar = NULL;
    WindowItem *item = NULL;

    item = _find_item_of_type(_window, WIN_MENU);

    if ( item == NULL || item->data == NULL ) {
        WindowPtr  wptr = _find_window(_window);
        if ( wptr == NULL ) return -1;

        mbar = _new_menu_bar();

        item = wptr->itm_lst;
        while ( item != NULL ) {
            item->top += MENU_BAR_HEIGHT;
            item->bottom += MENU_BAR_HEIGHT;
            if ( item->type == CTL_ITEM )
                ((Control*)(item->data))->top += MENU_BAR_HEIGHT;
            if ( item->type == PIC_ITEM )
                ((PictureItem*)(item->data))->top += MENU_BAR_HEIGHT;
            item = item->next;
        }
        wptr->mbarItm =
                    _add_item(WIN_MENU, mbar, 0, 0, win_width, MENU_BAR_HEIGHT);
        win_height += MENU_BAR_HEIGHT;
        XResizeWindow(display, _window, win_width, win_height);

        _draw_window_items();
    }
    else
        mbar = item->data;

    mbar->menus = realloc(mbar->menus, sizeof(Menu)*(mbar->nMenus+1));

    menu = &(mbar->menus)[mbar->nMenus];
    menu->menuID = mbar->nMenus++;
    menu->items = malloc(0); menu->flags = malloc(0);
    menu->width = 0;
    menu->nItems = 0;
    while (*title) {
        char *s, *t;
        int l, sl;

        t = (char*)&title[1];
        s = strchr(title, ';');
        if ( s != NULL ) l = s - t;
        else l = strlen(t);

        sl = XTextWidth(XQueryFont(display, font_b), t, l) + 20;
        if ( sl > menu->width )
            menu->width = sl;

        menu->items = realloc(menu->items, sizeof(char*)*(menu->nItems+1));
        menu->items[menu->nItems] = malloc(l + 1);
        strncpy(menu->items[menu->nItems], t, l+1);
        (menu->items[menu->nItems])[l] = 0;
        menu->flags = realloc(menu->flags, sizeof(char)*(menu->nItems+1));

        menu->flags[menu->nItems] = (title[0] == '-') ? 0x00 : 0x0F;

        menu->nItems++;
        if ( s == NULL ) title += l + 1;
        else             title += l + 2;
    }
    menu->height = MENU_ITEM_HEIGHT * (menu->nItems - 1);

    _draw_mbar(mbar);
//  if ( item == NULL )
//      return _add_item(WIN_MENU, mbar, left, top, width, height);
    return menu->menuID;
}
#endif

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static int _check_event()
{
    XEvent ev;
    int ret = 0;

    if ( _trking_ctl != NULL ) {
        if ( _check_track_control() == 1 && _finish_track_control() )
            return _trking_itmid;
        return 0;
    }

#if INCLUDE_MENUS
    if ( _trking_menu != NULL ) {
        if ( _check_track_menu() == 1 && _finish_track_menu() )
            return _trking_itmid;
        return 0;
    }
#endif

    ev.type = 0;
    while ( XCheckMaskEvent(display, -1L, &ev) == True ) {
        ret = _process_event(&ev);
        if ( ret == ButtonPress ) {
            WindowItem *itm = _which_item(cur_x, cur_y);
            if ( itm != NULL ) {
                _trking_itmid = itm->id;
                if ( itm->type == CTL_ITEM )
                    _start_track_control(itm->data, cur_x, cur_y, NULL);
#if INCLUDE_MENUS
                else if ( itm->type == WIN_MENU )
                    _start_track_menu(itm->data, cur_x, cur_y, NULL);
#endif
            }
        }
#if CLICKY_BITS
        else if (ret == ButtonRelease && ev.xbutton.button == 3) {
            tracking = False;
            return -1;
        }
#endif
    }
    return 0;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/

/******************************************************************************/
unsigned long int MakeColour(int red, int green, int blue)
{
    XColor color;

    color.red = red<<8;
    color.green = green<<8;
    color.blue = blue<<8;
    if ( !XAllocColor(display, cmap, &color) )
        fprintf(stderr, "XAllocColor failed?\n");;
    return color.pixel;
}

/******************************************************************************/
int InitX(int *width, int *height)
{
    display = XOpenDisplay(NULL);  /* open the default display */
    if ( display == NULL ) {
        fprintf(stderr, "Cannot open default display\n");
        return -1;
    }
    screen = DefaultScreen(display);
    visual = DefaultVisual(display, screen);
    display_width = DisplayWidth(display, screen);
    display_height = DisplayHeight(display, screen);

    /* get colours, probably better to get named colours, using this only if
     * that failed */
    Black = BlackPixel(display, screen);
    White = WhitePixel(display, screen);

    cmap = DefaultColormap(display, screen);

    Grey = MakeColour(128, 128, 134);
    Red = MakeColour(255, 0, 0);
    Green = MakeColour(0, 255, 0);
    Blue = MakeColour(0, 0, 255);

    /* get fonts */
    if ( (font_f = XLoadFont(display, FONT_F)) == 0 ) {
        fprintf(stderr, "Cannot allocate fixed font\n");
        exit(1);
    }
    if ( (font_r = XLoadFont(display, FONT_R)) == 0 )
        font_r = font_f;
    if ( (font_b = XLoadFont(display, FONT_B)) == 0 )
        font_b = font_f;

    if (  *width+30 > display_width )  *width  = display_width - 30;
    if ( *height+80 > display_height ) *height = display_height - 80;
    win_width = *width; win_height = *height;
    _window = _new_window(10, 10, *width, *height, False);
    return 0;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int CleanupX()
{
    if ( _window == 0L ) return -1;

    _delete_window(_window);

    if ( font_r != font_f ) XUnloadFont(display, font_r);
    if ( font_b != font_f ) XUnloadFont(display, font_b);
    XUnloadFont(display, font_f);
    _window = 0L;
    XCloseDisplay(display);
    return 0;
}
