/******************************************************************************
 *                                                                            *
 * WinBasic.c                                                                 *
 *                                                                            *
 *   A simple MS Windows interface.                                           *
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
#include <windows.h>
#include <windowsx.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <WinBasic.h>

#define PLOT_CLASS  L"Plot Window"

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

typedef HWND Window;

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
    unsigned char    *img;
    BITMAPINFOHEADER  bmpi;
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

static void _add_window(Window win);
static void _set_window(Window win);
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

typedef int Font;
typedef int XArc;

/******************************************************************************/
static void _draw_string(int h, int v, Font font, const char *str);
static void _make_arcs(XArc *arcs, int left, int top, int right, int bottom,
                                                 int ovalWidth, int ovalHeight);

/******************************************************************************/
extern char *progname;
static Window  _window = 0L;
static WindowPtr    _win_lst = NULL;
static HDC hdc;

static Font font_b, font_r;
static int cur_x, cur_y;

static HWND hWnd = NULL;
static HINSTANCE myInstance = NULL;

/******************************************************************************/
LRESULT CALLBACK WndProc(HWND hWnd, UINT iMessage, WPARAM wParam, LPARAM lParam);



/******************************************************************************/
HINSTANCE GetMyInstance(void)
{
    if ( myInstance == NULL ) {
        HWND pWnd = GetShellWindow();
        myInstance = (HINSTANCE)GetWindowLongPtr(pWnd, GWLP_HINSTANCE);
    }
    return myInstance;
}


#define DEPTH 3
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
    else {
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
#if INCLUDE_MENUS
    if ( wptr->mbarItm != -1 ) {
        item->top += MENU_BAR_HEIGHT;
        item->bottom += MENU_BAR_HEIGHT;
        if ( type == CTL_ITEM ) ((Control*)(data))->top += MENU_BAR_HEIGHT;
        if ( type == PIC_ITEM ) ((PictureItem*)(data))->top += MENU_BAR_HEIGHT;
    }
#endif

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
int InitX(int *width, int *height) {
    WNDCLASS WndClass;
    HINSTANCE hInstance = NULL;

    WndClass.cbClsExtra = 0;
    WndClass.cbWndExtra = 0;
    WndClass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH);
    WndClass.hCursor = LoadCursor (NULL, IDC_ARROW);
    WndClass.hIcon = LoadIcon (NULL, IDI_APPLICATION);
    WndClass.hInstance = hInstance;
    WndClass.lpfnWndProc = (WNDPROC) WndProc;
    WndClass.lpszClassName = PLOT_CLASS;
    WndClass.lpszMenuName = NULL;
    WndClass.style = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;

    if (!RegisterClass(&WndClass)){
        MessageBox(NULL, L"Registration of WinClass Failed!", L"Plot Window", MB_OK);
        return 0;
    }

    hWnd = CreateWindow(PLOT_CLASS, L"Plot Window",
                  WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
                    10, 10, *width+10, *height+20,
                        NULL, NULL, hInstance, NULL);

    _add_window(hWnd);
    _set_window(hWnd);

    ShowWindow (hWnd, SW_SHOWNORMAL);
    UpdateWindow(hWnd);

    return 0;
}

/******************************************************************************/
static WindowItem *_find_ctl_item(void *cp)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == CTL_ITEM ) {
            if ( item->data == cp )
                return item;
        }
        item = item->next;
    }

    return NULL;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
LRESULT CALLBACK WndProc(HWND hWnd, UINT iMessage,
                         WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    WindowItem *itm;

    switch (iMessage) {
        case WM_COMMAND :
            // fprintf(stderr, "Button hit - id %ld\n", lParam);
            itm = _find_ctl_item((void*)lParam);
            if ( itm != NULL )
                PostMessage(_window,iMessage,wParam,lParam);
            return 0;
            break;
/*
        case WM_CREATE :
            SetTimer (hWnd, 1, 10, NULL);
            break;
*/
        case WM_PAINT :
            hdc = BeginPaint(hWnd, &ps);
            _draw_window_items();
            EndPaint (hWnd, &ps);
            return 0;
            break;
/*
        case WM_TIMER :
            if ((wParam == 1) && g_bUserTimer) {
            }
            break;
        case WM_SIZE :
            break;
*/
        case WM_DESTROY :
            // KillTimer (hWnd, 1);
            PostQuitMessage(0);
            return 0;
            break;
        case WM_MOUSEMOVE:
            cur_x=(short)LOWORD(lParam);
            cur_y=(short)HIWORD(lParam);
            // Check to see if the left button is held down:
            // leftButtonDown=wParam & MK_LBUTTON;
            // Check if right button down:
            // rightButtonDown=wParam & MK_RBUTTON;
            break;

    }
    return DefWindowProc(hWnd, iMessage, wParam, lParam);
}

/******************************************************************************/
int CleanupX(void)
{
    if ( _window == NULL ) return -1;

    CloseWindow(_window);
    _window = NULL;

    UnregisterClass(PLOT_CLASS, NULL);
    return 0;
}

/******************************************************************************/
static int _check_event()
{
    MSG Message;
    WindowItem *itm;

    Message.wParam = 0;
    if (PeekMessage (&Message, NULL, 0, 0, PM_REMOVE)) {
        switch (Message.message) {
            case WM_QUIT :
                return -1;
            case WM_COMMAND :
                // This will only happen if the message was reposted by the
                // WinProc routine in which case we dont want to pass it on again
                if ( Message.wParam == 0 ) {
                    itm = _find_ctl_item((void*)Message.lParam);
                    if ( itm != NULL )
                        return itm->id;
                }
                break;
            default :
                TranslateMessage (&Message);
                DispatchMessage (&Message);
                break;
        }
    }
    return 0;
}

/******************************************************************************/
int CheckUI(void)
{
    if ( _window == 0L ) return -1;
    return _check_event();
}

/******************************************************************************/
int DoUI() {
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

#if INCLUDE_SAVED
char *DoSaveDialog(char *fname);
#endif

#include <wchar.h>
/******************************************************************************/
wchar_t * convstr (const char *src)
{
    mbstate_t state;
    int cnt = strlen(src);
    wchar_t *dest;
    size_t n=0;

    dest = malloc((cnt+1)*sizeof(wchar_t));

    memset (&state, '\0', sizeof (state));
    while (*src)
        mbtowc(&dest[n++],src++,1);
    dest[n] = L'\0';

    return dest;
}

/******************************************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height)
{
    LPCTSTR lp = (LPCTSTR)convstr(title);
    HWND hwndButton = CreateWindow(L"BUTTON", lp,
                    WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON,
                    left, top, width, height,
                    _window, NULL,
                    (HINSTANCE)GetWindowLong(_window, GWLP_HINSTANCE), NULL);

    return _add_item(CTL_ITEM, hwndButton, left, top, width, height);
}

/******************************************************************************/
void RenameControl(int itm_id, const char*title)
{
    LPCTSTR lp = (LPCTSTR)convstr(title);
    WindowItem *item = _find_item(itm_id);
    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( item->data == NULL ) return;
    Button_SetText((HWND)item->data, lp);
}

/******************************************************************************/
void DisableControl(int itm_id)
{
    WindowItem *item = _find_item(itm_id);
    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( item->data == NULL ) return;

    Button_Enable((HWND)item->data, FALSE);
}

/******************************************************************************/
void EnableControl(int itm_id)
{
    WindowItem *item = _find_item(itm_id);
    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( item->data == NULL ) return;

    Button_Enable((HWND)item->data, TRUE);
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
void _draw_picture(PictureItem *pic)
{
    RECT r;

    if ( pic->img != NULL ) {
        SetDIBitsToDevice (hdc,
                pic->left, pic->top,   /* dest x,y */
                pic->width, pic->height,
                0, 0,                  /* src x,y */
                0, pic->height, pic->img, (BITMAPINFO *)&pic->bmpi, DIB_RGB_COLORS);
    }

    r.top = pic->top-1; r.left = pic->left-1;
    r.bottom = pic->top + pic->height + 1;
    r.right = pic->left + pic->width + 1;
    FrameRect(hdc, &r, (HBRUSH)GetStockObject (BLACK_BRUSH));
}


/******************************************************************************/
void _draw_string(int h, int v, Font font, const char *str)
{
    RECT r;
    LPCTSTR lp = (LPCTSTR)convstr(str);

    r.top = v; r.bottom = v + 20; r.left = h, r.right = h + 100;
    DrawText(hdc,lp,-1,&r,DT_SINGLELINE | DT_VCENTER | DT_CENTER);
}

/******************************************************************************/
static void _draw_window_items()
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);
    RECT r;

    if ( wptr == NULL ) return;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        switch ( item->type ) {
//          case CTL_ITEM: _draw_control(item->data); break;
            case PIC_ITEM: _draw_picture(item->data); break;
            case TXT_ITEM:
            case EDT_ITEM:
                _draw_string(item->left+6, item->top+15,
                            (item->type == TXT_ITEM)?font_b:font_r, item->data);
                if ( item->type == EDT_ITEM ) {
                    r.top = item->top-1; r.left = item->left-1;
                    r.bottom = item->bottom;
                    r.right = item->right;
                    FrameRect(hdc, &r, (HBRUSH)GetStockObject (BLACK_BRUSH));
                }
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
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height)
{
    PictureItem *pic = malloc(sizeof(PictureItem));

    pic->img = malloc(gdImageSX(im) * gdImageSY(im) * DEPTH);
    pic->true_colour = true_colour;
    pic->left = left; pic->top = top;
    pic->width = width; pic->height = height;

    pic->bmpi.biSize = sizeof(BITMAPINFOHEADER);
    pic->bmpi.biWidth = gdImageSX(im);
    // express height as a negative number otherwise the picture is upsidedown
    pic->bmpi.biHeight = -gdImageSY(im);
    pic->bmpi.biPlanes = 1;
    pic->bmpi.biBitCount = 24;
    pic->bmpi.biCompression = 0;

    _copy_img(im, pic);

    hdc = GetDC(hWnd);
    _draw_picture(pic);
    ReleaseDC(_window, hdc);

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

    hdc = GetDC(hWnd);
    _draw_picture(pic);
    ReleaseDC(_window, hdc);
}

/******************************************************************************/
int NewEditTextItem(int left, int top, int width, int height, const char*text)
{
    return _add_item(EDT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************/
int NewTextItem(int left, int top, int width, int height, const char*text)
{
    return _add_item(TXT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static void _add_window(Window win )//, GC gc)
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
//    wrec->gc = gc;
    wrec->itm_lst = NULL;
    wrec->curEdit = -1;
    wrec->mbarItm = -1;
}

/******************************************************************************/
static void _set_window(Window win)
{
    WindowPtr wptr = _find_window(win);

    if ( wptr != NULL ) {
        _window  = wptr->win;
    }
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
