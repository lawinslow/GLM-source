/******************************************************************************
 *                                                                            *
 * macbasic.m                                                                 *
 *                                                                            *
 *   A simple Mac OS-X GUI example.                                           *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Earth & Environment                                          *
 *     The University of Western Australia                                    *
 *                                                                            *
 * Copyright 2014, 2015 -  The University of Western Australia                *
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
 *  Created CAB 20150105                                                      *
 *                                                                            *
 ******************************************************************************/

#import <Cocoa/Cocoa.h>
#include <ui_basic.h>

static int stopit = 0;

/******************************************************************************/

@interface Controller : NSObject
{
    id appName;
    id window;
    NSModalSession session;
}
- (void)runModalUI;
- (void)cleanUp;
- (void)startSession;
- (int)addWindow:(int) x ypos:(int) y width:(int) w height:(int) h;
- (id)addButton:(int) x ypos:(int) y width:(int) w height:(int) h title:(NSString*)title;
- (IBAction) buttonPressed:(id)sender;
@end

/******************************************************************************/
static int idhint = 1;
static Controller *sharedController;

/*============================================================================*/
@implementation Controller

/*----------------------------------------------------------------------------*/
- (id) init
{
    self = [super init];

    NSApplicationLoad();
    appName = [[NSProcessInfo processInfo] processName];

    sharedController = self;
    return self;
}

/*----------------------------------------------------------------------------*/
- (void) startSession
{
    session = [NSApp beginModalSessionForWindow:window];
}

/*----------------------------------------------------------------------------*/
- (void) addMenus
{
    /*------------------------------------------------------------------------*
     * This is commented out because neither variant works. Some exploring of *
     * the net tells me that making menus work can only be done with a .nib   *
     * file - so thats for the future                                         *
     *------------------------------------------------------------------------*/
#if 0
#if 1
    id menubar = [[NSMenu new] autorelease];
    id appMenuItem = [[NSMenuItem new] autorelease];
    [menubar addItem:appMenuItem];
    [NSApp setMainMenu:menubar];

    id appMenu = [[NSMenu new] autorelease];
    id quitTitle = [@"Quit " stringByAppendingString:appName];
    id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
        action:@selector(terminate:) keyEquivalent:@"q"] autorelease];
    [appMenu addItem:quitMenuItem];
    [appMenuItem setSubmenu:appMenu];
#else
    NSMenu *mm = [NSApp mainMenu];
    NSMenuItem *mi1 = [mm itemAtIndex:0];
    NSMenu *subMenu = [mi1 submenu];
    NSMenuItem *prefMenu = [subMenu itemWithTag:100];
    prefMenu.target = self;
//  prefMenu.action = @selector(showPreferencesMenu);
#endif
#endif
}

/*----------------------------------------------------------------------------*/
- (int) addWindow:(int) x ypos:(int) y width:(int) w height:(int) h
{
    window = [[NSWindow alloc] initWithContentRect:NSMakeRect(x, y, w, h)
        styleMask:NSTitledWindowMask backing:NSBackingStoreBuffered defer:NO];

    if ( !window ) {
        fprintf(stderr, "Failed to make window\n");
        return -1;
    }

    [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
    [window setTitle:appName];
    [window makeKeyAndOrderFront:nil];
    [window retain];

    [NSApp updateWindows];
    return 0;
}

/*----------------------------------------------------------------------------*/
- (id) addButton:(int) x ypos:(int) y width:(int) w height:(int) h title:(NSString*)title
{
    id button = [[NSButton alloc] initWithFrame:NSMakeRect(x, y, w, h)];
    [button setTitle:title];
    [button setTarget:self];
    [button setAction:@selector(buttonPressed:)];
    [button retain];

    [[window contentView] addSubview:button];
    return button;
}

/*----------------------------------------------------------------------------*/
- (void) runModalUI
{
    if ([NSApp runModalSession:session] != NSModalResponseContinue)
        stopit = 1;
}

/*----------------------------------------------------------------------------*/
- (IBAction) buttonPressed:(id)sender
{
    stopit = ((NSButton*)sender).tag;
//  fprintf(stderr, "Button pressed %d\n", stopit);
}

/*----------------------------------------------------------------------------*/
- (void) cleanUp
{
    [NSApp endModalSession:session];

    [window release];
}

@end
/******************************************************************************/


/******************************************************************************/
#define CTL_ITEM    1
#define PIC_ITEM    2
#define TXT_ITEM    3
#define EDT_ITEM    4
#define WIN_MENU    5
#define MNU_ITEM    6

/******************************************************************************/
typedef struct _pic_item {
    CGContextRef      context;
    unsigned char    *img;
    int               true_colour;
    int               left;
    int               top;
    int               width;
    int               height;
} PictureItem;


/******************************************************************************/
typedef struct _win_item {
    struct _win_item *next;
    int               id;
    int               type;
    void             *data;
} WindowItem;

static WindowItem  *itm_lst = NULL;

/******************************************************************************/
static int _add_item(int type, void *data,
                                       int left, int top, int width, int height)
{
    WindowItem *item = malloc(sizeof(WindowItem));
    WindowItem *ti = NULL;

    ti = itm_lst;

    item->next = NULL;
    if ( ti == NULL )
        itm_lst = item;
    else
        {
        while ( ti->next )
            ti = ti->next;
        ti->next = item;
        }

    item->type = type;
    item->data = data;
    item->id = idhint++;

    return item->id;
}

/******************************************************************************/
static void _release_items(void)
{
    WindowItem *item = NULL, *next;

    item = itm_lst;

    while ( item != NULL ) {
        if ( item->type == CTL_ITEM ) {
            [(id)(item->data) release];
        }
        else if ( item->type == PIC_ITEM ) {
            PictureItem *pic = item->data;
            [(id)(pic->context) release];
            free(pic->img);
            free(pic);
        }
        next = item->next;
        free(item);
        item = next;
    }
}

/******************************************************************************/
static WindowItem *_find_item(int itm_id)
{
    WindowItem *item = NULL;

    item = itm_lst;

    while ( item != NULL ) {
        if ( item->id == itm_id )
            return item;
        item = item->next;
    }
    return NULL;
}

/******************************************************************************/
int _draw_cycle = 0;

void _draw_picture(PictureItem *pic)
{
    if ( _draw_cycle ) return;
    // unfortunately, this makes it run very slowly
    // there must be a better way to do this....

    // I have tried adding the bitMapImage part to the Picture structure
    // and just flushing that with the DrawImage, but the image doesnt update
    // and it runs just as slowly.

    CGContextRef cg = pic->context;
    int x = pic->left, y = pic->top;
    int w = pic->width, h = pic->height;

/* for debugging, print out the transform matrix
 * scaling is one thing that supposedly slows things down
 * but unfortunately this does not seem to be scaled, so it's not that

    CGAffineTransform cga = CGContextGetCTM ( cg );
    fprintf(stderr, "CGA : a = %f b = %f c = %f d = %f tx = %f ty = %f\n",
               cga.a, cga.b, cga.c, cga.d, cga.tx, cga.ty);
*/

// This is now done just the once when the image is created (a bit faster)
//  NSFrameRect(NSMakeRect(x-1, y-1, w+2, h+2));

    CGImageRef bitMapImage = CGBitmapContextCreateImage(cg);
    CGContextDrawImage([[NSGraphicsContext currentContext] CGContext],
                        NSMakeRect(x, y, w, h), bitMapImage);
    CGImageRelease(bitMapImage);
}


/******************************************************************************/
#define DEPTH 4
#define R_OFS 2
#define G_OFS 1
#define B_OFS 0
/*----------------------------------------------------------------------------*/
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
    } else {
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


static int wHeight = 100;

/******************************************************************************/
int InitUI(int *width, int * height)
{
    fprintf(stderr, "InitUI\n");

    CGDirectDisplayID display = CGMainDisplayID();
    size_t sheight = CGDisplayPixelsHigh (display);
    size_t swidth  = CGDisplayPixelsWide (display);

    Controller *controller;

    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

    /* adjust height/width so window will fit on the display */
    if ( sheight - 40 < *height ) *height = sheight - 40;
    if ( swidth < *width ) *width = swidth;
    wHeight = *height;

    if (!(controller = [[Controller alloc] init]))
        return -1;

    [controller retain];
    [controller addMenus];
    [controller addWindow:0 ypos:0 width:(*width) height:(*height)];
    [controller startSession];

    return 0;
}


/******************************************************************************/
void FlushUI(void)
{
    [sharedController runModalUI];

    CGContextFlush([[NSGraphicsContext currentContext] CGContext]);
    // in order to overcome the worst of the slowness from _draw_picture
    // we do the drawing only every week
    if ( _draw_cycle++ > 7 ) _draw_cycle = 0;
}


/******************************************************************************/
int CheckUI(void)
{
    [sharedController runModalUI];
    return stopit;
}


/******************************************************************************/
int DoUI(void)
{
    _draw_cycle = 0;
    while (!CheckUI()) ;
    return stopit;
}

/******************************************************************************/
int CleanupUI(void)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    [sharedController cleanUp];

    _release_items();

    [localPool release];
    return 0;
}

/******************************************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

    int id_no;

    top = wHeight - top - height;
    id button = [sharedController addButton:left ypos:top
                                   width:width height:height
                                   title:[NSString stringWithUTF8String:title]];

    id_no = _add_item(CTL_ITEM, button, left, top, width, height);
    ((NSButton*)button).tag = id_no;

    [localPool release];
    return id_no;
}

/******************************************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

    CGContextRef    context = NULL;
    CGColorSpaceRef colorSpace = NULL;
    PictureItem *pic = malloc(sizeof(PictureItem));

    top = wHeight - top - height;

    pic->img = malloc(gdImageSX(im) * gdImageSY(im) * DEPTH);
    pic->true_colour = true_colour;
    pic->left = left; pic->top = top;
    pic->width = width; pic->height = height;

    width = gdImageSX(im);
    height = gdImageSY(im);

    _copy_img(im, pic);

    colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
//  colorSpace = CGColorSpaceCreateDeviceRGB();

    context = CGBitmapContextCreate(pic->img,
                                    width,
                                    height,
                                    8,      // bits per component
                                    width * DEPTH,
                                    colorSpace,
                                    (CGBitmapInfo)kCGImageAlphaNoneSkipLast);
    if (context== NULL) {
        free(pic->img); free(pic);
        return -1;
    }
    pic->context = context;
    CGColorSpaceRelease(colorSpace);

    // Much beter to do the frame here just the once (seems to be a high cost to this)
    NSFrameRect(NSMakeRect(pic->left-1, pic->top-1, pic->width+2, pic->height+2));
    _draw_picture(pic);

    [localPool release];
    return _add_item(PIC_ITEM, pic, left, top, width, height);
}

/******************************************************************************/
void FlushPicture(gdImagePtr im, int itm_id)
{
    WindowItem *itm = _find_item(itm_id);
    PictureItem *pic = itm->data;
    _copy_img(im, pic);
    _draw_picture(pic); // unfortunately, this makes it run very slowly
}

/******************************************************************************/
void EnableControl(int itm_id)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    WindowItem *itm = _find_item(itm_id);
    id myButton = itm->data;
    [myButton setEnabled: YES];
    [localPool release];
}

/******************************************************************************/
void DisableControl(int itm_id)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    WindowItem *itm = _find_item(itm_id);
    id myButton = itm->data;
    [myButton setEnabled: NO];
    [localPool release];
}
