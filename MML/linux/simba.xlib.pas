unit simba.xlib;

(* GTK can receive x errors caused by a script and halt thinking it caused a
   fatal error since scripts are multithreaded and XSetErrorHandler is process wide.

   This unit loads a new xlib instance (using dlmopen) which will handle all
   the xlib calls for a script.

   It's either we synchronize (call on main thread) each xlib call we make
   or do this which I think is much more efficient.
 *)

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, x, ctypes;

{$PACKRECORDS C}

const
  AllPlanes: culong = culong(not 0);

type
  PBool = ^TBool;
  TBool = cint;
  TBoolResult = LongBool;

  PStatus = ^TStatus;
  TStatus = cint;

  PDisplay = ^TDisplay;
  TDisplay = record end;

  CharArr32 = array[0..31] of Byte;

  PPcint = ^Pcint;
  PPcuchar = ^Pcuchar;

  PXErrorEvent = ^TXErrorEvent;
  TXErrorEvent = record
    _type: cint;
    display: PDisplay;
    resourceid: TXID;
    serial: culong;
    error_code: cuchar;
    request_code: cuchar;
    minor_code: cuchar;
  end;

  TXErrorHandler = function (para1:PDisplay; para2:PXErrorEvent):cint;cdecl;

  PXTextProperty = ^TXTextProperty;
  TXTextProperty = record
    value: pcuchar;
    encoding: TAtom;
    format: cint;
    nitems: culong;
  end;

  PXClassHint = ^TXClassHint;
  TXClassHint = record
    res_name: Pchar;
    res_class: Pchar;
  end;

  PScreen = ^TScreen;
  TScreen = record
    ext_data: Pointer;
    display: Pointer;
    root: TWindow;
    width, height: cint;
    mwidth, mheight: cint;
    ndepths: cint;
    depths: Pointer;
    root_depth: cint;
    root_visual: Pointer;
    default_gc: Pointer;
    cmap: TColormap;
    white_pixel: culong;
    black_pixel: culong;
    max_maps, min_maps: cint;
    backing_store: cint;
    save_unders: TBool;
    root_input_mask: clong;
  end;

  PXWindowAttributes = ^TXWindowAttributes;
  TXWindowAttributes = record
    x, y: cint;
    width, height: cint;
    border_width: cint;
    depth: cint;
    visual: Pointer;
    root: TWindow;
    c_class: cint;
    bit_gravity: cint;
    win_gravity: cint;
    backing_store: cint;
    backing_planes: culong;
    backing_pixel: culong;
    save_under: TBool;
    colormap: TColormap;
    map_installed: TBool;
    map_state: cint;
    all_event_masks: clong;
    your_event_mask: clong;
    do_not_propagate_mask: clong;
    override_redirect: TBool;
    screen: PScreen;
  end;

  PXImage = ^TXImage;
  TXImage = record
    width, height: cint;
    xoffset: cint;
    format: cint;
    data: Pchar;
    byte_order: cint;
    bitmap_unit: cint;
    bitmap_bit_order: cint;
    bitmap_pad: cint;
    depth: cint;
    bytes_per_line: cint;
    bits_per_pixel: cint;
    red_mask: culong;
    green_mask: culong;
    blue_mask: culong;
    obdata: Pointer;
    f: record
         create_image: Pointer;
         destroy_image: Pointer;
         get_pixel: Pointer;
         put_pixel: Pointer;
         sub_image: Pointer;
         add_pixel :Pointer;
      end;
  end;

  PXButtonEvent = ^TXButtonEvent;
  TXButtonEvent = record
    _type: cint;
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    root: TWindow;
    subwindow: TWindow;
    time: TTime;
    x, y: cint;
    x_root, y_root: cint;
    state: cuint;
    button: cuint;
    same_screen: TBool;
  end;

  PXClientMessageEvent = ^TXClientMessageEvent;
  TXClientMessageEvent = record
    _type: cint;
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    message_type: TAtom;
    format: cint;
    data: record
      case longint of
         0: ( b: array[0..19] of cchar );
         1: ( s: array[0..9] of cshort );
         2: ( l: array[0..4] of clong );
      end;
  end;

var
  XGetWindowAttributes: function(para1:PDisplay; para2:TWindow; para3:PXWindowAttributes):TStatus; cdecl;
  XFlush: function(para1: PDisplay): cint; cdecl;
  XTranslateCoordinates: function(ADisplay:PDisplay; ASrcWindow:TWindow; ADestWindow:TWindow; ASrcX:cint; ASrcY:cint;ADestXReturn:Pcint; ADestYReturn:Pcint; AChildReturn:PWindow):TBool; cdecl;
  XQueryTree: function(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow; para6:Pcuint):TStatus; cdecl;
  XQueryPointer: function(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:Pcint; para6:Pcint; para7:Pcint; para8:Pcint; para9:Pcuint):TBoolResult; cdecl;
  XWarpPointer: function(para1:PDisplay; para2:TWindow; para3:TWindow; para4:cint; para5:cint; para6:cuint; para7:cuint; para8:cint; para9:cint):cint; cdecl;
  XKeysymToKeycode: function(para1:PDisplay; para2:TKeySym):TKeyCode; cdecl;
  XKeycodeToKeysym: function(para1:PDisplay; para2:TKeyCode; para3:cint):TKeySym; cdecl;
  XDefaultRootWindow: function(ADisplay:PDisplay):TWindow; cdecl;
  XOpenDisplay: function(para1: Pchar): PDisplay; cdecl;
  XCloseDisplay: function(para1: PDisplay): cint; cdecl;
  XGetImage: function(para1: PDisplay; para2: TDrawable; para3: cint; para4: cint; para5: cuint; para6: cuint; para7: culong; para8: cint): PXImage; cdecl;
  XGetWindowProperty: function(para1:PDisplay; para2:TWindow; para3:TAtom; para4:clong; para5:clong; para6:TBool; para7:TAtom; para8:PAtom; para9:Pcint; para10:Pculong; para11:Pculong; para12:PPcuchar): cint;
  XSetErrorHandler: function(para1: TXErrorHandler): TXErrorHandler; cdecl;
  XStringToKeysym: function(para1: Pchar): TKeySym; cdecl;
  XMoveResizeWindow: function(ADisplay:PDisplay; AWindow:TWindow; AX:cint; AY:cint; AWidth:cuint;AHeight:cuint):cint; cdecl;
  XQueryKeymap: function(para1:PDisplay; para2: CharArr32):cint; cdecl;
  XGetWMName: function(para1:PDisplay; para2:TWindow; para3:PXTextProperty):TStatus; cdecl;
  Xutf8TextPropertyToTextList: function(para1: PDisplay; para2: PXTextProperty; para3: PPPchar; para4: Pcint): cint; cdecl;
  XFreeStringList: procedure(para1:PPchar); cdecl;
  XFree:function(para1:pointer):cint; cdecl;
  XGetClassHint: function(para1:PDisplay; para2:TWindow; para3:PXClassHint):TStatus; cdecl;
  XInternAtom: function(para1:PDisplay; para2:Pchar; para3:Boolean): TAtom; cdecl;
  XDestroyImage: function(ximage: PXImage): cint; cdecl;
  XSendEvent: function(para1: PDisplay; para2: TWindow; para3: Boolean; para4: clong; para5: Pointer): TStatus; cdecl;
  XSync: function(para1: PDisplay; para2: TBool): cint; cdecl;
  XInitThreads: function: cint; cdecl;

  function XTestFakeKeyEvent(Display: PDisplay; KeyCode: UInt32; Is_Press: Boolean; Delay: UInt32): Int32; cdecl; external 'Xtst';

implementation

uses
  dl;

function ErrorHandler(Display: PDisplay; Event: PXErrorEvent): cint; cdecl;
var
  Error: String = '';
begin
  Result := 0;

  case Event^.Error_Code of
    1:  Error := 'BadRequest';
    2:  Error := 'BadValue';
    3:  Error := 'BadWindow';
    4:  Error := 'BadPixmap';
    5:  Error := 'BadAtom';
    6:  Error := 'BadCursor';
    7:  Error := 'BadFont';
    8:  Error := 'BadMatch';
    9:  Error := 'BadDrawable';
    10: Error := 'BadAccess';
    11: Error := 'BadAlloc';
    12: Error := 'BadColor';
    13: Error := 'BadGC';
    14: Error := 'BadIDChoice';
    15: Error := 'BadName';
    16: Error := 'BadLength';
    17: Error := 'BadImplementation';
  end;

  if Error <> '' then
    WriteLn(Format('Xlib error suppressed: %s (thread: %d)', [Error, GetCurrentThreadId()]))
  else
    WriteLn(Format('Xlib error suppressed: %d (thread: %d)', [Event^.Error_Code, GetCurrentThreadID()]))
end;

function dlmopen(ID: clong; Path: PChar; Flags: cint): Pointer; cdecl; external;

var
  X11: Pointer;

procedure LoadX11;
begin
  X11 := dlmopen(-1, PChar('libX11.so.6'), RTLD_NOW);
  if (X11 = nil) then
    raise Exception.Create('Error loading X11');

  Pointer(XGetWindowAttributes) := dlsym(X11, 'XGetWindowAttributes');
  Pointer(XFlush) := dlsym(X11, 'XFlush');
  Pointer(XTranslateCoordinates) := dlsym(X11, 'XTranslateCoordinates');
  Pointer(XQueryTree) := dlsym(X11, 'XQueryTree');
  Pointer(XQueryPointer) := dlsym(X11, 'XQueryPointer');
  Pointer(XWarpPointer) := dlsym(X11, 'XWarpPointer');
  Pointer(XKeysymToKeycode) := dlsym(X11, 'XKeysymToKeycode');
  Pointer(XKeycodeToKeysym) := dlsym(X11, 'XKeycodeToKeysym');
  Pointer(XDefaultRootWindow) := dlsym(X11, 'XDefaultRootWindow');
  Pointer(XOpenDisplay) := dlsym(X11, 'XOpenDisplay');
  Pointer(XCloseDisplay) := dlsym(X11, 'XCloseDisplay');
  Pointer(XGetImage) := dlsym(X11, 'XGetImage');
  Pointer(XGetWindowProperty) := dlsym(X11, 'XGetWindowProperty');
  Pointer(XSetErrorHandler) := dlsym(X11, 'XSetErrorHandler');
  Pointer(XStringToKeysym) := dlsym(X11, 'XStringToKeysym');
  Pointer(XMoveResizeWindow) := dlsym(X11, 'XMoveResizeWindow');
  Pointer(XQueryKeymap) := dlsym(X11, 'XQueryKeymap');
  Pointer(XGetWMName) := dlsym(X11, 'XGetWMName');
  Pointer(XUTF8TextPropertyToTextList) := dlsym(X11, 'XUTF8TextPropertyToTextList');
  Pointer(XFreeStringList) := dlsym(X11, 'XFreeStringList');
  Pointer(XFree) := dlsym(X11, 'XFree');
  Pointer(XGetClassHint) := dlsym(X11, 'XGetClassHint');
  Pointer(XInternAtom) := dlsym(X11, 'XInternAtom');
  Pointer(XDestroyImage) := dlsym(X11, 'XDestroyImage');
  Pointer(XSendEvent) := dlsym(X11, 'XSendEvent');
  Pointer(XSync) := dlsym(X11, 'XSync');
  Pointer(XInitThreads) := dlsym(X11, 'XInitThreads');

  XInitThreads();
  XSetErrorHandler(@ErrorHandler);
end;

procedure UnloadX11;
begin
  dlclose(X11);
end;

initialization
  LoadX11();

finalization
  UnloadX11();

end.s
