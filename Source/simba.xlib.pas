{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.xlib;

(* GTK can receive x errors caused by a script and halt thinking it caused a
   fatal error since scripts are multithreaded and XSetErrorHandler is process wide.

   This unit loads a new xlib instance (using dlmopen) which will handle all
   the xlib calls for a script.

   It's either we synchronize (call on main thread) each xlib call we make
   or do this which I think is much more efficient.
*)

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  x, ctypes;

{$PACKRECORDS C}

const
  Button8 = 8;
  Button9 = 9;

  Button8Mask = 1 shl 15;
  Button9Mask = 1 shl 16;

const
  AllPlanes: culong = culong(not 0);

type
  TWindowArray = array of TWindow;

  PBool = ^TBool;
  TBool = Integer;

  PStatus = ^TStatus;
  TStatus = Integer;

  PDisplay = ^TDisplay;
  TDisplay = record end;

  CharArr32 = array[0..31] of Byte;

  PPcint = ^PInteger;
  PPcuchar = ^Pcuchar;

  PXErrorEvent = ^TXErrorEvent;
  TXErrorEvent = record
    _type: Integer;
    display: PDisplay;
    resourceid: TXID;
    serial: culong;
    error_code: cuchar;
    request_code: cuchar;
    minor_code: cuchar;
  end;

  TXErrorHandler = function(Display: PDisplay; para2: PXErrorEvent): Integer; cdecl;

  PXTextProperty = ^TXTextProperty;
  TXTextProperty = record
    value: pcuchar;
    encoding: TAtom;
    format: Integer;
    nitems: culong;
  end;

  PXClassHint = ^TXClassHint;
  TXClassHint = record
    res_name: PChar;
    res_class: PChar;
  end;

  PScreen = ^TScreen;
  TScreen = record
    ext_data: Pointer;
    display: Pointer;
    root: TWindow;
    width, height: Integer;
    mwidth, mheight: Integer;
    ndepths: Integer;
    depths: Pointer;
    root_depth: Integer;
    root_visual: Pointer;
    default_gc: Pointer;
    cmap: TColormap;
    white_pixel: culong;
    black_pixel: culong;
    max_maps, min_maps: Integer;
    backing_store: Integer;
    save_unders: TBool;
    root_input_mask: SizeInt;
  end;

  PXWindowAttributes = ^TXWindowAttributes;
  TXWindowAttributes = record
    x, y: Integer;
    width, height: Integer;
    border_width: Integer;
    depth: Integer;
    visual: Pointer;
    root: TWindow;
    c_class: Integer;
    bit_gravity: Integer;
    win_gravity: Integer;
    backing_store: Integer;
    backing_planes: culong;
    backing_pixel: culong;
    save_under: TBool;
    colormap: TColormap;
    map_installed: TBool;
    map_state: Integer;
    all_event_masks: SizeInt;
    your_event_mask: SizeInt;
    do_not_propagate_mask: SizeInt;
    override_redirect: TBool;
    screen: PScreen;
  end;

  PXImage = ^TXImage;
  TXImage = record
    width, height: Integer;
    xoffset: Integer;
    format: Integer;
    data: PChar;
    byte_order: Integer;
    bitmap_unit: Integer;
    bitmap_bit_order: Integer;
    bitmap_pad: Integer;
    depth: Integer;
    bytes_per_line: Integer;
    bits_per_pixel: Integer;
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
    _type: Integer;
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    root: TWindow;
    subwindow: TWindow;
    time: TTime;
    x, y: Integer;
    x_root, y_root: Integer;
    state: UInt32;
    button: UInt32;
    same_screen: TBool;
  end;

  PXClientMessageEvent = ^TXClientMessageEvent;
  TXClientMessageEvent = record
    _type: Integer;
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    message_type: TAtom;
    format: Integer;
    data: record
      case longint of
         0: ( b: array[0..19] of Int8 );
         1: ( s: array[0..9] of Int16 );
         2: ( l: array[0..4] of SizeInt );
      end;
  end;

  TSimbaXLib = record
  private
    X11Handle: Pointer;
    XTestHandle: Pointer;

    _XGetWindowAttributes: function(Display: PDisplay; para2:TWindow; para3:PXWindowAttributes):TStatus; cdecl;
    _XFlush: function(Display: PDisplay): Integer; cdecl;
    _XTranslateCoordinates: function(ADisplay: PDisplay; ASrcWindow:TWindow; ADestWindow:TWindow; ASrcX:Integer; ASrcY:Integer;ADestXReturn:PInteger; ADestYReturn:PInteger; AChildReturn:PWindow):TBool; cdecl;
    _XQueryTree: function(Display: PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow; para6:Pcuint):TStatus; cdecl;
    _XQueryPointer: function(Display: PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:PInteger; para6:PInteger; para7:PInteger; para8:PInteger; para9:Pcuint):LongBool; cdecl;
    _XWarpPointer: function(Display: PDisplay; para2:TWindow; para3:TWindow; para4:Integer; para5:Integer; para6:UInt32; para7:UInt32; para8:Integer; para9:Integer):Integer; cdecl;
    _XKeysymToKeycode: function(Display: PDisplay; para2:TKeySym):TKeyCode; cdecl;
    _XKeycodeToKeysym: function(Display: PDisplay; para2:TKeyCode; para3:Integer):TKeySym; cdecl;
    _XDefaultRootWindow: function(ADisplay:PDisplay):TWindow; cdecl;
    _XOpenDisplay: function(para1: PChar): PDisplay; cdecl;
    _XCloseDisplay: function(Display: PDisplay): Integer; cdecl;
    _XGetImage: function(Display: PDisplay; para2: TDrawable; para3: Integer; para4: Integer; para5: UInt32; para6: UInt32; para7: culong; para8: Integer): PXImage; cdecl;
    _XGetWindowProperty: function(Display: PDisplay; para2:TWindow; para3:TAtom; para4:SizeInt; para5:SizeInt;para6:TBool; para7:TAtom; para8:PAtom; para9:PInteger; para10:Pculong; para11:Pculong; para12:PPcuchar):Integer; cdecl;
    _XSetErrorHandler: function(para1: TXErrorHandler): TXErrorHandler; cdecl;
    _XStringToKeysym: function(para1: PChar): TKeySym; cdecl;
    _XMoveResizeWindow: function(ADisplay:PDisplay; AWindow:TWindow; AX:Integer; AY:Integer; AWidth:UInt32;AHeight:UInt32):Integer; cdecl;
    _XQueryKeymap: function(Display: PDisplay; para2: CharArr32):Integer; cdecl;
    _XGetWMName: function(Display: PDisplay; para2:TWindow; para3:PXTextProperty):TStatus; cdecl;
    _XFree: function(Data: Pointer):Integer; cdecl;
    _XGetClassHint: function(Display: PDisplay; para2:TWindow; para3:PXClassHint):TStatus; cdecl;
    _XInternAtom: function(Display: PDisplay; para2:PChar; para3:TBool): TAtom; cdecl;
    _XDestroyImage: function(ximage: PXImage): Integer; cdecl;
    _XSendEvent: function(Display: PDisplay; para2: TWindow; para3: TBool; para4: SizeInt; para5: Pointer): TStatus; cdecl;
    _XSync: function(Display: PDisplay; para2: TBool): Integer; cdecl;
    _XInitThreads: function: Integer; cdecl;

    _XTestFakeKeyEvent: function(Display: PDisplay; KeyCode: UInt32; Is_Press: TBool; Delay: UInt32): Integer; cdecl;
    _XTestFakeButtonEvent: function(Display: PDisplay; Button: UInt32; Is_Press: TBool; Delay: UInt32): Integer; cdecl;
  public
    // Atom mappings
    _NET_ACTIVE_WINDOW: TAtom;
    _NET_WM_PID: TAtom;
    _NET_WM_WINDOW_TYPE: TAtom;
    _NET_WM_WINDOW_TYPE_DESKTOP: TAtom;
    _NET_WM_WINDOW_TYPE_DOCK: TAtom;
    _NET_WM_WINDOW_TYPE_TOOLBAR: TAtom;
    _NET_WM_WINDOW_TYPE_MENU: TAtom;
    _NET_WM_WINDOW_TYPE_UTILITY: TAtom;
    _NET_WM_WINDOW_TYPE_SPLASH: TAtom;
    _NET_WM_WINDOW_TYPE_DIALOG: TAtom;
    _NET_WM_WINDOW_TYPE_NORMAL: TAtom;

    WM_STATE: TAtom; // For top level window detection
  public
    Display: PDisplay;

    function XGetWindowAttributes(para2:TWindow; para3:PXWindowAttributes): Boolean;
    function XFlush: Integer;
    function XTranslateCoordinates(ASrcWindow:TWindow; ADestWindow:TWindow; ASrcX:Integer; ASrcY:Integer;ADestXReturn:PInteger; ADestYReturn:PInteger; AChildReturn:PWindow): Boolean;
    function XQueryTree(para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow; para6:Pcuint): Boolean;
    function XQueryPointer(para2:TWindow; para3:PWindow; para4:PWindow; para5:PInteger; para6:PInteger; para7:PInteger; para8:PInteger; para9:Pcuint): LongBool;
    function XWarpPointer(para2:TWindow; para3:TWindow; para4:Integer; para5:Integer; para6:UInt32; para7:UInt32; para8:Integer; para9:Integer):Integer;
    function XKeysymToKeycode(para2:TKeySym):TKeyCode;
    function XKeycodeToKeysym(para2:TKeyCode; para3:Integer):TKeySym;
    function XDefaultRootWindow:TWindow;
    function XGetImage(para2: TDrawable; para3: Integer; para4: Integer; para5: UInt32; para6: UInt32; para7: culong; para8: Integer): PXImage;
    function XGetWindowProperty(para2:TWindow; para3:TAtom; para4:SizeInt; para5:SizeInt; para6:Boolean; para7:TAtom; para8:PAtom; para9:PInteger; para10:Pculong; para11:Pculong; para12:PPcuchar): Boolean;
    function XStringToKeysym(para1: PChar): TKeySym;
    function XMoveResizeWindow(AWindow:TWindow; AX:Integer; AY:Integer; AWidth:UInt32;AHeight:UInt32):Integer;
    function XQueryKeymap(para2: CharArr32):Integer;
    function XGetWMName(para2:TWindow; para3:PXTextProperty):TStatus;
    function XFree(para1:pointer):Integer;
    function XGetClassHint(para2:TWindow; para3:PXClassHint):TStatus;
    function XInternAtom(Name: PChar; OnlyIfExists: Boolean): TAtom;
    function XDestroyImage(ximage: PXImage): Integer;
    function XSendEvent(Window: TWindow; propagate: Boolean; para4: SizeInt; para5: Pointer): TStatus;
    function XSync(Discard: Boolean): Integer;
    function XTestFakeKeyEvent(KeyCode: UInt32; Press: Boolean; Delay: UInt32): Integer;
    function XTestFakeButtonEvent(Button: UInt32; Press: Boolean; Delay: UInt32): Integer;

    class function Create: TSimbaXLib; static;
    procedure Free;
  end;

var
  SimbaXLib: TSimbaXLib;

implementation

uses
  dl,
  simba.base;

function dlmopen(ID: SizeInt; Path: PChar; Flags: Integer): Pointer; cdecl; external;

function ErrorHandler(Display: PDisplay; Event: PXErrorEvent): Integer; cdecl;
var
  Error: String;
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
    else
      Error := IntToStr(Event^.Error_Code);
  end;

  DebugLn('Xlib error suppressed: %s', [Error]);
end;

function TSimbaXLib.XGetWindowAttributes(para2: TWindow; para3: PXWindowAttributes): Boolean;
begin
  Result := _XGetWindowAttributes(Display, para2, para3) <> 0;
end;

function TSimbaXLib.XFlush: Integer;
begin
  Result := _XFlush(Display);
end;

function TSimbaXLib.XTranslateCoordinates(ASrcWindow: TWindow; ADestWindow: TWindow; ASrcX: Integer; ASrcY: Integer; ADestXReturn: PInteger; ADestYReturn: PInteger; AChildReturn: PWindow): Boolean;
begin
  Result := Boolean(_XTranslateCoordinates(Display, ASrcWindow, ADestWindow, ASrcX, ASrcY, ADestXReturn, ADestYReturn, AChildReturn));
end;

function TSimbaXLib.XQueryTree(para2: TWindow; para3: PWindow; para4: PWindow; para5: PPWindow; para6: Pcuint): Boolean;
begin
  Result := _XQueryTree(Display, para2, para3, para4, para5, para6) <> 0;
end;

function TSimbaXLib.XQueryPointer(para2: TWindow; para3: PWindow; para4: PWindow; para5: PInteger; para6: PInteger; para7: PInteger; para8: PInteger; para9: Pcuint): LongBool;
begin
  Result := _XQueryPointer(display, para2, para3, para4, para5, para6, para7, para8, para9);
end;

function TSimbaXLib.XWarpPointer(para2: TWindow; para3: TWindow; para4: Integer; para5: Integer; para6: UInt32; para7: UInt32; para8: Integer; para9: Integer): Integer;
begin
  Result := _XWarpPointer(Display, para2, para3, para4, para5, para6, para7, para8, para9);
end;

function TSimbaXLib.XKeysymToKeycode(para2: TKeySym): TKeyCode;
begin
 Result := _XKeysymToKeycode(Display, para2);
end;

function TSimbaXLib.XKeycodeToKeysym(para2: TKeyCode; para3: Integer): TKeySym;
begin
  Result := _XKeycodeToKeysym(Display, para2, para3);
end;

function TSimbaXLib.XDefaultRootWindow: TWindow;
begin
  Result := _XDefaultRootWindow(Display);
end;

function TSimbaXLib.XGetImage(para2: TDrawable; para3: Integer; para4: Integer; para5: UInt32; para6: UInt32; para7: culong; para8: Integer): PXImage;
begin
  Result := _XGetImage(Display, para2, para3, para4, para5, para6, para7, para8);
end;

function TSimbaXLib.XGetWindowProperty(para2: TWindow; para3: TAtom; para4: SizeInt; para5: SizeInt; para6: Boolean; para7: TAtom; para8: PAtom; para9: PInteger; para10: Pculong; para11: Pculong; para12: PPcuchar): Boolean;
begin
  Result := _XGetWindowProperty(Display, para2, para3, para4, para5, TBool(para6), para7, para8, para9, para10, para11, para12) = 0;
end;

function TSimbaXLib.XStringToKeysym(para1: PChar): TKeySym;
begin
  Result := _XStringToKeysym(para1);
end;

function TSimbaXLib.XMoveResizeWindow(AWindow: TWindow; AX: Integer; AY: Integer; AWidth: UInt32; AHeight: UInt32): Integer;
begin
  Result := _XMoveResizeWindow(Display, AWindow, AX, AY, AWidth, AHeight);
end;

function TSimbaXLib.XQueryKeymap(para2: CharArr32): Integer;
begin
  Result := _XQueryKeymap(Display, para2);
end;

function TSimbaXLib.XGetWMName(para2: TWindow; para3: PXTextProperty): TStatus;
begin
  Result := _XGetWMName(Display, para2, para3);
end;

function TSimbaXLib.XFree(para1: pointer): Integer;
begin
  Result := _XFree(para1);
end;

function TSimbaXLib.XGetClassHint(para2: TWindow; para3: PXClassHint): TStatus;
begin
  Result := _XGetClassHint(Display, para2, para3);
end;

function TSimbaXLib.XInternAtom(Name: PChar; OnlyIfExists: Boolean): TAtom;
begin
  Result := _XInternAtom(Display, Name, TBool(OnlyIfExists));
end;

function TSimbaXLib.XDestroyImage(ximage: PXImage): Integer;
begin
  Result := _XDestroyImage(ximage);
end;

function TSimbaXLib.XSendEvent(Window: TWindow; propagate: Boolean; para4: SizeInt; para5: Pointer): TStatus;
begin
  Result := _XSendEvent(Display, Window, TBool(propagate), para4, para5);
end;

function TSimbaXLib.XSync(Discard: Boolean): Integer;
begin
  Result := _XSync(Display, TBool(Discard));
end;

function TSimbaXLib.XTestFakeKeyEvent(KeyCode: UInt32; Press: Boolean; Delay: UInt32): Integer;
begin
  Result := _XTestFakeKeyEvent(Display, KeyCode, TBool(Press), Delay);
end;

function TSimbaXLib.XTestFakeButtonEvent(Button: UInt32; Press: Boolean; Delay: UInt32): Integer;
begin
  Result := _XTestFakeButtonEvent(Display, Button, TBool(Press), Delay);
end;

class function TSimbaXLib.Create: TSimbaXLib;

  procedure LoadX11;
  begin
    with Result do
    begin
      X11Handle := dlmopen(-1, PChar('libX11.so.6'), RTLD_NOW);
      if (X11Handle = nil) then
        raise Exception.Create('TSimbaXLib.Create: Error loading "libX11.so.6"');

      Pointer(_XGetWindowAttributes) := dlsym(X11Handle, 'XGetWindowAttributes');
      Pointer(_XFlush) := dlsym(X11Handle, 'XFlush');
      Pointer(_XTranslateCoordinates) := dlsym(X11Handle, 'XTranslateCoordinates');
      Pointer(_XQueryTree) := dlsym(X11Handle, 'XQueryTree');
      Pointer(_XQueryPointer) := dlsym(X11Handle, 'XQueryPointer');
      Pointer(_XWarpPointer) := dlsym(X11Handle, 'XWarpPointer');
      Pointer(_XKeysymToKeycode) := dlsym(X11Handle, 'XKeysymToKeycode');
      Pointer(_XKeycodeToKeysym) := dlsym(X11Handle, 'XKeycodeToKeysym');
      Pointer(_XDefaultRootWindow) := dlsym(X11Handle, 'XDefaultRootWindow');
      Pointer(_XOpenDisplay) := dlsym(X11Handle, 'XOpenDisplay');
      Pointer(_XCloseDisplay) := dlsym(X11Handle, 'XCloseDisplay');
      Pointer(_XGetImage) := dlsym(X11Handle, 'XGetImage');
      Pointer(_XGetWindowProperty) := dlsym(X11Handle, 'XGetWindowProperty');
      Pointer(_XSetErrorHandler) := dlsym(X11Handle, 'XSetErrorHandler');
      Pointer(_XStringToKeysym) := dlsym(X11Handle, 'XStringToKeysym');
      Pointer(_XMoveResizeWindow) := dlsym(X11Handle, 'XMoveResizeWindow');
      Pointer(_XQueryKeymap) := dlsym(X11Handle, 'XQueryKeymap');
      Pointer(_XGetWMName) := dlsym(X11Handle, 'XGetWMName');
      Pointer(_XFree) := dlsym(X11Handle, 'XFree');
      Pointer(_XGetClassHint) := dlsym(X11Handle, 'XGetClassHint');
      Pointer(_XInternAtom) := dlsym(X11Handle, 'XInternAtom');
      Pointer(_XDestroyImage) := dlsym(X11Handle, 'XDestroyImage');
      Pointer(_XSendEvent) := dlsym(X11Handle, 'XSendEvent');
      Pointer(_XSync) := dlsym(X11Handle, 'XSync');
      Pointer(_XInitThreads) := dlsym(X11Handle, 'XInitThreads');

      _XInitThreads();
      _XSetErrorHandler(@ErrorHandler);
    end;
  end;

  procedure LoadXTest;
  begin
    with Result do
    begin
      XTestHandle := dlmopen(-1, PChar('libXtst.so.6'), RTLD_NOW);
      if (XTestHandle = nil) then
        raise Exception.Create('TSimbaXLib.Create: Error loading "libXtst.so.6"');

      Pointer(_XTestFakeKeyEvent) := dlsym(XTestHandle, 'XTestFakeKeyEvent');
      Pointer(_XTestFakeButtonEvent) := dlsym(XTestHandle, 'XTestFakeButtonEvent');
    end;
  end;

begin
  Result := Default(TSimbaXLib);

  LoadX11();
  LoadXTest();

  Result.Display := Result._XOpenDisplay(nil);
  if (Result.Display = nil) then
    raise Exception.Create('TSimbaLib.Create: Unable to open display');

  Result._NET_WM_WINDOW_TYPE               := Result.XInternAtom('_NET_WM_WINDOW_TYPE', False);
  Result._NET_WM_WINDOW_TYPE_DESKTOP       := Result.XInternAtom('_NET_WM_WINDOW_TYPE_DESKTOP', False);
  Result._NET_WM_WINDOW_TYPE_DOCK          := Result.XInternAtom('_NET_WM_WINDOW_TYPE_DOCK', False);
  Result._NET_WM_WINDOW_TYPE_TOOLBAR       := Result.XInternAtom('_NET_WM_WINDOW_TYPE_TOOLBAR', False);
  Result._NET_WM_WINDOW_TYPE_MENU          := Result.XInternAtom('_NET_WM_WINDOW_TYPE_MENU', False);
  Result._NET_WM_WINDOW_TYPE_UTILITY       := Result.XInternAtom('_NET_WM_WINDOW_TYPE_UTILITY', False);
  Result._NET_WM_WINDOW_TYPE_SPLASH        := Result.XInternAtom('_NET_WM_WINDOW_TYPE_SPLASH', False);
  Result._NET_WM_WINDOW_TYPE_DIALOG        := Result.XInternAtom('_NET_WM_WINDOW_TYPE_DIALOG', False);
  Result._NET_WM_WINDOW_TYPE_NORMAL        := Result.XInternAtom('_NET_WM_WINDOW_TYPE_NORMAL', False);
  Result._NET_WM_PID                       := Result.XInternAtom('_NET_WM_PID', False);
  Result._NET_ACTIVE_WINDOW                := Result.XInternAtom('_NET_ACTIVE_WINDOW', False);

  Result.WM_STATE                          := Result.XInternAtom('WM_STATE', False);
end;

procedure TSimbaXLib.Free;
begin
  if (Display <> nil) then
    _XCloseDisplay(Display);

  if (X11Handle <> nil) then
    dlclose(X11Handle);
  if (XTestHandle <> nil) then
    dlclose(XTestHandle);
end;

initialization
  SimbaXLib := TSimbaXLib.Create();

finalization
  SimbaXLib.Free();

end.
