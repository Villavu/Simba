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
  classes, sysutils, x, ctypes;

{$PACKRECORDS C}

const
  AllPlanes: culong = culong(not 0);

type
  TWindowArray = array of TWindow;

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

  TSimbaXLib = record
  public
    Display: PDisplay;

    // raw xlib exports
    function XGetWindowAttributes(para2:TWindow; para3:PXWindowAttributes):TStatus;
    function XFlush: cint;
    function XTranslateCoordinates(ASrcWindow:TWindow; ADestWindow:TWindow; ASrcX:cint; ASrcY:cint;ADestXReturn:Pcint; ADestYReturn:Pcint; AChildReturn:PWindow):TBool;
    function XQueryTree(para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow; para6:Pcuint):TStatus;
    function XQueryPointer(para2:TWindow; para3:PWindow; para4:PWindow; para5:Pcint; para6:Pcint; para7:Pcint; para8:Pcint; para9:Pcuint):TBoolResult;
    function XWarpPointer(para2:TWindow; para3:TWindow; para4:cint; para5:cint; para6:cuint; para7:cuint; para8:cint; para9:cint):cint;
    function XKeysymToKeycode(para2:TKeySym):TKeyCode;
    function XKeycodeToKeysym(para2:TKeyCode; para3:cint):TKeySym;
    function XDefaultRootWindow:TWindow;
    function XGetImage(para2: TDrawable; para3: cint; para4: cint; para5: cuint; para6: cuint; para7: culong; para8: cint): PXImage;
    function XGetWindowProperty(para2:TWindow; para3:TAtom; para4:clong; para5:clong; para6:TBool; para7:TAtom; para8:PAtom; para9:Pcint; para10:Pculong; para11:Pculong; para12:PPcuchar): cint;
    function XStringToKeysym(para1: Pchar): TKeySym;
    function XMoveResizeWindow(AWindow:TWindow; AX:cint; AY:cint; AWidth:cuint;AHeight:cuint):cint;
    function XQueryKeymap(para2: CharArr32):cint;
    function XGetWMName(para2:TWindow; para3:PXTextProperty):TStatus;
    function Xutf8TextPropertyToTextList(para2: PXTextProperty; para3: PPPchar; para4: Pcint): cint;
    procedure XFreeStringList(para1:PPchar);
    function XFree(para1:pointer):cint;
    function XGetClassHint(para2:TWindow; para3:PXClassHint):TStatus;
    function XInternAtom(para2:Pchar; para3:TBool): TAtom;
    function XDestroyImage(ximage: PXImage): cint;
    function XSendEvent(para2: TWindow; para3: TBool; para4: clong; para5: Pointer): TStatus;
    function XSync(para2: TBool): cint;
    function XTestFakeKeyEvent(KeyCode: UInt32; Is_Press: TBool; Delay: UInt32): Int32;
    function XTestFakeButtonEvent(Button: UInt32; Is_Press: TBool; Delay: UInt32): Int32;

    // Helpers
    function XQueryTree(Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TWindowArray; out Count: Int32): Boolean;
    function XGetWindowProperty(Window: TWindow; Prop: TAtom): Int64;
    function XHasWindowProperty(Window: TWindow; Name: String): Boolean;
    procedure XGetKeyCode(Key: Char; out KeyCode, ModifierCode: TKeyCode); overload;
    function XGetKeyCode(VirtualKey: UInt16): TKeyCode; overload;
    function XGetWindowTitle(Window: TWindow): String;
    function XGetWindowClass(Window: TWindow): String;
    function XGetRootWindow(Window: TWindow): TWindow;
    procedure XSetActiveWindow(Window: TWindow);
    function XGetActiveWindow: TWindow;
    function XGetChildren(Window: TWindow; Recursive: Boolean): TWindowArray;

    class function Create: TSimbaXLib; static;
    procedure Free;
  end;

var
  SimbaXLib: TSimbaXLib;

implementation

uses
  dl, LazLoggerBase, lcltype, keysym;

var
 _XGetWindowAttributes: function(para1:PDisplay; para2:TWindow; para3:PXWindowAttributes):TStatus; cdecl;
 _XFlush: function(para1: PDisplay): cint; cdecl;
 _XTranslateCoordinates: function(ADisplay:PDisplay; ASrcWindow:TWindow; ADestWindow:TWindow; ASrcX:cint; ASrcY:cint;ADestXReturn:Pcint; ADestYReturn:Pcint; AChildReturn:PWindow):TBool; cdecl;
 _XQueryTree: function(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow; para6:Pcuint):TStatus; cdecl;
 _XQueryPointer: function(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:Pcint; para6:Pcint; para7:Pcint; para8:Pcint; para9:Pcuint):TBoolResult; cdecl;
 _XWarpPointer: function(para1:PDisplay; para2:TWindow; para3:TWindow; para4:cint; para5:cint; para6:cuint; para7:cuint; para8:cint; para9:cint):cint; cdecl;
 _XKeysymToKeycode: function(para1:PDisplay; para2:TKeySym):TKeyCode; cdecl;
 _XKeycodeToKeysym: function(para1:PDisplay; para2:TKeyCode; para3:cint):TKeySym; cdecl;
 _XDefaultRootWindow: function(ADisplay:PDisplay):TWindow; cdecl;
 _XOpenDisplay: function(para1: Pchar): PDisplay; cdecl;
 _XCloseDisplay: function(para1: PDisplay): cint; cdecl;
 _XGetImage: function(para1: PDisplay; para2: TDrawable; para3: cint; para4: cint; para5: cuint; para6: cuint; para7: culong; para8: cint): PXImage; cdecl;
 _XGetWindowProperty: function(para1:PDisplay; para2:TWindow; para3:TAtom; para4:clong; para5:clong; para6:TBool; para7:TAtom; para8:PAtom; para9:Pcint; para10:Pculong; para11:Pculong; para12:PPcuchar): cint;
 _XSetErrorHandler: function(para1: TXErrorHandler): TXErrorHandler; cdecl;
 _XStringToKeysym: function(para1: Pchar): TKeySym; cdecl;
 _XMoveResizeWindow: function(ADisplay:PDisplay; AWindow:TWindow; AX:cint; AY:cint; AWidth:cuint;AHeight:cuint):cint; cdecl;
 _XQueryKeymap: function(para1:PDisplay; para2: CharArr32):cint; cdecl;
 _XGetWMName: function(para1:PDisplay; para2:TWindow; para3:PXTextProperty):TStatus; cdecl;
 _Xutf8TextPropertyToTextList: function(para1: PDisplay; para2: PXTextProperty; para3: PPPchar; para4: Pcint): cint; cdecl;
 _XFreeStringList: procedure(para1:PPchar); cdecl;
 _XFree:function(para1:pointer):cint; cdecl;
 _XGetClassHint: function(para1:PDisplay; para2:TWindow; para3:PXClassHint):TStatus; cdecl;
 _XInternAtom: function(para1:PDisplay; para2:Pchar; para3:TBool): TAtom; cdecl;
 _XDestroyImage: function(ximage: PXImage): cint; cdecl;
 _XSendEvent: function(para1: PDisplay; para2: TWindow; para3: TBool; para4: clong; para5: Pointer): TStatus; cdecl;
 _XSync: function(para1: PDisplay; para2: TBool): cint; cdecl;
 _XInitThreads: function: cint; cdecl;

 _XTestFakeKeyEvent: function(Display: PDisplay; KeyCode: UInt32; Is_Press: TBool; Delay: UInt32): Int32; cdecl;
 _XTestFakeButtonEvent: function(Display: PDisplay; Button: UInt32; Is_Press: TBool; Delay: UInt32): Int32; cdecl;

function ErrorHandler(Display: PDisplay; Event: PXErrorEvent): cint; cdecl;
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
    17: Error := 'BadImplementation'
    else
      Error := Format('Unknown (%d)', [Event^.Error_Code]);
  end;

  DebugLn('Xlib error suppressed: %s (thread: %d)', [Error, GetCurrentThreadID()]);
end;

function dlmopen(ID: clong; Path: PChar; Flags: cint): Pointer; cdecl; external;

var
  X11: Pointer;

procedure LoadX11;
begin
  X11 := dlmopen(-1, PChar('libX11.so.6'), RTLD_NOW);
  if (X11 = nil) then
    raise Exception.Create('Error loading X11');

  Pointer(_XGetWindowAttributes) := dlsym(X11, 'XGetWindowAttributes');
  Pointer(_XFlush) := dlsym(X11, 'XFlush');
  Pointer(_XTranslateCoordinates) := dlsym(X11, 'XTranslateCoordinates');
  Pointer(_XQueryTree) := dlsym(X11, 'XQueryTree');
  Pointer(_XQueryPointer) := dlsym(X11, 'XQueryPointer');
  Pointer(_XWarpPointer) := dlsym(X11, 'XWarpPointer');
  Pointer(_XKeysymToKeycode) := dlsym(X11, 'XKeysymToKeycode');
  Pointer(_XKeycodeToKeysym) := dlsym(X11, 'XKeycodeToKeysym');
  Pointer(_XDefaultRootWindow) := dlsym(X11, 'XDefaultRootWindow');
  Pointer(_XOpenDisplay) := dlsym(X11, 'XOpenDisplay');
  Pointer(_XCloseDisplay) := dlsym(X11, 'XCloseDisplay');
  Pointer(_XGetImage) := dlsym(X11, 'XGetImage');
  Pointer(_XGetWindowProperty) := dlsym(X11, 'XGetWindowProperty');
  Pointer(_XSetErrorHandler) := dlsym(X11, 'XSetErrorHandler');
  Pointer(_XStringToKeysym) := dlsym(X11, 'XStringToKeysym');
  Pointer(_XMoveResizeWindow) := dlsym(X11, 'XMoveResizeWindow');
  Pointer(_XQueryKeymap) := dlsym(X11, 'XQueryKeymap');
  Pointer(_XGetWMName) := dlsym(X11, 'XGetWMName');
  Pointer(_XUTF8TextPropertyToTextList) := dlsym(X11, 'XUTF8TextPropertyToTextList');
  Pointer(_XFreeStringList) := dlsym(X11, 'XFreeStringList');
  Pointer(_XFree) := dlsym(X11, 'XFree');
  Pointer(_XGetClassHint) := dlsym(X11, 'XGetClassHint');
  Pointer(_XInternAtom) := dlsym(X11, 'XInternAtom');
  Pointer(_XDestroyImage) := dlsym(X11, 'XDestroyImage');
  Pointer(_XSendEvent) := dlsym(X11, 'XSendEvent');
  Pointer(_XSync) := dlsym(X11, 'XSync');
  Pointer(_XInitThreads) := dlsym(X11, 'XInitThreads');

  _XInitThreads();
  _XSetErrorHandler(@ErrorHandler);
end;

procedure UnloadX11;
begin
  if (X11 <> nil) then
    dlclose(X11);
end;

var
  XTst: Pointer;

procedure LoadXtst;
begin
  XTst := dlmopen(-1, PChar('libXtst.so.6'), RTLD_NOW);
  if (XTst = nil) then
    raise Exception.Create('Error loading Xtst');

  Pointer(_XTestFakeKeyEvent) := dlsym(XTst, 'XTestFakeKeyEvent');
  Pointer(_XTestFakeButtonEvent) := dlsym(XTst, 'XTestFakeButtonEvent');
end;

procedure UnloadXtst;
begin
  if (Xtst <> nil) then
    dlclose(Xtst);
end;

function TSimbaXLib.XGetWindowAttributes(para2: TWindow; para3: PXWindowAttributes): TStatus;
begin
  Result := _XGetWindowAttributes(Display, para2, para3);
end;

function TSimbaXLib.XFlush: cint;
begin
  Result := _XFlush(Display);
end;

function TSimbaXLib.XTranslateCoordinates(ASrcWindow: TWindow; ADestWindow: TWindow; ASrcX: cint; ASrcY: cint; ADestXReturn: Pcint; ADestYReturn: Pcint; AChildReturn: PWindow): TBool;
begin
  Result := _XTranslateCoordinates(Display, ASrcWindow, ADestWindow, ASrcX, ASrcY, ADestXReturn, ADestYReturn, AChildReturn);
end;

function TSimbaXLib.XQueryTree(para2: TWindow; para3: PWindow; para4: PWindow; para5: PPWindow; para6: Pcuint): TStatus;
begin
  Result := _XQueryTree(Display, para2, para3, para4, para5, para6);
end;

function TSimbaXLib.XQueryPointer(para2: TWindow; para3: PWindow; para4: PWindow; para5: Pcint; para6: Pcint; para7: Pcint; para8: Pcint; para9: Pcuint): TBoolResult;
begin
  Result := _XQueryPointer(display, para2, para3, para4, para5, para6, para7, para8, para9);
end;

function TSimbaXLib.XWarpPointer(para2: TWindow; para3: TWindow; para4: cint; para5: cint; para6: cuint; para7: cuint; para8: cint; para9: cint): cint;
begin
  Result := _XWarpPointer(Display, para2, para3, para4, para5, para6, para7, para8, para9);
end;

function TSimbaXLib.XKeysymToKeycode(para2: TKeySym): TKeyCode;
begin
 Result := _XKeysymToKeycode(Display, para2);
end;

function TSimbaXLib.XKeycodeToKeysym(para2: TKeyCode; para3: cint): TKeySym;
begin
  Result := _XKeycodeToKeysym(Display, para2, para3);
end;

function TSimbaXLib.XDefaultRootWindow: TWindow;
begin
  Result := _XDefaultRootWindow(Display);
end;

function TSimbaXLib.XGetImage(para2: TDrawable; para3: cint; para4: cint; para5: cuint; para6: cuint; para7: culong; para8: cint): PXImage;
begin
  Result := _XGetImage(Display, para2, para3, para4, para5, para6, para7, para8);
end;

function TSimbaXLib.XGetWindowProperty(para2: TWindow; para3: TAtom; para4: clong; para5: clong; para6: TBool; para7: TAtom; para8: PAtom; para9: Pcint; para10: Pculong; para11: Pculong; para12: PPcuchar): cint;
begin
  Result := _XGetWindowProperty(Display, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12);
end;

function TSimbaXLib.XStringToKeysym(para1: Pchar): TKeySym;
begin
  Result := _XStringToKeysym(para1);
end;

function TSimbaXLib.XMoveResizeWindow(AWindow: TWindow; AX: cint; AY: cint; AWidth: cuint; AHeight: cuint): cint;
begin
  Result := _XMoveResizeWindow(Display, AWindow, AX, AY, AWidth, AHeight);
end;

function TSimbaXLib.XQueryKeymap(para2: CharArr32): cint;
begin
  Result := _XQueryKeymap(Display, para2);
end;

function TSimbaXLib.XGetWMName(para2: TWindow; para3: PXTextProperty): TStatus;
begin
  Result := _XGetWMName(Display, para2, para3);
end;

function TSimbaXLib.Xutf8TextPropertyToTextList(para2: PXTextProperty; para3: PPPchar; para4: Pcint): cint;
begin
  Result := _Xutf8TextPropertyToTextList(Display, para2, para3, para4);
end;

procedure TSimbaXLib.XFreeStringList(para1: PPchar);
begin
  _XFreeStringList(para1);
end;

function TSimbaXLib.XFree(para1: pointer): cint;
begin
  Result := _XFree(para1);
end;

function TSimbaXLib.XGetClassHint(para2: TWindow; para3: PXClassHint): TStatus;
begin
  Result := _XGetClassHint(Display, para2, para3);
end;

function TSimbaXLib.XInternAtom(para2: Pchar; para3: TBool): TAtom;
begin
  Result := _XInternAtom(display, para2, para3);
end;

function TSimbaXLib.XDestroyImage(ximage: PXImage): cint;
begin
  Result := _XDestroyImage(ximage);
end;

function TSimbaXLib.XSendEvent(para2: TWindow; para3: TBool; para4: clong; para5: Pointer): TStatus;
begin
  Result := _XSendEvent(Display, para2, para3, para4, para5);
end;

function TSimbaXLib.XSync(para2: TBool): cint;
begin
  Result := _XSync(Display, para2);
end;

function TSimbaXLib.XTestFakeKeyEvent(KeyCode: UInt32; Is_Press: TBool; Delay: UInt32): Int32;
begin
  Result := _XTestFakeKeyEvent(Display, KeyCode, Is_Press, Delay);
end;

function TSimbaXLib.XTestFakeButtonEvent(Button: UInt32; Is_Press: TBool; Delay: UInt32): Int32;
begin
  Result := _XTestFakeButtonEvent(Display, Button, Is_Press, Delay);
end;

function TSimbaXLib.XQueryTree(Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TWindowArray; out Count: Int32): Boolean;
var
  Data: ^TWindow;
  i: Int32;
begin
  SetLength(Children, 0);

  if (SimbaXLib.XQueryTree(Window, @Root, @Parent, @Data, @Count) <> 0) then
  begin
    if (Data <> nil) then
    begin
      SetLength(Children, Count);
      for i := 0 to High(Children) do
        Children[i] := Data[i];

      SimbaXLib.XFree(Data);
     end;

    Exit(True);
  end;

  Exit(False);
end;

function TSimbaXLib.XGetWindowProperty(Window: TWindow; Prop: TAtom): Int64;
var
  Atom: TAtom;
  Format, Count, Bytes: Int32;
  Data: PByte;
begin
  Result := -1;

  if SimbaXLib.XGetWindowProperty(Window, Prop, 0, 2, 0, AnyPropertyType, @Atom, @Format, @Count, @Bytes, @Data) = Success then
  begin
    if (Data <> nil) then
    begin
      Result := PInt64(Data)^;

      SimbaXLib.XFree(Data);
    end;
  end;
end;

function TSimbaXLib.XHasWindowProperty(Window: TWindow; Name: String): Boolean;
begin
  Result := XGetWindowProperty(Window, SimbaXLib.XInternAtom(PChar(Name), TBool(False))) > 0;
end;

procedure TSimbaXLib.XGetKeyCode(Key: Char; out KeyCode, ModifierCode: TKeyCode);

  function GetSpecialKeySym(Key: Char): TKeySym;
  begin
    Result := 0;

    case Key of
      #9 : Result := XK_TAB;
      #10: Result := XK_RETURN;
      #32: Result := XK_SPACE;
      #34: Result := XK_QUOTEDBL;
      #39: Result := XK_APOSTROPHE;
      '!': Result := XK_EXCLAM;
      '#': Result := XK_NUMBERSIGN;
      '%': Result := XK_PERCENT;
      '$': Result := XK_DOLLAR;
      '&': Result := XK_AMPERSAND;
      '(': Result := XK_PARENLEFT;
      ')': Result := XK_PARENRIGHT;
      '=': Result := XK_EQUAL;
      ',': Result := XK_COMMA;
      '.': Result := XK_PERIOD;
      ':': Result := XK_COLON;
      ';': Result := XK_SEMICOLON;
      '<': Result := XK_LESS;
      '>': Result := XK_GREATER;
      '?': Result := XK_QUESTION;
      '@': Result := XK_AT;
      '[': Result := XK_BRACKETLEFT;
      ']': Result := XK_BRACKETRIGHT;
      '\': Result := XK_BACKSLASH;
      '^': Result := XK_ASCIICIRCUM;
      '_': Result := XK_UNDERSCORE;
      '`': Result := XK_GRAVE;
      '{': Result := XK_BRACELEFT;
      '|': Result := XK_BAR;
      '}': Result := XK_BRACERIGHT;
      '~': Result := XK_ASCIITILDE;
      '+': Result := XK_PLUS;
      '-': Result := XK_MINUS;
      '*': Result := XK_ASTERISK;
      '/': Result := XK_SLASH;
    end;
  end;

var
  Symbol: TKeySym;
  Index: Int32;
begin
  KeyCode := 0;
  ModifierCode := 0;

  if GetSpecialKeySym(Key) <> 0 then
    Symbol := GetSpecialKeySym(Key)
  else
    Symbol := SimbaXLib.XStringToKeysym(PChar(String(Key)));

  KeyCode := SimbaXLib.XKeySymToKeycode(Symbol);
  if (KeyCode = 0) then
    raise Exception.Create('Unknown keycode for "' + Key + '"');

  Index := 0;
  while (Index < 8) and (SimbaXLib.XKeyCodeToKeySym(KeyCode, Index) <> Symbol) do
    Inc(Index);

  if (Index <> 0) then
  begin
    Dec(Index);

    if (Index = ShiftMapIndex) then
      ModifierCode := SimbaXLib.XKeysymToKeycode(XK_Shift_L)
    else
    if (Index >= Mod1MapIndex) then
      ModifierCode := SimbaXLib.XKeysymToKeycode(XK_ISO_Level3_Shift);

    if (ModifierCode = 0) then
      raise Exception.Create('Unknown keycode for *modifier* "' + Key + '"');
  end;
end;

function TSimbaXLib.XGetWindowTitle(Window: TWindow): String;
var
  Struct: TXTextProperty;
begin
  Result := '';

  if (SimbaXLib.XGetWMName(Window, @Struct) <> 0) and (Struct.NItems > 0) and (Struct.Value <> nil) then
  begin
    SetLength(Result, Struct.NItems);
    Move(Struct.value^, Result[1], Length(Result));

    SimbaXLib.XFree(Struct.Value);
  end;
end;

function TSimbaXLib.XGetWindowClass(Window: TWindow): String;
var
  Hint: TXClassHint;
begin
  Result := '';

  if (SimbaXLib.XGetClassHint(Window, @Hint) <> 0) then
  begin
    Result := Hint.Res_Name;

    if (Hint.Res_Name <> nil) then
      SimbaXLib.XFree(Hint.Res_Name);
    if (Hint.Res_Class <> nil) then
      SimbaXLib.XFree(Hint.Res_Class);
  end;
end;

function TSimbaXLib.XGetRootWindow(Window: TWindow): TWindow;
var
  Root, Parent: TWindow;
  Children: TWindowArray;
  Count: Int32;
begin
  Result := Window;
  if XHasWindowProperty(Result, 'WM_STATE') then
    Exit;

  while (Window > 0) and XQueryTree(Window, Root, Parent, Children, Count) do
  begin
    if (Parent > 0) and XHasWindowProperty(Parent, 'WM_STATE') then
      Exit(Parent);

    Window := Parent;
  end;
end;

procedure TSimbaXLib.XSetActiveWindow(Window: TWindow);
var
  Event: TXClientMessageEvent;
  Struct: TXWindowAttributes;
begin
  Window := XGetRootWindow(Window);

  Event := Default(TXClientMessageEvent);
  Event._Type := ClientMessage;
  Event.Display := SimbaXLib.Display;
  Event.Window := Window;
  Event.Message_Type := SimbaXLib.XInternAtom('_NET_ACTIVE_WINDOW', TBool(False));
  Event.Format := 32;
  Event.Data.L[0] := 2;
  Event.Data.L[1] := CurrentTime;

  if SimbaXLib.XGetWindowAttributes(Window, @Struct) <> 0 then
    SimbaXLib.XSendEvent(Struct.Screen^.Root, TBool(False), SubstructureNotifyMask or SubstructureRedirectMask, @Event);

  SimbaXLib.XSync(0);
end;

function TSimbaXLib.XGetActiveWindow: TWindow;
begin
  Result := XGetWindowProperty(SimbaXLib.XDefaultRootWindow(), SimbaXLib.XInternAtom('_NET_ACTIVE_WINDOW', TBool(False)));
end;

function TSimbaXLib.XGetKeyCode(VirtualKey: UInt16): TKeyCode;
var
  Symbol: TKeySym;
begin
  case VirtualKey of
    VK_BACK:      Symbol := XK_BackSpace;
    VK_TAB:       Symbol := XK_Tab;
    VK_CLEAR:     Symbol := XK_Clear;
    VK_RETURN:    Symbol := XK_Return;
    VK_SHIFT:     Symbol := XK_Shift_L;
    VK_CONTROL:   Symbol := XK_Control_L;
    VK_MENU:      Symbol := XK_Alt_R;
    VK_CAPITAL:   Symbol := XK_Caps_Lock;

    VK_ESCAPE:    Symbol := XK_Escape;
    VK_SPACE:     Symbol := XK_Space;
    VK_PRIOR:     Symbol := XK_Prior;
    VK_NEXT:      Symbol := XK_Next;
    VK_END:       Symbol := XK_End;
    VK_HOME:      Symbol := XK_Home;
    VK_LEFT:      Symbol := XK_Left;
    VK_UP:        Symbol := XK_Up;
    VK_RIGHT:     Symbol := XK_Right;
    VK_DOWN:      Symbol := XK_Down;
    VK_SELECT:    Symbol := XK_Select;
    VK_PRINT:     Symbol := XK_Print;
    VK_EXECUTE:   Symbol := XK_Execute;

    VK_INSERT:    Symbol := XK_Insert;
    VK_DELETE:    Symbol := XK_Delete;
    VK_HELP:      Symbol := XK_Help;
    VK_0:         Symbol := XK_0;
    VK_1:         Symbol := XK_1;
    VK_2:         Symbol := XK_2;
    VK_3:         Symbol := XK_3;
    VK_4:         Symbol := XK_4;
    VK_5:         Symbol := XK_5;
    VK_6:         Symbol := XK_6;
    VK_7:         Symbol := XK_7;
    VK_8:         Symbol := XK_8;
    VK_9:         Symbol := XK_9;

    VK_A:         Symbol := XK_A;
    VK_B:         Symbol := XK_B;
    VK_C:         Symbol := XK_C;
    VK_D:         Symbol := XK_D;
    VK_E:         Symbol := XK_E;
    VK_F:         Symbol := XK_F;
    VK_G:         Symbol := XK_G;
    VK_H:         Symbol := XK_H;
    VK_I:         Symbol := XK_I;
    VK_J:         Symbol := XK_J;
    VK_K:         Symbol := XK_K;
    VK_L:         Symbol := XK_L;
    VK_M:         Symbol := XK_M;
    VK_N:         Symbol := XK_N;
    VK_O:         Symbol := XK_O;
    VK_P:         Symbol := XK_P;
    VK_Q:         Symbol := XK_Q;
    VK_R:         Symbol := XK_R;
    VK_S:         Symbol := XK_S;
    VK_T:         Symbol := XK_T;
    VK_U:         Symbol := XK_U;
    VK_V:         Symbol := XK_V;
    VK_W:         Symbol := XK_W;
    VK_X:         Symbol := XK_X;
    VK_Y:         Symbol := XK_Y;
    VK_Z:         Symbol := XK_Z;

    VK_NUMPAD0:   Symbol := XK_KP_0;
    VK_NUMPAD1:   Symbol := XK_KP_1;
    VK_NUMPAD2:   Symbol := XK_KP_2;
    VK_NUMPAD3:   Symbol := XK_KP_3;
    VK_NUMPAD4:   Symbol := XK_KP_4;
    VK_NUMPAD5:   Symbol := XK_KP_5;
    VK_NUMPAD6:   Symbol := XK_KP_6;
    VK_NUMPAD7:   Symbol := XK_KP_7;
    VK_NUMPAD8:   Symbol := XK_KP_8;
    VK_NUMPAD9:   Symbol := XK_KP_9;
    VK_MULTIPLY:  Symbol := XK_KP_Multiply;
    VK_ADD:       Symbol := XK_KP_Add;
    VK_SEPARATOR: Symbol := XK_KP_Separator;
    VK_SUBTRACT:  Symbol := XK_KP_Subtract;
    VK_DECIMAL:   Symbol := XK_KP_Decimal;
    VK_DIVIDE:    Symbol := XK_KP_Divide;
    VK_F1:        Symbol := XK_F1;
    VK_F2:        Symbol := XK_F2;
    VK_F3:        Symbol := XK_F3;
    VK_F4:        Symbol := XK_F4;
    VK_F5:        Symbol := XK_F5;
    VK_F6:        Symbol := XK_F6;
    VK_F7:        Symbol := XK_F7;
    VK_F8:        Symbol := XK_F8;
    VK_F9:        Symbol := XK_F9;
    VK_F10:       Symbol := XK_F10;
    VK_F11:       Symbol := XK_F11;
    VK_F12:       Symbol := XK_F12;
    VK_F13:       Symbol := XK_F13;
    VK_F14:       Symbol := XK_F14;
    VK_F15:       Symbol := XK_F15;
    VK_F16:       Symbol := XK_F16;
    VK_F17:       Symbol := XK_F17;
    VK_F18:       Symbol := XK_F18;
    VK_F19:       Symbol := XK_F19;
    VK_F20:       Symbol := XK_F20;
    VK_F21:       Symbol := XK_F21;
    VK_F22:       Symbol := XK_F22;
    VK_F23:       Symbol := XK_F23;
    VK_F24:       Symbol := XK_F24;
    VK_NUMLOCK:   Symbol := XK_Num_Lock;
    VK_SCROLL:    Symbol := XK_Scroll_Lock;
  else
    Symbol := XK_VoidSymbol;
  end;

  Result := SimbaXLib.XKeySymToKeyCode(Symbol);
end;

function TSimbaXLib.XGetChildren(Window: TWindow; Recursive: Boolean): TWindowArray;

  procedure GetChildren(Window: TWindow);
  var
    Parent, Root: TWindow;
    Children: TWindowArray;
    Count, i: Int32;
  begin
    if (Window > 0) then
    begin
      if XQueryTree(Window, Root, Parent, Children, Count) then
      begin
        for i := 0 to High(Children) do
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Children[i];

          if Recursive then
            GetChildren(Children[i]);
        end;
      end;
    end;
  end;

begin
  SetLength(Result, 0);

  GetChildren(Window);
end;

class function TSimbaXLib.Create: TSimbaXLib;
begin
  Result := Default(TSimbaXLib);
  Result.Display := _XOpenDisplay(nil);
  if (Result.Display = nil) then
    raise Exception.Create('Unable to open XDisplay');
end;

procedure TSimbaXLib.Free;
begin
  _XCloseDisplay(Display);
end;

initialization
  LoadX11();
  LoadXtst();

  SimbaXLib := TSimbaXLib.Create();

finalization
  SimbaXLib.Free();

  UnloadX11();
  UnloadXTst();

end.
