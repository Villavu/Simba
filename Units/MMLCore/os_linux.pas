{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

      Linux OS specific implementation for Mufasa Macro Library
}
{$mode objfpc}{$H+} 
unit os_linux;

interface

  uses
    Classes, SysUtils, mufasatypes, xlib, x, xutil, IOManager, XKeyInput, ctypes, xtest, keysym,
    syncobjs, mufasabase;
  
  type

    TNativeWindow = x.TWindow;

    TKeyInput = class(TXKeyInput)
      public
        procedure Down(Key: Word);
        procedure Up(Key: Word);
    end;

    { TWindow }

    TWindow = class(TWindow_Abstract)
      public
        constructor Create(display: PDisplay; screennum: integer; window: x.TWindow); 
        destructor Destroy; override;
        procedure GetTargetDimensions(var w, h: integer); override;
        procedure GetTargetPosition(var left, top: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;
        procedure FreeReturnData; override;

        function  GetError: String; override;
        function  ReceivedError: Boolean; override;
        procedure ResetError; override;

        function TargetValid: boolean; override;
        procedure ActivateClient; override;
        procedure GetMousePosition(var x,y: integer); override;
        procedure MoveMouse(x,y: integer); override;
        procedure HoldMouse(x,y: integer; button: TClickType); override;
        procedure ReleaseMouse(x,y: integer; button: TClickType); override;
        function  IsMouseButtonHeld( button : TClickType) : boolean;override;

        procedure SendString(str: string); override;
        procedure HoldKey(key: integer); override;
        procedure ReleaseKey(key: integer); override;
        function IsKeyHeld(key: integer): boolean; override;
        function GetKeyCode(c : char) : integer;override;

        function GetNativeWindow: TNativeWindow;
      private
        { display is the connection to the X server }
        display: PDisplay;

        { screen-number and selected window }
        screennum: integer;
        window: x.TWindow;

        { Reference to the XImage }
        buffer: PXImage;

        { For memory-leak checks }
        dirty: Boolean;  //true if image loaded

        { KeyInput class }
        keyinput: TKeyInput;

        { X Error Handler }
        oldXHandler: TXErrorHandler;
    end;
    
    TIOManager = class(TIOManager_Abstract)
      public
        constructor Create;
        constructor Create(plugin_dir: string);
        function SetTarget(target: TNativeWindow): integer; overload;
        procedure SetDesktop; override;

        function GetProcesses: TProcArr; override;
        procedure SetTargetEx(Proc: TProc); override;
      private
        procedure NativeInit; override;
        procedure NativeFree; override;
      public
        display: PDisplay;
        screennum: integer;
        desktop: x.TWindow;
    end;

  function MufasaXErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint; cdecl;
    
implementation

  uses GraphType, interfacebase, lcltype;

  { PROBLEM: .Create is called on the main thread. ErrorCS etc aren't
    created on other threads. We will create them on the fly...
    More info below...}
  threadvar
    xerror: string;
  threadvar
    ErrorCS: syncobjs.TCriticalSection;


 {
    This is extremely hacky, but also very useful.
    We have to install a X error handler, because otherwise X
    will terminate out entire app on error.

    Since we want the right thread to recieve the right error, we have to
    fiddle a bit with threadvars, mutexes / semaphores.

    Another problem is that the script thread is initialised on the main thread.
    This means that all (threadvar!) semaphores initialised on the mainthread
    are NOT initialised on the script thread, which has yet to be started.
    Therefore, we check if it hasn't been created yet.

    ** Horrible solution, but WFM **

    This is the Handler function.

  }

  function MufasaXErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint; cdecl;

  begin
    case para2^.error_code of
      1:  xerror := 'BadRequest';
      2:  xerror := 'BadValue';
      3:  xerror := 'BadWindow';
      4:  xerror := 'BadPixmap';
      5:  xerror := 'BadAtom';
      6:  xerror := 'BadCursor';
      7:  xerror := 'BadFont';
      8:  xerror := 'BadMatch';
      9:  xerror := 'BadDrawable';
      10: xerror := 'BadAccess';
      11: xerror := 'BadAlloc';
      12: xerror := 'BadColor';
      13: xerror := 'BadGC';
      14: xerror := 'BadIDChoice';
      15: xerror := 'BadName';
      16: xerror := 'BadLength';
      17: xerror := 'BadImplementation';
      else
        xerror := 'UNKNOWN';
    end;
    result := 0;
    mDebugLn('X Error: ' + xerror);
    mDebugLn('Error code: ' + inttostr(para2^.error_code));
    mDebugLn('Display: ' + inttostr(LongWord(para2^.display)));
    mDebugLn('Minor code: ' + inttostr(para2^.minor_code));
    mDebugLn('Request code: ' + inttostr(para2^.request_code));
    mDebugLn('Resource ID: ' + inttostr(para2^.resourceid));
    mDebugLn('Serial: ' + inttostr(para2^.serial));
    mDebugLn('Type: ' + inttostr(para2^._type));
  end;

 { TKeyInput }

  procedure TKeyInput.Down(Key: Word);
  begin
    DoDown(Key);
  end;

  procedure TKeyInput.Up(Key: Word);
  begin
    DoUp(Key);
  end;

  { TWindow }

  function TWindow.GetError: String;
  begin
    exit(xerror);
  end;

  function  TWindow.ReceivedError: Boolean;
  begin
    result := xerror <> '';
  end;

  procedure TWindow.ResetError;
  begin
    xerror := '';
  end;

  { See if the semaphores / CS are initialised }
  constructor TWindow.Create(display: PDisplay; screennum: integer; window: x.TWindow); 
  begin 
    inherited Create;
    self.display:= display;
    self.screennum:= screennum;
    self.window:= window;
    self.dirty:= false;
    self.keyinput:= TKeyInput.Create;

    xerror := '';

    { XXX FIXME TODO O GOD WTF }
    if not assigned(ErrorCS) then
      ErrorCS := syncobjs.TCriticalSection.Create;
    ErrorCS.Enter;
    try
      oldXHandler:=XSetErrorHandler(@MufasaXErrorHandler);
    finally
      ErrorCS.Leave;
    end;
  end; 
  
  destructor TWindow.Destroy;
    var
      erh: TXErrorHandler;
  begin
    FreeReturnData;
    keyinput.Free;

    { XXX FIXME TODO O GOD WTF }
    if not assigned(ErrorCS) then
      ErrorCS := syncobjs.TCriticalSection.Create;
    ErrorCS.Enter;

    erh := XSetErrorHandler(oldXHandler);
    try
      if erh <> @MufasaXErrorHandler then
        XSetErrorHandler(erh);
    finally
      ErrorCS.Leave;
    end;
    inherited Destroy;
  end;

  function TWindow.GetNativeWindow: TNativeWindow;
  begin
    result := self.window;
  end;

  procedure TWindow.GetTargetDimensions(var w, h: integer); 
  var
    Attrib: TXWindowAttributes;
    newx, newy: integer;
    childwindow: x.TWindow;
  begin
    if XGetWindowAttributes(display, window, @Attrib) <> 0 Then
    begin
      W := Attrib.Width;
      H := Attrib.Height;
    end else
    begin
      W := -1;
      H := -1;
    end;
  end;

  procedure TWindow.GetTargetPosition(var left, top: integer);
  var
    Attrib: TXWindowAttributes;
    newx, newy: integer;
    childwindow: x.TWindow;
  begin
    if XGetWindowAttributes(display, window, @Attrib) <> 0 Then
    begin
      left := Attrib.x;
      top := attrib.y;
    end else
    begin
      // XXX: this is tricky; what do we return when it doesn't exist?
      // The window can very well be at -1, -1. We'll return 0 for now.
      left := 0;
      top := 0;
    end;
  end;

  function TWindow.TargetValid: boolean;
  var
    Attrib: TXWindowAttributes;
  begin
    XGetWindowAttributes(display, window, @Attrib);
    result := not ReceivedError;
  end;

  procedure TWindow.ActivateClient;

  begin
    XSetInputFocus(display,window,RevertToParent,CurrentTime);
    XFlush(display);
    if ReceivedError then
      raise Exception.Create('Error: ActivateClient: ' + GetError);
  end;
  
  function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;  
  var
    w,h: integer;
  begin
    GetTargetDimensions(w,h);
    if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) then
      raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);
    if dirty then
      raise Exception.CreateFmt('ReturnData was called again without freeing'+
                                ' the previously used data. Do not forget to'+
                                ' call FreeReturnData', []);

    buffer := XGetImage(display, window, xs, ys, width, height, AllPlanes, ZPixmap);
    if buffer = nil then
    begin
      mDebugLn('ReturnData: XGetImage Error. Dumping data now:');
      mDebugLn('xs, ys, width, height: ' + inttostr(xs) + ', '  + inttostr(ys) +
              ', ' + inttostr(width) + ', ' + inttostr(height));
      Result.Ptr := nil;
      Result.IncPtrWith := 0;
      raise Exception.CreateFMT('TMWindow.ReturnData: ReturnData: XGetImage Error', []);
      exit;
    end;
    Result.Ptr := PRGB32(buffer^.data);
    Result.IncPtrWith := 0;
    Result.RowLen := width;
    dirty:= true;
    //XSetErrorHandler(Old_Handler);
  end;
  
  procedure TWindow.FreeReturnData; 
  begin
    if dirty then
    begin
      if (buffer <> nil) then
        XDestroyImage(buffer);
      buffer:= nil;
      dirty:= false;
    end;
  end;

  procedure TWindow.GetMousePosition(var x,y: integer);
  var
    b:integer;
    root, child: twindow;
    xmask: Cardinal;

  begin
    XQueryPointer(display,window,@root,@child,@b,@b,@x,@y,@xmask);
  end;

  procedure TWindow.MoveMouse(x,y: integer);
  var
    w,h: integer;
  begin
    GetTargetDimensions(w, h);
    if (x < 0) or (y < 0) or (x > w) or (y > h) then
      raise Exception.CreateFmt('SetMousePos: X, Y (%d, %d) is not valid (0,0,%d,%d)', [x, y, w, h]);
    XWarpPointer(display, 0, window, 0, 0, 0, 0, X, Y);
    XFlush(display);
  end;

  procedure TWindow.HoldMouse(x,y: integer; button: TClickType);
  var
    ButtonP: cuint;
    _isPress: cbool;

  begin
    _isPress := cbool(1);
    case button of
      mouse_Left:   ButtonP:= Button1;
      mouse_Middle: ButtonP:= Button2;
      mouse_Right:  ButtonP:= Button3;
    end;
    XTestFakeButtonEvent(display, ButtonP, _isPress, CurrentTime);;
  end;

  procedure TWindow.ReleaseMouse(x,y: integer; button: TClickType);
  var
    ButtonP: cuint;
    _isPress: cbool;
  begin
    _isPress := cbool(0);
    case button of
      mouse_Left:   ButtonP:= Button1;
      mouse_Middle: ButtonP:= Button2;
      mouse_Right:  ButtonP:= Button3;
    end;
    XTestFakeButtonEvent(display, ButtonP, _isPress, CurrentTime);
  end;

  function TWindow.IsMouseButtonHeld(button: TClickType): boolean;
    var
     b:integer;
     root, child: twindow;
     xmask: Cardinal;
     ButtonP: cuint;

  begin
    XQueryPointer(display,window,@root,@child,@b,@b,@b,@b,@xmask);
    case button of
      mouse_Left:   ButtonP:= Button1Mask;
      mouse_Middle: ButtonP:= Button2Mask;
      mouse_Right:  ButtonP:= Button3Mask;
    end;
    result := xmask and ButtonP > 0;
  end;

  procedure TWindow.SendString(str: string);
  var
    i: integer;
    key: byte;
    HoldShift : boolean;
  begin
    HoldShift := false;
    for i := 1 to length(str) do
    begin
      if((str[i] >= 'A') and (str[i] <= 'Z')) then
      begin
        HoldKey(VK_SHIFT);
        HoldShift:= True;
        str[i] := lowerCase(str[i]);
      end else
        if HoldShift then
        begin
          HoldShift:= false;
          ReleaseKey(VK_SHIFT);
        end;
      key:= GetKeyCode(str[i]);
      HoldKey(key);
      //BenLand100: You should probably wait here...
      ReleaseKey(key);
    end;
    if HoldShift then
      ReleaseKey(VK_SHIFT);
  end;

  procedure TWindow.HoldKey(key: integer);
  begin
    keyinput.Down(key);
  end;

  procedure TWindow.ReleaseKey(key: integer);
  begin
    keyinput.Up(key);
  end;

  function TWindow.IsKeyHeld(key: integer): boolean;
  begin
    raise Exception.CreateFmt('IsKeyDown isn''t implemented yet on Linux', []);
  end;

  function TWindow.GetKeyCode(c: char): integer;
  begin
    case C of
      '0'..'9' :Result := VK_0 + Ord(C) - Ord('0');
      'a'..'z' :Result := VK_A + Ord(C) - Ord('a');
      'A'..'Z' :Result := VK_A + Ord(C) - Ord('A');
      ' ' : result := VK_SPACE;
    else
      Raise Exception.CreateFMT('GetSimpleKeyCode - char (%s) is not in A..z',[c]);
    end
  end;
  
  { ***implementation*** IOManager }

  constructor TIOManager.Create;
  begin
    inherited Create;
  end;

  constructor TIOManager.Create(plugin_dir: string);
  begin
    inherited Create(plugin_dir);
  end;

  procedure TIOManager.NativeInit;
  begin
    display := XOpenDisplay(nil);
    if display = nil then
      raise Exception.Create('Could not open a connection to the X Display');

    { DefaultScreen }
    screennum:= DefaultScreen(display);

    { Get the Desktop Window }
    desktop:= RootWindow(display,screennum)
  end;
  
  procedure TIOManager.NativeFree; 
  begin
    XCloseDisplay(display);
  end;
  
  procedure TIOManager.SetDesktop;
  begin
    SetBothTargets(TWindow.Create(display, screennum, desktop));
  end;
  
  function TIOManager.SetTarget(target: x.TWindow): integer;
  begin
    SetBothTargets(TWindow.Create(display, screennum, target))
  end;
  
  function TIOManager.GetProcesses: TProcArr;
  begin
    raise Exception.Create('GetProcesses: Not Implemented.');
  end;

  procedure TIOManager.SetTargetEx(Proc: TProc);
  begin
    raise Exception.Create('SetTargetEx: Not Implemented.');
  end;

  
end.
