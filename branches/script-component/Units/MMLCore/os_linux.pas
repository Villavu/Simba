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

      Linux OS specific implemetation for Mufasa Macro Library
}
{$mode objfpc}{$H+} 
unit os_linux;

interface

  uses
    Classes, SysUtils, mufasatypes, xlib, x, xutil, IOManager, XKeyInput, ctypes, xtest, keysym;
  
  type

    TNativeWindow = x.TWindow;

    TKeyInput = class(TXKeyInput)
      public
        procedure Down(Key: Word);
        procedure Up(Key: Word);
    end;

    TWindow = class(TWindow_Abstract)
      public
        constructor Create(display: PDisplay; screennum: integer; window: x.TWindow); 
        destructor Destroy; override;
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;
        procedure FreeReturnData; override;

        function TargetValid: boolean; override;
        procedure ActivateClient; override;
        procedure GetMousePosition(var x,y: integer); override;
        procedure MoveMouse(x,y: integer); override;
        procedure HoldMouse(x,y: integer; button: TClickType); override;
        procedure ReleaseMouse(x,y: integer; button: TClickType); override;

        procedure SendString(str: string); override;
        procedure HoldKey(key: integer); override;
        procedure ReleaseKey(key: integer); override;
        function IsKeyHeld(key: integer): boolean; override;

        function GetNativeWindow: x.TWindow;
      private
        display: PDisplay;
        screennum: integer;
        window: x.TWindow;
        buffer: PXImage;
        dirty: Boolean;  //true if image loaded
        keyinput: TKeyInput;
    end;
    
    TIOManager = class(TIOManager_Abstract)
      public
        constructor Create;
        constructor Create(plugin_dir: string);
        function SetTarget(target: TNativeWindow): integer; overload;
        procedure SetDesktop; override;
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

  // Too global.
  function MufasaXErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint;cdecl;
  begin;
    result := 0;
    Writeln('X Error: ');
    writeln('Error code: ' + inttostr(para2^.error_code));
    writeln('Display: ' + inttostr(LongWord(para2^.display)));
    writeln('Minor code: ' + inttostr(para2^.minor_code));
    writeln('Request code: ' + inttostr(para2^.request_code));
    writeln('Resource ID: ' + inttostr(para2^.resourceid));
    writeln('Serial: ' + inttostr(para2^.serial));
    writeln('Type: ' + inttostr(para2^._type));
  end;

//***implementation*** TKeyInput

  procedure TKeyInput.Down(Key: Word);
  begin
    DoDown(Key);
  end;

  procedure TKeyInput.Up(Key: Word);
  begin
    DoUp(Key);
  end;

//***implementation*** TWindow

  constructor TWindow.Create(display: PDisplay; screennum: integer; window: x.TWindow); 
  begin 
    inherited Create;
    self.display:= display;
    self.screennum:= screennum;
    self.window:= window;
    self.dirty:= false;
    self.keyinput:= TKeyInput.Create
  end; 
  
  destructor TWindow.Destroy; 
  begin
    FreeReturnData;
    keyinput.Free;
    inherited Destroy; 
  end;

  function TWindow.GetNativeWindow: x.TWindow;
  begin
    result := self.window;
  end;

  procedure TWindow.GetTargetDimensions(var w, h: integer); 
  var
    Old_Handler: TXErrorHandler;
    Attrib: TXWindowAttributes;
    newx, newy: integer;
    childwindow: x.TWindow;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    if XGetWindowAttributes(display, window, @Attrib) <> 0 Then
    begin
      { I don't think we need this XTranslateCoordinates... :D }
      XTranslateCoordinates(display, window, RootWindow(display, screennum), 0,0, @newx, @newy, @childwindow);
      W := Attrib.Width;
      H := Attrib.Height;
    end else
    begin
      { TODO: Raise Exception because the Window does not exist? }
      W := -1;
      H := -1;
    end;
    XSetErrorHandler(Old_Handler);
  end;

  function TWindow.TargetValid: boolean;
  var
    old_handler: TXErrorHandler;
    Attrib: TXWindowAttributes;
  begin
    old_handler := XSetErrorHandler(@MufasaXErrorHandler);
    //This was in the repos, but it doesn't seem to work...
    //Maybe I missed something?
    result:= XGetWindowAttributes(display, window, @Attrib) <> 0;
    XSetErrorHandler(old_handler);
  end;

  procedure TWindow.ActivateClient;
  var
     Old_Handler: TXErrorHandler;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    { TODO: Check if Window is valid? }
    XSetInputFocus(display,window,RevertToParent,CurrentTime);
    XFlush(display);
    XSetErrorHandler(Old_Handler);
  end;
  
  function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;  
  var
    Old_Handler: TXErrorHandler;
    w,h: integer;
  begin
    GetTargetDimensions(w,h);
    if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) then
      raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);
    if dirty then
      raise Exception.CreateFmt('ReturnData was called again without freeing'+
                                ' the previously used data. Do not forget to'+
                                ' call FreeReturnData', []);
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    buffer := XGetImage(display, window, xs, ys, width, height, AllPlanes, ZPixmap);
    if buffer = nil then
    begin
      Writeln('ReturnData: XGetImage Error. Dumping data now:');
      Writeln('xs, ys, width, height: ' + inttostr(xs) + ', '  + inttostr(ys) +
              ', ' + inttostr(width) + ', ' + inttostr(height));
      Result.Ptr := nil;
      Result.IncPtrWith := 0;
      XSetErrorHandler(Old_Handler);
      raise Exception.CreateFMT('TMWindow.ReturnData: ReturnData: XGetImage Error', []);
      exit;
    end;
    Result.Ptr := PRGB32(buffer^.data);
    Result.IncPtrWith := 0;
    Result.RowLen := width;
    dirty:= true;
    XSetErrorHandler(Old_Handler);
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
   Old_Handler: TXErrorHandler;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    XQueryPointer(display,window,@root,@child,@b,@b,@x,@y,@xmask);
    XSetErrorHandler(Old_Handler);
  end;
  procedure TWindow.MoveMouse(x,y: integer);
  var
   Old_Handler: TXErrorHandler;
    w,h: integer;
  begin
    GetTargetDimensions(w, h);
    if (x < 0) or (y < 0) or (x > w) or (y > h) then
      raise Exception.CreateFmt('SetMousePos: X, Y (%d, %d) is not valid (0,0,%d,%d)', [x, y, w, h]);
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    XWarpPointer(display, 0, window, 0, 0, 0, 0, X, Y);
    XFlush(display);
    XSetErrorHandler(Old_Handler);
  end;
  procedure TWindow.HoldMouse(x,y: integer; button: TClickType);
  var
    ButtonP: cuint;
    _isPress: cbool;
    Old_Handler: TXErrorHandler;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    _isPress := cbool(1);
    case button of
      mouse_Left:   ButtonP:= Button1;
      mouse_Middle: ButtonP:= Button2;
      mouse_Right:  ButtonP:= Button3;
    end;
    XTestFakeButtonEvent(display, ButtonP, _isPress, CurrentTime);
    XSetErrorHandler(Old_Handler);
  end;
  procedure TWindow.ReleaseMouse(x,y: integer; button: TClickType);
  var
    ButtonP: cuint;
    _isPress: cbool;
    Old_Handler: TXErrorHandler;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    _isPress := cbool(0);
    case button of
      mouse_Left:   ButtonP:= Button1;
      mouse_Middle: ButtonP:= Button2;
      mouse_Right:  ButtonP:= Button3;
    end;
    XTestFakeButtonEvent(display, ButtonP, _isPress, CurrentTime);
    XSetErrorHandler(Old_Handler);
  end;

  function GetSimpleKeyCode(c: char): word;
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
      key:= GetSimpleKeyCode(str[i]);
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
  
//***implementation*** IOManager

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
    begin
      // throw Exception
    end;
    screennum:= DefaultScreen(display);
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
  
end.
