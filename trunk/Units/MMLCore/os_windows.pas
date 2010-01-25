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

     Windows OS specific implementation for Mufasa Macro Library
}

{$mode objfpc}{$H+}
unit os_windows;

interface

  uses
    Classes, SysUtils, mufasatypes, windows, graphics, LCLType, bitmaps, LCLIntf, IOManager, WinKeyInput;
    
  type

    TNativeWindow = Hwnd;

    TKeyInput = class(TWinKeyInput)
      public
        procedure Down(Key: Word);
        procedure Up(Key: Word);
    end;

    TWindow = class(TWindow_Abstract)
      public
        constructor Create(target: Hwnd); 
        destructor Destroy; override;
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;
        function GetColor(x,y : integer) : TColor; override;

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

        function GetNativeWindow: Hwnd;
      private
        handle: Hwnd;
        dc: HDC;
        buffer: TBitmap;
        buffer_raw: prgb32;
        width,height: integer;
        keyinput: TKeyInput;
        procedure ValidateBuffer(w,h:integer);
    end;
    
    TIOManager = class(TIOManager_Abstract)
      public
        constructor Create;
        constructor Create(plugin_dir: string);
        function SetTarget(target: Hwnd): integer; overload;
        procedure SetDesktop; override;
      protected
        DesktopHWND : Hwnd;
        procedure NativeInit; override;
        procedure NativeFree; override;
    end;
    
implementation

  uses GraphType, interfacebase;

  type
    PMouseInput = ^TMouseInput;
    tagMOUSEINPUT = packed record
      dx: Longint;
      dy: Longint;
      mouseData: DWORD;
      dwFlags: DWORD;
      time: DWORD;
      dwExtraInfo: DWORD;
    end;
    TMouseInput = tagMOUSEINPUT;

    PKeybdInput = ^TKeybdInput;
    tagKEYBDINPUT = packed record
      wVk: WORD;
      wScan: WORD;
      dwFlags: DWORD;
      time: DWORD;
      dwExtraInfo: DWORD;
    end;
    TKeybdInput = tagKEYBDINPUT;

    PHardwareInput = ^THardwareInput;
    tagHARDWAREINPUT = packed record
      uMsg: DWORD;
      wParamL: WORD;
      wParamH: WORD;
    end;
    THardwareInput = tagHARDWAREINPUT;
    PInput = ^TInput;
    tagINPUT = packed record
      Itype: DWORD;
      case Integer of
        0: (mi: TMouseInput);
        1: (ki: TKeybdInput);
        2: (hi: THardwareInput);
    end;
    TInput = tagINPUT;

  const
    INPUT_MOUSE = 0;
    INPUT_KEYBOARD = 1;
    INPUT_HARDWARE = 2;

  function SendInput(cInputs: UINT; var pInputs: TInput; cbSize: Integer): UINT; stdcall; external user32 name 'SendInput';

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

  constructor TWindow.Create(target: Hwnd); begin 
    inherited Create;
    self.handle:= target;
    self.dc:= GetWindowDC(target);
    self.buffer:= TBitmap.Create;
    self.buffer.PixelFormat:= pf32bit;
    keyinput:= TKeyInput.Create;
  end; 
  
  destructor TWindow.Destroy; begin
    ReleaseDC(handle,dc);
    buffer.Free;
    keyinput.Free;
    inherited Destroy; 
  end;

  function TWindow.GetNativeWindow: Hwnd;
  begin
    result := handle;
  end;

  function TWindow.TargetValid: boolean;
  begin
    result:= IsWindow(handle);
  end;

  procedure TWindow.ActivateClient;
  begin
    SetForegroundWindow(handle);
  end;

  procedure TWindow.GetTargetDimensions(var w, h: integer);
  var
    Rect : TRect; 
  begin 
    GetWindowRect(handle, Rect);
    w:= Rect.Right - Rect.Left;
    h:= Rect.Bottom - Rect.Top;
  end;
  
  function TWindow.GetColor(x,y : integer) : TColor;
  begin
    result:= GetPixel(self.dc,x,y)
  end;
  
  procedure TWindow.ValidateBuffer(w,h:integer);
  var
     BmpInfo : Windows.TBitmap;
  begin
    if (w <> self.width) or (height <> self.height) then
    begin
      buffer.SetSize(w,h);
      self.width:= w;
      self.height:= h;
      GetObject(buffer.Handle, SizeOf(BmpInfo), @BmpInfo);
      self.buffer_raw := BmpInfo.bmBits;
    end;
  end;
  
  function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;
  var
    temp: PRGB32;
    w,h : integer;
  begin
    GetTargetDimensions(w,h);
    ValidateBuffer(w,h);
    if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) then
      raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);
    BitBlt(self.buffer.Canvas.Handle,0,0, width, height, self.dc, xs,ys, SRCCOPY);
    Result.Ptr:= self.buffer_raw;
    Result.IncPtrWith:= w - width;
    Result.RowLen:= w;
  end;

  procedure TWindow.GetMousePosition(var x,y: integer);
  var
    MousePoint : TPoint;
    Rect : TRect;
  begin
    Windows.GetCursorPos(MousePoint);
    GetWindowRect(handle,Rect);
    x := MousePoint.x - Rect.Left;
    y := MousePoint.y - Rect.Top;
  end;
  procedure TWindow.MoveMouse(x,y: integer);
  var
    rect : TRect;
    w,h: integer;
  begin
    GetWindowRect(handle, Rect);
    x := x + rect.left;
    y := y + rect.top;
    if (x<0) or (y<0) then
      writeln('Negative coords, what now?');
    Windows.SetCursorPos(x, y);
  end;
  procedure TWindow.HoldMouse(x,y: integer; button: TClickType);
  var
    Input : TInput;
    Rect : TRect;
  begin
    GetWindowRect(handle, Rect);
    Input.Itype:= INPUT_MOUSE;
    FillChar(Input,Sizeof(Input),0);
    Input.mi.dx:= x + Rect.left;
    Input.mi.dy:= y + Rect.Top;
    case button of
      Mouse_Left: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN;
      Mouse_Middle: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN;
      Mouse_Right: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN;
    end;
    SendInput(1,Input, sizeof(Input));
  end;
  procedure TWindow.ReleaseMouse(x,y: integer; button: TClickType);
  var
    Input : TInput;
    Rect : TRect;
  begin
    GetWindowRect(handle, Rect);
    Input.Itype:= INPUT_MOUSE;
    FillChar(Input,Sizeof(Input),0);
    Input.mi.dx:= x + Rect.left;
    Input.mi.dy:= y + Rect.Top;
   case button of
      Mouse_Left: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP;
      Mouse_Middle: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP;
      Mouse_Right: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP;
    end;
    SendInput(1,Input, sizeof(Input));
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
      key:= VkKeyScan(str[i]) and $FF;
      HoldKey(key);
      //BenLand100 note: probably should wait here
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
    raise Exception.CreateFmt('IsKeyHeld isn''t implemented yet on Windows', []);
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
     self.DesktopHWND:= GetDesktopWindow;
  end;
  
  procedure TIOManager.NativeFree; 
  begin
  end;
  
  procedure TIOManager.SetDesktop;
  begin
    SetBothTargets(TWindow.Create(DesktopHWND));
  end;
  
  function TIOManager.SetTarget(target: Hwnd): integer;
  begin
    SetBothTargets(TWindow.Create(target));
  end;

end.
