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

    Input Class for the Mufasa Macro Library
}

unit Input;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  mufasatypes, // for common mufasa types
  windowutil, // for mufasa window utils
  {$IFDEF LINUX}
  ctypes,x, xlib,xtest,keysym,// for X* stuff
   // do non silent keys/mouse with XTest / TKeyInput.
  {Later on we should use xdotool, as it allows silent input}
  {$ENDIF}
  MMLKeyInput,  lclintf,math;

type
    TMInput = class(TObject)
            constructor Create(Client: TObject);
            destructor Destroy; override;

            procedure GetMousePos(var X, Y: Integer);
            procedure SetMousePos(X, Y: Integer);
            procedure MouseButtonAction(x,y : integer; mClick: TClickType; mPress: TMousePress);
            procedure MouseButtonActionSilent(x,y : integer; mClick: TClickType; mPress: TMousePress);
            procedure ClickMouse(X, Y: Integer; mClick: TClickType);

            procedure KeyUp(key: Word);
            procedure KeyDown(key: Word);
            procedure PressKey(key: Word);
            procedure SendText(text: string);
            function isKeyDown(key: Word): Boolean;

            // Not used yet.
            procedure SetSilent(_Silent: Boolean);

            {
              Possibly change to GetMouseButtonStates? Then people can get the
              states bitwise. Like X and WinAPI.
            }
            function IsMouseButtonDown(mType: TClickType): Boolean;

         public
            Client: TObject;
         private
             // Not used yet.
            Silent: Boolean;
            KeyInput: TMMLKeyInput;

    end;

implementation

uses
    Client,{$IFDEF MSWINDOWS}windows {$ELSE}lcltype{$ENDIF};

{$IFDEF MSWINDOWS}
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

{Mouse}
function SendInput(cInputs: UINT; var pInputs: TInput; cbSize: Integer): UINT; stdcall; external user32 name 'SendInput';
{$ENDIF}

constructor TMInput.Create(Client: TObject);
begin
  inherited Create;
  Self.Client := Client;
  Self.KeyInput := TMMLKeyInput.Create;

end;

destructor TMInput.Destroy;
begin

  Self.KeyInput.Free;
  inherited;
end;

procedure TMInput.KeyUp(key: Word);

begin
  Self.KeyInput.Up(Key);
end;

procedure TMInput.KeyDown(key: Word);

begin
  Self.KeyInput.Down(Key);
end;

procedure TMInput.PressKey(key: Word);
begin
  Self.KeyDown(key);
  Self.KeyUp(key);
end;

{ No using VkKeyScan }
function GetSimpleKeyCode(c: char): word;

begin
  //result := ord(UpCase(c));
  c := lowerCase(c);
  if ((c >= 'a') and (c <= 'z')) then
    Exit(VK_A + (Byte(c) - 97));
   Raise Exception.CreateFMT('GetSimpleKeyCode - char is not in A..z',[]);
end;

procedure TMInput.SendText(text: string);
var
   i: integer;
begin
  for i := 1 to length(text) do
  begin
    if((text[i] >= 'A') and (text[i] <= 'Z')) then
      Self.KeyDown(VK_SHIFT);

    Self.PressKey(ord(upcase(text[i])));
    //Self.PressKey(GetSimpleKeyCode(text[i]));

    if((text[i] >= 'A') and (text[i] <= 'Z')) then
      Self.KeyUp(VK_SHIFT);
  end;
end;

function TMInput.isKeyDown(key: Word): Boolean;
{$IFDEF LINUX}
{var
   key_states: chararr32;
   i, j: integer;
   _key: TKeySym;
   _code: TKeyCode;
   wat: integer; }
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}

  {$ELSE}
  raise Exception.CreateFmt('IsKeyDown isn''t implemented yet on Linux', []);
  {XQueryKeymap(TClient(Client).MWindow.XDisplay, key_states);
  _key :=  VirtualKeyToXKeySym(key);
  _code := XKeysymToKeycode(TClient(Client).MWindow.XDisplay, _key);

  for i := 0 to 31 do
    for j := 7 to 0 do
    begin
      wat := Byte(key_states[i]) and (1 shl (j));
      if wat > 0 then
      begin
        writeln(inttostr((i * 8) + j) + ': ' + inttostr(Byte(key_states[i]) and (1 shl j)));
        writeln(inttostr((i * 8) + j) + ': ' + inttostr(Byte(key_states[i]) and (1 shl (8-j))));
      end;
    end;
  writeln(Format('key: %d, _key: %d, _code: %d', [key, _key, _code]));
  writeln('Wat: ' + inttostr((Byte(key_states[floor(_code / 8)]) and 1 shl (_code mod 8))));
  result := (Byte(key_states[floor(_code / 8)]) and 1 shl (_code mod 8)) > 0; }
  {XQueryKeymap -> Print all values !  }
  {$ENDIF}
end;

procedure TMInput.GetMousePos(var X, Y: Integer);
{$IFDEF LINUX}
var
   b:integer;
   root, child: twindow;
   xmask: Cardinal;
   Old_Handler: TXErrorHandler;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  MousePoint : TPoint;
  Rect : TRect;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Windows.GetCursorPos(MousePoint);
  GetWindowRect(TClient(Client).MWindow.TargetHandle,Rect);
  x := MousePoint.x - Rect.Left;
  y := MousePoint.y - Rect.Top;
  {$ENDIF}
  {$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
  XQueryPointer(TClient(Client).MWindow.XDisplay,TClient(Client).MWindow.CurWindow,@root,@child,@b,@b,@x,@y,@xmask);
  XSetErrorHandler(Old_Handler);
  {$ENDIF}
end;

procedure TMInput.SetMousePos(X, Y: Integer);
{$IFDEF LINUX}
var
   Old_Handler: TXErrorHandler;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  rect : TRect;
{$ENDIF}
begin

{$IFDEF MSWINDOWS}
  GetWindowRect(TClient(Client).MWindow.TargetHandle, Rect);
  Windows.SetCursorPos(x + Rect.Left, y + Rect.Top);
{$ENDIF}

{$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
  XWarpPointer(TClient(Client).MWindow.XDisplay, 0, TClient(Client).MWindow.CurWindow, 0, 0, 0, 0, X, Y);
  XFlush(TClient(Client).MWindow.XDisplay);
  XSetErrorHandler(Old_Handler);
{$ENDIF}

end;

procedure TMInput.MouseButtonAction(x,y : integer; mClick: TClickType; mPress: TMousePress);
{$IFDEF LINUX}
var
  ButtonP: cuint;
  _isPress: cbool;
  Old_Handler: TXErrorHandler;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Input : TInput;
  Rect : TRect;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetWindowRect(TClient(Client).MWindow.TargetHandle, Rect);
  Input.Itype:= INPUT_MOUSE;
  Input.mi.dx:= x + Rect.left;
  Input.mi.dy:= y + Rect.Top;
  if mPress = mouse_Down then
  begin
    case mClick of
      Mouse_Left: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN;
      Mouse_Middle: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN;
      Mouse_Right: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN;
    end;
  end else
   case mClick of
      Mouse_Left: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP;
      Mouse_Middle: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP;
      Mouse_Right: Input.mi.dwFlags:= MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP;
    end;
  SendInput(1,Input, sizeof(Input));
{$ENDIF}

{$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

  if mPress = mouse_Down then
    _isPress := cbool(1)
  else
    _isPress := cbool(0);

  case mClick of
    mouse_Left: ButtonP  := Button1;
    mouse_Middle:ButtonP := Button2;
    mouse_Right: ButtonP := Button3;
  end;

  XTestFakeButtonEvent(TClient(Client).MWindow.XDisplay, ButtonP,
                       _isPress, CurrentTime);

  XSetErrorHandler(Old_Handler);
{$ENDIF}
end;

procedure TMInput.MouseButtonActionSilent(x,y : integer; mClick: TClickType; mPress: TMousePress);
{$IFDEF LINUX}
var
  event : TXEvent;
  Garbage : QWord;
  Old_Handler: TXErrorHandler;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Input : TInput;
  Rect : TRect;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  writeln('Not implemented');
{$ENDIF}

{$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

  FillChar(event,sizeof(TXevent),0);

  if mPress = mouse_Down then
    Event._type:= ButtonPress
  else
    Event._type:= ButtonRelease;

  case mClick of
       mouse_Left: Event.xbutton.button:= Button1;
       mouse_Middle: Event.xbutton.button:= Button2;
       mouse_Right: Event.xbutton.button:= Button3;
  end;

  event.xbutton.send_event := TBool(1); // true if this came from a "send event"
  event.xbutton.same_screen:= TBool(1);
  event.xbutton.subwindow:= 0;  // this can't be right.
  event.xbutton.root := TClient(Client).MWindow.DesktopWindow;
  event.xbutton.window := TClient(Client).MWindow.CurWindow;
  event.xbutton.x_root:= x;
  event.xbutton.y_root:= y;
  event.xbutton.x := x;
  event.xbutton.y := y;
  event.xbutton.state:= 0;
  if(XSendEvent(TClient(Client).MWindow.XDisplay, PointerWindow, True, $fff, @event) = 0) then
    Writeln('Errorrrr :-(');
  XFlush(TClient(Client).MWindow.XDisplay);

  XSetErrorHandler(Old_Handler);
{$ENDIF}
end;

procedure TMInput.ClickMouse(X, Y: Integer; mClick: TClickType);

begin
  Self.SetMousePos(x,y);
  Self.MouseButtonAction(X, Y, mClick, mouse_Down);
  Self.MouseButtonAction(X, Y, mClick, mouse_Up);
end;

procedure TMInput.SetSilent(_Silent: Boolean);
begin
  raise exception.CreateFmt('Input - SetSilent / Silent is not implemented',[]);
  Self.Silent := _Silent;
end;

function TMInput.IsMouseButtonDown(mType: TClickType): Boolean;
{$IFDEF LINUX}
var
   rootx, rooty, x, y:integer;
   root, child: twindow;
   xmask: Cardinal;
   Old_Handler: TXErrorHandler;
{$ENDIF}
begin

{$IFDEF MSWINDOWS}
  case mType of
    Mouse_Left:   Result := (GetAsyncKeyState(VK_LBUTTON) <> 0);
    Mouse_Middle: Result := (GetAsyncKeyState(VK_MBUTTON) <> 0);
    mouse_Right:  Result := (GetAsyncKeyState(VK_RBUTTON) <> 0);
   end;
{$ENDIF}

{$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
  XQueryPointer(TClient(Client).MWindow.XDisplay,TClient(Client).MWindow.CurWindow,@root,@child,@rootx,@rooty,@x,@y,@xmask);

  case mType of
       mouse_Left:   Result := (xmask and Button1Mask) <> 0;
       mouse_Middle: Result := (xmask and Button2Mask) <> 0;
       mouse_Right:  Result := (xmask and Button3Mask) <> 0;
  end;

  XSetErrorHandler(Old_Handler);
{$ENDIF}

end;

end.

