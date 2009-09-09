unit Input;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  mufasatypes, // for common mufasa types
  windowutil // for mufasa window utils
  {$IFDEF LINUX}
  ,x, xlib // for X* stuff
  {$ENDIF};
type
    TMInput = class(TObject)
            constructor Create(Client: TObject);
            destructor Destroy; override;

            procedure GetMousePos(var X, Y: Integer);
            procedure SetMousePos(X, Y: Integer);
            procedure MouseButtonAction(x,y : integer; mClick: TClickType; mPress: TMousePress);
            procedure ClickMouse(X, Y: Integer; mClick: TClickType);

            {
              Possibly change to GetMouseButtonStates? Then people can get the
              states bitwise. Like X and WinAPI.
            }
            function IsMouseButtonDown(mType: TClickType): Boolean;

         public
            Client: TObject;

    end;

implementation

uses
    Client{$IFDEF MSWINDOWS},windows{$ENDIF};

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
end;

destructor TMInput.Destroy;
begin

  inherited;
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

  event.xbutton.send_event := True;
  event.xbutton.same_screen:= True;
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

