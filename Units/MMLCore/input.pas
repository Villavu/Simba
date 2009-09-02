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
            procedure MouseButtonAction(X, Y: Integer; mClick: TClickType; mPress: TMousePress);
            procedure ClickMouse(X, Y: Integer; mClick: TClickType);
            function IsMouseButtonDown(mType: TClickType): Boolean;

         public
            Client: TObject;

    end;

implementation

uses
    Client;

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
begin
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
begin
{$IFDEF LINUX}
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
  XWarpPointer(TClient(Client).MWindow.XDisplay, 0, TClient(Client).MWindow.CurWindow, 0, 0, 0, 0, X, Y);
  XFlush(TClient(Client).MWindow.XDisplay);
  XSetErrorHandler(Old_Handler);
{$ENDIF}
end;

procedure TMInput.MouseButtonAction(X, Y: Integer; mClick: TClickType; mPress: TMousePress);
{$IFDEF LINUX}
var
  event : TXEvent;
  Garbage : QWord;
  Old_Handler: TXErrorHandler;
{$ENDIF}

begin
  Self.SetMousePos(X, Y);
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

  event.xbutton.send_event := 1;
  event.xbutton.same_screen:= 1;
  event.xbutton.subwindow:= 0;
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

// ff omzetten naar MouseButtonDown(), en dan Click gewoon down en dan up.
// holdmouse releasemouse
procedure TMInput.ClickMouse(X, Y: Integer; mClick: TClickType);

begin
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

