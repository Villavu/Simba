unit windowselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ctypes,
  window, windowutil,
  {$IFNDEF MSWINDOWS}x, xlib
  {$ELSE}
  windows
  {$ENDIF}

  ;

type
    TMWindowSelector = class(TObject)
          constructor Create(aWindow: TMWindow);
          destructor Destroy; override;

          {$IFDEF LINUX}
          function Drag: x.TWindow;
          {$ELSE}
          function Drag: Hwnd;
          {$ENDIF}

    public
          Window: TMWindow;

    end;


implementation


constructor TMWindowSelector.Create(aWindow: TMWindow);
begin
  inherited create;

  Self.Window := aWindow;

end;


destructor TMWindowSelector.Destroy;
begin

  inherited;
end;

{$IFDEF LINUX}
function TMWindowSelector.Drag: x.TWindow;
var
  Tempwindow : x.TWindow;
  root : x.TWindow;
  subwindow : x.TWindow;
  x_root, y_root : cint;
  xmask : cuint;
  x, y : cint;
  Old_Handler : TXErrorHandler;

begin
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

  Result := 0;

  repeat
    XQueryPointer(Window.XDisplay, Window.DesktopWindow, @root,
                @Tempwindow, @x_root, @y_root,
                @x, @y, @xmask);
    subwindow:= Tempwindow;

    while subwindow <> 0 do
    begin
        Tempwindow := subwindow;
        XQueryPointer(Window.XDisplay, Tempwindow, @root,
                 @subwindow, @x_root, @y_root,
                 @x, @y, @xmask);
    end;
    if Result <> Tempwindow then
    begin
      WriteLn('Changing Window to: ' + IntToStr(Tempwindow));
      Result := Tempwindow;
    end;

    Sleep(16);

  until (xmask and Button1Mask) = 0;

  XSetErrorHandler(Old_handler);
end;

{$ELSE}

function TMWindowSelector.Drag: Hwnd;

begin
  // Moet jij maar ff doen, ray.
end;
{$ENDIF}



end.

