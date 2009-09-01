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

end.

