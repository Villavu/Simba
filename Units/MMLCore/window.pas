unit Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mufasatypes

  {$IFDEF LINUX}, xlib, x, xutil, ctypes{$ENDIF};

type
    TMWindow = class(TObject)
            function ReturnData(xs, ys, width, height: Integer): PRGB32;
            procedure FreeReturnData;
            procedure GetDimensions(var W, H: Integer);

            constructor Create(Client: TObject);
            destructor Destroy; override;
        protected
              // Reference to client.
              Client: TObject;

              // Target Window Mode.
              TargetMode: TTargetWindowMode;

              {$IFDEF LINUX}
              // X Display
              XDisplay: PDisplay;

              // Connection Number
              XConnectionNumber: Integer;

              // X Window
              CurWindow: QWord;

              // Desktop Window
              DesktopWindow: QWord;

              // X Screen
              XScreen: PScreen;

              // X Screen Number
              XScreenNum: Integer;

              // The X Image pointer.
              XWindowImage: PXImage;

                 {$IFDEF M_MEMORY_DEBUG}
                  // XImageFreed should be True if there is currently no
                  // XImage loaded. If one is loaded, XImageFreed is true.

                  // If ReturnData is called while XImageFreed is false,
                  // we throw an exception.
                  // Same for FreeReturnData with XImageFreed true.
                  XImageFreed: Boolean;
                  {$ENDIF}

              {$ELSE}

              {$ENDIF}



    end;

implementation

uses
    Client;

constructor TMWindow.Create(Client: TObject);
begin
  inherited Create;
  Self.Client := Client;
  {$IFDEF LINUX}
  Self.TargetMode := w_XWindow;

  Self.XDisplay := XOpenDisplay(nil);
  if Self.XDisplay = nil then
  begin
    // throw Exception
  end;
  Self.XConnectionNumber:= ConnectionNumber(Self.XDisplay);
  Self.XScreen := XDefaultScreenOfDisplay(Self.XDisplay);
  Self.XScreenNum:= DefaultScreen(Self.XDisplay);
  Self.DesktopWindow:= RootWindow(Self.XDisplay, Self.XScreenNum);
  Self.CurWindow:= Self.DesktopWindow;

  {$ELSE}
  // Set Target mode for windows.
  {$ENDIF}

end;

destructor TMWindow.Destroy;
begin

  {$IFDEF LINUX}
  XCloseDisplay(Self.XDisplay);
  {$ENDIF}

  inherited;
end;

function TMWindow.ReturnData(xs, ys, width, height: Integer): PRGB32;
begin

end;

procedure TMWindow.FreeReturnData;
begin

end;


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

procedure TMWindow.GetDimensions(var W, H: Integer);
{$IFDEF LINUX}
var
   Attrib: TXWindowAttributes;
   newx,newy : integer;
   childwindow : x.TWindow;
   Old_Handler: TXErrorHandler;
{$ENDIF}
begin
  case TargetMode of
    w_Window:
    begin
    end;
    w_XWindow:
    begin
      {$IFDEF LINUX}
      Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
      if XGetWindowAttributes(Self.XDisplay, Self.CurWindow, @Attrib) <> 0 Then
      begin
        XTranslateCoordinates(Self.XDisplay, Self.CurWindow, self.DesktopWindow, 0,0,@newx,@newy, @childwindow);
        W := Attrib.Width;
        H := Attrib.Height;
      end else
      begin
        W := -1;
        H := -1;
      end;
      XSetErrorHandler(Old_Handler);
      {$ELSE}
      WriteLn('You dummy! How are you going to use w_XWindow on non Linux systems?');
      {$ENDIF}
    End;
    w_ArrayPtr:
    begin
    end;
  end;
end;



end.

