unit Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mufasatypes, graphics

  {$IFDEF LINUX}, xlib, x, xutil, ctypes{$ENDIF};

type
    TMWindow = class(TObject)
            function ReturnData(xs, ys, width, height: Integer): PRGB32;
            procedure FreeReturnData;
            procedure GetDimensions(var W, H: Integer);
            function CopyClientToBitmap(xs, ys, xe, ye: integer): TBitmap;

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
    Client, // For the Client Class
    windowutil, // For utilities such as XImageToRawImage
    GraphType // For TRawImage
    ;

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

  // The Root Window is the Desktop. :-)
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

// Too global.
{$IFDEF LINUX}
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
{$ENDIF}

function TMWindow.ReturnData(xs, ys, width, height: Integer): PRGB32;
{$IFDEF LINUX}
var
   Old_Handler: TXErrorHandler;
{$ENDIF}
begin
  case Self.TargetMode of
    w_XWindow:
    begin
      {$IFDEF LINUX}
      { Should be this. }
      Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

      Self.XWindowImage := XGetImage(Self.XDisplay, Self.curWindow, xs, ys, width, height, AllPlanes, ZPixmap);
      if QWord(Self.XWindowImage) = 0 then
      begin
        Writeln('ReturnData: XGetImage Error. Dumping data now:');
        Writeln('xs, ys, width, height: ' + inttostr(xs) + ', '  + inttostr(ys) +
                ', ' + inttostr(width) + ', ' + inttostr(height));
        Result := nil;
        XSetErrorHandler(Old_Handler);
        Exit;
      end;
      WriteLn(IntToStr(Self.XWindowImage^.width) + ', ' + IntToStr(Self.XWindowImage^.height));
      Result := PRGB32(Self.XWindowImage^.data);

      XSetErrorHandler(Old_Handler);
      {$ELSE}
        WriteLn('Windows doesn''t support XImage');
      {$ENDIF}
    End;
  end;
end;

procedure TMWindow.FreeReturnData;
begin
  if Self.TargetMode <> w_XWindow then
  begin
    // throw exception.
    exit;
  end;
  {$IFDEF LINUX}
  if(QWord(Self.XWindowImage) <> 0) then      // 0, nil?
  begin
    XDestroyImage(Self.XWindowImage);
  end;
  {$ENDIF}
end;

function TMWindow.CopyClientToBitmap(xs, ys, xe, ye: integer): TBitmap;
{$IFDEF LINUX}
var
   Old_Handler: TXErrorHandler;
   w, h, ww, hh: Integer;
   Img: PXImage;
   Raw: TRawImage;
   Bmp: TBitmap;
{$ENDIF}
begin
  Self.GetDimensions(w, h);
  writeln(inttostr(xs) + ', ' + inttostr(ys) + ' : ' + inttostr(xe) + ', ' +
          inttostr(ye));
  ww := xe-xs;
  hh := ye-ys;

    case Self.TargetMode Of
       w_XWindow:
       begin
         {$IFDEF LINUX}
         Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

         if(xs < 0) or (ys < 0) or (xe > W) or (ye > H) then
         begin
           writeln('Faulty coordinates');
           XSetErrorHandler(Old_Handler);
           exit;
         end;

         Img := XGetImage(Self.XDisplay, Self.curWindow, xs, ys, ww, hh, AllPlanes, ZPixmap);
         XImageToRawImage(Img, Raw);

         Bmp := TBitmap.Create;
         Bmp.LoadFromRawImage(Raw, False);
         Result := Bmp;

         {
           If you want to use some internal Bitmap system, BitBlt to it here.
           Don't forget to free Bmp!
         }
         //lclintf.BitBlt(Bmps[bitmap].Canvas.Handle, 0, 0, ww + 1, hh + 1, Bmp.Canvas.Handle, xs, ys, SRCCOPY);
         //Bmp.Free;

         XDestroyImage(Img);
         XSetErrorHandler(Old_Handler);
         {$ELSE}
         writeln('Windows and tXWindow');
         {$ENDIF}
       end;
    end;
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
        XTranslateCoordinates(Self.XDisplay, Self.CurWindow, Self.DesktopWindow, 0,0, @newx, @newy, @childwindow);
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

