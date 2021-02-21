unit simba.imagebox;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, comctrls, lmessages, lcltype,
  simba.client, simba.mufasatypes, simba.bitmap, simba.dtm;

type
  TSimbaImageBox = class;

  TSimbaImageBox_ScrollBox = class(TScrollBox)
  protected
    FImageBox: TSimbaImageBox;
    FWidth, FHeight: Int32;

    procedure WMHScroll(var Message: TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean=false; WithThemeSpace: boolean=true); override;
    procedure SetSize(AWidth, AHeight: Int32);

    constructor Create(ImageBox: TSimbaImageBox); reintroduce;
  end;

  TSimbaImageBox_Background = class(TBitmap)
  public
    procedure LoadFromFile(const Filename: string); override;
    procedure LoadFromMufasaBitmap(Bitmap: TMufasaBitmap);
    procedure LoadFromPointer(Data: PRGB32; AWidth, AHeight: Int32);
  end;

  TSimbaImageBox_Overlay = class
  protected
    FImageBox: TSimbaImageBox;
    FBitmap: TBitmap;

    function GetCanvas: TCanvas;
  public
    property Canvas: TCanvas read GetCanvas;
    property Bitmap: TBitmap read FBitmap;

    procedure Clear;

    procedure DebugTPA(TPA: TPointArray);

    function DebugColorCTS0(Color, Tolerance: Int32): Int32;
    function DebugColorCTS1(Color, Tolerance: Int32): Int32;
    function DebugColorCTS2(Color, Tolerance: Int32; Hue, Sat: Extended): Int32;

    function DebugDTM(DTM: TMDTM): Int32;

    constructor Create(ImageBox: TSimbaImageBox);
    destructor Destroy; override;
  end;

  TSimbaImageBox_PaintArea = procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object;

  TSimbaImageBox = class(TCustomControl)
  protected
    FScrollBox: TSimbaImageBox_ScrollBox;
    FStatusBar: TStatusBar;
    FMousePanel: TStatusPanel;
    FDimensionsPanel: TStatusPanel;
    FZoomPanel: TStatusPanel;
    FStatusPanel: TStatusPanel;
    FZoom: Double;
    FWidth: Int32;
    FHeight: Int32;
    FOnPaintArea: TSimbaImageBox_PaintArea;
    FBackground: TSimbaImageBox_Background;
    FOverlay: TSimbaImageBox_Overlay;
    FScroll: record
      X, Y: Int32;
      Active: Boolean;
    end;

    procedure ScrollBoxPaint(Sender: TObject);

    procedure FontChanged(Sender: TObject); override;

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ImageMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ImageMouseEnter(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
    procedure ImageDoubleClick(Sender: TObject);

    function GetCursor: TCursor; override;
    procedure SetCursor(Value: TCursor); override;

    procedure SetZoom(Value: Double);
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDblClick;
  public
    property OnPaintArea: TSimbaImageBox_PaintArea read FOnPaintArea write FOnPaintArea;

    property Background: TSimbaImageBox_Background read FBackground;
    property Overlay: TSimbaImageBox_Overlay read FOverlay;

    property Zoom: Double read FZoom write SetZoom;
    property StatusBar: TStatusBar read FStatusBar;
    property StatusPanel: TStatusPanel read FStatusPanel;

    procedure MoveTo(X, Y: Int32);
    function IsVisible(X, Y: Int32): Boolean; overload;

    procedure BackgroundChanged(UpdateOverlay: Boolean = True; Reset: Boolean = True);

    procedure Update; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  types, math, GraphType, lclintf, FPimage, simba.colormath;

procedure TSimbaImageBox_Background.LoadFromFile(const Filename: string);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create();

  try
    Picture.LoadFromFile(FileName);

    Self.LoadFromRawImage(Picture.Bitmap.RawImage, False);
  finally
    Picture.Free();
  end;
end;

procedure TSimbaImageBox_Background.LoadFromMufasaBitmap(Bitmap: TMufasaBitmap);
begin
  Self.LoadFromRawImage(Bitmap.ToRawImage(), False);
end;

procedure TSimbaImageBox_Background.LoadFromPointer(Data: PRGB32; AWidth, AHeight: Int32);
var
  Upper: PtrUInt;
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Int32;
  Row: procedure(Source, Dest: PByte) is nested;

  procedure RowARGB(Source, Dest: PByte);
  var
    Upper: PtrUInt;
  begin
    Upper := PtrUInt(Source + SourceBytesPerLine);

    while PtrUInt(Source) < Upper do
    begin
      PARGB32(Dest)^.R := PRGB32(Source)^.R;
      PARGB32(Dest)^.G := PRGB32(Source)^.G;
      PARGB32(Dest)^.B := PRGB32(Source)^.B;

      Inc(Source, SizeOf(TRGB32));
      Inc(Dest, SizeOf(TARGB32));
    end;
  end;

  procedure RowBGR(Source, Dest: PByte);
  var
    Upper: PtrUInt;
  begin
    Upper := PtrUInt(Source + SourceBytesPerLine);

    while PtrUInt(Source) < Upper do
    begin
      PRGB24(Dest)^ := PRGB24(Source)^;

      Inc(Source, SizeOf(TRGB32));
      Inc(Dest, SizeOf(TRGB24));
    end;
  end;

  procedure RowBGRA(Source, Dest: PByte);
  var
    Upper: PtrUInt;
  begin
    Upper := PtrUInt(Source + SourceBytesPerLine);

    while PtrUInt(Source) < Upper do
    begin
      PRGB32(Dest)^ := PRGB32(Source)^;

      Inc(Source, SizeOf(TRGB32));
      Inc(Dest, SizeOf(TRGB32));
    end;
  end;

begin
  BeginUpdate();

  Width := AWidth;
  Height := AHeight;

  case DataFormat of
    dfARGB: Row := @RowARGB;
    dfBGRA: Row := @RowBGRA;
    dfBGR:  Row := @RowBGR;
    else
      raise Exception.CreateFmt('TSimbaImageBox_Background.LoadFromPointer: Bitmap format "%d" not supported', [Ord(DataFormat)]);
  end;

  Dest := RawImage.Data;
  DestBytesPerLine := RawImage.Description.BytesPerLine;

  Source := PByte(Data);
  SourceBytesPerLine := Width * SizeOf(TRGB32);

  Upper := PtrUInt(Source + (SourceBytesPerLine * Height));

  while PtrUInt(Source) < Upper do
  begin
    Row(Source, Dest);

    Inc(Source, SourceBytesPerLine);
    Inc(Dest, DestBytesPerLine);
  end;

  EndUpdate();
end;

function TSimbaImageBox_Overlay.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

procedure TSimbaImageBox_Overlay.Clear;
begin
  FBitmap.Canvas.Draw(0, 0, FImageBox.Background);
end;

procedure TSimbaImageBox_Overlay.DebugTPA(TPA: TPointArray);
var
  R, G, B: Byte;

  procedure PixelARGB(constref Ptr: Pointer);
  var
    Pixel: PARGB32 absolute Ptr;
  begin
    Pixel^.A := 0;
    Pixel^.R := R;
    Pixel^.G := G;
    Pixel^.B := B;
  end;

  procedure PixelBGRA(constref Ptr: Pointer);
  var
    Pixel: PRGB32 absolute Ptr;
  begin
    Pixel^.R := R;
    Pixel^.G := G;
    Pixel^.B := B;
    Pixel^.A := 0;
  end;

  procedure PixelBGR(constref Ptr: Pointer);
  var
    Pixel: PRGB24 absolute Ptr;
  begin
    Pixel^.R := R;
    Pixel^.G := G;
    Pixel^.B := B;
  end;

var
  Data: PByte;
  BytesPerLine, BytesPerPixel: Int32;
  P: TPoint;
  Pixel: procedure(constref Pixel: Pointer) is nested;
begin
  if (Length(TPA) = 0) then
    Exit;

  ColorToRGB(FBitmap.Canvas.Pen.Color, R, G, B);

  case FBitmap.DataFormat of
    dfARGB: Pixel := @PixelARGB;
    dfBGRA: Pixel := @PixelBGRA;
    dfBGR:  Pixel := @PixelBGR;
    else
      raise Exception.CreateFmt('TSimbaImageBox_Overlay.DebugTPA: Data format not supported', [Ord(FBitmap.DataFormat)]);
  end;

  FBitmap.BeginUpdate();

  try
    Data := FBitmap.RawImage.Data;
    BytesPerLine := FBitmap.RawImage.Description.BytesPerLine;
    BytesPerPixel := FBitmap.RawImage.Description.BitsPerPixel div 8;

    for P in TPA do
      Pixel(Pointer(Data + (P.Y * BytesPerLine + P.X * BytesPerPixel)));
  finally
    FBitmap.EndUpdate();
  end;
end;

function TSimbaImageBox_Overlay.DebugDTM(DTM: TMDTM): Int32;
var
  Mufasa: TMufasaBitmap;
  Client: TClient;
  TPA: TPointArray;
  P: TPoint;
begin
  Self.Clear();

  Mufasa := TMufasaBitmap.Create();
  Mufasa.LoadFromTBitmap(FImageBox.Background);

  Client := TClient.Create();
  Client.IOManager.SetTarget(Mufasa);

  if Client.MFinder.FindDTMs(DTM, TPA, 0, 0, Bitmap.Width - 1, Bitmap.Height - 1) then
  begin
    FBitmap.BeginUpdate(True);

    for P in TPA do
    begin
      FBitmap.Canvas.Line(P.X - 4, P.Y - 4, P.X + 5, P.Y + 5);
      FBitmap.Canvas.Line(P.X + 4, P.Y - 4, P.X - 5, P.Y + 5);
    end;

    FBitmap.EndUpdate();
  end;

  Result := Length(TPA);

  Client.Free();
  Mufasa.Free();
end;

function TSimbaImageBox_Overlay.DebugColorCTS0(Color, Tolerance: Int32): Int32;
var
  Mufasa: TMufasaBitmap;
  Client: TClient;
  TPA: TPointArray;
begin
  Self.Clear();

  Mufasa := TMufasaBitmap.Create();
  Mufasa.LoadFromTBitmap(FImageBox.Background);

  Client := TClient.Create();
  Client.IOManager.SetTarget(Mufasa);

  Client.MFinder.SetToleranceSpeed(0);
  if Client.MFinder.FindColorsTolerance(TPA, Color, 0, 0, Bitmap.Width - 1, Bitmap.Height - 1, Tolerance) then
    DebugTPA(TPA);

  Result := Length(TPA);

  Client.Free();
  Mufasa.Free();
end;

function TSimbaImageBox_Overlay.DebugColorCTS1(Color, Tolerance: Int32): Int32;
var
  Mufasa: TMufasaBitmap;
  Client: TClient;
  TPA: TPointArray;
begin
  Self.Clear();

  Mufasa := TMufasaBitmap.Create();
  Mufasa.LoadFromTBitmap(FImageBox.Background);

  Client := TClient.Create();
  Client.IOManager.SetTarget(Mufasa);

  Client.MFinder.SetToleranceSpeed(1);
  if Client.MFinder.FindColorsTolerance(TPA, Color, 0, 0, Bitmap.Width - 1, Bitmap.Height - 1, Tolerance) then
    DebugTPA(TPA);

  Result := Length(TPA);

  Client.Free();
  Mufasa.Free();
end;

function TSimbaImageBox_Overlay.DebugColorCTS2(Color, Tolerance: Int32; Hue, Sat: Extended): Int32;
var
  Mufasa: TMufasaBitmap;
  Client: TClient;
  TPA: TPointArray;
begin
  Self.Clear();

  Mufasa := TMufasaBitmap.Create();
  Mufasa.LoadFromTBitmap(FImageBox.Background);

  Client := TClient.Create();
  Client.IOManager.SetTarget(Mufasa);

  Client.MFinder.SetToleranceSpeed(2);
  Client.MFinder.SetToleranceSpeed2Modifiers(Hue, Sat);
  if Client.MFinder.FindColorsTolerance(TPA, Color, 0, 0, Bitmap.Width - 1, Bitmap.Height - 1, Tolerance) then
    DebugTPA(TPA);

  Result := Length(TPA);

  Client.Free();
  Mufasa.Free();
end;

constructor TSimbaImageBox_Overlay.Create(ImageBox: TSimbaImageBox);
begin
  inherited Create();

  FImageBox := ImageBox;
  FBitmap := TBitmap.Create();
end;

destructor TSimbaImageBox_Overlay.Destroy;
begin
  FBitmap.Free();

  inherited Destroy();
end;

procedure TSimbaImageBox_ScrollBox.GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  PreferredWidth := FWidth;
  PreferredHeight := FHeight;
end;

procedure TSimbaImageBox_ScrollBox.WMHScroll(var Message: TLMHScroll);
begin
  if Message.ScrollCode = SB_LINEDOWN then
    HorzScrollBar.Position := HorzScrollBar.Position + Ceil(FImageBox.Zoom)
  else
  if Message.ScrollCode = SB_LINEUP then
    HorzScrollBar.Position := HorzScrollBar.Position - Ceil(FImageBox.Zoom)
  else
  begin
    Message.Pos := (Message.Pos div Ceil(FImageBox.Zoom)) * Ceil(FImageBox.Zoom);

    inherited WMHScroll(Message);
  end;

  FImageBox.Update();
end;

procedure TSimbaImageBox_ScrollBox.WMVScroll(var Message: TLMVScroll);
begin
  if Message.ScrollCode = SB_LINEDOWN then
    VertScrollBar.Position := VertScrollBar.Position + Ceil(FImageBox.Zoom)
  else
  if Message.ScrollCode = SB_LINEUP then
    VertScrollBar.Position := VertScrollBar.Position - Ceil(FImageBox.Zoom)
  else
  begin
    Message.Pos := (Message.Pos div Ceil(FImageBox.Zoom)) * Ceil(FImageBox.Zoom);

    inherited WMVScroll(Message);
  end;

  FImageBox.Update();
end;

procedure TSimbaImageBox_ScrollBox.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TSimbaImageBox_ScrollBox.SetSize(AWidth, AHeight: Int32);
begin
  FWidth  := AWidth;
  FHeight := AHeight;

  ComputeScrollbars();
end;

constructor TSimbaImageBox_ScrollBox.Create(ImageBox: TSimbaImageBox);
begin
  inherited Create(ImageBox);

  FImageBox := ImageBox;
end;

procedure TSimbaImageBox.SetZoom(Value: Double);
var
  Current: TPoint;
begin
  if (FZoom <> 0) then
  begin
    Current := FScrollBox.ScreenToClient(Mouse.CursorPos); // Zoom into where the cursor is
    Current.X += FScrollBox.HorzScrollBar.Position;
    Current.Y += FScrollBox.VertScrollBar.Position;
    Current.X := Trunc(Current.X / FZoom);
    Current.Y := Trunc(Current.Y / FZoom);
  end else
    Current := Point(-1, -1);

  if (Value = 0.50) or (Value = 1.00) or (Value = 2.00) or (Value = 4.00) or (Value = 8.00) or (Value = 16.00) then
  begin
    FZoom := Value;
    FZoomPanel.Text := IntToStr(Round(Value * 100)) + '%';
    FScrollBox.SetSize(Trunc(FBackground.Width * FZoom), Trunc(FBackground.Height * FZoom));

    if (Current.X <> -1) and (Current.Y <> -1) then
      MoveTo(Current.X, Current.Y);

    Self.Update();
  end;
end;

procedure TSimbaImageBox.ScrollBoxPaint(Sender: TObject);
var
  LocalRect, ScreenRect: TRect;
  W, H: Int32;
  DC: HDC;
begin
  W := FScrollBox.ClientWidth;
  while W mod Ceil(1 * FZoom) <> 0 do
    W += 1;

  H := FScrollBox.ClientHeight;
  while H mod Ceil(1 * FZoom) <> 0 do
    H += 1;

  ScreenRect.Left := FScrollBox.HorzScrollBar.Position;
  ScreenRect.Top := FScrollBox.VertScrollBar.Position;
  ScreenRect.Width := W;
  ScreenRect.Height := H;

  LocalRect.Left := Trunc(FScrollBox.HorzScrollBar.Position / FZoom);
  LocalRect.Top := Trunc(FScrollBox.VertScrollBar.Position / FZoom);
  LocalRect.Width := Trunc(W / FZoom);
  LocalRect.Height := Trunc(H / FZoom);

  FScrollBox.Canvas.Brush.Color := clBlack;
  FScrollBox.Canvas.FillRect(ScreenRect);

  if (FOnPaintArea <> nil) then
  begin
    BitBlt(FOverlay.Canvas.Handle,
           LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height,
           FBackground.Canvas.Handle,
           LocalRect.Left, LocalRect.Top,
           SRCCOPY);

    FOnPaintArea(Self, FOverlay.Canvas, LocalRect);
  end;

  if (FOverlay.Bitmap.Width > 0) and (FOverlay.Bitmap.Height > 0) then
    DC := FOverlay.Canvas.Handle
  else
    DC := FBackground.Canvas.Handle;

  StretchBlt(
    FScrollBox.Canvas.Handle,
    ScreenRect.Left, ScreenRect.Top, ScreenRect.Width, ScreenRect.Height,
    DC,
    LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height,
    SRCCOPY
  );
end;

procedure TSimbaImageBox.BackgroundChanged(UpdateOverlay: Boolean; Reset: Boolean);
begin
  if (FBackground.Width <> FWidth) or (FBackground.Height <> FHeight) then
  begin
    FWidth := FBackground.Width;
    FHeight := FBackground.Height;

    FDimensionsPanel.Text := Format('%dx%d', [FBackground.Width, FBackground.Height]);

    if Reset then
    begin
      Zoom := 1;
      MoveTo(0, 0);
    end;
  end;

  if UpdateOverlay then
  begin
    FOverlay.Bitmap.LoadFromRawImage(FBackground.RawImage, False);
    FOverlay.Bitmap.Canvas.AntialiasingMode := amOff;
  end;
end;

procedure TSimbaImageBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FMousePanel <> nil) and (FDimensionsPanel <> nil) and (FZoomPanel <> nil) and (FStatusBar <> nil) then
    with TBitmap.Create() do
    try
      Canvas.Font := Self.Font;

      with Canvas.TextExtent('999x999') do
      begin
        FMousePanel.Width := Width*2;
        FDimensionsPanel.Width := Width*2;
        FZoomPanel.Width := Width;
        FStatusBar.Height := Height;
      end;
    finally
      Free();
    end;
end;

procedure TSimbaImageBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CanSetFocus() then
    SetFocus();

  X := X + FScrollBox.HorzScrollBar.Position;
  Y := Y + FScrollBox.VertScrollBar.Position;

  if (Button = mbRight) then
  begin
    FScroll.X := X;
    FScroll.Y := Y;
    FScroll.Active := True;
    FScrollBox.Cursor := crSizeAll;
  end;

  if (OnMouseDown <> nil) then
    OnMouseDown(Self, Button, Shift, Trunc(X / FZoom), Trunc(Y / FZoom));
end;

procedure TSimbaImageBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X + FScrollBox.HorzScrollBar.Position;
  Y := Y + FScrollBox.VertScrollBar.Position;

  if (Button = mbRight) then
  begin
    FScroll.Active := False;
    FScrollBox.Cursor := crDefault;
  end;

  if (OnMouseUp <> nil) then
    OnMouseUp(Self, Button, Shift, Trunc(X / FZoom), Trunc(Y / FZoom));
end;

procedure TSimbaImageBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Steps: Int32;
begin
  X := X + FScrollBox.HorzScrollBar.Position;
  Y := Y + FScrollBox.VertScrollBar.Position;

  if FScroll.Active then
  begin
    // Move in steps of pixel size
    if (Abs(FScroll.Y - Y) > 0) then
    begin
      Steps := Abs(FScroll.Y - Y) div Ceil(FZoom);

      if Steps > 0 then
      begin
        if (FScroll.Y - Y > 0) then
          FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + Steps * Ceil(FZoom)
        else
          FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - Steps * Ceil(FZoom)
      end;
    end;

    if (Abs(FScroll.X - X) > 0) then
    begin
      Steps := Abs(FScroll.X - X) div Ceil(FZoom);

      if Steps > 0 then
      begin
        if (FScroll.X - X > 0) then
          FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position + Steps * Ceil(FZoom)
        else
          FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position - Steps * Ceil(FZoom)
      end;
    end;

    Update();
  end else
  begin
    if FZoom <> 1 then
    begin
      X := Trunc(X / FZoom);
      Y := Trunc(Y / FZoom);
    end;

    FMousePanel.Text := Format('%d, %d', [X, Y]);

    if (OnMouseMove <> nil) then
      OnMouseMove(Self, Shift, X, Y);
  end;
end;

procedure TSimbaImageBox.ImageMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
    Zoom := Zoom * 2;
end;

procedure TSimbaImageBox.ImageMouseEnter(Sender: TObject);
begin
  if (OnMouseEnter <> nil) then
    OnMouseEnter(Self);
end;

procedure TSimbaImageBox.ImageMouseLeave(Sender: TObject);
begin
  FScroll.Active := False;
  FScrollBox.Cursor := crDefault;
  FMousePanel.Text := '(-1, -1)';

  if (OnMouseLeave <> nil) then
    OnMouseLeave(Self);
end;

procedure TSimbaImageBox.ImageDoubleClick(Sender: TObject);
begin
  if (OnDblClick <> nil) then
    OnDblClick(Self);
end;

procedure TSimbaImageBox.ImageMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
    Zoom := Zoom / 2;
end;

procedure TSimbaImageBox.SetCursor(Value: TCursor);
begin
  FScrollBox.Cursor := Value;
end;

function TSimbaImageBox.GetCursor: TCursor;
begin
  Result := FScrollBox.Cursor;
end;

procedure TSimbaImageBox.MoveTo(X, Y: Int32);
var
  W, H: Int32;
begin
  FScrollBox.HandleNeeded();

  W := FScrollBox.ClientWidth;
  while W mod Ceil(1 * FZoom) <> 0 do
    W += 1;

  H := FScrollBox.ClientHeight;
  while H mod Ceil(1 * FZoom) <> 0 do
    H += 1;

  if X + Ceil(W / FZoom) >= FBackground.Width then
    X := FBackground.Width - Ceil((W div 2) / FZoom);
  if Y + Ceil(H / FZoom) >= FBackground.Height then
    Y := FBackground.Height - Ceil((H div 2) / FZoom);

  X := Trunc(X * FZoom) - (W div 2);
  Y := Trunc(Y * FZoom) - (H div 2);

  X := (X div Ceil(Zoom)) * Ceil(Zoom);
  Y := (Y div Ceil(Zoom)) * Ceil(Zoom);

  FScrollBox.HorzScrollBar.Position := X;
  FScrollBox.VertScrollBar.Position := Y;
end;

function TSimbaImageBox.IsVisible(X, Y: Int32): Boolean;
begin
  X := Trunc(X * FZoom);
  Y := Trunc(Y * FZoom);

  Result := InRange(X, FScrollBox.HorzScrollBar.Position, FScrollBox.HorzScrollBar.Position + FScrollBox.ClientWidth) and
            InRange(Y, FScrollBox.VertScrollBar.Position, FScrollBox.VertScrollBar.Position + FScrollBox.ClientHeight);
end;

procedure TSimbaImageBox.Update;
begin
  {$IFDEF WINDOWS}
  FScrollBox.Repaint();
  {$ELSE}
  FScrollBox.Update();
  {$ENDIF}
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackground := TSimbaImageBox_Background.Create();
  FOverlay := TSimbaImageBox_Overlay.Create(Self);

  FScrollBox := TSimbaImageBox_ScrollBox.Create(Self);
  FScrollBox.Parent := Self;
  FScrollBox.Align := alClient;
  FScrollBox.BorderStyle := bsNone;
  FScrollBox.HorzScrollBar.Tracking := True;
  FScrollBox.VertScrollBar.Tracking := True;
  FScrollBox.DoubleBuffered := True;
  FScrollBox.OnPaint := @ScrollBoxPaint;
  FScrollBox.OnMouseDown := @ImageMouseDown;
  FScrollBox.OnMouseUp := @ImageMouseUp;
  FScrollBox.OnMouseMove := @ImageMouseMove;
  FScrollBox.OnMouseWheelDown := @ImageMouseWheelDown;
  FScrollBox.OnMouseWheelUp := @ImageMouseWheelUp;
  FScrollBox.OnMouseLeave := @ImageMouseLeave;
  FScrollBox.OnMouseEnter := @ImageMouseEnter;
  FScrollBox.OnDblClick := @ImageDoubleClick;
  FScrollBox.Canvas.AntialiasingMode := amOff;

  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.SimplePanel := False;

  FMousePanel := FStatusBar.Panels.Add();
  FMousePanel.Text := '(-1, -1)';
  FDimensionsPanel := FStatusBar.Panels.Add();
  FDimensionsPanel.Text := '(0, 0)';
  FZoomPanel := FStatusBar.Panels.Add();
  FZoomPanel.Text := '100%';
  FStatusPanel := FStatusBar.Panels.Add();

  FWidth := 0;
  FHeight := 0;

  Zoom := 1;

  FontChanged(Self);
end;

destructor TSimbaImageBox.Destroy;
begin
  FOverlay.Free();
  FBackground.Free();

  inherited Destroy();
end;

end.

