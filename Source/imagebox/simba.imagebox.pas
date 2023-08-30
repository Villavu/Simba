{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls, LCLType,
  simba.mufasatypes, simba.bitmap, simba.dtm, simba.imagebox_bitmap,
  simba.colormath, simba.colormath_distance;

type
  TSimbaImageBox_ScrollBox = class(TScrollBox)
  protected
    FWidth, FHeight: Integer;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EraseBackground(DC: HDC); override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: Integer; Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

  PSimbaImageBoxPaintAreaEvent = ^TSimbaImageBoxPaintAreaEvent;
  TSimbaImageBoxPaintAreaEvent = procedure(Sender: TObject; Bitmap: TSimbaImageBoxBitmap; R: TRect) of object;

  PSimbaImageBox = ^TSimbaImageBox;
  TSimbaImageBox = class(TWinControl)
  protected
    FScrollBox: TSimbaImageBox_ScrollBox;
    FStatusBar: TStatusBar;
    FMousePanel: TStatusPanel;
    FDimensionsPanel: TStatusPanel;
    FZoomPanel: TStatusPanel;
    FStatusPanel: TStatusPanel;
    FZoom: Single;
    FZoomPixels: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FOnPaintArea: TSimbaImageBoxPaintAreaEvent;
    FBackground: TBitmap;
    FScroll: record
      X, Y: Integer;
      Active: Boolean;
    end;
    FMousePoint: TPoint;
    FBitmap: TSimbaImageBoxBitmap;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect); virtual;

    function GetScrolledRect: TRect; virtual;

    procedure ScrollBoxPaint(Sender: TObject); virtual;

    procedure FontChanged(Sender: TObject); override;
    procedure BackgroundChanged(Sender: TObject); virtual;

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure ImageMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure ImageMouseEnter(Sender: TObject); virtual;
    procedure ImageMouseLeave(Sender: TObject); virtual;
    procedure ImageDoubleClick(Sender: TObject); virtual;
    procedure ImageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;

    function GetCursor: TCursor; override;
    procedure SetCursor(Value: TCursor); override;

    procedure SetParent(Value: TWinControl); override;
    procedure SetZoom(Value: Single); virtual;
  public
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDblClick;
    property OnPaintArea: TSimbaImageBoxPaintAreaEvent read FOnPaintArea write FOnPaintArea;

    property MousePoint: TPoint read FMousePoint;
    property Zoom: Single read FZoom write SetZoom;
    property StatusBar: TStatusBar read FStatusBar;
    property StatusPanel: TStatusPanel read FStatusPanel;
    property Background: TBitmap read FBackground write FBackground;

    procedure MoveTo(X, Y: Integer);
    function IsVisible(X, Y: Integer): Boolean; overload;

    function FindDTM(DTM: TDTM): TPointArray;
    function FindColor(AColor: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
    function MatchColor(AColor: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;

    procedure SetBackground(Data: PColorBGRA; AWidth, AHeight: Integer); overload;
    procedure SetBackground(FileName: String); overload;
    procedure SetBackground(Bitmap: TSimbaImage); overload;
    procedure SetBackground(Window: TWindowHandle); overload;

    procedure Paint;

    constructor Create(AOwner: TComponent); virtual; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Math, GraphType, LCLIntf,
  simba.finder, simba.image_lazbridge, simba.windowhandle;

procedure TSimbaImageBox_ScrollBox.GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  PreferredWidth  := FWidth;
  PreferredHeight := FHeight;
end;

procedure TSimbaImageBox_ScrollBox.EraseBackground(DC: HDC);
begin
  Brush.Color := clBlack;

  inherited;
end;

procedure TSimbaImageBox_ScrollBox.SetSize(AWidth, AHeight: Integer);
begin
  FWidth  := AWidth;
  FHeight := AHeight;

  ComputeScrollbars();
end;

constructor TSimbaImageBox_ScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
end;

procedure TSimbaImageBox.SetZoom(Value: Single);
var
  Center: TPoint;
begin
  Center := TPoint.Create(0, 0);
  if (FZoom <> 0) then
  begin
    Center   := FScrollBox.ScreenToClient(Mouse.CursorPos); // Zoom into where the cursor is
    Center.X := Trunc((Center.X + FScrollBox.HorzScrollBar.Position) / FZoom);
    Center.Y := Trunc((Center.Y + FScrollBox.VertScrollBar.Position) / FZoom);
  end;

  if (Value = 0.25) or (Value = 0.50) or (Value = 1.00) or (Value = 2.00) or (Value = 4.00) or (Value = 8.00) or (Value = 16.00) or (Value = 32.00) then
  begin
    FZoom := Value;

    if (FZoom >= 1) then
    begin
      FZoomPixels := Trunc(FZoom * 1);

      FScrollBox.SetSize(
        (FWidth * FZoomPixels),
        (FHeight * FZoomPixels)
      );
    end else
    begin
      if (FZoom = 0.50) then
        FZoomPixels := 2
      else
      if (FZoom = 0.25) then
        FZoomPixels := 4;

      FScrollBox.SetSize(
        FWidth div FZoomPixels,
        FHeight div FZoomPixels
      );
    end;

    FDimensionsPanel.Text := Format('%dx%d', [FBackground.Width, FBackground.Height]);
    FZoomPanel.Text := IntToStr(Round(FZoom * 100)) + '%';

    if (Center.X > 0) and (Center.Y > 0) then
      MoveTo(Center.X, Center.Y);

    Paint();
  end;
end;

generic procedure ZoomOut<_T>(Ratio, SrcX, SrcY, LoopEndX, LoopEndY: Integer; SrcImg, DestImg: TRawImage);
type
  PType = ^_T;
var
  SourceData, DestData: PByte;
  SourceBytesPerLine, DestBytesPerLine: PtrUInt;
var
  X, Y: Integer;
  SrcStart, SrcPtr, DestPtr: PByte;
begin
  SourceData         := SrcImg.Data;
  SourceBytesPerLine := SrcImg.Description.BytesPerLine;
  DestData           := DestImg.Data;
  DestBytesPerLine   := DestImg.Description.BytesPerLine;

  SrcStart := SourceData + (SrcY * SourceBytesPerLine);

  for Y := 0 to LoopEndY do
  begin
    SrcPtr := SrcStart + ((Y * Ratio) * SourceBytesPerLine);
    DestPtr := DestData;

    for X := 0 to LoopEndX do
    begin
      PType(DestPtr)^ := PType(SrcPtr + ((SrcX + X * Ratio) * SizeOf(_T)))^;

      Inc(DestPtr, SizeOf(_T));
    end;

    Inc(DestData, DestBytesPerLine);
  end
end;

generic procedure ZoomIn<_T>(Ratio, SrcX, SrcY, LoopEndX, LoopEndY: Integer; SrcImg, DestImg: TRawImage);
type
  PType = ^_T;
var
  SourceData, DestData: PByte;
  SourceBytesPerLine, DestBytesPerLine: PtrUInt;
var
  X, Y: Integer;
  SrcStart, SrcPtr, DestPtr: PByte;
begin
  SourceData         := SrcImg.Data;
  SourceBytesPerLine := SrcImg.Description.BytesPerLine;
  DestData           := DestImg.Data;
  DestBytesPerLine   := DestImg.Description.BytesPerLine;

  SrcStart := SourceData + (SrcY * SourceBytesPerLine);

  for Y := 0 to LoopEndY do
  begin
    SrcPtr := SrcStart + ((Y div Ratio) * SourceBytesPerLine);
    DestPtr := DestData;

    for X := 0 to LoopEndX do
    begin
      PType(DestPtr)^ := PType(SrcPtr + ((SrcX + X div Ratio) * SizeOf(_T)))^;

      Inc(DestPtr, SizeOf(_T));
    end;

    Inc(DestData, DestBytesPerLine);
  end
end;

generic procedure NoZoom<_T>(SrcX, SrcY, Wid, Hei: Integer; SrcImg, DestImg: TRawImage);
var
  SourceData, DestData: PByte;
  SourceBytesPerLine, DestBytesPerLine: PtrUInt;
  Y: Integer;
  RowSize: Integer;
begin
  SourceData         := SrcImg.Data;
  SourceBytesPerLine := SrcImg.Description.BytesPerLine;
  DestData           := DestImg.Data;
  DestBytesPerLine   := DestImg.Description.BytesPerLine;

  SourceData := SourceData + (SrcY * SourceBytesPerLine)
                           + (SrcX * SizeOf(_T));

  RowSize := Wid * SizeOf(_T);
  for Y := 0 to Hei - 1 do
  begin
    Move(SourceData^, DestData^, RowSize);

    Inc(SourceData, SourceBytesPerLine);
    Inc(DestData, DestBytesPerLine);
  end;
end;

procedure TSimbaImageBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  ImageKeyDown(Self, Key, Shift);
end;

procedure TSimbaImageBox.DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect);
begin
  if Assigned(FOnPaintArea) then
    FOnPaintArea(Self, Bitmap, R);
end;

function TSimbaImageBox.GetScrolledRect: TRect;
begin
  Result := FScrollBox.GetScrolledClientRect();

  Result.Left   := Max(0, Result.Left   + ((FZoomPixels - Result.Left) mod FZoomPixels));
  Result.Top    := Max(0, Result.Top    + ((FZoomPixels - Result.Top)  mod FZoomPixels));
  Result.Right  := Result.Right  + FZoomPixels;
  Result.Bottom := Result.Bottom + FZoomPixels;
end;

procedure TSimbaImageBox.ScrollBoxPaint(Sender: TObject);
type
  PixelRGB  = packed record R,G,B: Byte; end;
  PixelRGBA = Integer;

  procedure RenderZoomOut(Ratio: Integer; src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize ZoomOut<PixelRGB>(Ratio, SrcX, SrcY, FBitmap.Width - 1, FBitmap.Height - 1, Src.RawImage, Dest.RawImage);
      4: specialize ZoomOut<PixelRGBA>(Ratio, SrcX, SrcY, FBitmap.Width - 1, FBitmap.Height - 1, Src.RawImage, Dest.RawImage);
    end;
  end;

  procedure RenderZoomIn(Ratio: Integer; src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize ZoomIn<PixelRGB>(Ratio, SrcX, SrcY, FBitmap.Width - 1, FBitmap.Height - 1, Src.RawImage, Dest.RawImage);
      4: specialize ZoomIn<PixelRGBA>(Ratio, SrcX, SrcY, FBitmap.Width - 1, FBitmap.Height - 1, Src.RawImage, Dest.RawImage);
    end;
  end;

  procedure RenderNoZoom(src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize NoZoom<PixelRGB>(SrcX, SrcY, FBitmap.Width, FBitmap.Height, Src.RawImage, Dest.RawImage);
      4: specialize NoZoom<PixelRGBA>(SrcX, SrcY, FBitmap.Width, FBitmap.Height, Src.RawImage, Dest.RawImage);
    end;
  end;

var
  LocalRect, ScreenRect: TRect;
  BackgroundBitmap: TBitmap;
//  T: Double;
begin
  if (FBackground.Width = 0) or (FBackground.Height = 0) or (FZoom = 0) then
  begin
    FScrollBox.Canvas.Brush.Color := clBlack;
    FScrollBox.Canvas.Clear();

    Exit;
  end;

  ScreenRect := GetScrolledRect();

  if (FZoom > 1) then
  begin
    LocalRect.Left   := Trunc(ScreenRect.Left   / FZoomPixels);
    LocalRect.Top    := Trunc(ScreenRect.Top    / FZoomPixels);
    LocalRect.Right  := Trunc(ScreenRect.Right  / FZoomPixels);
    LocalRect.Bottom := Trunc(ScreenRect.Bottom / FZoomPixels);
  end else
  if (FZoom < 1) then
  begin
    LocalRect.Left   := Trunc(ScreenRect.Left   * FZoomPixels);
    LocalRect.Top    := Trunc(ScreenRect.Top    * FZoomPixels);
    LocalRect.Right  := Trunc(ScreenRect.Right  * FZoomPixels);
    LocalRect.Bottom := Trunc(ScreenRect.Bottom * FZoomPixels);
  end else
    LocalRect        := ScreenRect;

  BackgroundBitmap := FBackground;

  LocalRect.Right  := Min(LocalRect.Right,  BackgroundBitmap.Width);
  LocalRect.Bottom := Min(LocalRect.Bottom, BackgroundBitmap.Height);

  if (LocalRect.Width < 1) or (LocalRect.Height < 1) then
    Exit;

  // T := HighResolutionTime();

  FBitmap.BeginUpdate(
    LocalRect,
    Trunc(LocalRect.Width * FZoom),
    Trunc(LocalRect.Height * FZoom),
    FZoomPixels,
    FZoom
  );

  if (FZoom = 1.00) then
    RenderNoZoom(BackgroundBitmap, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap.Bitmap)
  else
  if (FZoom > 1.00) then
    RenderZoomIn(FZoomPixels, BackgroundBitmap, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap.Bitmap)
  else
    RenderZoomOut(FZoomPixels, BackgroundBitmap, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap.Bitmap);

  DoPaintArea(FBitmap, LocalRect);

  // FBitmap.DrawBoxTransparent(TBox.Create(FMousePoint.X - 15, FMousePoint.Y - 15, FMousePoint.X + 15, FMousePoint.Y + 15), clRed, 0.25);

  FBitmap.EndUpdate();

  BitBlt(
    FScrollBox.Canvas.Handle,
    ScreenRect.Left, ScreenRect.Top,
    FBitmap.Width, FBitmap.Height,
    FBitmap.Handle,
    0, 0,
    SRCCOPY
  );

  // WriteLn('Render time: ', FormatFloat('0.00', HighResolutionTime() - T), 'ms');
end;

procedure TSimbaImageBox.Paint;
begin
  FScrollBox.Refresh();
end;

procedure TSimbaImageBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    // Measure on larger font size
    // Font size can be 0 so use GetFontData
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 6;

    FMousePanel.Width := Canvas.TextWidth('(1000, 1000)');
    FDimensionsPanel.Width  := Canvas.TextWidth('1000x1000');
    FZoomPanel.Width  := Canvas.TextWidth('100%');

    FStatusBar.Height := Round(Canvas.TextHeight('Taylor Swift') * 0.8);
    FStatusBar.Font := Self.Font;
  finally
    Free();
  end;
end;

procedure TSimbaImageBox.BackgroundChanged(Sender: TObject);
begin
  if (FBackground.Width <> FWidth) or (FBackground.Height <> FHeight) then
  begin
    FWidth := FBackground.Width;
    FHeight := FBackground.Height;

    Zoom := 1;
    MoveTo(0, 0);
  end;

  Paint();
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
  Steps: Integer;
begin
  X := X + FScrollBox.HorzScrollBar.Position;
  Y := Y + FScrollBox.VertScrollBar.Position;

  FMousePoint.X := Trunc(X / FZoom);
  FMousePoint.Y := Trunc(Y / FZoom);

  if FScroll.Active then
  begin
    if (Abs(FScroll.Y - Y) > 0) then
    begin
      Steps := Abs(FScroll.Y - Y) div FZoomPixels;

      if (Steps > 0) then
      begin
        if (FScroll.Y - Y > 0) then
          FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + Steps * FZoomPixels
        else
          FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - Steps * FZoomPixels;
      end;
    end;

    if (Abs(FScroll.X - X) > 0) then
    begin
      Steps := Abs(FScroll.X - X) div FZoomPixels;

      if (Steps > 0) then
      begin
        if (FScroll.X - X > 0) then
          FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position + Steps * FZoomPixels
        else
          FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position - Steps * FZoomPixels;
      end;
    end;
  end else
  begin
    FMousePanel.Text := Format('%d, %d', [FMousePoint.X, FMousePoint.Y]);
    if (OnMouseMove <> nil) then
      OnMouseMove(Self, Shift, FMousePoint.X, FMousePoint.Y);
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

procedure TSimbaImageBox.ImageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (OnKeyDown <> nil) then
    OnKeyDown(Self, Key, Shift);
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

procedure TSimbaImageBox.SetParent(Value: TWinControl);
begin
  inherited SetParent(Value);

  if (Value <> nil) then
    Font := Value.Font;
end;

function TSimbaImageBox.GetCursor: TCursor;
begin
  Result := FScrollBox.Cursor;
end;

procedure TSimbaImageBox.MoveTo(X, Y: Integer);
begin
  FScrollBox.HandleNeeded();
  FScrollBox.HorzScrollBar.Position := Trunc(X * FZoom);
  FScrollBox.VertScrollBar.Position := Trunc(Y * FZoom);

  if FScrollBox.HorzScrollBar.Position < (FScrollBox.HorzScrollBar.Range - FScrollBox.HorzScrollBar.Page) then
    FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position - (FScrollBox.ClientWidth div 2);

  if FScrollBox.VertScrollBar.Position < (FScrollBox.VertScrollBar.Range - FScrollBox.VertScrollBar.Page) then
    FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - (FScrollBox.ClientHeight div 2);
end;

function TSimbaImageBox.IsVisible(X, Y: Integer): Boolean;
begin
  X := Trunc(X * FZoom);
  Y := Trunc(Y * FZoom);

  Result := InRange(X, FScrollBox.HorzScrollBar.Position, FScrollBox.HorzScrollBar.Position + FScrollBox.ClientWidth) and
            InRange(Y, FScrollBox.VertScrollBar.Position, FScrollBox.VertScrollBar.Position + FScrollBox.ClientHeight);
end;

function TSimbaImageBox.FindDTM(DTM: TDTM): TPointArray;
var
  Finder: TSimbaFinder;
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Finder.Target.SetImage(Img);

    Result := Finder.FindDTMEx(DTM, -1, Box(-1, -1, -1, -1));
  finally
    Img.Free();
  end;
end;

function TSimbaImageBox.FindColor(AColor: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
var
  Finder: TSimbaFinder;
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Finder.Target.SetImage(Img);

    Result := Finder.FindColor(AColor, Tolerance, ColorSpace, Multipliers, Box(-1, -1, -1, -1));
  finally
    Img.Free();
  end;
end;

function TSimbaImageBox.MatchColor(AColor: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;
var
  Finder: TSimbaFinder;
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Finder.Target.SetImage(Img);

    Result := Finder.MatchColor(AColor, ColorSpace, Multipliers, Box(-1, -1, -1, -1));
  finally
    Img.Free();
  end;
end;

procedure TSimbaImageBox.SetBackground(Data: PColorBGRA; AWidth, AHeight: Integer);
begin
  LazImage_FromData(FBackground, Data, AWidth, AHeight);
end;

procedure TSimbaImageBox.SetBackground(FileName: String);
begin
  FBackground.LoadFromFile(FileName);
end;

procedure TSimbaImageBox.SetBackground(Bitmap: TSimbaImage);
begin
  SetBackground(Bitmap.Data, Bitmap.Width, Bitmap.Height);
end;

procedure TSimbaImageBox.SetBackground(Window: TWindowHandle);
var
  Image: TSimbaImage;
begin
  if Window.IsValid() then
  begin
    Image := TSimbaImage.CreateFromWindow(Window);
    try
      SetBackground(Image);
    finally
      Image.Free();
    end;
  end;
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackground := TBitmap.Create();
  FBackground.OnChange := @BackgroundChanged;

  FBitmap := TSimbaImageBoxBitmap.Create();

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
  FScrollBox.OnKeyDown := @ImageKeyDown;

  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.SimplePanel := False;
  FStatusBar.AutoSize := False;
  FStatusBar.BorderSpacing.Left := 5;
  FStatusBar.BorderSpacing.Right := 5;

  FMousePanel := FStatusBar.Panels.Add();
  FMousePanel.Text := '(-1, -1)';
  FDimensionsPanel := FStatusBar.Panels.Add();
  FDimensionsPanel.Text := '(0, 0)';
  FZoomPanel := FStatusBar.Panels.Add();
  FStatusPanel := FStatusBar.Panels.Add();

  Zoom := 1;

  FontChanged(Self);
end;

destructor TSimbaImageBox.Destroy;
begin
  if (FBitmap <> nil) then
    FreeAndNil(FBitmap);
  if (FBackground <> nil) then
    FreeAndNil(FBackground);

  inherited Destroy();
end;

end.

