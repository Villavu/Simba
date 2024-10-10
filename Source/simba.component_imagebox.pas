{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ATScrollBar, LCLType, LMessages,
  simba.base, simba.component_statusbar, simba.image_lazbridge, simba.component_imageboxcanvas,
  simba.image, simba.dtm, simba.colormath, simba.target;

const
  ZOOM_LEVELS: TIntegerArray = (
    25, 50, 100, 200, 400, 800, 1600, 3200
  );
  ZOOM_PIXELS: TIntegerArray = (
    4, 2, 1, 2, 4, 8, 16, 32
  );

type
  TSimbaImageBox = class;

  TSimbaImageScrollBox = class(TCustomControl)
  protected
    FBackground: TBitmap; // background image
    FCanvas: TSimbaImageBoxCanvas; // canvas for user to draw on in PaintArea
    FResizeBuffer: TBitmap; // buffer to resize canvas onto for zoom

    FImageBox: TSimbaImageBox;
    FImageWidth: Integer;
    FImageHeight: Integer;

    FZoomMin: Integer;
    FZoomMax: Integer;
    FZoomLevel: Integer;
    FZoomPixels: Integer;

    FVertScroll: TATScrollbar;
    FHorzScroll: TATScrollbar;

    FDragging: record
      X, Y: Integer;
      Active: Boolean;
    end;

    FPaintTime: Double;

    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Click; override;
    procedure DblClick; override;

    procedure Paint; override;
    procedure Resize; override;

    procedure DoBackgroundChange(Sender: TObject);
    procedure DoScrollChange(Sender: TObject);

    procedure UpdateScrollBars;
    procedure ChangeZoom(ZoomIndex: Integer);
    procedure IncreaseZoom(Inc: Boolean);

    function VisibleTopX: Integer;
    function VisibleTopY: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScreenToImage(ScreenXY: TPoint): TPoint;
    function ImageToScreen(ImageXY: TPoint): TPoint;
    procedure MoveTo(ImageXY: TPoint);
    function IsPointVisible(ImageXY: TPoint): Boolean;

    property Background: TBitmap read FBackground;
  end;

  TImageBoxPaintEvent = procedure(Sender: TSimbaImageBox; Canvas: TSimbaImageBoxCanvas; R: TRect) of object;
  TImageBoxEvent = procedure(Sender: TSimbaImageBox) of object;
  TImageBoxClickEvent = procedure(Sender: TSimbaImageBox; X, Y: Integer) of object;
  TImageBoxKeyEvent = procedure(Sender: TSimbaImageBox; var Key: Word; Shift: TShiftState);
  TImageBoxMouseEvent = procedure(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TImageBoxMouseMoveEvent = procedure(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer) of object;

  PSimbaImageBox = ^TSimbaImageBox;
  TSimbaImageBox = class(TCustomControl)
  protected
    FImageScrollBox: TSimbaImageScrollBox;
    FStatusBar: TSimbaStatusBar;
    FBackground: TBitmap;
    FPixelFormat: ELazPixelFormat;
    FMouseX: Integer;
    FMouseY: Integer;

    FShowStatusBar: Boolean;
    FShowScrollBars: Boolean;

    FOnImgKeyDown: TImageBoxKeyEvent;
    FOnImgKeyUp: TImageBoxKeyEvent;

    FOnImgMouseEnter: TImageBoxEvent;
    FOnImgMouseLeave: TImageBoxEvent;
    FOnImgMouseDown: TImageBoxMouseEvent;
    FOnImgMouseUp: TImageBoxMouseEvent;
    FOnImgMouseMove: TImageBoxMouseMoveEvent;
    FOnImgClick: TImageBoxClickEvent;
    FOnImgDoubleClick: TImageBoxClickEvent;
    FOnImgPaint: TImageBoxPaintEvent;

    procedure Paint; override;

    procedure ImgKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure ImgKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure ImgMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgPaintArea(ACanvas: TSimbaImageBoxCanvas; R: TRect); virtual;
    procedure ImgClick(X, Y: Integer); virtual;
    procedure ImgDoubleClick(X, Y: Integer); virtual;
    procedure ImgMouseEnter; virtual;
    procedure ImgMouseLeave; virtual;

    function GetLastPaintTime: Double;
    function GetMousePoint: TPoint;
    function GetCursor: TCursor; override;
    function GetStatus: String;
    function GetShowScrollbars: Boolean;
    function GetShowStatusBar: Boolean;

    procedure SetCursor(Value: TCursor); override;
    procedure SetStatus(Value: String);
    procedure SetShowScrollbars(AValue: Boolean);
    procedure SetShowStatusBar(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    function FindDTM(DTM: TDTM): TPointArray;
    function FindColor(AColor: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
    function MatchColor(AColor: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;

    function ScreenToImage(ScreenXY: TPoint): TPoint;
    function ImageToScreen(ImageXY: TPoint): TPoint;
    function IsPointVisible(ImageXY: TPoint): Boolean;
    procedure MoveTo(ImageXY: TPoint);

    procedure SetBackground(Img: TSimbaImage);
    procedure SetBackgroundFromFile(FileName: String);
    procedure SetBackgroundFromWindow(Window: TWindowHandle);
    procedure SetBackgroundFromTarget(Target: TSimbaTarget; Bounds: TBox); overload;
    procedure SetBackgroundFromTarget(Target: TSimbaTarget); overload;

    property ShowStatusBar: Boolean read GetShowStatusBar write SetShowStatusBar;
    property ShowScrollbars: Boolean read GetShowScrollbars write SetShowScrollbars;

    property LastPaintTime: Double read GetLastPaintTime;
    property StatusBar: TSimbaStatusBar read FStatusBar;
    property Status: String read GetStatus write SetStatus;
    property PixelFormat: ELazPixelFormat read FPixelFormat;
    property Background: TBitmap read FBackground;
    property MouseX: Integer read FMouseX;
    property MouseY: Integer read FMouseY;
    property MousePoint: TPoint read GetMousePoint;

    property OnImgKeyDown: TImageBoxKeyEvent read FOnImgKeyDown write FOnImgKeyDown;
    property OnImgKeyUp: TImageBoxKeyEvent read FOnImgKeyUp write FOnImgKeyUp;

    property OnImgMouseEnter: TImageBoxEvent read FOnImgMouseEnter write FOnImgMouseEnter;
    property OnImgMouseLeave: TImageBoxEvent read FOnImgMouseLeave write FOnImgMouseLeave;
    property OnImgMouseDown: TImageBoxMouseEvent read FOnImgMouseDown write FOnImgMouseDown;
    property OnImgMouseUp: TImageBoxMouseEvent read FOnImgMouseUp write FOnImgMouseUp;
    property OnImgMouseMove: TImageBoxMouseMoveEvent read FOnImgMouseMove write FOnImgMouseMove;
    property OnImgClick: TImageBoxClickEvent read FOnImgClick write FOnImgClick;
    property OnImgDoubleClick: TImageBoxClickEvent read FOnImgDoubleClick write FOnImgDoubleClick;
    property OnImgPaint: TImageBoxPaintEvent read FOnImgPaint write FOnImgPaint;
  end;

implementation

uses
  simba.vartype_windowhandle, simba.datetime, simba.vartype_box,
  GraphType, LCLIntf;

generic procedure ZoomOut<_T>(Ratio, SrcX, SrcY, LoopEndX, LoopEndY: Integer; SrcImg, DestImg: TRawImage);
type
  PType = ^_T;
var
  SourceData, DestData: PByte;
  SourceBytesPerLine, DestBytesPerLine: PtrUInt;
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
  Y, RowSize: Integer;
begin
  SourceData         := SrcImg.Data;
  SourceBytesPerLine := SrcImg.Description.BytesPerLine;
  DestData           := DestImg.Data;
  DestBytesPerLine   := DestImg.Description.BytesPerLine;

  SourceData := SourceData + (SrcY * SourceBytesPerLine)
                           + (SrcX * SizeOf(_T));

  RowSize := (Wid * SizeOf(_T));
  for Y := 0 to Hei - 1 do
  begin
    Move(SourceData^, DestData^, RowSize);

    Inc(SourceData, SourceBytesPerLine);
    Inc(DestData, DestBytesPerLine);
  end;
end;

procedure TSimbaImageScrollBox.ChangeZoom(ZoomIndex: Integer);
var
  Level, Pixels: Integer;
begin
  Level := ZOOM_LEVELS[ZoomIndex];
  Pixels := ZOOM_PIXELS[ZoomIndex];

  if (Level < FZoomMin) then
    Level := FZoomMin;
  if (Level > FZoomMax) then
    Level := FZoomMax;

  if (Level <> FZoomLevel) then
  begin
    FZoomLevel := Level;
    FZoomPixels := Pixels;

    Invalidate();
  end;

  FImageBox.StatusBar.PanelText[2] := Format('%d%s', [FZoomLevel, '%']);
end;

function TSimbaImageScrollBox.VisibleTopX: Integer;
begin
  Result := Max(0, FHorzScroll.Position + ((FZoomPixels - FHorzScroll.Position) mod FZoomPixels));
end;

function TSimbaImageScrollBox.VisibleTopY: Integer;
begin
  Result := Max(0, FVertScroll.Position + ((FZoomPixels - FVertScroll.Position) mod FZoomPixels));
end;

procedure TSimbaImageScrollBox.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSimbaImageScrollBox.MouseLeave;
begin
  FDragging.Active := False;

  FImageBox.StatusBar.PanelText[0] := '';
  FImageBox.ImgMouseLeave();

  inherited MouseLeave();
end;

procedure TSimbaImageScrollBox.MouseEnter;
begin
  FImageBox.ImgMouseEnter();

  inherited MouseEnter();
end;

function TSimbaImageScrollBox.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if (ssCtrl in Shift) then
    IncreaseZoom(True);

  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

function TSimbaImageScrollBox.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if (ssCtrl in Shift) then
    IncreaseZoom(False);

  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

procedure TSimbaImageScrollBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Steps: Integer;
begin
  inherited;

  X += VisibleTopX;
  Y += VisibleTopY;

  if FDragging.Active then
  begin
    if (Abs(FDragging.Y - Y) > 0) then
    begin
      Steps := Abs(FDragging.Y - Y) div FZoomPixels;
      if (FZoomLevel > 100) then
        Steps *= FZoomPixels;

      if (Steps > 0) then
      begin
        if (FDragging.Y - Y > 0) then
          FVertScroll.ScrollBy(Steps)
        else
          FVertScroll.ScrollBy(-Steps);
      end;
    end;

    if (Abs(FDragging.X - X) > 0) then
    begin
      Steps := Abs(FDragging.X - X) div FZoomPixels;
      if (FZoomLevel > 100) then
        Steps *= FZoomPixels;

      if (Steps > 0) then
      begin
        if (FDragging.X - X > 0) then
          FHorzScroll.ScrollBy(Steps)
        else
          FHorzScroll.ScrollBy(-Steps);
      end;
    end;
  end;

  with FImageBox do
  begin
    FMouseX := IfThen(FZoomLevel >= 100, X div FZoomPixels, X * FZoomPixels);
    FMouseY := IfThen(FZoomLevel >= 100, Y div FZoomPixels, Y * FZoomPixels);

    StatusBar.PanelText[0] := Format('(%d, %d)', [FMouseX, FMouseY]);

    ImgMouseMove(Shift, FMouseX, FMouseY);
  end;
end;

procedure TSimbaImageScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if CanSetFocus() then
    SetFocus();

  X += VisibleTopX;
  Y += VisibleTopY;

  if (Button = mbRight) then
  begin
    FDragging.X := X;
    FDragging.Y := Y;
    FDragging.Active := True;

    Cursor := crSizeAll;
  end;

  with FImageBox do
  begin
    FMouseX := IfThen(FZoomLevel >= 100, X div FZoomPixels, X * FZoomPixels);
    FMouseY := IfThen(FZoomLevel >= 100, Y div FZoomPixels, Y * FZoomPixels);

    ImgMouseDown(Button, Shift, FMouseX, FMouseY);
  end;
end;

procedure TSimbaImageScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  X += VisibleTopX;
  Y += VisibleTopY;

  if (Button = mbRight) then
  begin
    FDragging.Active := False;

    Cursor := crDefault;
  end;

  with FImageBox do
  begin
    FMouseX := IfThen(FZoomLevel >= 100, X div FZoomPixels, X * FZoomPixels);
    FMouseY := IfThen(FZoomLevel >= 100, Y div FZoomPixels, Y * FZoomPixels);

    ImgMouseUp(Button, Shift, FMouseX, FMouseY);
  end;
end;

procedure TSimbaImageScrollBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  FImageBox.ImgKeyDown(Key, Shift);
end;

procedure TSimbaImageScrollBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  FImageBox.ImgKeyUp(Key, Shift);
end;

procedure TSimbaImageScrollBox.Click;
begin
  inherited Click;

  with FImageBox do
    ImgClick(FMouseX, FMouseY);
end;

procedure TSimbaImageScrollBox.DblClick;
begin
  inherited DblClick();

  with FImageBox do
    ImgDoubleClick(FMouseX, FMouseY);
end;

procedure TSimbaImageScrollBox.DoBackgroundChange(Sender: TObject);
begin
  if (FBackground.Width <> FImageWidth) or (FBackground.Height <> FImageHeight) then
  begin
    FImageWidth := FBackground.Width;
    FImageHeight := FBackground.Height;

    FZoomLevel := 100;
    FZoomPixels := 1;

    FHorzScroll.Position := 0;
    FVertScroll.Position := 0;

    UpdateScrollBars();
  end;

  Invalidate();

  FImageBox.StatusBar.PanelText[1] := Format('%d x %d', [FImageWidth, FImageHeight]);
end;

procedure TSimbaImageScrollBox.DoScrollChange(Sender: TObject);
begin
  Invalidate();
end;

procedure TSimbaImageScrollBox.Paint;
type
  PixelRGB  = record R,G,B: Byte; end;
  PixelRGBA = record R,G,B,A: Byte; end;

  procedure RenderZoomOut(Ratio: Integer; src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize ZoomOut<PixelRGB>(Ratio, SrcX, SrcY, srcW div Ratio, srcH div Ratio, Src.RawImage, Dest.RawImage);
      4: specialize ZoomOut<PixelRGBA>(Ratio, SrcX, SrcY, srcW div Ratio, srcH div Ratio, Src.RawImage, Dest.RawImage);
    end;
  end;

  procedure RenderZoomIn(Ratio: Integer; src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize ZoomIn<PixelRGB>(Ratio, SrcX, SrcY, srcW * Ratio, srcH * Ratio, Src.RawImage, Dest.RawImage);
      4: specialize ZoomIn<PixelRGBA>(Ratio, SrcX, SrcY, srcW * Ratio, srcH * Ratio, Src.RawImage, Dest.RawImage);
    end;
  end;

  procedure RenderNoZoom(src: TBitmap; srcX, srcY, srcW, srcH: Integer; dest: TBitmap);
  begin
    case (Src.RawImage.Description.BitsPerPixel shr 3) of
      3: specialize NoZoom<PixelRGB>(SrcX, SrcY, srcW, srcH, Src.RawImage, Dest.RawImage);
      4: specialize NoZoom<PixelRGBA>(SrcX, SrcY, srcW, srcH, Src.RawImage, Dest.RawImage);
    end;
  end;

var
  ScreenRect, LocalRect: TRect;
  W, H: Integer;
begin
  FPaintTime := HighResolutionTime();

  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);

  ScreenRect.Left := VisibleTopX;
  ScreenRect.Top := VisibleTopY;
  ScreenRect.Right := ScreenRect.Left + ClientWidth + FZoomPixels;
  ScreenRect.Bottom := ScreenRect.Top + ClientHeight + FZoomPixels;

  if (FZoomLevel = 100) then
    LocalRect := ScreenRect
  else
  begin
    LocalRect.Left   := IfThen(FZoomLevel > 100, ScreenRect.Left   div FZoomPixels, ScreenRect.Left   * FZoomPixels);
    LocalRect.Top    := IfThen(FZoomLevel > 100, ScreenRect.Top    div FZoomPixels, ScreenRect.Top    * FZoomPixels);
    LocalRect.Right  := IfThen(FZoomLevel > 100, ScreenRect.Right  div FZoomPixels, ScreenRect.Right  * FZoomPixels);
    LocalRect.Bottom := IfThen(FZoomLevel > 100, ScreenRect.Bottom div FZoomPixels, ScreenRect.Bottom * FZoomPixels);
  end;

  LocalRect.Right  := Min(LocalRect.Right,  FImageWidth);
  LocalRect.Bottom := Min(LocalRect.Bottom, FImageHeight);
  if (LocalRect.Width < 1) or (LocalRect.Height < 1) then
    Exit;

  W := IfThen(FZoomLevel >= 100, LocalRect.Width * FZoomPixels, LocalRect.Width div FZoomPixels);
  H := IfThen(FZoomLevel >= 100, LocalRect.Height * FZoomPixels, LocalRect.Height div FZoomPixels);

  FCanvas.BeginUpdate(
    LocalRect,
    LocalRect.Width, LocalRect.Height
  );

  RenderNoZoom(background, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FCanvas.Bitmap);

  FImageBox.ImgPaintArea(FCanvas, LocalRect);

  FResizeBuffer.BeginUpdate();
  FResizeBuffer.SetSize(
    Max(FResizeBuffer.Width,  W + 150), // over allocate a little
    Max(FResizeBuffer.Height, H + 150)
  );

  if (FZoomLevel = 100) then
    RenderNoZoom(FCanvas.Bitmap, 0, 0, LocalRect.Width, LocalRect.Height, FResizeBuffer)
  else
  if (FZoomLevel > 100) then
    RenderZoomIn(FZoomPixels, FCanvas.Bitmap, 0, 0, LocalRect.Width, LocalRect.Height, FResizeBuffer)
  else
    RenderZoomOut(FZoomPixels, FCanvas.Bitmap, 0, 0, LocalRect.Width, LocalRect.Height, FResizeBuffer);

  FResizeBuffer.EndUpdate();
  FCanvas.EndUpdate();

  BitBlt(
    Canvas.Handle,
    0, 0,
    W, H,
    FResizeBuffer.Canvas.Handle,
    0, 0,
    SRCCOPY
  );

  FPaintTime := HighResolutionTime() - FPaintTime;
end;

procedure TSimbaImageScrollBox.Resize;
begin
  inherited Resize();

  UpdateScrollBars();
end;

procedure TSimbaImageScrollBox.UpdateScrollBars;
var
  W, H: Integer;
begin
  W := IfThen(FZoomLevel >= 100, FImageWidth * FZoomPixels, FImageWidth div FZoomPixels);
  H := IfThen(FZoomLevel >= 100, FImageHeight * FZoomPixels, FImageHeight div FZoomPixels);

  if Assigned(FVertScroll) then
  begin
    FVertScroll.SmallChange := FZoomPixels;
    FVertScroll.LargeChange := FZoomPixels;
    FVertScroll.PageSize := ClientHeight - FHorzScroll.Height;
    FVertScroll.Max := Max(0, ((H - ClientHeight) + FHorzScroll.Height) + FVertScroll.PageSize);
    if FVertScroll.Visible then
      FVertScroll.Invalidate;
  end;

  if Assigned(FHorzScroll) then
  begin
    FHorzScroll.SmallChange := FZoomPixels;
    FHorzScroll.LargeChange := FZoomPixels;
    FHorzScroll.PageSize := ClientWidth - FVertScroll.Width;
    FHorzScroll.Max := Max(0, ((W - ClientWidth) + FVertScroll.Width) + FHorzScroll.PageSize);
    if FHorzScroll.Visible then
      FHorzScroll.Invalidate;
  end;
end;

procedure TSimbaImageScrollBox.IncreaseZoom(Inc: Boolean);
var
  I: Integer;
  OldImagePoint: TPoint;
  DidChange: Boolean;
begin
  DidChange := False;
  if MouseInClient then
    OldImagePoint := ScreenToImage(ScreenToClient(Mouse.CursorPos));

  case Inc of
    True:
      for I := Low(ZOOM_LEVELS) to High(ZOOM_LEVELS) do
        if (ZOOM_LEVELS[I] > FZoomLevel) then
        begin
          ChangeZoom(I);
          DidChange := True;
          Break;
        end;

    False:
      for I := High(ZOOM_LEVELS) downto Low(ZOOM_LEVELS) do
        if (ZOOM_LEVELS[I] < FZoomLevel) then
        begin
          ChangeZoom(I);
          DidChange := True;
          Break;
        end;
  end;

  if DidChange then
  begin
    UpdateScrollBars();

    if MouseInClient then
      MoveTo(OldImagePoint{%H-});
  end;
end;

function TSimbaImageScrollBox.ScreenToImage(ScreenXY: TPoint): TPoint;
begin
  Result.X := (VisibleTopX + ScreenXY.X) div FZoomPixels;
  Result.Y := (VisibleTopY + ScreenXY.Y) div FZoomPixels;
end;

function TSimbaImageScrollBox.ImageToScreen(ImageXY: TPoint): TPoint;
begin
  Result.X := IfThen(FZoomLevel >= 100, ImageXY.X * FZoomPixels, ImageXY.X div FZoomPixels) - VisibleTopX;
  Result.Y := IfThen(FZoomLevel >= 100, ImageXY.Y * FZoomPixels, ImageXY.Y div FZoomPixels) - VisibleTopY;
end;

procedure TSimbaImageScrollBox.MoveTo(ImageXY: TPoint);
begin
  FHorzScroll.Position := Min(IfThen(FZoomLevel >= 100, ImageXY.X * FZoomPixels, ImageXY.X div FZoomPixels) - (ClientWidth div 2), FHorzScroll.Max - FHorzScroll.PageSize);
  FVertScroll.Position := Min(IfThen(FZoomLevel >= 100, ImageXY.Y * FZoomPixels, ImageXY.Y div FZoomPixels) - (ClientHeight div 2), FVertScroll.Max - FVertScroll.PageSize);
end;

function TSimbaImageScrollBox.IsPointVisible(ImageXY: TPoint): Boolean;
begin
  with ImageToScreen(ImageXY) do
    Result := (X >= 0) and
              (Y >= 0) and
              (X < ClientWidth - IfThen(FVertScroll.Visible, FVertScroll.Width, 0)) and
              (Y < ClientHeight - IfThen(FHorzScroll.Visible, FHorzScroll.Height, 0));
end;

constructor TSimbaImageScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;

  FImageBox := AOwner as TSimbaImageBox;

  FZoomMin := ZOOM_LEVELS[Low(ZOOM_LEVELS)];
  FZoomMax := ZOOM_LEVELS[High(ZOOM_LEVELS)];
  FZoomLevel := 100;
  FZoomPixels := 1;

  FVertScroll := TATScrollbar.Create(Self);
  FVertScroll.Parent := Self;
  FVertScroll.Kind := sbVertical;
  FVertScroll.Align := alRight;
  FVertScroll.OnChange := @DoScrollChange;

  FHorzScroll := TATScrollbar.Create(Self);
  FHorzScroll.Parent := Self;
  FHorzScroll.Kind := sbHorizontal;
  FHorzScroll.Align := alBottom;
  FHorzScroll.OnChange := @DoScrollChange;
  FHorzScroll.IndentCorner := 100;

  FCanvas := TSimbaImageBoxCanvas.Create();
  FResizeBuffer := TBitmap.Create();
  FBackground := TBitmap.Create();
  FBackground.OnChange := @DoBackgroundChange;
end;

destructor TSimbaImageScrollBox.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FBackground);
  FreeAndNil(FResizeBuffer);

  inherited Destroy();
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImageScrollBox := TSimbaImageScrollBox.Create(Self);
  FImageScrollBox.Parent := Self;
  FImageScrollBox.Align := alClient;

  FStatusBar := TSimbaStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := 4;
  FStatusBar.PanelTextMeasure[0] := '(1235, 1234)';
  FStatusBar.PanelTextMeasure[1] := '1234 x 1234';
  FStatusBar.PanelTextMeasure[2] := '1000%';
  FStatusBar.PanelText[2] := '100%';

  FBackground := FImageScrollBox.Background;
  FPixelFormat := LazImage_PixelFormat(FBackground);

  FShowStatusBar := True;
  FShowScrollBars := True;
end;

function TSimbaImageBox.FindDTM(DTM: TDTM): TPointArray;
var
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Result := Img.FindDTM(DTM, -1);
  finally
    Img.Free();
  end;
end;

function TSimbaImageBox.FindColor(AColor: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
var
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Result := Img.FindColor(AColor, Tolerance, ColorSpace, Multipliers);
  finally
    Img.Free();
  end;
end;

function TSimbaImageBox.MatchColor(AColor: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;
var
  Img: TSimbaImage;
begin
  Img := LazImage_ToSimbaImage(FBackground);
  try
    Result := Img.MatchColor(AColor, ColorSpace, Multipliers);
  finally
    Img.Free();
  end;
end;

function TSimbaImageBox.ScreenToImage(ScreenXY: TPoint): TPoint;
begin
  Result := FImageScrollBox.ScreenToImage(ScreenXY);
end;

function TSimbaImageBox.ImageToScreen(ImageXY: TPoint): TPoint;
begin
  Result := FImageScrollBox.ImageToScreen(ImageXY);
end;

function TSimbaImageBox.IsPointVisible(ImageXY: TPoint): Boolean;
begin
  Result := FImageScrollBox.IsPointVisible(ImageXY);
end;

procedure TSimbaImageBox.MoveTo(ImageXY: TPoint);
begin
  FImageScrollBox.MoveTo(ImageXY);
end;

procedure TSimbaImageBox.SetStatus(Value: String);
begin
  FStatusBar.PanelText[3] := Value;
end;

procedure TSimbaImageBox.SetBackground(Img: TSimbaImage);
begin
  LazImage_FromData(FBackground, Img.Data, Img.Width, Img.Height);
end;

procedure TSimbaImageBox.SetBackgroundFromFile(FileName: String);
var
  SimbaImage: TSimbaImage;
begin
  SimbaImage := TSimbaImage.Create(FileName);
  try
    LazImage_FromSimbaImage(FBackground, SimbaImage);
  finally
    SimbaImage.Free();
  end;
end;

procedure TSimbaImageBox.SetBackgroundFromWindow(Window: TWindowHandle);
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

procedure TSimbaImageBox.SetBackgroundFromTarget(Target: TSimbaTarget; Bounds: TBox);
var
  Image: TSimbaImage;
begin
  Image := Target.GetImage(Bounds);
  try
    SetBackground(Image);
  finally
    Image.Free();
  end;
end;

procedure TSimbaImageBox.SetBackgroundFromTarget(Target: TSimbaTarget);
var
  Image: TSimbaImage;
begin
  Image := Target.GetImage(TBox.Create(-1,-1,-1,-1));
  try
    SetBackground(Image);
  finally
    Image.Free();
  end;
end;

function TSimbaImageBox.GetShowScrollbars: Boolean;
begin
  Result := FShowScrollBars;
end;

function TSimbaImageBox.GetShowStatusBar: Boolean;
begin
  Result := FShowStatusBar;
end;

procedure TSimbaImageBox.SetShowScrollbars(AValue: Boolean);
begin
  FShowScrollBars := AValue;

  FImageScrollBox.FVertScroll.Visible := FShowScrollBars;
  FImageScrollBox.FHorzScroll.Visible := FShowScrollBars;
end;

procedure TSimbaImageBox.SetShowStatusBar(AValue: Boolean);
begin
  FShowStatusBar := AValue;

  FStatusBar.Visible := FShowStatusBar;
end;

procedure TSimbaImageBox.Paint;
begin
  FImageScrollBox.Invalidate();

  inherited Paint();
end;

function TSimbaImageBox.GetMousePoint: TPoint;
begin
  Result.X := FMouseX;
  Result.Y := FMouseY;
end;

procedure TSimbaImageBox.ImgKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnImgKeyDown) then
    FOnImgKeyDown(Self, Key, Shift);
end;

procedure TSimbaImageBox.ImgKeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnImgKeyUp) then
    FOnImgKeyUp(Self, Key, Shift);
end;

procedure TSimbaImageBox.ImgMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnImgMouseDown) then
    FOnImgMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSimbaImageBox.ImgMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnImgMouseUp) then
    FOnImgMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSimbaImageBox.ImgMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnImgMouseMove) then
    FOnImgMouseMove(Self, Shift, X, Y);
end;

procedure TSimbaImageBox.ImgPaintArea(ACanvas: TSimbaImageBoxCanvas; R: TRect);
begin
  if Assigned(FOnImgPaint) then
    FOnImgPaint(Self, ACanvas, R);
end;

procedure TSimbaImageBox.ImgClick(X, Y: Integer);
begin
  if Assigned(FOnImgClick) then
    FOnImgClick(Self, X, Y);
end;

procedure TSimbaImageBox.ImgDoubleClick(X, Y: Integer);
begin
  if Assigned(FOnImgDoubleClick) then
    FOnImgDoubleClick(Self, X, Y);
end;

procedure TSimbaImageBox.ImgMouseEnter;
begin
  if Assigned(FOnImgMouseEnter) then
    FOnImgMouseEnter(Self);
end;

procedure TSimbaImageBox.ImgMouseLeave;
begin
  if Assigned(FOnImgMouseLeave) then
    FOnImgMouseLeave(Self);
end;

function TSimbaImageBox.GetLastPaintTime: Double;
begin
  if Assigned(FImageScrollBox) then
    Result := FImageScrollBox.FPaintTime
  else
    Result := -1;
end;

procedure TSimbaImageBox.SetCursor(Value: TCursor);
begin
  if Assigned(FImageScrollBox) then
    FImageScrollBox.Cursor := Value
  else
    inherited SetCursor(Value);
end;

function TSimbaImageBox.GetCursor: TCursor;
begin
  if Assigned(FImageScrollBox) then
    Result := FImageScrollBox.Cursor
  else
    Result := inherited GetCursor();
end;

function TSimbaImageBox.GetStatus: String;
begin
  Result := FStatusBar.PanelText[3];
end;

end.

