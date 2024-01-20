{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  TODO: make this support everything and remove old simba.imagebox
}
unit simba.imagebox_new;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ATScrollBar, LCLType, LMessages,
  simba.base, simba.component_statusbar;

const
  ZOOM_LEVELS: array[1..9] of Integer = (
    25, 50, 100, 200, 400, 600, 800, 1600, 2400
  );
  ZOOM_PIXELS: array[1..9] of Integer = (
    4, 2, 1, 2, 4, 6, 8, 16, 24
  );

type
  TSimbaImageBoxNew = class;

  TSimbaImageScrollBox = class(TCustomControl)
  protected
    FImageBox: TSimbaImageBoxNew;
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

    FBackground: TBitmap;
    FBitmap: TBitmap;

    FMousePoint: TPoint;

    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    procedure DoBackgroundChange(Sender: TObject);
    procedure DoScrollChange(Sender: TObject);
    procedure UpdateScrollBars;

    procedure ChangeZoom(ZoomIndex: Integer);
    procedure IncreaseZoom(Inc: Boolean);

    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ZoomLevel: Integer read FZoomLevel;
    property ImageWidth: Integer read FImageWidth;
    property ImageHeight: Integer read FImageHeight;
    property MousePoint: TPoint read FMousePoint;

    function ScreenToImageXY(ScreenPoint: TPoint): TPoint;
    procedure MoveTo(ImageXY: TPoint);

    property Background: TBitmap read FBackground;
  end;

  TSimbaImageBoxNew = class(TCustomControl)
  protected
    FImageScrollBox: TSimbaImageScrollBox;
    FStatusBar: TSimbaStatusBar;

    function GetBackground: TBitmap;
    function GetMousePoint: TPoint;
  public
    constructor Create(AOwner: TComponent); override;

    property StatusBar: TSimbaStatusBar read FStatusBar;
    property Background: TBitmap read GetBackground;
    property MousePoint: TPoint read GetMousePoint;
    property OnDblClick;
  end;

implementation

uses
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

procedure TSimbaImageScrollBox.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSimbaImageScrollBox.MouseLeave;
begin
  FDragging.Active := False;
  FImageBox.StatusBar.PanelText[0] := '';

  inherited MouseLeave();
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
  X := X + FHorzScroll.Position;
  Y := Y + FVertScroll.Position;

  FMousePoint.X := IfThen(FZoomLevel >= 100, X div FZoomPixels, X * FZoomPixels);
  FMousePoint.Y := IfThen(FZoomLevel >= 100, Y div FZoomPixels, Y * FZoomPixels);

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
  end else
    FImageBox.StatusBar.PanelText[0] := Format('(%d, %d)', [X, Y]);

  inherited MouseMove(Shift, X, Y);
end;

procedure TSimbaImageScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X + FHorzScroll.Position;
  Y := Y + FVertScroll.Position;

  if (Button = mbRight) then
  begin
    FDragging.X := X;
    FDragging.Y := Y;
    FDragging.Active := True;

    Cursor := crSizeAll;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaImageScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X + FHorzScroll.Position;
  Y := Y + FVertScroll.Position;

  if (Button = mbRight) then
  begin
    FDragging.Active := False;

    Cursor := crDefault;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaImageScrollBox.DblClick;
begin
  inherited DblClick();

  if Assigned(FImageBox.OnDblClick) then
    FImageBox.OnDblClick(FImageBox);
end;

procedure TSimbaImageScrollBox.DoBackgroundChange(Sender: TObject);
begin
  if (FBackground.Width <> FImageWidth) or (FBackground.Height <> FImageHeight) then
  begin
    FImageWidth := FBackground.Width;
    FImageHeight := FBackground.Height;

    ChangeZoom(3);

    FHorzScroll.Position := 0;
    FVertScroll.Position := 0;
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
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);

  ScreenRect.Left   := FHorzScroll.Position;
  ScreenRect.Top    := FVertScroll.Position;
  ScreenRect.Right  := ((FHorzScroll.Position + ClientWidth) - FVertScroll.Width) + FZoomPixels;
  ScreenRect.Bottom := ((FVertScroll.Position + ClientHeight) - FHorzScroll.Height) + FZoomPixels;

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

  FBitmap.BeginUpdate();
  FBitmap.SetSize(
    Max(FBitmap.Width,  W + 150), // over allocate a little
    Max(FBitmap.Height, H + 150)
  );

  if (FZoomLevel = 100) then
    RenderNoZoom(background, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap)
  else
  if (FZoomLevel > 100) then
    RenderZoomIn(FZoomPixels, background, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap)
  else
    RenderZoomOut(FZoomPixels, background, LocalRect.Left, LocalRect.Top, LocalRect.Width, LocalRect.Height, FBitmap);

  FBitmap.EndUpdate();

  BitBlt(
    Canvas.Handle,
    0, 0,
    W, H,
    FBitmap.Canvas.Handle,
    0, 0,
    SRCCOPY
  );
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
    FVertScroll.PageSize := ClientHeight - FHorzScroll.Height;
    FVertScroll.Max := Max(0, ((H - ClientHeight) + FHorzScroll.Height) + FVertScroll.PageSize);
    FVertScroll.Invalidate;
  end;

  if Assigned(FHorzScroll) then
  begin
    FHorzScroll.PageSize := ClientWidth - FVertScroll.Width;
    FHorzScroll.Max := Max(0, ((W - ClientWidth) + FVertScroll.Width) + FHorzScroll.PageSize);
    FHorzScroll.Invalidate;
  end;
end;

procedure TSimbaImageScrollBox.IncreaseZoom(Inc: Boolean);
var
  I: Integer;
  OldImagePoint: TPoint;
begin
  if MouseInClient then
    OldImagePoint := ScreenToImageXY(ScreenToClient(Mouse.CursorPos));

  case Inc of
    True:
      for I := Low(ZOOM_LEVELS) to High(ZOOM_LEVELS) do
        if (ZOOM_LEVELS[I] > FZoomLevel) then
        begin
          ChangeZoom(I);
          Break;
        end;

    False:
      for I := High(ZOOM_LEVELS) downto Low(ZOOM_LEVELS) do
        if (ZOOM_LEVELS[I] < FZoomLevel) then
        begin
          ChangeZoom(I);
          Break;
        end;
  end;

  UpdateScrollBars();

  if MouseInClient then
    MoveTo(OldImagePoint);
end;

function TSimbaImageScrollBox.ScreenToImageXY(ScreenPoint: TPoint): TPoint;
begin
  Result.X := (FHorzScroll.Position + ScreenPoint.X) div FZoomPixels;
  Result.Y := (FVertScroll.Position + ScreenPoint.Y) div FZoomPixels;
end;

procedure TSimbaImageScrollBox.MoveTo(ImageXY: TPoint);
begin
  FHorzScroll.Position := Min((ImageXY.X * FZoomPixels) - (ClientWidth div 2), FHorzScroll.Max - FHorzScroll.PageSize);
  FVertScroll.Position := Min((ImageXY.Y * FZoomPixels) - (ClientHeight div 2), FVertScroll.Max - FVertScroll.PageSize);
end;

constructor TSimbaImageScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;

  FImageBox := AOwner as TSimbaImageBoxNew;

  FZoomLevel := 100;
  FZoomPixels := 1;

  FZoomMin := 25;
  FZoomMax := 2400;

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

  FBitmap := TBitmap.Create();
  FBackground := TBitmap.Create();
  FBackground.OnChange := @DoBackgroundChange;
end;

destructor TSimbaImageScrollBox.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBackground);

  inherited Destroy();
end;

function TSimbaImageBoxNew.GetBackground: TBitmap;
begin
  Result := FImageScrollBox.Background;
end;

function TSimbaImageBoxNew.GetMousePoint: TPoint;
begin
  Result := FImageScrollBox.MousePoint;
end;

constructor TSimbaImageBoxNew.Create(AOwner: TComponent);
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
end;

end.

