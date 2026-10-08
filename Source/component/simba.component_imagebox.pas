{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  LCLType, LMessages,
  simba.base,
  simba.containers,
  simba.component_statusbar,
  simba.component_scrollbar,
  simba.canvas,
  simba.component_imageboxrender,
  simba.image;

const
  WHEEL_LINE_HEIGHT = 24;

  // the widest MinZoom..MaxZoom can be, in percent: halving 100 below 25 stops being a whole percent
  ZOOM_LOWEST  = 25;
  ZOOM_HIGHEST = 3200;

  PANEL_MOUSE  = 0;
  PANEL_SIZE   = 1;
  PANEL_ZOOM   = 2;
  PANEL_STATUS = 3;
  PANEL_COUNT  = 4;

type
  TSimbaImageBox = class;

  // A canvas over a transparent image the size of the box's background: alpha
  // is zero everywhere until something is drawn
  TSimbaImageBoxLayer = class(TSimbaCanvas)
  protected
    FBox: TSimbaImageBox;
    FPixels: TSimbaImage;
    FVisible: Boolean;
    FPriority: Integer;
    FOpacity: Byte;

    procedure Resize(W, H: Integer);
    procedure InsertByPriority;
    procedure SetVisible(Value: Boolean);
    procedure SetPriority(Value: Integer);
    procedure SetOpacity(Value: Byte);
  public
    constructor Create(Box: TSimbaImageBox); reintroduce;
    destructor Destroy; override;

    property Visible: Boolean read FVisible write SetVisible;
    // paint order: lower first so a higher priority paints on top
    property Priority: Integer read FPriority write SetPriority;
    property Opacity: Byte read FOpacity write SetOpacity;
  end;
  TSimbaImageBoxLayerList = specialize TSimbaObjectList<TSimbaImageBoxLayer>;

  TImageBoxPaintEvent = procedure(Sender: TSimbaImageBox; Canvas: TSimbaCanvas; R: TRect) of object;
  TImageBoxEvent = procedure(Sender: TSimbaImageBox) of object;
  TImageBoxClickEvent = procedure(Sender: TSimbaImageBox; X, Y: Integer) of object;
  TImageBoxKeyEvent = procedure(Sender: TSimbaImageBox; var Key: UInt16; Shift: TShiftState) of object;
  TImageBoxMouseEvent = procedure(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TImageBoxMouseMoveEvent = procedure(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer) of object;

  TSimbaImageBox = class(TCustomControl)
  protected
    FStatusBar: TSimbaStatusBar;
    FStatusBarPanel: TPanel;
    FUserPanel: TPanel;
    FVertScroll: TSimbaScrollBar;
    FHorzScroll: TSimbaScrollBar;
    FRenderer: TSimbaImageBoxRenderer;

    FBackground: TSimbaImage;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FLayers: TSimbaImageBoxLayerList;
    FMousePos: TPoint; // in image space

    FZoom: Integer;
    FMinZoom: Integer;
    FMaxZoom: Integer;
    FAllowUserZoom: Boolean;

    FPanning: record
      X, Y: Integer;
      Active: Boolean;
      Enabled: Boolean;
      RestoreCursor: TCursor;
    end;

    FDebug: record
      Show: Boolean;
      LastFrameTime: Double;
    end;

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

    function ZoomsOut: Boolean; inline;
    function ZoomRatio: Integer; inline;

    // Nothing to erase - every paint covers the whole client area
    procedure EraseBackground(DC: HDC); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

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
    procedure DoOnResize; override;

    procedure DoScrollChange(Sender: TObject);
    procedure DoStatusBarResize(Sender: TObject);

    procedure PaintDebugInfo;

    procedure SetScrollPos(Bar: TSimbaScrollBar; Value: Integer);
    procedure PanAxis(Bar: TSimbaScrollBar; Delta: Integer);
    procedure EndPan;
    procedure UpdateZoomStatus;
    procedure UpdateScrollBars;
    procedure BackgroundResized;
    function ImageToScroll(V: Integer): Integer; overload;
    function ImageToScroll(ImageXY: TPoint): TPoint; overload;
    function ScrollToImage(V: Integer): Integer;
    function SnapScroll(V: Integer): Integer;

    function VisibleTopX: Integer;
    function VisibleTopY: Integer;
    function VisibleImageRect: TRect;

    procedure ImgKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure ImgKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure ImgMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImgPaintArea(ACanvas: TSimbaCanvas; R: TRect); virtual;
    procedure ImgClick(X, Y: Integer); virtual;
    procedure ImgDoubleClick(X, Y: Integer); virtual;
    procedure ImgMouseEnter; virtual;
    procedure ImgMouseLeave; virtual;

    function GetLayer(Index: Integer): TSimbaImageBoxLayer;
    function GetLayerCount: Integer;
    function GetVisibleLayers: TSimbaImageBoxRenderLayers;

    function GetShowStatusBar: Boolean;
    function GetShowScrollbars: Boolean;
    function GetAllowMoving: Boolean;
    function GetStatus: String;
    function GetCursor: TCursor; override;

    procedure SetShowStatusBar(AValue: Boolean);
    procedure SetShowScrollbars(AValue: Boolean);
    procedure SetAllowMoving(AValue: Boolean);
    procedure SetStatus(Value: String);
    procedure SetBackground(AValue: TSimbaImage);
    procedure SetZoom(Level: Integer);
    procedure SetMinZoom(AValue: Integer);
    procedure SetMaxZoom(AValue: Integer);
    procedure SetCursor(Value: TCursor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Invalidates the image only, not the scrollbars or status bar.
    procedure Invalidate; override;

    // The part of the control the image is drawn in: all but the scroll bars and
    // status bar.
    function ViewWidth: Integer;
    function ViewHeight: Integer;

    function ScreenToImage(ScreenXY: TPoint): TPoint;
    function IsPointVisible(ImageXY: TPoint): Boolean;
    procedure MoveTo(ImageXY: TPoint);

    procedure BackgroundChanged;
    // Background := AValue, but hands the old image back instead of freeing it.
    function SwapBackground(AValue: TSimbaImage): TSimbaImage;

    property ShowStatusBar: Boolean read GetShowStatusBar write SetShowStatusBar;
    property ShowScrollbars: Boolean read GetShowScrollbars write SetShowScrollbars;
    property AllowMoving: Boolean read GetAllowMoving write SetAllowMoving;
    property AllowUserZoom: Boolean read FAllowUserZoom write FAllowUserZoom;

    property UserPanel: TPanel read FUserPanel;
    property StatusBar: TSimbaStatusBar read FStatusBar;
    property Status: String read GetStatus write SetStatus;
    // The image being shown, owned by the box: assigning hands the image over.
    property Background: TSimbaImage read FBackground write SetBackground;
    // in paint order, bottom first
    property LayerCount: Integer read GetLayerCount;
    property Layers[Index: Integer]: TSimbaImageBoxLayer read GetLayer;
    property MouseXY: TPoint read FMousePos;

    property Zoom: Integer read FZoom write SetZoom;
    property MinZoom: Integer read FMinZoom write SetMinZoom;
    property MaxZoom: Integer read FMaxZoom write SetMaxZoom;

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
  Forms, Graphics, LCLIntf,
  simba.datetime, simba.component_theme;

function SnapZoom(Level, Lo, Hi: Integer): Integer;
begin
  Result := Lo;
  while (Result * 2 <= Hi) and (Level > Result + Result div 2) do
    Result := Result * 2;
end;

procedure TSimbaImageBoxLayer.Resize(W, H: Integer);
begin
  FPixels.SetSize(W, H);
  SetData(FPixels.Data, W, W, H);
  Clear();
end;

procedure TSimbaImageBoxLayer.InsertByPriority;
var
  I: Integer;
begin
  FBox.FLayers.Delete(Self);
  FBox.FLayers.Add(Self);

  I := FBox.FLayers.Count - 1;
  while (I > 0) and (FBox.FLayers[I - 1].FPriority > FPriority) do
  begin
    FBox.FLayers[I] := FBox.FLayers[I - 1];
    Dec(I);
  end;
  FBox.FLayers[I] := Self;
end;

procedure TSimbaImageBoxLayer.SetVisible(Value: Boolean);
begin
  if (FVisible = Value) then
    Exit;

  FVisible := Value;
  FBox.Invalidate();
end;

// the same priority again still moves it on top of its equals
procedure TSimbaImageBoxLayer.SetPriority(Value: Integer);
begin
  FPriority := Value;
  InsertByPriority();
  FBox.Invalidate();
end;

procedure TSimbaImageBoxLayer.SetOpacity(Value: Byte);
begin
  if (FOpacity = Value) then
    Exit;

  FOpacity := Value;
  FBox.Invalidate();
end;

constructor TSimbaImageBoxLayer.Create(Box: TSimbaImageBox);
begin
  if (Box = nil) then
    SimbaException('TSimbaImageBoxLayer.Create: Box cannot be nil');

  inherited Create();

  FBox := Box;
  FVisible := True;
  FOpacity := ALPHA_OPAQUE;

  FPixels := TSimbaImage.Create();
  DefaultPixel := Default(TColorBGRA); // transparent black, so a cleared layer shows what is under it
  Resize(FBox.Background.Width, FBox.Background.Height);

  InsertByPriority();
end;

destructor TSimbaImageBoxLayer.Destroy;
begin
  if (FBox <> nil) then
  begin
    FBox.FLayers.Delete(Self);
    FBox.Invalidate();
  end;

  SetData(nil, 0, 0, 0);
  FreeAndNil(FPixels);

  inherited Destroy();
end;

function TSimbaImageBox.ZoomsOut: Boolean;
begin
  Result := (FZoom < 100);
end;

function TSimbaImageBox.ZoomRatio: Integer;
begin
  if ZoomsOut() then
    Result := 100 div FZoom
  else
    Result := FZoom div 100;
end;

procedure TSimbaImageBox.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

procedure TSimbaImageBox.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TSimbaImageBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Bar: TSimbaScrollBar;
  Lines, Step: Integer;
begin
  if (ssCtrl in Shift) then
  begin
    if FAllowUserZoom then
    begin
      if (WheelDelta > 0) then
        SetZoom(FZoom * 2)
      else
        SetZoom(FZoom div 2);

      Exit(True);
    end;

    Exit(inherited DoMouseWheel(Shift, WheelDelta, MousePos));
  end;

  if (ssShift in Shift) then
    Bar := FHorzScroll
  else
    Bar := FVertScroll;

  Lines := Mouse.WheelScrollLines;
  if (Lines < 1) then
    Lines := 3;

  Step := (WheelDelta * Lines * WHEEL_LINE_HEIGHT) div 120;
  if (Step = 0) then
    Exit(inherited DoMouseWheel(Shift, WheelDelta, MousePos));

  Bar.ScrollBy(-Step);
  Update();

  Result := True;
end;

procedure TSimbaImageBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  // Ctrl+Shift+D toggles debug overlay
  if (Key = VK_D) and (Shift = [ssCtrl, ssShift]) then
  begin
    FDebug.Show := not FDebug.Show;
    Invalidate();
    Key := 0;

    Exit;
  end;

  ImgKeyDown(Key, Shift);
end;

procedure TSimbaImageBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  ImgKeyUp(Key, Shift);
end;

procedure TSimbaImageBox.MouseLeave;
begin
  FStatusBar.PanelText[PANEL_MOUSE] := '';
  ImgMouseLeave();

  inherited MouseLeave();
end;

procedure TSimbaImageBox.MouseEnter;
begin
  ImgMouseEnter();

  inherited MouseEnter();
end;

procedure TSimbaImageBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMousePos := ScreenToImage(TPoint.Create(X, Y));
  if FPanning.Active and (not (ssRight in Shift)) then
    EndPan();

  if FPanning.Active then
  begin
    PanAxis(FVertScroll, FPanning.Y - (Y + VisibleTopY));
    PanAxis(FHorzScroll, FPanning.X - (X + VisibleTopX));

    Update();
  end;

  FStatusBar.PanelText[PANEL_MOUSE] := Format('(%d, %d)', [FMousePos.X, FMousePos.Y]);

  ImgMouseMove(Shift, FMousePos.X, FMousePos.Y);
end;

procedure TSimbaImageBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if CanSetFocus() then
    SetFocus();

  FMousePos := ScreenToImage(TPoint.Create(X, Y));

  if FPanning.Enabled and (Button = mbRight) and (not FPanning.Active) then
  begin
    FPanning.X := X + VisibleTopX;
    FPanning.Y := Y + VisibleTopY;
    FPanning.RestoreCursor := Cursor;

    Cursor := crSizeAll; // still not panning, so this is the visible cursor
    FPanning.Active := True;
  end;

  ImgMouseDown(Button, Shift, FMousePos.X, FMousePos.Y);
end;

procedure TSimbaImageBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMousePos := ScreenToImage(TPoint.Create(X, Y));

  if (Button = mbRight) then
    EndPan();

  ImgMouseUp(Button, Shift, FMousePos.X, FMousePos.Y);
end;

procedure TSimbaImageBox.Click;
begin
  inherited Click;

  ImgClick(FMousePos.X, FMousePos.Y);
end;

procedure TSimbaImageBox.DblClick;
begin
  inherited DblClick();

  ImgDoubleClick(FMousePos.X, FMousePos.Y);
end;

procedure TSimbaImageBox.Paint;
var
  Overlay: TSimbaImageBoxOverlayEvent;
  LayerPixels: TSimbaImageBoxRenderLayers;
  Drawn: TPoint;
  Started, Finished: Double;
begin
  Started := HighResolutionTime();

  Canvas.Brush.Color := clBlack;
  Canvas.Brush.Style := bsSolid;

  // nil when nothing draws over the image, so the renderer can blit the background
  // straight to the screen. A descendant (the shape box) draws in ImgPaintArea.
  Overlay := nil;
  if Assigned(FOnImgPaint) or (ClassType <> TSimbaImageBox) then
    Overlay := @ImgPaintArea;

  BackgroundResized();
  LayerPixels := GetVisibleLayers();

  Drawn := FRenderer.Render(
    Canvas, FBackground, LayerPixels, VisibleImageRect(),
    ZoomRatio(), not ZoomsOut(), Overlay
  );

  // only what the image does not reach needs clearing
  if (Drawn.X < ClientWidth) then
    Canvas.FillRect(Drawn.X, 0, ClientWidth, ClientHeight);
  if (Drawn.Y < ClientHeight) then
    Canvas.FillRect(0, Drawn.Y, Drawn.X, ClientHeight);

  if FDebug.Show then
    PaintDebugInfo();

  Finished := HighResolutionTime();
  FDebug.LastFrameTime := Finished - Started;
end;

procedure TSimbaImageBox.DoOnResize;
begin
  inherited DoOnResize();

  UpdateScrollBars();
  Repaint();
end;

procedure TSimbaImageBox.DoScrollChange(Sender: TObject);
var
  Snapped: Integer;
begin
  Snapped := SnapScroll(TSimbaScrollBar(Sender).Position);
  if (Snapped <> TSimbaScrollBar(Sender).Position) then
  begin
    TSimbaScrollBar(Sender).Position := Snapped;
    Exit;
  end;

  Invalidate();
  if (GetCaptureControl() = Sender) then
    Update();
end;

procedure TSimbaImageBox.DoStatusBarResize(Sender: TObject);
begin
  UpdateScrollBars();
  Invalidate();
end;

procedure TSimbaImageBox.PaintDebugInfo;
begin
  if (Canvas.Font.Name <> 'Courier New') then
  begin
    Canvas.Font.Name := 'Courier New';
    Canvas.Font.Size := 10;
    Canvas.Font.Bold := True;
  end;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clLime;
  Canvas.TextOut(4, 4, Format('%s last frame: %.2f ms', [FRenderer.BlitName, FDebug.LastFrameTime]));
  Canvas.Brush.Style := bsSolid;
end;

procedure TSimbaImageBox.SetScrollPos(Bar: TSimbaScrollBar; Value: Integer);
begin
  Bar.Position := Max(0, Min(Value, Bar.Max - Bar.PageSize));
end;

// Delta is how far the grabbed point has drifted from the cursor
procedure TSimbaImageBox.PanAxis(Bar: TSimbaScrollBar; Delta: Integer);
var
  Steps: Integer;
begin
  Steps := SnapScroll(Abs(Delta));
  if (Steps < 1) then
    Exit;

  if (Delta > 0) then
    Bar.ScrollBy(Steps)
  else
    Bar.ScrollBy(-Steps);
end;

procedure TSimbaImageBox.EndPan;
begin
  if (not FPanning.Active) then
    Exit;

  FPanning.Active := False;
  Cursor := FPanning.RestoreCursor;
end;

procedure TSimbaImageBox.UpdateZoomStatus;
begin
  FStatusBar.PanelText[PANEL_ZOOM] := Format('%d%%', [FZoom]);
end;

procedure TSimbaImageBox.UpdateScrollBars;
var
  W, H, Line: Integer;
begin
  if (FVertScroll = nil) or (FHorzScroll = nil) then
    Exit;

  if ZoomsOut() then
  begin
    W := (FImageWidth + ZoomRatio() - 1) div ZoomRatio();
    H := (FImageHeight + ZoomRatio() - 1) div ZoomRatio();
  end else
  begin
    W := FImageWidth * ZoomRatio();
    H := FImageHeight * ZoomRatio();
  end;

  Line := Max(SnapScroll(WHEEL_LINE_HEIGHT), ZoomRatio());

  FVertScroll.SmallChange := Line;
  FVertScroll.PageSize := ViewHeight;
  FVertScroll.Max := H;

  FHorzScroll.SmallChange := Line;
  FHorzScroll.PageSize := ViewWidth;
  FHorzScroll.Max := W;

  SetScrollPos(FVertScroll, FVertScroll.Position);
  SetScrollPos(FHorzScroll, FHorzScroll.Position);
end;

procedure TSimbaImageBox.BackgroundResized;
begin
  if (FBackground.Width = FImageWidth) and (FBackground.Height = FImageHeight) then
    Exit;

  FImageWidth := FBackground.Width;
  FImageHeight := FBackground.Height;
  FZoom := 100;
  FHorzScroll.Position := 0;
  FVertScroll.Position := 0;
  FStatusBar.PanelText[PANEL_SIZE] := Format('%d x %d', [FImageWidth, FImageHeight]);

  UpdateScrollBars();
  UpdateZoomStatus();
end;

function TSimbaImageBox.ImageToScroll(V: Integer): Integer;
begin
  if ZoomsOut() then
    Result := V div ZoomRatio()
  else
    Result := V * ZoomRatio();
end;

function TSimbaImageBox.ImageToScroll(ImageXY: TPoint): TPoint;
begin
  Result.X := ImageToScroll(ImageXY.X);
  Result.Y := ImageToScroll(ImageXY.Y);
end;

function TSimbaImageBox.ScrollToImage(V: Integer): Integer;
begin
  if ZoomsOut() then
    Result := V * ZoomRatio()
  else
    Result := V div ZoomRatio();
end;

function TSimbaImageBox.SnapScroll(V: Integer): Integer;
begin
  Result := V;
  if (FZoom > 100) then // zoomed in
    Dec(Result, Result mod ZoomRatio());
end;

function TSimbaImageBox.VisibleTopX: Integer;
begin
  Result := SnapScroll(FHorzScroll.Position);
end;

function TSimbaImageBox.VisibleTopY: Integer;
begin
  Result := SnapScroll(FVertScroll.Position);
end;

function TSimbaImageBox.VisibleImageRect: TRect;
var
  TopX, TopY, Over: Integer;
begin
  TopX := VisibleTopX;
  TopY := VisibleTopY;

  // Overshoot by one screen pixel's worth of image so a partially visible pixel still gets drawn
  Over := IfThen(ZoomsOut(), 1, ZoomRatio());

  Result.Left   := ScrollToImage(TopX);
  Result.Top    := ScrollToImage(TopY);
  Result.Right  := Min(ScrollToImage(TopX + ViewWidth  + Over), FImageWidth);
  Result.Bottom := Min(ScrollToImage(TopY + ViewHeight + Over), FImageHeight);
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

procedure TSimbaImageBox.ImgPaintArea(ACanvas: TSimbaCanvas; R: TRect);
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

function TSimbaImageBox.GetLayer(Index: Integer): TSimbaImageBoxLayer;
begin
  Result := FLayers[Index];
end;

function TSimbaImageBox.GetLayerCount: Integer;
begin
  Result := FLayers.Count;
end;

function TSimbaImageBox.GetVisibleLayers: TSimbaImageBoxRenderLayers;
var
  Layer: TSimbaImageBoxLayer;
  I, Count: Integer;
begin
  SetLength(Result, FLayers.Count);
  Count := 0;
  for I := 0 to FLayers.Count - 1 do
  begin
    Layer := FLayers[I];
    if (Layer.Width <> FBackground.Width) or (Layer.Height <> FBackground.Height) then
      Layer.Resize(FBackground.Width, FBackground.Height);

    if Layer.Visible and (Layer.Opacity > ALPHA_TRANSPARENT) then
    begin
      Result[Count].Pixels := Layer.FPixels;
      Result[Count].Opacity := Layer.Opacity;
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

function TSimbaImageBox.GetShowStatusBar: Boolean;
begin
  Result := FStatusBar.Visible;
end;

function TSimbaImageBox.GetShowScrollbars: Boolean;
begin
  Result := FVertScroll.Visible;
end;

function TSimbaImageBox.GetAllowMoving: Boolean;
begin
  Result := FPanning.Enabled;
end;

function TSimbaImageBox.GetStatus: String;
begin
  Result := FStatusBar.PanelText[PANEL_STATUS];
end;

// while panning the box shows crSizeAll
function TSimbaImageBox.GetCursor: TCursor;
begin
  if FPanning.Active then
    Result := FPanning.RestoreCursor
  else
    Result := inherited GetCursor();
end;

procedure TSimbaImageBox.SetShowStatusBar(AValue: Boolean);
begin
  FStatusBar.Visible := AValue;
end;

procedure TSimbaImageBox.SetShowScrollbars(AValue: Boolean);
begin
  FVertScroll.Visible := AValue;
  FHorzScroll.Visible := AValue;

  UpdateScrollBars();
  Invalidate();
end;

procedure TSimbaImageBox.SetAllowMoving(AValue: Boolean);
begin
  FPanning.Enabled := AValue;
end;

procedure TSimbaImageBox.SetStatus(Value: String);
begin
  FStatusBar.PanelText[PANEL_STATUS] := Value;
end;

procedure TSimbaImageBox.SetBackground(AValue: TSimbaImage);
begin
  if (AValue <> FBackground) then
    SwapBackground(AValue).Free();
end;

procedure TSimbaImageBox.SetZoom(Level: Integer);
var
  Ref, Anchor, Scroll: TPoint;
begin
  Level := SnapZoom(Level, FMinZoom, FMaxZoom);
  if (Level = FZoom) then
    Exit;

  // keep whatever image pixel is under the cursor (or the middle of the view) where it is
  if MouseInClient then
    Ref := ScreenToClient(Mouse.CursorPos)
  else
    Ref := TPoint.Create(ViewWidth div 2, ViewHeight div 2);

  Anchor := ScreenToImage(Ref);
  Anchor.X := Max(0, Min(Anchor.X, FImageWidth - 1));
  Anchor.Y := Max(0, Min(Anchor.Y, FImageHeight - 1));

  FZoom := Level;

  UpdateScrollBars();

  Scroll := ImageToScroll(Anchor);
  SetScrollPos(FHorzScroll, Scroll.X - Ref.X);
  SetScrollPos(FVertScroll, Scroll.Y - Ref.Y);

  Invalidate();

  UpdateZoomStatus();
end;

procedure TSimbaImageBox.SetMinZoom(AValue: Integer);
begin
  FMinZoom := SnapZoom(AValue, ZOOM_LOWEST, 100);
  SetZoom(FZoom);
end;

procedure TSimbaImageBox.SetMaxZoom(AValue: Integer);
begin
  FMaxZoom := SnapZoom(AValue, 100, ZOOM_HIGHEST);
  SetZoom(FZoom);
end;

procedure TSimbaImageBox.SetCursor(Value: TCursor);
begin
  if FPanning.Active then
    FPanning.RestoreCursor := Value
  else
    inherited SetCursor(Value);
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  FZoom := 100;
  FMinZoom := ZOOM_LOWEST;
  FMaxZoom := ZOOM_HIGHEST;
  FAllowUserZoom := True;
  FDebug.Show := False; // Ctrl+Shift+D
  FPanning.Enabled := True;
  FPanning.RestoreCursor := crDefault;

  FStatusBarPanel := TPanel.Create(Self);
  FStatusBarPanel.Parent := Self;
  FStatusBarPanel.BevelOuter := bvNone;
  FStatusBarPanel.Align := alBottom;
  FStatusBarPanel.AutoSize := True;
  FStatusBarPanel.Color := SimbaComponentTheme.ColorFrame;
  FStatusBarPanel.OnResize := @DoStatusBarResize;

  FUserPanel := TPanel.Create(Self);
  FUserPanel.BevelOuter := bvNone;
  FUserPanel.Parent := FStatusBarPanel;
  FUserPanel.Color := SimbaComponentTheme.ColorFrame;
  FUserPanel.AutoSize := True;
  FUserPanel.Align := alClient;

  FStatusBar := TSimbaStatusBar.Create(Self);
  FStatusBar.Parent := FStatusBarPanel;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := PANEL_COUNT;
  FStatusBar.PanelTextMeasure[PANEL_MOUSE] := '(1235, 1234)';
  FStatusBar.PanelTextMeasure[PANEL_SIZE] := '1234 x 1234';
  FStatusBar.PanelTextMeasure[PANEL_ZOOM] := '1000%';

  FVertScroll := TSimbaScrollBar.Create(Self);
  FVertScroll.Parent := Self;
  FVertScroll.Kind := sbVertical;
  FVertScroll.Align := alRight;
  FVertScroll.OnChange := @DoScrollChange;

  FHorzScroll := TSimbaScrollBar.Create(Self);
  FHorzScroll.Parent := Self;
  FHorzScroll.Kind := sbHorizontal;
  FHorzScroll.Align := alBottom;
  FHorzScroll.OnChange := @DoScrollChange;
  FHorzScroll.IndentCorner := 100;

  FRenderer := CreateImageBoxRenderer();

  FBackground := TSimbaImage.Create();
  FLayers := TSimbaImageBoxLayerList.Create();

  UpdateZoomStatus();
end;

destructor TSimbaImageBox.Destroy;
begin
  FreeAndNil(FRenderer);
  FreeAndNil(FBackground);
  while (FLayers.Count > 0) do
    FLayers.Last.Free();
  FreeAndNil(FLayers);

  inherited Destroy();
end;

// Only the view, not the scrollbars or the status bar: they repaint themselves.
procedure TSimbaImageBox.Invalidate;
var
  R: TRect;
begin
  if not HandleAllocated or (csDestroying in ComponentState) then
    Exit;

  R := TRect.Create(0, 0, ViewWidth, ViewHeight);
  InvalidateRect(Handle, @R, False);
end;

function TSimbaImageBox.ViewWidth: Integer;
begin
  Result := ClientWidth;
  if FVertScroll.Visible then
    Dec(Result, FVertScroll.Width);
  if (Result < 0) then
    Result := 0;
end;

function TSimbaImageBox.ViewHeight: Integer;
begin
  Result := ClientHeight;
  if FStatusBarPanel.Visible then
    Dec(Result, FStatusBarPanel.Height);
  if FHorzScroll.Visible then
    Dec(Result, FHorzScroll.Height);
  if (Result < 0) then
    Result := 0;
end;

function TSimbaImageBox.ScreenToImage(ScreenXY: TPoint): TPoint;
begin
  Result.X := ScrollToImage(VisibleTopX + ScreenXY.X);
  Result.Y := ScrollToImage(VisibleTopY + ScreenXY.Y);
end;

function TSimbaImageBox.IsPointVisible(ImageXY: TPoint): Boolean;
var
  P: TPoint;
begin
  P := ImageToScroll(ImageXY);
  Dec(P.X, VisibleTopX);
  Dec(P.Y, VisibleTopY);

  Result := (P.X >= 0) and (P.Y >= 0) and (P.X < ViewWidth) and (P.Y < ViewHeight);
end;

procedure TSimbaImageBox.MoveTo(ImageXY: TPoint);
var
  Scroll: TPoint;
begin
  Scroll := ImageToScroll(ImageXY);

  SetScrollPos(FHorzScroll, Scroll.X - (ViewWidth  div 2));
  SetScrollPos(FVertScroll, Scroll.Y - (ViewHeight div 2));
end;

procedure TSimbaImageBox.BackgroundChanged;
var
  I: Integer;
begin
  // a new image: every layer starts blank
  for I := 0 to FLayers.Count - 1 do
    FLayers[I].Resize(FBackground.Width, FBackground.Height);

  BackgroundResized();
  Invalidate();
end;

function TSimbaImageBox.SwapBackground(AValue: TSimbaImage): TSimbaImage;
begin
  if (AValue = nil) then
    SimbaException('TSimbaImageBox.Background cannot be nil');

  Result := FBackground;
  FBackground := AValue;

  BackgroundChanged();
end;

end.
