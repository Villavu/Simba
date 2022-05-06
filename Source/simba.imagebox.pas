{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.imagebox;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, comctrls,
  lmessages, lclintf, lcltype,
  simba.mufasatypes, simba.bitmap, simba.dtm, simba.iomanager;

type
  PSimbaImageBox = ^TSimbaImageBox;
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

  PSimbaImageBox_PaintArea = ^TSimbaImageBox_PaintArea;
  TSimbaImageBox_PaintArea = procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object;

  TSimbaImageBox = class(TCustomControl)
  protected
    FScrollBox: TSimbaImageBox_ScrollBox;
    FStatusBar: TStatusBar;
    FMousePanel: TStatusPanel;
    FDimensionsPanel: TStatusPanel;
    FZoomPanel: TStatusPanel;
    FStatusPanel: TStatusPanel;
    FZoom: Single;
    FWidth: Int32;
    FHeight: Int32;
    FOnPaintArea: TSimbaImageBox_PaintArea;
    FBackground: TBitmap;
    FBackgroundCanvas: TCanvas;
    FOverlay: TBitmap;
    FHasOverlay: Boolean;
    FScroll: record
      X, Y: Int32;
      Active: Boolean;
    end;

    procedure ScrollBoxPaint(Sender: TObject);

    procedure FontChanged(Sender: TObject); override;
    procedure BackgroundChanged(Sender: TObject);

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

    procedure SetParent(Value: TWinControl); override;
    procedure SetZoom(Value: Single);
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDblClick;
  public
    property OnPaintArea: TSimbaImageBox_PaintArea read FOnPaintArea write FOnPaintArea;
    property Zoom: Single read FZoom write SetZoom;
    property StatusBar: TStatusBar read FStatusBar;
    property StatusPanel: TStatusPanel read FStatusPanel;
    property Background: TCanvas read FBackgroundCanvas;

    procedure MoveTo(X, Y: Integer);
    function IsVisible(X, Y: Integer): Boolean; overload;

    procedure SetBackground(Data: PRGB32; AWidth, AHeight: Integer); overload;
    procedure SetBackground(FileName: String); overload;
    procedure SetBackground(Bitmap: TMufasaBitmap); overload;
    procedure SetBackground(IOManager: TIOManager; X1, Y1, X2, Y2: Integer); overload;
    procedure SetBackground(IOManager: TIOManager); overload;

    procedure DebugColor(CTS: Integer; Col, Tol: Integer; HueMod: Extended = 0.2; SatMod: Extended = 0.2);
    procedure DebugDTM(DTM: TMDTM);
    procedure DebugTPA(TPA: TPointArray);

    procedure Clear;
    procedure Paint; override;

    constructor Create(AOwner: TComponent); overload; override;
    constructor CreateNoOverlay(AOwner: TComponent); overload; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  math,
  simba.nativeinterface, simba.bitmap_misc, simba.overallocatearray, simba.tpa,
  simba.finder_dtm, simba.finder_color;

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

procedure TSimbaImageBox_ScrollBox.SetSize(AWidth, AHeight: Integer);
begin
  FWidth  := AWidth;
  FHeight := AHeight;

  ComputeScrollbars();
end;

constructor TSimbaImageBox_ScrollBox.Create(ImageBox: TSimbaImageBox);
begin
  inherited Create(ImageBox);

  ControlStyle := ControlStyle + [csOpaque];

  FImageBox := ImageBox;
end;

procedure TSimbaImageBox.SetZoom(Value: Single);
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

    FScrollBox.SetSize(
      Trunc(FBackground.Width * FZoom),
      Trunc(FBackground.Height * FZoom)
    );

    MoveTo(Current.X, Current.Y);

    Paint();
  end;
end;

procedure TSimbaImageBox.ScrollBoxPaint(Sender: TObject);
var
  LocalRect, ScreenRect: TRect;
  W, H: Integer;
  DC: HDC;
begin
  SimbaNativeInterface.ClearInterpolation(FScrollBox.Canvas);

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

  if FHasOverlay then
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
    // Measure on slightly bigger font size
    // Font size can be 0 so use GetFontData
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 3;

    FMousePanel.Width := Canvas.TextWidth('(10000, 10000)');
    FDimensionsPanel.Width  := Canvas.TextWidth('10000x10000');
    FZoomPanel.Width  := Canvas.TextWidth('1000%');

    FStatusBar.Height := Canvas.TextHeight('TaylorSwift');
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
    FDimensionsPanel.Text := Format('%dx%d', [FWidth, FHeight]);

    FZoom := 1;
    FZoomPanel.Text := '100%';

    FScrollBox.SetSize(FBackground.Width, FBackground.Height);
    FScrollBox.HorzScrollBar.Position := 0;
    FScrollBox.VertScrollBar.Position := 0;
  end;

  if FHasOverlay then
    FOverlay.LoadFromRawImage(FBackground.RawImage, False);

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
    if (FZoom <> 1) then
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
    FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position - (ClientWidth div 2);

  if FScrollBox.VertScrollBar.Position < (FScrollBox.VertScrollBar.Range - FScrollBox.VertScrollBar.Page) then
    FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - (ClientHeight div 2);
end;

function TSimbaImageBox.IsVisible(X, Y: Integer): Boolean;
begin
  X := Trunc(X * FZoom);
  Y := Trunc(Y * FZoom);

  Result := InRange(X, FScrollBox.HorzScrollBar.Position, FScrollBox.HorzScrollBar.Position + FScrollBox.ClientWidth) and
            InRange(Y, FScrollBox.VertScrollBar.Position, FScrollBox.VertScrollBar.Position + FScrollBox.ClientHeight);
end;

procedure TSimbaImageBox.SetBackground(Data: PRGB32; AWidth, AHeight: Integer);
begin
  FBackground.FromData(Data, AWidth, AHeight);
end;

procedure TSimbaImageBox.SetBackground(FileName: String);
var
  Bitmap: TMufasaBitmap;
begin
  Bitmap := TMufasaBitmap.Create();
  Bitmap.LoadFromFile(FileName);

  FBackground.FromData(Bitmap.Data, Bitmap.Width, Bitmap.Height);

  Bitmap.Free();
end;

procedure TSimbaImageBox.SetBackground(Bitmap: TMufasaBitmap);
begin
  FBackground.FromData(Bitmap.Data, Bitmap.Width, Bitmap.Height);
end;

procedure TSimbaImageBox.SetBackground(IOManager: TIOManager; X1, Y1, X2, Y2: Integer);
var
  Data: PRGB32;
begin
  Data := IOManager.CopyData(X1, Y1, X2-X1+1, Y2-Y1+1);

  if (Data <> nil) then
  try
    FBackground.FromData(Data, X2-X1+1, Y2-Y1+1);
  finally
    FreeMem(Data);
  end;
end;

procedure TSimbaImageBox.SetBackground(IOManager: TIOManager);
var
  Wid, Hei: Integer;
begin
  IOManager.GetDimensions(Wid, Hei);

  SetBackground(IOManager, 0, 0, Wid-1, Hei-1);
end;

procedure TSimbaImageBox.DebugColor(CTS: Integer; Col, Tol: Integer; HueMod: Extended; SatMod: Extended);
var
  Bitmap: TMufasaBitmap;
  Buffer: TFindColorBuffer;
  TPA: TPointArray;
begin
  Bitmap := FBackground.ToMufasaBitmap();

  with Buffer do
  begin
    Buffer.Ptr := Bitmap.Data;
    Buffer.PtrInc := 0;

    X1 := 0;
    Y1 := 0;
    X2 := Bitmap.Width - 1;
    Y2 := Bitmap.Height - 1;

    case CTS of
      0: FindCTS0(TPA, Col, Tol);
      1: FindCTS1(TPA, Col, Tol);
      2: FindCTS2(TPA, Col, Tol, HueMod, SatMod);
    end;
  end;

  FOverlay.DrawPoints(TPA);
  FStatusPanel.Text := Format('Found %.0n pixels (CTS%d)', [Single(Length(TPA)), CTS]);

  Bitmap.Free();
end;

procedure TSimbaImageBox.DebugDTM(DTM: TMDTM);
var
  Bitmap: TMufasaBitmap;
  TPA: TPointArray;
  Buffer: TFindDTMBuffer;
  I: Integer;
  Crosses: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Bitmap := FBackground.ToMufasaBitmap();

  with Buffer do
  begin
    Buffer.Data := Bitmap.Data;
    Buffer.LineWidth := Bitmap.Width;

    X1 := 0;
    Y1 := 0;
    X2 := Bitmap.Width - 1;
    Y2 := Bitmap.Height - 1;

    TPA := FindDTMs(DTM);
  end;

  Crosses.Init();
  for I := 0 to High(TPA) do
    with TPA[I] do
    begin
      Crosses.Add(TPAFromLine(X - 5, Y - 5, X + 5, Y + 5));
      Crosses.Add(TPAFromLine(X + 5, Y - 5, X - 5, Y + 5));
    end;

  FOverlay.DrawPoints(Crosses.Trim());
  FStatusPanel.Text := Format('Found %.0n DTMs', [Single(Length(TPA))]);

  Bitmap.Free();
end;

procedure TSimbaImageBox.DebugTPA(TPA: TPointArray);
begin
  FBackground.DrawPoints(TPA);
end;

procedure TSimbaImageBox.Clear;
begin
  FOverlay.Canvas.Draw(0, 0, FBackground);
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackground := TBitmap.Create();
  FBackground.OnChange := @BackgroundChanged;
  FBackgroundCanvas := FBackground.Canvas;
  FBackgroundCanvas.AntialiasingMode := amOff;

  FOverlay := TBitmap.Create();

  FHasOverlay := True;

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

  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.SimplePanel := False;
  FStatusBar.AutoSize := False;

  FMousePanel := FStatusBar.Panels.Add();
  FMousePanel.Text := '(-1, -1)';
  FDimensionsPanel := FStatusBar.Panels.Add();
  FDimensionsPanel.Text := '(0, 0)';
  FZoomPanel := FStatusBar.Panels.Add();
  FZoomPanel.Text := '100%';
  FStatusPanel := FStatusBar.Panels.Add();

  FWidth := 0;
  FHeight := 0;
  FZoom := 1;

  Canvas.Free();
  Canvas := FOverlay.Canvas;
  Canvas.AntialiasingMode := amOff;

  FontChanged(Self);
end;

constructor TSimbaImageBox.CreateNoOverlay(AOwner: TComponent);
begin
  Create(AOwner);

  FHasOverlay := False;
end;

destructor TSimbaImageBox.Destroy;
begin
  Canvas := nil; // is FOverlay.Canvas

  if (FOverlay <> nil) then
    FOverlay.Free();
  if (FBackground <> nil) then
    FBackground.Free();

  inherited Destroy();
end;

end.

