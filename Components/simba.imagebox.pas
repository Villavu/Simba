unit simba.imagebox;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, comctrls,
  simba.bitmap, simba.mufasatypes;

type
  TSimbaImageBox = class(TCustomControl)
  protected
    FScrollBox: TScrollBox;
    FImage: TImage;
    FStatusBar: TStatusBar;
    FMousePanel: TStatusPanel;
    FDimensionsPanel: TStatusPanel;
    FStatusPanel: TStatusPanel;
    FOnImageMouseDown: TMouseEvent;
    FOnImageMouseUp: TMouseEvent;
    FOnImageMouseMove: TMouseMoveEvent;
    FOnImageDoubleClick: TMouseEvent;
    FZoom: Double;
    FWidth: Int32;
    FHeight: Int32;
    FScroll: record
      X, Y: Int32;
      Active: Boolean;
    end;
    FMouse: record
      X, Y: Int32;
    end;

    procedure FontChanged(Sender: TObject); override;

    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ImageMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure ImageMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure ImageMouseLeave(Sender: TObject); virtual;
    procedure ImageDoubleClick(Sender: TObject); virtual;

    function GetCursor: TCursor; override;
    procedure SetCursor(Value: TCursor); override;

    procedure SetZoom(Value: Double); virtual;
  protected
  const
    ZOOM_MIN = 0.25;
    ZOOM_MAX = {$IFDEF LINUX}2.0{$ELSE}16.0{$ENDIF}; // GTK2 resizing sucks. Let's hope it's better in GTK3.
  public
    property Zoom: Double read FZoom write SetZoom;
    property StatusBar: TStatusBar read FStatusBar;
    property StatusPanel: TStatusPanel read FStatusPanel;

    property OnImageMouseDown: TMouseEvent read FOnImageMouseDown write FOnImageMouseDown;
    property OnImageMouseUp: TMouseEvent read FOnImageMouseUp write FOnImageMouseUp;
    property OnImageMouseMove: TMouseMoveEvent read FOnImageMouseMove write FOnImageMouseMove;
    property OnImageDoubleClick: TMouseEvent read FOnImageDoubleClick write FOnImageDoubleClick;

    procedure Changed; virtual;

    procedure MoveToPoint(X, Y: Int32); virtual;
    function IsPointVisible(X, Y: Int32): Boolean; virtual;

    procedure Clear; virtual;
    procedure Draw(Data: PRGB32; AWidth, AHeight: Int32); virtual; overload;
    procedure Write(Stream: TMemoryStream); virtual; overload;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  types, math;

procedure TSimbaImageBox.SetZoom(Value: Double);
var
  X, Y, W, H: Int32;
begin
  if ((Value = 0.25) or (Value = 0.50) or (Value = 1.00) or (Value = 2.00) or (Value = 4.00) or (Value = 8.00) or (Value = 16.00)) and
     ((Value >= ZOOM_MIN) and (Value <= ZOOM_MAX)) then
  begin
    FZoom := Value;

    X := FImage.ScreenToClient(Mouse.CursorPos).X;
    Y := FImage.ScreenToClient(Mouse.CursorPos).Y;
    W := FImage.Width;
    H := FImage.Height;

    if (FImage.Picture.Width > 0) and (FImage.Picture.Height > 0) then
    begin
      FImage.Width := Trunc(FImage.Picture.Width * FZoom);
      FImage.Height := Trunc(FImage.Picture.Height * FZoom);

      FScrollBox.HorzScrollBar.Position := Round(X * FImage.Width / W) - Round(FScrollBox.ClientWidth / 2);
      FScrollBox.VertScrollBar.Position := Round(Y * FImage.Height / H) - Round(FScrollBox.ClientHeight / 2);
    end;

    if (FImage.Width < FScrollBox.ClientWidth) then
      FImage.Left := (FScrollBox.ClientWidth div 2) - (FImage.Width div 2)
    else
      FImage.Left := 0;

    if (FImage.Height < FScrollBox.ClientHeight) then
      FImage.Top := (FScrollBox.ClientHeight div 2) - (FImage.Height div 2)
    else
      FImage.Top := 0;
  end;
end;

procedure TSimbaImageBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  with Canvas.TextExtent('9999x9999') do
  begin
    FMousePanel.Width := Round(CX * 1.50);
    FDimensionsPanel.Width := Round(CX * 1.50);
    FStatusBar.Constraints.MinHeight := Round(CY * 1.25);
  end;
end;

procedure TSimbaImageBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := Trunc(X / FZoom);
  Y := Trunc(Y / FZoom);

  case Button of
    mbRight:
      begin
        FImage.Cursor := crSizeAll;

        FScroll.X := Trunc(X * FZoom);
        FScroll.Y := Trunc(Y * FZoom);
        FScroll.Active := True;
      end;
  end;

  if (FOnImageMouseDown <> nil) then
    FOnImageMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSimbaImageBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbRight:
      begin
        FScroll.Active := not FScroll.Active;
        if (not FScroll.Active) then
          FImage.Cursor := crDefault;
      end;
  end;

  if FZoom <> 1 then
  begin
    X := Trunc(X / FZoom);
    Y := Trunc(Y / FZoom);
  end;

  if (FOnImageMouseUp <> nil) then
    FOnImageMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSimbaImageBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FScroll.Active then
  begin
    if (Abs(FScroll.Y - Y) > 0) then
      FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + (FScroll.Y - Y);
    if (Abs(FScroll.X - X) > 0) then
      FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position + (FScroll.X - X);
  end else
  begin
    if FZoom <> 1 then
    begin
      X := Trunc(X / FZoom);
      Y := Trunc(Y / FZoom);
    end;

    FMousePanel.Text := Format('%d, %d', [X, Y]);

    if (FOnImageMouseMove <> nil) then
      FOnImageMouseMove(Self, Shift, X, Y);

    FMouse.X := X;
    FMouse.Y := Y;
  end;
end;

procedure TSimbaImageBox.ImageMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
    Zoom := Zoom * 2
  else
    FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - (FScrollBox.VertScrollBar.Increment * 2);
end;

procedure TSimbaImageBox.ImageMouseLeave(Sender: TObject);
begin
  FScroll.Active := False;
  FImage.Cursor := crDefault;
  FMousePanel.Text := '(-1, -1)';
end;

procedure TSimbaImageBox.ImageDoubleClick(Sender: TObject);
begin
  if (FOnImageDoubleClick <> nil) then
    FOnImageDoubleClick(Self, mbLeft, [], FMouse.X, FMouse.Y);
end;

procedure TSimbaImageBox.ImageMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
    Zoom := Zoom / 2
  else
    FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + (FScrollBox.VertScrollBar.Increment * 2);
end;

procedure TSimbaImageBox.SetCursor(Value: TCursor);
begin
  FImage.Cursor := Value;
end;

function TSimbaImageBox.GetCursor: TCursor;
begin
  Result := FImage.Cursor;
end;

procedure TSimbaImageBox.MoveToPoint(X, Y: Int32);
begin
  X := Trunc(X * FZoom);
  Y := Trunc(Y * FZoom);

  FScrollBox.VertScrollBar.Position := Y - (FScrollBox.ClientHeight div 2);
  FScrollBox.HorzScrollBar.Position := X - (FScrollBox.ClientWidth div 2);
end;

function TSimbaImageBox.IsPointVisible(X, Y: Int32): Boolean;
begin
  X := Trunc(X * FZoom);
  Y := Trunc(Y * FZoom);

  Result := InRange(X, FScrollBox.HorzScrollBar.Position, FScrollBox.HorzScrollBar.Position + FScrollBox.ClientWidth) and
            InRange(Y, FScrollBox.VertScrollBar.Position, FScrollBox.VertScrollBar.Position + FScrollBox.ClientHeight);
end;

procedure TSimbaImageBox.Clear;
begin
  FImage.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  FImage.Picture.Bitmap.Canvas.FillRect(0, 0, FImage.Picture.Bitmap.Width, FImage.Picture.Bitmap.Height);
  FImage.Update();
end;

procedure TSimbaImageBox.Draw(Data: PRGB32; AWidth, AHeight: Int32);
var
  Upper: PtrUInt;
  Source, Dest: PByte;
  SourceBytesPerLine, DestBytesPerLine: Int32;

  procedure Row(Source, Dest: PByte);
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

var
  Bitmap: TBitmap;
begin
  Bitmap := FImage.Picture.Bitmap;
  Bitmap.BeginUpdate();
  Bitmap.Width := AWidth;
  Bitmap.Height := AHeight;

  Dest := Bitmap.RawImage.Data;
  DestBytesPerLine := Bitmap.RawImage.Description.BytesPerLine;

  Source := PByte(Data);
  SourceBytesPerLine := AWidth * SizeOf(TRGB32);

  Upper := PtrUInt(Source + (SourceBytesPerLine * AHeight));

  while PtrUInt(Source) < Upper do
  begin
    Row(Source, Dest);

    Inc(Source, SourceBytesPerLine);
    Inc(Dest, DestBytesPerLine);
  end;

  Bitmap.EndUpdate();

  Changed();
end;

procedure TSimbaImageBox.Write(Stream: TMemoryStream);
var
  Upper: PtrUInt;
  Source: PByte;
  SourceBytesPerLine: Int32;

  procedure Row(Source: PByte);
  var
    Upper: PtrUInt;
  begin
    Upper := PtrUInt(Source + (FWidth * SizeOf(TRGB24)));

    while PtrUInt(Source) < Upper do
    begin
      Stream.Write(PRGB24(Source)^, SizeOf(TRGB24));
      Stream.WriteByte(0); // Alpha

      Inc(Source, SizeOf(TRGB24));
    end;
  end;

begin
  Stream.Write(FWidth, SizeOf(Int32));
  Stream.Write(FHeight, SizeOf(Int32));

  Source := PByte(FImage.Picture.Bitmap.RawImage.Data);
  SourceBytesPerLine := FImage.Picture.Bitmap.RawImage.Description.BytesPerLine;

  Upper := PtrUInt(Source + (SourceBytesPerLine * (FHeight)));

  while PtrUInt(Source) < Upper do
  begin
    Row(Source);

    Inc(Source, SourceBytesPerLine);
  end;
end;

procedure TSimbaImageBox.Changed;
begin
  if (FWidth <> FImage.Picture.Bitmap.Width) or (FHeight <> FImage.Picture.Bitmap.Height) then
  begin
    FWidth := FImage.Picture.Bitmap.Width;
    FHeight := FImage.Picture.Bitmap.Height;
    FDimensionsPanel.Text := Format('%dx%d', [FWidth, FHeight]);

    Zoom := 1;
    MoveToPoint(0, 0);
  end;

  FImage.Update();
end;

constructor TSimbaImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.Parent := Self;
  FScrollBox.Align := alClient;
  FScrollBox.BorderStyle := bsNone;
  FScrollBox.HorzScrollBar.Tracking := True;
  FScrollBox.VertScrollBar.Tracking := True;
  FScrollBox.Color := 0;
  FScrollBox.DoubleBuffered := True;

  FImage := TImage.Create(Self);
  FImage.Parent := FScrollBox;
  FImage.Stretch := True;
  FImage.OnMouseDown := @ImageMouseDown;
  FImage.OnMouseUp := @ImageMouseUp;
  FImage.OnMouseMove := @ImageMouseMove;
  FImage.OnMouseWheelUp := @ImageMouseWheelUp;
  FImage.OnMouseWheelDown := @ImageMouseWheelDown;
  FImage.OnMouseLeave := @ImageMouseLeave;
  FImage.OnDblClick := @ImageDoubleClick;

  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.Align := alBottom;
  FStatusBar.SimplePanel := False;

  FMousePanel := FStatusBar.Panels.Add();
  FMousePanel.Text := '(-1, -1)';
  FDimensionsPanel := FStatusBar.Panels.Add();
  FDimensionsPanel.Text := '(0, 0)';
  FStatusPanel := FStatusBar.Panels.Add();

  FWidth := 0;
  FHeight := 0;

  Zoom := 1;
end;

end.

