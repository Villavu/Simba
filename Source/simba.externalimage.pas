{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Provides a way to draw on an image which data is externally allocated.
}
unit simba.externalimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.baseclass, simba.image, simba.image_utils, simba.image_textdrawer,
  simba.simplelock;

type
  PSimbaExternalImage = ^TSimbaExternalImage;
  TSimbaExternalImage = class(TSimbaBaseClass)
  protected
    FLock: TSimpleEnterableLock;

    FBackBuffer: TSimbaImage;

    FData: PColorBGRA;
    FWidth, FHeight: Integer;
    FUserData: Pointer;
    FInUpdate: Integer;

    FDirty: Boolean;
    FDirtyBox: TBox;

    FAutoResize: Boolean;

    procedure CheckInUpdate; inline;

    procedure addDirty(b: TBox); inline;
    procedure addAllDirty; inline;

    procedure Flush;

    function GetFontAntialiasing: Boolean;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;
    function GetUserData: Pointer;

    procedure SetUserData(UserData: Pointer);
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property UserData: Pointer read GetUserData write SetUserData;
    property AutoResize: Boolean read FAutoResize write FAutoResize;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    procedure SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
    procedure Resize(NewWidth, NewHeight: Integer);

    procedure SetAlpha(Value: Byte); overload;
    procedure SetAlpha(Points: TPointArray; Value: Byte); overload;
    procedure SetAlpha(Color: TColor; Value: Byte); overload;

    // Point
    procedure DrawATPA(ATPA: T2DPointArray; Color: TColor = -1; Alpha: Byte = 0);
    procedure DrawTPA(TPA: TPointArray; Color: TColor; Alpha: Byte = 0);

    // Line
    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawLine(Start, Stop: TPoint; Color: TColor; Alpha: Byte = 0);
    procedure DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte = 0);

    procedure Clear; overload;
    procedure Clear(Box: TBox); overload;
    procedure ClearInverted(Box: TBox);

    procedure Fill(Color: TColor; Alpha: Byte = 0);

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EDrawTextAlignSet; Color: TColor); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);

    // Image
    procedure DrawImage(Image: TSimbaImage; Location: TPoint; Alpha: Byte = 0);

    // Box
    procedure DrawBox(Box: TBox; Color: TColor; Alpha: Byte = 0);
    procedure DrawBoxFilled(Box: TBox; Color: TColor; Alpha: Byte = 0);
    procedure DrawBoxInverted(Box: TBox; Color: TColor; Alpha: Byte = 0);

    // Poly
    procedure DrawPolygon(Points: TPointArray; Color: TColor; Alpha: Byte = 0);
    procedure DrawPolygonFilled(Points: TPointArray; Color: TColor; Alpha: Byte = 0);
    procedure DrawPolygonInverted(Points: TPointArray; Color: TColor; Alpha: Byte = 0);

    // Quad
    procedure DrawQuad(Quad: TQuad; Color: TColor; Alpha: Byte = 0);
    procedure DrawQuadFilled(Quad: TQuad; Color: TColor; Alpha: Byte = 0);
    procedure DrawQuadInverted(Quad: TQuad; Color: TColor; Alpha: Byte = 0);

    // Circle
    procedure DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);

    // Antialiased
    procedure DrawLineAA(Start, Stop: TPoint; Color: TColor; Thickness: Single = 1.5);
    procedure DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Color: TColor; Thickness: Single = 1.5);
    procedure DrawCircleAA(ACenter: TPoint; Radius: Integer; Color: TColor; Thickness: Single = 1.5);

    // Arrays
    procedure DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
  end;

implementation

uses
  Math,
  simba.vartype_box, simba.vartype_quad,
  simba.vartype_pointarray, simba.vartype_boxarray;

procedure TSimbaExternalImage.CheckInUpdate;
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate/EndUpdate');
end;

procedure TSimbaExternalImage.addDirty(b: TBox);
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate/EndUpdate');

  if FDirty then
    FDirtyBox := FDirtyBox.Combine(b.Normalize())
  else
  begin
    FDirty := True;
    FDirtyBox := b.Normalize();
  end;
end;

procedure TSimbaExternalImage.addAllDirty;
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate/EndUpdate');

  FDirty := True;
  FDirtyBox.X1 := -$FFFFFF;
  FDirtyBox.Y1 := -$FFFFFF;
  FDirtyBox.X2 := $FFFFFF;
  FDirtyBox.Y2 := $FFFFFF;
end;

procedure TSimbaExternalImage.Flush;
var
  Y: Integer;
begin
  if not FDirty then
    Exit;

  FDirty := False;
  FDirtyBox := FDirtyBox.Expand(1).Clip(TBox.Create(0, 0, FWidth-1, FHeight-1));

  for Y := FDirtyBox.Y1 to FDirtyBox.Y2 do
    Move(FBackBuffer.Data[Y * FWidth + FDirtyBox.X1], FData[Y * FWidth + FDirtyBox.X1], (FDirtyBox.X2 - FDirtyBox.X1) * SizeOf(TColorBGRA));
end;

function TSimbaExternalImage.GetFontAntialiasing: Boolean;
begin
  Result := FBackBuffer.FontAntialiasing;
end;

function TSimbaExternalImage.GetFontBold: Boolean;
begin
  Result := FBackBuffer.FontBold;
end;

function TSimbaExternalImage.GetFontItalic: Boolean;
begin
  Result := FBackBuffer.FontItalic;
end;

function TSimbaExternalImage.GetFontName: String;
begin
  Result := FBackBuffer.FontName;
end;

function TSimbaExternalImage.GetFontSize: Single;
begin
  Result := FBackBuffer.FontSize;
end;

procedure TSimbaExternalImage.SetFontAntialiasing(Value: Boolean);
begin
  FBackBuffer.FontAntialiasing := Value;
end;

procedure TSimbaExternalImage.SetFontBold(Value: Boolean);
begin
  FBackBuffer.FontBold := Value;
end;

procedure TSimbaExternalImage.SetFontItalic(Value: Boolean);
begin
  FBackBuffer.FontItalic := Value;
end;

procedure TSimbaExternalImage.SetFontName(Value: String);
begin
  FBackBuffer.FontName := Value;
end;

procedure TSimbaExternalImage.SetFontSize(Value: Single);
begin
  FBackBuffer.FontSize := Value;
end;

constructor TSimbaExternalImage.Create;
begin
  inherited Create();

  FBackBuffer := TSimbaImage.Create();
  FBackBuffer.DefaultPixel.A := ALPHA_TRANSPARENT;
end;

destructor TSimbaExternalImage.Destroy;
begin
  if Assigned(FBackBuffer) then
    FreeAndNil(FBackBuffer);

  inherited Destroy();
end;

procedure TSimbaExternalImage.BeginUpdate;
begin
  FLock.Enter();

  Inc(FInUpdate);
  if (FInUpdate > 1) then
    Exit;

  FDirty := False;
end;

procedure TSimbaExternalImage.EndUpdate;
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate');

  Dec(FInUpdate);
  if (FInUpdate > 0) then
    Exit;

  Flush();

  FLock.Leave();
end;

procedure TSimbaExternalImage.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
begin
  BeginUpdate();
  try
    FData := Data;
    FWidth := AWidth;
    FHeight := AHeight;

    FBackBuffer.SetSize(AWidth, AHeight);
  finally
    EndUpdate();
  end;
end;

procedure TSimbaExternalImage.Resize(NewWidth, NewHeight: Integer);
var
  Y: Integer;
begin
  if (FWidth = NewWidth) and (FHeight = NewHeight) then
    Exit;

  BeginUpdate();
  try
    FWidth := NewWidth;
    FHeight := NewHeight;

    FBackBuffer.SetSize(FWidth, FHeight);
    for Y := 0 to FHeight - 1 do
      Move(FBackBuffer.Data[Y * FWidth], FData[Y * FWidth], FWidth * SizeOf(TColorBGRA));
  finally
    EndUpdate();
  end;
end;

procedure TSimbaExternalImage.SetAlpha(Value: Byte);
begin
  addAllDirty();

  FBackBuffer.SetAlpha(Value);
end;

procedure TSimbaExternalImage.SetAlpha(Points: TPointArray; Value: Byte);
begin
  addAllDirty();

  FBackBuffer.SetAlpha(Points, Value);
end;

procedure TSimbaExternalImage.SetAlpha(Color: TColor; Value: Byte);
begin
  addAllDirty();

  FBackBuffer.SetAlpha(Color, Value);
end;

procedure TSimbaExternalImage.DrawATPA(ATPA: T2DPointArray; Color: TColor; Alpha: Byte);
begin
  addDirty(ATPA.Bounds());

  FBackBuffer.DrawATPA(ATPA, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawTPA(TPA: TPointArray; Color: TColor; Alpha: Byte);
begin
  addDirty(TPA.Bounds());

  FBackBuffer.DrawTPA(TPA, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(ACenter, Size, Size));

  FBackBuffer.DrawCrosshairs(ACenter, Size, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCross(ACenter, Radius, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(Start.X, Start.Y, Stop.X, Stop.Y));

  FBackBuffer.DrawLine(Start, Stop, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(Start.X, Start.Y, Stop.X, Stop.Y));

  FBackBuffer.DrawLineGap(Start, Stop, GapSize, Color, Alpha);
end;

procedure TSimbaExternalImage.Clear;
begin
  addAllDirty();

  FBackBuffer.Clear();
end;

procedure TSimbaExternalImage.Clear(Box: TBox);
begin
  addDirty(Box);

  FBackBuffer.Clear(Box);
end;

procedure TSimbaExternalImage.ClearInverted(Box: TBox);
begin
  addAllDirty();

  FBackBuffer.ClearInverted(Box);
end;

procedure TSimbaExternalImage.Fill(Color: TColor; Alpha: Byte);
begin
  addAllDirty();

  FBackBuffer.Fill(Color, Alpha);
end;

procedure TSimbaExternalImage.SetUserData(UserData: Pointer);
begin
  FUserData := UserData;
end;

function TSimbaExternalImage.GetUserData: Pointer;
begin
  Result := FUserData;
end;

function TSimbaExternalImage.TextWidth(Text: String): Integer;
begin
  Result := FBackBuffer.TextWidth(Text);
end;

function TSimbaExternalImage.TextHeight(Text: String): Integer;
begin
  Result := FBackBuffer.TextHeight(Text);
end;

function TSimbaExternalImage.TextSize(Text: String): TPoint;
begin
  Result := FBackBuffer.TextSize(Text);
end;

procedure TSimbaExternalImage.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  CheckInUpdate();

  FBackBuffer.DrawText(Text, Position, Color);
  addDirty(FBackBuffer.TextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawText(Text: String; Box: TBox; Alignments: EDrawTextAlignSet; Color: TColor);
begin
  CheckInUpdate();

  FBackBuffer.DrawText(Text, Box, Alignments, Color);
  addDirty(FBackBuffer.TextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
begin
  CheckInUpdate();

  FBackBuffer.DrawTextLines(Text, Position, Color);
  addDirty(FBackBuffer.TextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawImage(Image: TSimbaImage; Location: TPoint; Alpha: Byte);
begin
  addDirty(TBox.Create(Location.X, Location.Y, Location.X + Image.Width, Location.Y + Image.Height));

  FBackBuffer.DrawImage(Image, Location, Alpha);
end;

procedure TSimbaExternalImage.DrawBox(Box: TBox; Color: TColor; Alpha: Byte);
begin
  addDirty(Box);

  FBackBuffer.DrawBox(Box, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawBoxFilled(Box: TBox; Color: TColor; Alpha: Byte);
begin
  addDirty(Box);

  FBackBuffer.DrawBoxFilled(Box, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawBoxInverted(Box: TBox; Color: TColor; Alpha: Byte);
begin
  addAllDirty();

  FBackBuffer.DrawBoxInverted(Box, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawPolygon(Points: TPointArray; Color: TColor; Alpha: Byte);
begin
  addDirty(Points.Bounds);

  FBackBuffer.DrawPolygon(Points, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawPolygonFilled(Points: TPointArray; Color: TColor; Alpha: Byte);
begin
  addDirty(Points.Bounds);

  FBackBuffer.DrawPolygonFilled(Points, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawPolygonInverted(Points: TPointArray; Color: TColor; Alpha: Byte);
begin
  addAllDirty();

  FBackBuffer.DrawPolygonInverted(Points, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawQuad(Quad: TQuad; Color: TColor; Alpha: Byte);
begin
  addDirty(Quad.Bounds);

  FBackBuffer.DrawQuad(Quad, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawQuadFilled(Quad: TQuad; Color: TColor; Alpha: Byte);
begin
  addDirty(Quad.Bounds);

  FBackBuffer.DrawQuadFilled(Quad, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawQuadInverted(Quad: TQuad; Color: TColor; Alpha: Byte);
begin
  addAllDirty();

  FBackBuffer.DrawQuadInverted(Quad, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCircle(ACenter, Radius, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCircleFilled(ACenter, Radius, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  addAllDirty();

  FBackBuffer.DrawCircleInverted(ACenter, Radius, Color, Alpha);
end;

procedure TSimbaExternalImage.DrawLineAA(Start, Stop: TPoint; Color: TColor; Thickness: Single);
begin
  addDirty(TBox.Create(Ceil(Start.X - Thickness), Ceil(Start.Y - Thickness), Ceil(Stop.X - Thickness), Ceil(Stop.Y - Thickness)));
  addDirty(TBox.Create(Ceil(Start.X + Thickness), Ceil(Start.Y + Thickness), Ceil(Stop.X + Thickness), Ceil(Stop.Y + Thickness)));

  FBackBuffer.DrawLineAA(Start, Stop, Color, Thickness);
end;

procedure TSimbaExternalImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Color: TColor; Thickness: Single);
begin
  addDirty(TBox.Create(ACenter, Ceil(XRadius + Thickness), Ceil(YRadius + Thickness)));

  FBackBuffer.DrawEllipseAA(ACenter, XRadius, YRadius, Color, Thickness);
end;

procedure TSimbaExternalImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Color: TColor; Thickness: Single);
begin
  addDirty(TBox.Create(ACenter, Ceil(Radius + Thickness), Ceil(Radius + Thickness)));

  FBackBuffer.DrawCircleAA(ACenter, Radius, Color, Thickness);
end;

procedure TSimbaExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Quads) do
    addDirty(Quads[I].Bounds());

  FBackBuffer.DrawQuadArray(Quads, Filled, Color);
end;

procedure TSimbaExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor);
begin
  addDirty(Boxes.Merge());

  FBackBuffer.DrawBoxArray(Boxes, Filled, Color);
end;

procedure TSimbaExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor);
begin
  addDirty(Polygons.Bounds());

  FBackBuffer.DrawPolygonArray(Polygons, Filled, Color);
end;

procedure TSimbaExternalImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor);
begin
  addDirty(Centers.Bounds().Expand(Radius));

  FBackBuffer.DrawCircleArray(Centers, Radius, Filled, Color);
end;

procedure TSimbaExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor);
begin
  addDirty(Points.Bounds().Expand(Radius));

  FBackBuffer.DrawCrossArray(Points, Radius, Color);
end;

end.

