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
  simba.threading;

type
  PSimbaExternalImage = ^TSimbaExternalImage;
  TSimbaExternalImage = class(TSimbaBaseClass)
  protected
    FLock: TEnterableLock;

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
    function GetPixel(const X, Y: Integer): TColor;
    function GetAlpha(const X, Y: Integer): Byte;
    function GetDefaultPixel: TColorBGRA;
    function GetDrawAlpha: Byte;
    function GetDrawColor: TColor;

    procedure SetUserData(UserData: Pointer);
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
    procedure SetPixel(const X, Y: Integer; const Color: TColor);
    procedure SetAlpha(const X, Y: Integer; const Value: Byte);
    procedure SetDefaultPixel(AValue: TColorBGRA);
    procedure SetDrawAlpha(const AValue: Byte);
    procedure SetDrawColor(const AValue: TColor);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property DefaultPixel: TColorBGRA read GetDefaultPixel write SetDefaultPixel;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property UserData: Pointer read GetUserData write SetUserData;
    property AutoResize: Boolean read FAutoResize write FAutoResize;

    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    property Alpha[X, Y: Integer]: Byte read GetAlpha write SetAlpha;

    function GetPixels(Points: TPointArray): TColorArray;
    procedure SetPixels(Points: TPointArray; Color: TColor); overload;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload;

    property DrawColor: TColor read GetDrawColor write SetDrawColor;
    property DrawAlpha: Byte read GetDrawAlpha write SetDrawAlpha;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    procedure SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
    procedure Resize(NewWidth, NewHeight: Integer);

    procedure Fill(Color: TColor);
    procedure FillWithAlpha(Value: Byte);

    procedure Clear; overload;
    procedure Clear(Box: TBox); overload;
    procedure ClearInverted(Box: TBox);

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint);

    // Image
    procedure DrawImage(Image: TSimbaImage; Location: TPoint);

    // Point
    procedure DrawATPA(ATPA: T2DPointArray);
    procedure DrawTPA(TPA: TPointArray);

    // Line
    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer);
    procedure DrawCross(ACenter: TPoint; Radius: Integer);
    procedure DrawLine(Start, Stop: TPoint);
    procedure DrawLineGap(Start, Stop: TPoint; GapSize: Integer);

    // Box
    procedure DrawBox(Box: TBox);
    procedure DrawBoxFilled(Box: TBox);
    procedure DrawBoxInverted(Box: TBox);

    // Poly
    procedure DrawPolygon(Points: TPointArray);
    procedure DrawPolygonFilled(Points: TPointArray);
    procedure DrawPolygonInverted(Points: TPointArray);

    // Quad
    procedure DrawQuad(Quad: TQuad);
    procedure DrawQuadFilled(Quad: TQuad);
    procedure DrawQuadInverted(Quad: TQuad);

    // Circle
    procedure DrawCircle(ACenter: TPoint; Radius: Integer);
    procedure DrawCircleFilled(ACenter: TPoint; Radius: Integer);
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer);

    // Antialiased
    procedure DrawLineAA(Start, Stop: TPoint; Thickness: Single = 1.5);
    procedure DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single = 1.5);
    procedure DrawCircleAA(ACenter: TPoint; Radius: Integer; Thickness: Single = 1.5);

    // Arrays
    procedure DrawQuadArray(Quads: TQuadArray; Filled: Boolean);
    procedure DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);
    procedure DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean);
    procedure DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer);
  end;

implementation

uses
  Math,
  simba.vartype_box, simba.vartype_quad, simba.vartype_pointarray, simba.vartype_boxarray;

function TSimbaExternalImage.GetPixel(const X, Y: Integer): TColor;
begin
  Result := FBackBuffer.Pixel[X, Y];
end;

function TSimbaExternalImage.GetAlpha(const X, Y: Integer): Byte;
begin
  Result := FBackBuffer.Alpha[X, Y];
end;

function TSimbaExternalImage.GetDrawAlpha: Byte;
begin
  Result := FBackBuffer.DrawAlpha;
end;

function TSimbaExternalImage.GetDrawColor: TColor;
begin
  Result := FBackBuffer.DrawColor;
end;

procedure TSimbaExternalImage.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  FBackBuffer.Pixel[X, Y] := Color;
end;

procedure TSimbaExternalImage.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  FBackBuffer.Alpha[X, Y] := Value;
end;

procedure TSimbaExternalImage.SetDrawAlpha(const AValue: Byte);
begin
  FBackBuffer.DrawAlpha := AValue;
end;

procedure TSimbaExternalImage.SetDrawColor(const AValue: TColor);
begin
  FBackBuffer.DrawColor := AValue;
end;

function TSimbaExternalImage.GetDefaultPixel: TColorBGRA;
begin
  Result := FBackBuffer.DefaultPixel;
end;

procedure TSimbaExternalImage.SetDefaultPixel(AValue: TColorBGRA);
begin
  FBackBuffer.DefaultPixel := AValue;
end;

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

function TSimbaExternalImage.GetPixels(Points: TPointArray): TColorArray;
begin
  Result := FBackBuffer.GetPixels(Points);
end;

procedure TSimbaExternalImage.SetPixels(Points: TPointArray; Color: TColor);
begin
  FBackBuffer.SetPixels(Points, Color);
end;

procedure TSimbaExternalImage.SetPixels(Points: TPointArray; Colors: TColorArray);
begin
  FBackBuffer.SetPixels(Points, Colors);
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

procedure TSimbaExternalImage.FillWithAlpha(Value: Byte);
begin
  addAllDirty();

  FBackBuffer.FillWithAlpha(Value);
end;

procedure TSimbaExternalImage.DrawATPA(ATPA: T2DPointArray);
begin
  addDirty(ATPA.Bounds());

  FBackBuffer.DrawATPA(ATPA);
end;

procedure TSimbaExternalImage.DrawTPA(TPA: TPointArray);
begin
  addDirty(TPA.Bounds());

  FBackBuffer.DrawTPA(TPA);
end;

procedure TSimbaExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer);
begin
  addDirty(TBox.Create(ACenter, Size, Size));

  FBackBuffer.DrawCrosshairs(ACenter, Size);
end;

procedure TSimbaExternalImage.DrawCross(ACenter: TPoint; Radius: Integer);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCross(ACenter, Radius);
end;

procedure TSimbaExternalImage.DrawLine(Start, Stop: TPoint);
begin
  addDirty(TBox.Create(Start.X, Start.Y, Stop.X, Stop.Y));

  FBackBuffer.DrawLine(Start, Stop);
end;

procedure TSimbaExternalImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer);
begin
  addDirty(TBox.Create(Start.X, Start.Y, Stop.X, Stop.Y));

  FBackBuffer.DrawLineGap(Start, Stop, GapSize);
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

procedure TSimbaExternalImage.Fill(Color: TColor);
begin
  addAllDirty();

  FBackBuffer.Fill(Color);
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

type
  TSimbaImageProtected = class(TSimbaImage);

procedure TSimbaExternalImage.DrawText(Text: String; Position: TPoint);
begin
  CheckInUpdate();

  FBackBuffer.DrawText(Text, Position);
  addDirty(TSimbaImageProtected(FBackBuffer).FTextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign);
begin
  CheckInUpdate();

  FBackBuffer.DrawText(Text, Box, Alignments);
  addDirty(TSimbaImageProtected(FBackBuffer).FTextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint);
begin
  CheckInUpdate();

  FBackBuffer.DrawTextLines(Text, Position);
  addDirty(TSimbaImageProtected(FBackBuffer).FTextDrawer.DrawnBox);
end;

procedure TSimbaExternalImage.DrawImage(Image: TSimbaImage; Location: TPoint);
begin
  addDirty(TBox.Create(Location.X, Location.Y, Location.X + Image.Width, Location.Y + Image.Height));

  FBackBuffer.DrawImage(Image, Location);
end;

procedure TSimbaExternalImage.DrawBox(Box: TBox);
begin
  addDirty(Box);

  FBackBuffer.DrawBox(Box);
end;

procedure TSimbaExternalImage.DrawBoxFilled(Box: TBox);
begin
  addDirty(Box);

  FBackBuffer.DrawBoxFilled(Box);
end;

procedure TSimbaExternalImage.DrawBoxInverted(Box: TBox);
begin
  addAllDirty();

  FBackBuffer.DrawBoxInverted(Box);
end;

procedure TSimbaExternalImage.DrawPolygon(Points: TPointArray);
begin
  addDirty(Points.Bounds);

  FBackBuffer.DrawPolygon(Points);
end;

procedure TSimbaExternalImage.DrawPolygonFilled(Points: TPointArray);
begin
  addDirty(Points.Bounds);

  FBackBuffer.DrawPolygonFilled(Points);
end;

procedure TSimbaExternalImage.DrawPolygonInverted(Points: TPointArray);
begin
  addAllDirty();

  FBackBuffer.DrawPolygonInverted(Points);
end;

procedure TSimbaExternalImage.DrawQuad(Quad: TQuad);
begin
  addDirty(Quad.Bounds);

  FBackBuffer.DrawQuad(Quad);
end;

procedure TSimbaExternalImage.DrawQuadFilled(Quad: TQuad);
begin
  addDirty(Quad.Bounds);

  FBackBuffer.DrawQuadFilled(Quad);
end;

procedure TSimbaExternalImage.DrawQuadInverted(Quad: TQuad);
begin
  addAllDirty();

  FBackBuffer.DrawQuadInverted(Quad);
end;

procedure TSimbaExternalImage.DrawCircle(ACenter: TPoint; Radius: Integer);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCircle(ACenter, Radius);
end;

procedure TSimbaExternalImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer);
begin
  addDirty(TBox.Create(ACenter, Radius, Radius));

  FBackBuffer.DrawCircleFilled(ACenter, Radius);
end;

procedure TSimbaExternalImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer);
begin
  addAllDirty();

  FBackBuffer.DrawCircleInverted(ACenter, Radius);
end;

procedure TSimbaExternalImage.DrawLineAA(Start, Stop: TPoint; Thickness: Single);
begin
  addDirty(TBox.Create(Ceil(Start.X - Thickness), Ceil(Start.Y - Thickness), Ceil(Stop.X - Thickness), Ceil(Stop.Y - Thickness)));
  addDirty(TBox.Create(Ceil(Start.X + Thickness), Ceil(Start.Y + Thickness), Ceil(Stop.X + Thickness), Ceil(Stop.Y + Thickness)));

  FBackBuffer.DrawLineAA(Start, Stop, Thickness);
end;

procedure TSimbaExternalImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single);
begin
  addDirty(TBox.Create(ACenter, Ceil(XRadius + Thickness), Ceil(YRadius + Thickness)));

  FBackBuffer.DrawEllipseAA(ACenter, XRadius, YRadius, Thickness);
end;

procedure TSimbaExternalImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Thickness: Single);
begin
  addDirty(TBox.Create(ACenter, Ceil(Radius + Thickness), Ceil(Radius + Thickness)));

  FBackBuffer.DrawCircleAA(ACenter, Radius, Thickness);
end;

procedure TSimbaExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);
var
  I: Integer;
begin
  for I := 0 to High(Quads) do
    addDirty(Quads[I].Bounds);

  FBackBuffer.DrawQuadArray(Quads, Filled);
end;

procedure TSimbaExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);
begin
  addDirty(Boxes.Merge());

  FBackBuffer.DrawBoxArray(Boxes, Filled);
end;

procedure TSimbaExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean);
begin
  addDirty(Polygons.Bounds());

  FBackBuffer.DrawPolygonArray(Polygons, Filled);
end;

procedure TSimbaExternalImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);
begin
  addDirty(Centers.Bounds().Expand(Radius));

  FBackBuffer.DrawCircleArray(Centers, Radius, Filled);
end;

procedure TSimbaExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer);
begin
  addDirty(Points.Bounds().Expand(Radius));

  FBackBuffer.DrawCrossArray(Points, Radius);
end;

end.

