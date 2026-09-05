{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Provides a way to draw on an image which data is externally allocated.
}
unit simba.externalcanvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.baseclass, simba.image, simba.image_textdrawer,
  simba.threading,
  simba.vartype_quad;

type
  TSimbaExternalCanvas = class(TSimbaBaseClass)
  protected
    FData: PColorBGRA;
    FWidth, FHeight: Integer;
    FImg: TSimbaImage; // pointed to above data
    FLock: TEnterableLock;
    FUserData: Pointer;

    FDoubleBuffered: Boolean;
    FInUpdate: Integer;
    FInvalidateNeeded: Boolean;
    FInvalidateBox: TBox;

    procedure Invalidate(b: TBox);
    procedure InvalidateAll;

    procedure Flush;

    function GetFontAntialiasing: Boolean;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;
    function GetPixel(const X, Y: Integer): TColor;
    function GetAlpha(const X, Y: Integer): Byte;
    function GetDefaultPixel: TColorBGRA;
    function GetDrawAlpha: Byte;
    function GetDrawColor: TColor;

    procedure SetDoubleBuffered(AValue: Boolean);
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
    function GetDrawThickness: Single;
    function GetDrawAntialiasing: Boolean;
    function GetDrawFeather: Single;
    procedure SetDrawThickness(const AValue: Single);
    procedure SetDrawAntialiasing(const AValue: Boolean);
    procedure SetDrawFeather(const AValue: Single);
  public
    UserData: Pointer;
    AutoResize: Boolean;

    constructor Create; reintroduce;
    destructor Destroy; override;

    property DefaultPixel: TColorBGRA read GetDefaultPixel write SetDefaultPixel;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered;

    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    property Alpha[X, Y: Integer]: Byte read GetAlpha write SetAlpha;

    procedure SetPixels(Points: TPointArray; Color: TColor); overload;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload;

    property DrawColor: TColor read GetDrawColor write SetDrawColor;
    property DrawAlpha: Byte read GetDrawAlpha write SetDrawAlpha;
    property DrawThickness: Single read GetDrawThickness write SetDrawThickness;
    property DrawAntialiasing: Boolean read GetDrawAntialiasing write SetDrawAntialiasing;
    property DrawFeather: Single read GetDrawFeather write SetDrawFeather;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    procedure SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
    procedure Resize(NewWidth, NewHeight: Integer);

    procedure BeginUpdate;
    procedure EndUpdate;

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

    // Ellipse
    procedure DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer);
    procedure DrawEllipseFilled(ACenter: TPoint; XRadius, YRadius: Integer);
    procedure DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer);

    // Misc

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
  simba.image_utils, simba.vartype_box, simba.vartype_pointarray;

// How far the current stroke can bleed past a shape's nominal bounds, so double-buffered invalidation covers
// the thick/anti-aliased edge. Uses DrawThickness (+ the feather grow model when antialiasing). Over-covering
// is harmless - Flush clips to the image.
function StrokeBleed(Img: TSimbaImage): Integer;
var
  T, Grow: Single;
begin
  T := Img.DrawThickness;
  if Img.DrawAntialiasing then
  begin
    Grow := Img.DrawFeather * (T * 0.5 + 0.5);
    T := T + 2 * Grow + 1;
  end;
  Result := Max(1, Ceil(T * 0.5) + 1);   // never shrink the invalidation (guards a negative DrawThickness)
end;

procedure TSimbaExternalCanvas.Invalidate(b: TBox);
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate/EndUpdate');

  if FInvalidateNeeded then
    FInvalidateBox := FInvalidateBox.Combine(b.Normalize())
  else
  begin
    FInvalidateNeeded := True;
    FInvalidateBox := b.Normalize();
  end;
end;

procedure TSimbaExternalCanvas.InvalidateAll;
begin
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate/EndUpdate');

  FInvalidateNeeded := True;
  FInvalidateBox.X1 := -$FFFFFF;
  FInvalidateBox.Y1 := -$FFFFFF;
  FInvalidateBox.X2 := $FFFFFF;
  FInvalidateBox.Y2 := $FFFFFF;
end;

procedure TSimbaExternalCanvas.Flush;
var
  Y: Integer;
begin
  if FInvalidateNeeded then
  begin
    FInvalidateNeeded := False;
    FInvalidateBox := FInvalidateBox.Expand(1).Clip(TBox.Create(0, 0, FWidth-1, FHeight-1));
    for Y := FInvalidateBox.Y1 to FInvalidateBox.Y2 do
      Move(FImg.Data[Y * FWidth + FInvalidateBox.X1], FData[Y * FWidth + FInvalidateBox.X1], (FInvalidateBox.X2 - FInvalidateBox.X1) * SizeOf(TColorBGRA));
  end;
end;

function TSimbaExternalCanvas.GetFontAntialiasing: Boolean;
begin
  Result := FImg.FontAntialiasing;
end;

function TSimbaExternalCanvas.GetFontBold: Boolean;
begin
  Result := FImg.FontBold;
end;

function TSimbaExternalCanvas.GetFontItalic: Boolean;
begin
  Result := FImg.FontItalic;
end;

function TSimbaExternalCanvas.GetFontName: String;
begin
  Result := FImg.FontName;
end;

function TSimbaExternalCanvas.GetFontSize: Single;
begin
  Result := FImg.FontSize;
end;

function TSimbaExternalCanvas.GetPixel(const X, Y: Integer): TColor;
begin
  Result := FImg.Pixel[X, Y];
end;

function TSimbaExternalCanvas.GetAlpha(const X, Y: Integer): Byte;
begin
  Result := FImg.Alpha[X, Y];
end;

function TSimbaExternalCanvas.GetDrawAlpha: Byte;
begin
  Result := FImg.DrawAlpha;
end;

function TSimbaExternalCanvas.GetDrawColor: TColor;
begin
  Result := FImg.DrawColor;
end;

function TSimbaExternalCanvas.GetDefaultPixel: TColorBGRA;
begin
  Result := FImg.DefaultPixel;
end;

procedure TSimbaExternalCanvas.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  FImg.Pixel[X, Y] := Color;
end;

procedure TSimbaExternalCanvas.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  FImg.Alpha[X, Y] := Value;
end;

procedure TSimbaExternalCanvas.SetDrawAlpha(const AValue: Byte);
begin
  FImg.DrawAlpha := AValue;
end;

procedure TSimbaExternalCanvas.SetDrawColor(const AValue: TColor);
begin
  FImg.DrawColor := AValue;
end;

function TSimbaExternalCanvas.GetDrawThickness: Single;
begin
  Result := FImg.DrawThickness;
end;

function TSimbaExternalCanvas.GetDrawAntialiasing: Boolean;
begin
  Result := FImg.DrawAntialiasing;
end;

function TSimbaExternalCanvas.GetDrawFeather: Single;
begin
  Result := FImg.DrawFeather;
end;

procedure TSimbaExternalCanvas.SetDrawThickness(const AValue: Single);
begin
  FImg.DrawThickness := AValue;
end;

procedure TSimbaExternalCanvas.SetDrawAntialiasing(const AValue: Boolean);
begin
  FImg.DrawAntialiasing := AValue;
end;

procedure TSimbaExternalCanvas.SetDrawFeather(const AValue: Single);
begin
  FImg.DrawFeather := AValue;
end;

procedure TSimbaExternalCanvas.SetDefaultPixel(AValue: TColorBGRA);
begin
  FImg.DefaultPixel := AValue;
end;

procedure TSimbaExternalCanvas.SetDoubleBuffered(AValue: Boolean);
begin
  if (FDoubleBuffered = AValue) then
    Exit;

  FDoubleBuffered := AValue;
  if FDoubleBuffered then
    FImg.ResetExternalData(FWidth, FHeight)
  else
    FImg.SetExternalData(FData, FWidth, FHeight);
end;

procedure TSimbaExternalCanvas.SetFontAntialiasing(Value: Boolean);
begin
  FImg.FontAntialiasing := Value;
end;

procedure TSimbaExternalCanvas.SetFontBold(Value: Boolean);
begin
  FImg.FontBold := Value;
end;

procedure TSimbaExternalCanvas.SetFontItalic(Value: Boolean);
begin
  FImg.FontItalic := Value;
end;

procedure TSimbaExternalCanvas.SetFontName(Value: String);
begin
  FImg.FontName := Value;
end;

procedure TSimbaExternalCanvas.SetFontSize(Value: Single);
begin
  FImg.FontSize := Value;
end;

constructor TSimbaExternalCanvas.Create;
begin
  inherited Create();

  FImg := TSimbaImage.Create();
  FImg.DefaultPixel.A := ALPHA_TRANSPARENT;
end;

destructor TSimbaExternalCanvas.Destroy;
begin
  if Assigned(FImg) then
    FreeAndNil(FImg);

  inherited Destroy();
end;

procedure TSimbaExternalCanvas.SetPixels(Points: TPointArray; Color: TColor);
begin
  FLock.Enter();
  try
    FImg.SetPixels(Points, Color);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetPixels(Points: TPointArray; Colors: TColorArray);
begin
  FLock.Enter();
  try
    FImg.SetPixels(Points, Colors);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
begin
  FLock.Enter();
  try
    FData := Data;
    FWidth := AWidth;
    FHeight := AHeight;

    if FDoubleBuffered then
      FImg.ResetExternalData(FWidth, FHeight)
    else
      FImg.SetExternalData(FData, FWidth, FHeight);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Resize(NewWidth, NewHeight: Integer);
var
  Y: Integer;
begin
  if (FWidth = NewWidth) and (FHeight = NewHeight) then
    Exit;

  FLock.Enter();
  try
    FWidth := NewWidth;
    FHeight := NewHeight;

    FImg.SetSize(FWidth, FHeight);
    for Y := 0 to FHeight - 1 do
      Move(FImg.Data[Y * FWidth], FData[Y * FWidth], FWidth * SizeOf(TColorBGRA));
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.BeginUpdate;
begin
  if (not FDoubleBuffered) then
    Exit;
  FLock.Enter();

  Inc(FInUpdate);
  if (FInUpdate > 1) then
    Exit;

  FInvalidateNeeded := False;
end;

procedure TSimbaExternalCanvas.EndUpdate;
begin
  if (not FDoubleBuffered) then
    Exit;
  if (FInUpdate <= 0) then
    SimbaException('Not in BeginUpdate');

  Dec(FInUpdate);
  if (FInUpdate > 0) then
    Exit;

  Flush();

  FLock.Leave();
end;

procedure TSimbaExternalCanvas.FillWithAlpha(Value: Byte);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.FillWithAlpha(Value);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawATPA(ATPA: T2DPointArray);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(ATPA.Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawATPA(ATPA);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawTPA(TPA: TPointArray);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TPA.Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawTPA(TPA);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCrosshairs(ACenter: TPoint; Size: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, Size, Size).Expand(StrokeBleed(FImg)));

    FImg.DrawCrosshairs(ACenter, Size);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCross(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, Radius, Radius).Expand(StrokeBleed(FImg)));

    FImg.DrawCross(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawLine(Start, Stop: TPoint);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(Start.X, Start.Y, Stop.X, Stop.Y).Expand(StrokeBleed(FImg)));

    FImg.DrawLine(Start, Stop);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Clear;
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.Clear();
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Clear(Box: TBox);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Box);

    FImg.Clear(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.ClearInverted(Box: TBox);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.ClearInverted(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Fill(Color: TColor);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.Fill(Color);
  finally
    FLock.Leave();
  end;
end;

function TSimbaExternalCanvas.TextWidth(Text: String): Integer;
begin
  Result := FImg.TextWidth(Text);
end;

function TSimbaExternalCanvas.TextHeight(Text: String): Integer;
begin
  Result := FImg.TextHeight(Text);
end;

function TSimbaExternalCanvas.TextSize(Text: String): TPoint;
begin
  Result := FImg.TextSize(Text);
end;

type
  TSimbaImageProtected = class(TSimbaImage);

procedure TSimbaExternalCanvas.DrawText(Text: String; Position: TPoint);
begin
  FLock.Enter();
  try
    FImg.DrawText(Text, Position);
    if FDoubleBuffered then
      Invalidate(TSimbaImageProtected(FImg).FTextDrawer.DrawnBox);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign);
begin
  FLock.Enter();
  try
    FImg.DrawText(Text, Box, Alignments);
    if FDoubleBuffered then
      Invalidate(TSimbaImageProtected(FImg).FTextDrawer.DrawnBox);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);
begin
  FLock.Enter();
  try
    FImg.DrawTextLines(Text, Position);
    if FDoubleBuffered then
      Invalidate(TSimbaImageProtected(FImg).FTextDrawer.DrawnBox);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawImage(Image: TSimbaImage; Location: TPoint);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(Location.X, Location.Y, Location.X + Image.Width, Location.Y + Image.Height));

    FImg.DrawImage(Image, Location);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBox(Box: TBox);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Box.Expand(StrokeBleed(FImg)));

    FImg.DrawBox(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBoxFilled(Box: TBox);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Box);

    FImg.DrawBoxFilled(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBoxInverted(Box: TBox);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.DrawBoxInverted(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygon(Points: TPointArray);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Points.Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawPolygon(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygonFilled(Points: TPointArray);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Points.Bounds);

    FImg.DrawPolygonFilled(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygonInverted(Points: TPointArray);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.DrawPolygonInverted(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuad(Quad: TQuad);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Quad.Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawQuad(Quad);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuadFilled(Quad: TQuad);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Quad.Bounds);

    FImg.DrawQuadFilled(Quad);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuadInverted(Quad: TQuad);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.DrawQuadInverted(Quad);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircle(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, Radius, Radius).Expand(StrokeBleed(FImg)));

    FImg.DrawCircle(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircleFilled(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, Radius, Radius));

    FImg.DrawCircleFilled(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircleInverted(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.DrawCircleInverted(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, XRadius, YRadius).Expand(StrokeBleed(FImg)));

    FImg.DrawEllipse(ACenter, XRadius, YRadius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawEllipseFilled(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(TBox.Create(ACenter, XRadius, YRadius));

    FImg.DrawEllipseFilled(ACenter, XRadius, YRadius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      InvalidateAll();

    FImg.DrawEllipseInverted(ACenter, XRadius, YRadius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);
var
  I: Integer;
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      for I := 0 to High(Quads) do
        Invalidate(Quads[I].Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawQuadArray(Quads, Filled);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Boxes.Merge().Expand(StrokeBleed(FImg)));

    FImg.DrawBoxArray(Boxes, Filled);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean);
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      Invalidate(Polygons.Bounds.Expand(StrokeBleed(FImg)));

    FImg.DrawPolygonArray(Polygons, Filled);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);
var
  I: Integer;
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      for I := 0 to High(Centers) do
        Invalidate(TBox.Create(Centers[I], Radius, Radius).Expand(StrokeBleed(FImg)));

    FImg.DrawCircleArray(Centers, Radius, Filled);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCrossArray(Points: TPointArray; Radius: Integer);
var
  I: Integer;
begin
  FLock.Enter();
  try
    if FDoubleBuffered then
      for I := 0 to High(Points) do
        Invalidate(TBox.Create(Points[I], Radius, Radius).Expand(StrokeBleed(FImg)));

    FImg.DrawCrossArray(Points, Radius);
  finally
    FLock.Leave();
  end;
end;

end.

