{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Drawing onto a raw BGRA pixel buffer.
   - Does not own/manage/allocate the data.
   - Offset is added to every coordinate before drawing.
       Offset=10,20 | 100,100 -> 110,120
   - ScaleShift shifts coordinates right: 0 = 1:1, 1 = half, 2 = quarter.
       ScaleShift=1 | 100,100 -> 50,50
       ScaleShift=2 | 100,100 -> 25,25
}
unit simba.canvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.baseclass,
  simba.colormath,
  simba.image_drawtext,
  simba.vartype_polygon,
  simba.vartype_quad;

type
  // everything about how a canvas draws, so it can be saved and put back
  TSimbaCanvasState = packed record
    DrawColor: TColor;
    DrawAlpha: Byte;
    DrawThickness: Single;
    DrawAntialiasing: Boolean;
    DrawFeather: Single;
    DrawFilled: Boolean;

    FontName: String;
    FontSize: Single;
    FontStyles: ECanvasFontStyles;
  end;

  TSimbaCanvas = class(TSimbaBaseClass)
  protected
    FData: PColorBGRA;      // the buffer we draw onto
    FPixelsPerRow: Integer; // pixels per row of FData
    FWidth: Integer;        // the logical size. The buffer's may be bigger (PixelsPerRow)
    FHeight: Integer;
    FOffset: TPoint;        // draw space -> pixel space
    FScaleShift: Integer;   // log2 of the draw pixels per canvas pixel; 0 = 1:1

    FDrawColor: TColor;
    FDrawAlpha: Byte;
    FDrawColorBGRA: TColorBGRA; // DrawColor + DrawAlpha
    FDrawThickness: Single;
    FDrawAntialiasing: Boolean;
    FDrawFeather: Single;
    FDrawFilled: Boolean;

    FFontName: String;
    FFontSize: Single;
    FFontStyles: ECanvasFontStyles;

    procedure SetDrawColor(Value: TColor);
    procedure SetDrawAlpha(Value: Byte);
    function GetFontStyle(Style: Integer): Boolean;
    procedure SetFontStyle(Style: Integer; Value: Boolean);
    function GetState: TSimbaCanvasState;
    procedure SetState(const Value: TSimbaCanvasState);

    function ToLocal(const P: TPoint): TPoint; inline; overload;
    function ToLocal(const B: TBox): TBox; inline; overload;
    function ToLocal(const Q: TQuad): TQuad; inline; overload;
    function ToLocal(const Len: Integer): Integer; inline; overload;
    function ToLocal(const Points: TPointArray): TPointArray; overload;
    function ToLocal(const Points: T2DPointArray): T2DPointArray; overload;

    // pixel-space drawing
    procedure DoLine(Start, Stop: TPoint);
    procedure DoBox(Box: TBox; Inverted: Boolean);
    procedure DoBoxEdge(Box: TBox);
    procedure DoPolygon(Poly: TPointArray; Inverted: Boolean);
    procedure DoPolygonEdge(Poly: TPointArray);
    procedure DoEllipse(ACenter: TPoint; XRadius, YRadius: Integer; Inverted: Boolean);
    procedure DoEllipseEdge(ACenter: TPoint; XRadius, YRadius: Integer);
    procedure DoData(Src: PColorBGRA; SrcW, SrcH: Integer; P: TPoint);
    procedure DoText(Text: String; Position: TPoint); overload;
    procedure DoText(Text: String; Box: TBox; Alignments: ECanvasTextAligns); overload;

    procedure RaiseOutOfBounds(X, Y: Integer); virtual;

    function GetPixel(const X, Y: Integer): TColor; virtual;
    function GetAlpha(const X, Y: Integer): Byte; virtual;

    procedure SetPixel(const X, Y: Integer; const Color: TColor); virtual;
    procedure SetAlpha(const X, Y: Integer; const Value: Byte); virtual;
  public
    // What Clear writes. Opaque black by default.
    DefaultPixel: TColorBGRA;
  public
    class function LoadFontsInDir(Dir: String): Boolean;
    class function FontNames: TStringArray;

    constructor Create; reintroduce;

    procedure SetData(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer);
    procedure SetTransform(Offset: TPoint; ScaleShift: Integer);

    procedure Fill(Color: TColor); virtual;
    procedure FillWithAlpha(Value: Byte); virtual;

    procedure Clear; overload; virtual;
    procedure Clear(Box: TBox); overload; virtual;
    procedure ClearInverted(Box: TBox); virtual;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    // Single pixel access. Bounds checked: out of bounds raises.
    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    property Alpha[X, Y: Integer]: Byte read GetAlpha write SetAlpha;

    function GetPixels(Points: TPointArray): TColorArray; virtual;
    procedure SetPixels(Points: TPointArray; Color: TColor); overload; virtual;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload; virtual;

    property DrawColor: TColor read FDrawColor write SetDrawColor;
    property DrawAlpha: Byte read FDrawAlpha write SetDrawAlpha;
    property DrawThickness: Single read FDrawThickness write FDrawThickness;
    property DrawAntialiasing: Boolean read FDrawAntialiasing write FDrawAntialiasing;
    property DrawFeather: Single read FDrawFeather write FDrawFeather;
    property DrawFilled: Boolean read FDrawFilled write FDrawFilled;

    property FontName: String read FFontName write FFontName;
    property FontSize: Single read FFontSize write FFontSize;
    property FontAntialiasing: Boolean index Ord(ECanvasFontStyle.ANTIALIASED) read GetFontStyle write SetFontStyle;
    property FontBold: Boolean index Ord(ECanvasFontStyle.BOLD) read GetFontStyle write SetFontStyle;
    property FontItalic: Boolean index Ord(ECanvasFontStyle.ITALIC) read GetFontStyle write SetFontStyle;
    property FontUnderline: Boolean index Ord(ECanvasFontStyle.UNDERLINE) read GetFontStyle write SetFontStyle;

    // every drawing parameter above in one, to save and put back
    property State: TSimbaCanvasState read GetState write SetState;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint); overload; virtual;
    procedure DrawText(Text: String; Box: TBox; Alignments: ECanvasTextAligns); overload; virtual;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint); virtual;

    procedure DrawImage(Src: PColorBGRA; SrcW, SrcH: Integer; Location: TPoint); overload; virtual;
    // Mat must be normalized to 0..1
    procedure DrawHeatmap(Mat: TSingleMatrix); virtual;

    procedure DrawATPA(ATPA: T2DPointArray); virtual;
    procedure DrawTPA(TPA: TPointArray); virtual;

    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer); virtual;
    procedure DrawCross(ACenter: TPoint; Radius: Integer); virtual;
    procedure DrawLine(Start, Stop: TPoint); virtual;

    procedure DrawBox(Box: TBox); virtual;
    procedure DrawBoxInverted(Box: TBox); virtual;

    procedure DrawPolygon(Points: TPointArray); virtual;
    procedure DrawPolygonInverted(Points: TPointArray); virtual;

    procedure DrawQuad(Quad: TQuad); virtual;
    procedure DrawQuadInverted(Quad: TQuad); virtual;

    procedure DrawCircle(ACenter: TPoint; Radius: Integer); virtual;
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer); virtual;

    procedure DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer); virtual;
    procedure DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer); virtual;

    procedure DrawQuadArray(Quads: TQuadArray); virtual;
    procedure DrawBoxArray(Boxes: TBoxArray); virtual;
    procedure DrawPolygonArray(Polygons: TPolygonArray); virtual;
    procedure DrawCircleArray(Centers: TPointArray; Radius: Integer); virtual;
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer); virtual;
  end;

implementation

uses
  Math,
  simba.image_utils,
  simba.image_draw,
  simba.image_drawantialias,
  simba.image_drawmatrix,
  simba.vartype_box,
  simba.vartype_matrix;

// Draw space -> pixel space: add the offset, then shift right by the scale shift.
//   X := (P.X + FOffset.X) sar FScaleShift
function TSimbaCanvas.ToLocal(const P: TPoint): TPoint;
begin
  Result.X := SarLongint(P.X + FOffset.X, FScaleShift);
  Result.Y := SarLongint(P.Y + FOffset.Y, FScaleShift);
end;

function TSimbaCanvas.ToLocal(const B: TBox): TBox;
begin
  Result.X1 := SarLongint(B.X1 + FOffset.X, FScaleShift);
  Result.Y1 := SarLongint(B.Y1 + FOffset.Y, FScaleShift);
  Result.X2 := SarLongint(B.X2 + FOffset.X, FScaleShift);
  Result.Y2 := SarLongint(B.Y2 + FOffset.Y, FScaleShift);
end;

function TSimbaCanvas.ToLocal(const Q: TQuad): TQuad;
begin
  Result := TQuad.Create(ToLocal(Q.Top), ToLocal(Q.Right), ToLocal(Q.Bottom), ToLocal(Q.Left));
end;

function TSimbaCanvas.ToLocal(const Len: Integer): Integer;
begin
  if (FScaleShift = 0) then
    Result := Len
  else
    Result := Max(1, Len shr FScaleShift); // never let a shape vanish entirely
end;

function TSimbaCanvas.ToLocal(const Points: TPointArray): TPointArray;
var
  I: Integer;
begin
  if (FScaleShift = 0) and (FOffset.X = 0) and (FOffset.Y = 0) then
    Exit(Points);

  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    Result[I] := ToLocal(Points[I]);
end;

function TSimbaCanvas.ToLocal(const Points: T2DPointArray): T2DPointArray;
var
  I: Integer;
begin
  if (FScaleShift = 0) and (FOffset.X = 0) and (FOffset.Y = 0) then
    Exit(Points);

  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    Result[I] := ToLocal(Points[I]);
end;

procedure TSimbaCanvas.SetDrawColor(Value: TColor);
begin
  FDrawColor := Value;
  if (Value = -1) then
    Value := GetDistinctColor(0);

  FDrawColorBGRA := Value.ToBGRA(FDrawAlpha);
end;

procedure TSimbaCanvas.SetDrawAlpha(Value: Byte);
begin
  FDrawAlpha := Value;
  FDrawColorBGRA.A := Value;
end;

function TSimbaCanvas.GetFontStyle(Style: Integer): Boolean;
begin
  Result := ECanvasFontStyle(Style) in FFontStyles;
end;

procedure TSimbaCanvas.SetFontStyle(Style: Integer; Value: Boolean);
begin
  if Value then
    Include(FFontStyles, ECanvasFontStyle(Style))
  else
    Exclude(FFontStyles, ECanvasFontStyle(Style));
end;

function TSimbaCanvas.GetState: TSimbaCanvasState;
begin
  Result.DrawColor := FDrawColor;
  Result.DrawAlpha := FDrawAlpha;
  Result.DrawThickness := FDrawThickness;
  Result.DrawAntialiasing := FDrawAntialiasing;
  Result.DrawFeather := FDrawFeather;
  Result.DrawFilled := FDrawFilled;

  Result.FontName := FFontName;
  Result.FontSize := FFontSize;
  Result.FontStyles := FFontStyles;
end;

procedure TSimbaCanvas.SetState(const Value: TSimbaCanvasState);
begin
  FDrawAlpha := Value.DrawAlpha;
  SetDrawColor(Value.DrawColor); // rebuilds the drawing colour from the colour and alpha together
  FDrawThickness := Value.DrawThickness;
  FDrawAntialiasing := Value.DrawAntialiasing;
  FDrawFeather := Value.DrawFeather;
  FDrawFilled := Value.DrawFilled;

  FFontName := Value.FontName;
  FFontSize := Value.FontSize;
  FFontStyles := Value.FontStyles;
end;

constructor TSimbaCanvas.Create;
begin
  inherited Create();

  DefaultPixel.AsInteger := $FF000000; // opaque black

  DrawColor := -1;
  DrawAlpha := ALPHA_OPAQUE;
  FDrawThickness := 1;
  FDrawAntialiasing := False;
  FDrawFeather := 1;
  FDrawFilled := False;

  FFontName := '';
  FFontSize := 20;
  FFontStyles := [];
end;

procedure TSimbaCanvas.SetData(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer);
begin
  if (Data <> nil) and ((PixelsPerRow < AWidth) or (AWidth < 0) or (AHeight < 0)) then
    SimbaException('SimbaCanvas: %dx%d does not fit a PixelsPerRow of %d', [AWidth, AHeight, PixelsPerRow]);

  FData := Data;
  FPixelsPerRow := PixelsPerRow;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TSimbaCanvas.SetTransform(Offset: TPoint; ScaleShift: Integer);
begin
  if (ScaleShift < 0) or (ScaleShift > 30) then
    SimbaException('SimbaCanvas: Scale shift %d out of range', [ScaleShift]);

  FOffset := Offset;
  FScaleShift := ScaleShift;
end;

procedure TSimbaCanvas.RaiseOutOfBounds(X, Y: Integer);
begin
  SimbaException('%d,%d is outside the canvas bounds (0,0,%d,%d)', [X, Y, FWidth - 1, FHeight - 1]);
end;

function TSimbaCanvas.GetPixel(const X, Y: Integer): TColor;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  Result := FData[Y * FPixelsPerRow + X].ToColor;
end;

function TSimbaCanvas.GetAlpha(const X, Y: Integer): Byte;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  Result := FData[Y * FPixelsPerRow + X].A;
end;

procedure TSimbaCanvas.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  FData[Y * FPixelsPerRow + X] := Color.ToBGRA(ALPHA_OPAQUE);
end;

procedure TSimbaCanvas.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  FData[Y * FPixelsPerRow + X].A := Value;
end;

function TSimbaCanvas.GetPixels(Points: TPointArray): TColorArray;
var
  P, PointEnd: PPoint;
  Dst: PInteger;
begin
  SetLength(Result, Length(Points));
  if (Length(Points) = 0) then
    Exit;

  P := @Points[0];
  PointEnd := P + Length(Points);
  Dst := PInteger(Result);
  while (P < PointEnd) do
  begin
    if (P^.X < 0) or (P^.Y < 0) or (P^.X >= FWidth) or (P^.Y >= FHeight) then
      RaiseOutOfBounds(P^.X, P^.Y);

    Dst^ := FData[P^.Y * FPixelsPerRow + P^.X].ToColor;

    Inc(P);
    Inc(Dst);
  end;
end;

procedure TSimbaCanvas.SetPixels(Points: TPointArray; Color: TColor);
var
  BGRA: TColorBGRA;
  P, PointEnd: PPoint;
begin
  if (Length(Points) = 0) then
    Exit;

  BGRA := Color.ToBGRA(ALPHA_OPAQUE);

  P := @Points[0];
  PointEnd := P + Length(Points);
  while (P < PointEnd) do
  begin
    if (P^.X < 0) or (P^.Y < 0) or (P^.X >= FWidth) or (P^.Y >= FHeight) then
      RaiseOutOfBounds(P^.X, P^.Y);

    FData[P^.Y * FPixelsPerRow + P^.X] := BGRA;

    Inc(P);
  end;
end;

procedure TSimbaCanvas.SetPixels(Points: TPointArray; Colors: TColorArray);
var
  P, PointEnd: PPoint;
  Col: PColor;
begin
  if (Length(Points) <> Length(Colors)) then
    SimbaException('SimbaCanvas.SetPixels: Pixel & Color arrays must be same lengths (%d, %d)', [Length(Points), Length(Colors)]);
  if (Length(Points) = 0) then
    Exit;

  P := @Points[0];
  PointEnd := P + Length(Points);
  Col := PColor(Colors);
  while (P < PointEnd) do
  begin
    if (P^.X < 0) or (P^.Y < 0) or (P^.X >= FWidth) or (P^.Y >= FHeight) then
      RaiseOutOfBounds(P^.X, P^.Y);

    FData[P^.Y * FPixelsPerRow + P^.X] := Col^.ToBGRA(ALPHA_OPAQUE);

    Inc(P);
    Inc(Col);
  end;
end;

procedure TSimbaCanvas.Fill(Color: TColor);
begin
  if (FData <> nil) then
    FillData(FData, FPixelsPerRow * FHeight, Color.ToBGRA(ALPHA_OPAQUE));
end;

procedure TSimbaCanvas.FillWithAlpha(Value: Byte);
var
  Ptr, Upper: PColorBGRA;
begin
  if (FData = nil) then
    Exit;

  Ptr := FData;
  Upper := FData + FPixelsPerRow * FHeight;
  while (Ptr < Upper) do
  begin
    Ptr^.A := Value;
    Inc(Ptr);
  end;
end;

procedure TSimbaCanvas.Clear;
begin
  if (FData <> nil) then
    FillData(FData, FPixelsPerRow * FHeight, DefaultPixel);
end;

procedure TSimbaCanvas.Clear(Box: TBox);

  procedure _Fill(Y, X1, X2: Integer);
  begin
    if (X1 <= X2) then
      FillData(@FData[Y * FPixelsPerRow + X1], (X2 - X1) + 1, DefaultPixel);
  end;

  {$define _Row := _Fill}
  {$i shapebuilders/shapebuilder_box.inc}

begin
  if (FData <> nil) and (FWidth > 0) and (FHeight > 0) then
    _BuildBox(Box, False, TBox.Create(0, 0, FWidth - 1, FHeight - 1));
end;

procedure TSimbaCanvas.ClearInverted(Box: TBox);

  procedure _Fill(Y, X1, X2: Integer);
  begin
    if (X1 <= X2) then
      FillData(@FData[Y * FPixelsPerRow + X1], (X2 - X1) + 1, DefaultPixel);
  end;

  {$define _Row := _Fill}
  {$i shapebuilders/shapebuilder_box.inc}

begin
  if (FData <> nil) and (FWidth > 0) and (FHeight > 0) then
    _BuildBox(Box, True, TBox.Create(0, 0, FWidth - 1, FHeight - 1));
end;

procedure TSimbaCanvas.DoLine(Start, Stop: TPoint);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawLineAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Start, Stop, FDrawThickness, FDrawFeather)
  else
    SimbaImage_DrawLine(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Start, Stop, FDrawThickness);
end;

procedure TSimbaCanvas.DoBox(Box: TBox; Inverted: Boolean);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawBoxAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Box, FDrawFeather, Inverted)
  else
    SimbaImage_DrawBox(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Box, Inverted);
end;

procedure TSimbaCanvas.DoBoxEdge(Box: TBox);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawBoxEdgeAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Box, FDrawThickness, FDrawFeather)
  else
    SimbaImage_DrawBoxEdge(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Box, FDrawThickness);
end;

procedure TSimbaCanvas.DoPolygon(Poly: TPointArray; Inverted: Boolean);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawPolygonAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Poly, FDrawFeather, Inverted)
  else
    SimbaImage_DrawPolygon(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Poly, Inverted);
end;

procedure TSimbaCanvas.DoPolygonEdge(Poly: TPointArray);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawPolygonEdgeAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Poly, FDrawThickness, FDrawFeather)
  else
    SimbaImage_DrawPolygonEdge(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, Poly, FDrawThickness);
end;

procedure TSimbaCanvas.DoEllipse(ACenter: TPoint; XRadius, YRadius: Integer; Inverted: Boolean);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawEllipseAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ACenter, XRadius, YRadius, FDrawFeather, Inverted)
  else
    SimbaImage_DrawEllipse(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ACenter, XRadius, YRadius, Inverted);
end;

procedure TSimbaCanvas.DoEllipseEdge(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawEllipseEdgeAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ACenter, XRadius, YRadius, FDrawThickness, FDrawFeather)
  else
    SimbaImage_DrawEllipseEdge(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ACenter, XRadius, YRadius, FDrawThickness);
end;

procedure TSimbaCanvas.DoData(Src: PColorBGRA; SrcW, SrcH: Integer; P: TPoint);
var
  X1, Y1, X2, Y2, Y: Integer;
  SrcPtr, DestPtr: PColorBGRA;
begin
  X1 := Max(0, P.X);
  Y1 := Max(0, P.Y);
  X2 := Max(-1, Min(FPixelsPerRow - 1, P.X + SrcW - 1));
  Y2 := Max(-1, Min(FHeight - 1, P.Y + SrcH - 1));
  if (X1 > X2) or (Y1 > Y2) then // entirely off the canvas
    Exit;

  for Y := Y1 to Y2 do
  begin
    SrcPtr  := @Src[(Y - P.Y) * SrcW + (X1 - P.X)];
    DestPtr := @FData[Y * FPixelsPerRow + X1];

    BlendDataAlpha(DestPtr, SrcPtr, X2 - X1 + 1, FDrawAlpha);
  end;
end;

procedure TSimbaCanvas.DoText(Text: String; Position: TPoint);
begin
  SimbaImage_DrawText(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, FFontName, FFontSize, FFontStyles, Text, Position);
end;

procedure TSimbaCanvas.DoText(Text: String; Box: TBox; Alignments: ECanvasTextAligns);
begin
  SimbaImage_DrawText(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, FFontName, FFontSize, FFontStyles, Text, Box, Alignments);
end;

class function TSimbaCanvas.LoadFontsInDir(Dir: String): Boolean;
begin
  Result := SimbaImage_LoadFontsInDir(Dir);
end;

class function TSimbaCanvas.FontNames: TStringArray;
begin
  Result := SimbaImage_FontNames();
end;

function TSimbaCanvas.TextWidth(Text: String): Integer;
begin
  Result := SimbaImage_MeasureText(FFontName, FFontSize, FFontStyles, Text).X;
end;

function TSimbaCanvas.TextHeight(Text: String): Integer;
begin
  Result := SimbaImage_MeasureText(FFontName, FFontSize, FFontStyles, Text).Y;
end;

function TSimbaCanvas.TextSize(Text: String): TPoint;
begin
  Result := SimbaImage_MeasureText(FFontName, FFontSize, FFontStyles, Text);
end;

procedure TSimbaCanvas.DrawText(Text: String; Position: TPoint);
begin
  DoText(Text, ToLocal(Position));
end;

procedure TSimbaCanvas.DrawText(Text: String; Box: TBox; Alignments: ECanvasTextAligns);
begin
  DoText(Text, ToLocal(Box), Alignments);
end;

procedure TSimbaCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);
var
  I, LineHeight: Integer;
  P: TPoint;
begin
  LineHeight := TextHeight('TaylorSwift') + 1;

  // text is not scaled, so the step down the page is in pixels: walk the pixel-space
  // position rather than the draw-space one, which a scale shift would have shrunk
  P := ToLocal(Position);
  for I := 0 to High(Text) do
  begin
    if (P.Y >= FHeight) then // off the bottom, and so is every line after it
      Break;
    DoText(Text[I], P);

    Inc(P.Y, LineHeight);
  end;
end;

procedure TSimbaCanvas.DrawImage(Src: PColorBGRA; SrcW, SrcH: Integer; Location: TPoint);
var
  Step, X1, X2, Y1, Y2, Y: Integer;
  SrcPtr, Dest, RowEnd: PColorBGRA;
  Faded: TColorBGRA;
begin
  if (FScaleShift = 0) then
  begin
    DoData(Src, SrcW, SrcH, ToLocal(Location));
    Exit;
  end;

  Step := 1 shl FScaleShift;
  Location.X := Location.X + FOffset.X;
  Location.Y := Location.Y + FOffset.Y;

  // canvas pixel (X, Y) samples source pixel (X * Step - Location.X, Y * Step - Location.Y).
  // the first canvas pixel rounds up and the last rounds down, so every sample is inside the source
  X1 := Max(SarLongint(Location.X + Step - 1, FScaleShift), 0);
  Y1 := Max(SarLongint(Location.Y + Step - 1, FScaleShift), 0);
  X2 := Min(SarLongint(Location.X + SrcW - 1, FScaleShift), FWidth - 1);
  Y2 := Min(SarLongint(Location.Y + SrcH - 1, FScaleShift), FHeight - 1);
  if (X1 > X2) or (Y1 > Y2) then
    Exit;

  for Y := Y1 to Y2 do
  begin
    SrcPtr := @Src[(Y * Step - Location.Y) * SrcW + (X1 * Step - Location.X)];
    Dest   := @FData[Y * FPixelsPerRow + X1];
    RowEnd := Dest + (X2 - X1 + 1);
    while (Dest < RowEnd) do
    begin
      // the same rules as DoData: an opaque pixel is copied, a transparent one skipped,
      // anything between blends - all of it faded by DrawAlpha
      if (SrcPtr^.A <> ALPHA_TRANSPARENT) then
      begin
        Faded := SrcPtr^;
        if (FDrawAlpha <> ALPHA_OPAQUE) then
          Faded.A := (Faded.A * FDrawAlpha) div 255;

        if (Faded.A = ALPHA_OPAQUE) then
          Dest^ := Faded
        else if (Faded.A <> ALPHA_TRANSPARENT) then
          BlendPixel(Dest, @Faded);
      end;

      Inc(SrcPtr, Step);
      Inc(Dest);
    end;
  end;
end;

procedure TSimbaCanvas.DrawHeatmap(Mat: TSingleMatrix);
var
  Step, X1, Y1, X2, Y2, Y: Integer;
  Cell: PSingle;
  Dest, RowEnd: PColorBGRA;
  Color: TColorBGRA;
begin
  Step := 1 shl FScaleShift;

  X1 := Max(SarLongint(FOffset.X + Step - 1, FScaleShift), 0);
  Y1 := Max(SarLongint(FOffset.Y + Step - 1, FScaleShift), 0);
  X2 := Min(SarLongint(FOffset.X + Mat.Width - 1, FScaleShift), FWidth - 1);
  Y2 := Min(SarLongint(FOffset.Y + Mat.Height - 1, FScaleShift), FHeight - 1);
  if (X1 > X2) or (Y1 > Y2) then
    Exit;

  for Y := Y1 to Y2 do
  begin
    Cell := @Mat[Y * Step - FOffset.Y][X1 * Step - FOffset.X];
    Dest := @FData[Y * FPixelsPerRow + X1];
    RowEnd := Dest + (X2 - X1 + 1);

    while (Dest < RowEnd) do
    begin
      if (Cell^ >= 0) and (Cell^ <= 1.0) then
        Color := TColorBGRA(HeatmapTable[Round(Cell^ * High(HeatmapTable))]) // value in 0..1 -> index 0..255
      else
        Color.AsInteger := $FFFFFFFF; // white

      if (FDrawAlpha = ALPHA_OPAQUE) then
        Dest^ := Color
      else
      begin
        Color.A := FDrawAlpha;
        BlendPixel(Dest, @Color);
      end;

      Inc(Cell, Step);
      Inc(Dest);
    end;
  end;
end;

procedure TSimbaCanvas.DrawATPA(ATPA: T2DPointArray);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(ATPA) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);
    DrawTPA(ATPA[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaCanvas.DrawTPA(TPA: TPointArray);
begin
  if FDrawAntialiasing then
    SimbaImage_DrawTPAAA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ToLocal(TPA), FDrawThickness, FDrawFeather)
  else
    SimbaImage_DrawTPA(FData, FPixelsPerRow, FHeight, FDrawColorBGRA, ToLocal(TPA), FDrawThickness);
end;

procedure TSimbaCanvas.DrawCrosshairs(ACenter: TPoint; Size: Integer);
begin
  ACenter := ToLocal(ACenter);
  Size := Max(1, ToLocal(Size));

  with ACenter do
  begin
    DoLine(Point(X - Size, Y), Point(X + Size, Y));
    DoLine(Point(X, Y - Size), Point(X, Y + Size));
  end;
end;

procedure TSimbaCanvas.DrawCross(ACenter: TPoint; Radius: Integer);
begin
  ACenter := ToLocal(ACenter);
  Radius := Max(1, Round(ToLocal(Radius) / 2 * Sqrt(2)));

  with ACenter do
  begin
    DoLine(Point(X - Radius, Y - Radius), Point(X + Radius, Y + Radius));
    DoLine(Point(X + Radius, Y - Radius), Point(X - Radius, Y + Radius));
  end;
end;

procedure TSimbaCanvas.DrawLine(Start, Stop: TPoint);
begin
  DoLine(ToLocal(Start), ToLocal(Stop));
end;

procedure TSimbaCanvas.DrawBox(Box: TBox);
begin
  if FDrawFilled then
    DoBox(ToLocal(Box), False)
  else
    DoBoxEdge(ToLocal(Box));
end;

procedure TSimbaCanvas.DrawBoxInverted(Box: TBox);
begin
  DoBox(ToLocal(Box), True);
end;

procedure TSimbaCanvas.DrawPolygon(Points: TPointArray);
begin
  if FDrawFilled then
    DoPolygon(ToLocal(Points), False)
  else
    DoPolygonEdge(ToLocal(Points));
end;

procedure TSimbaCanvas.DrawPolygonInverted(Points: TPointArray);
begin
  DoPolygon(ToLocal(Points), True);
end;

procedure TSimbaCanvas.DrawQuad(Quad: TQuad);
begin
  Quad := ToLocal(Quad);
  if FDrawFilled then
    DoPolygon([Quad.Top, Quad.Right, Quad.Bottom, Quad.Left], False)
  else
    DoPolygonEdge([Quad.Top, Quad.Right, Quad.Right, Quad.Bottom, Quad.Bottom, Quad.Left, Quad.Left, Quad.Top]);
end;

procedure TSimbaCanvas.DrawQuadInverted(Quad: TQuad);
begin
  Quad := ToLocal(Quad);
  DoPolygon([Quad.Top, Quad.Right, Quad.Bottom, Quad.Left], True);
end;

procedure TSimbaCanvas.DrawCircle(ACenter: TPoint; Radius: Integer);
begin
  if FDrawFilled then
    DoEllipse(ToLocal(ACenter), ToLocal(Radius), ToLocal(Radius), False)
  else
    DoEllipseEdge(ToLocal(ACenter), ToLocal(Radius), ToLocal(Radius));
end;

procedure TSimbaCanvas.DrawCircleInverted(ACenter: TPoint; Radius: Integer);
begin
  DoEllipse(ToLocal(ACenter), ToLocal(Radius), ToLocal(Radius), True);
end;

procedure TSimbaCanvas.DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  if FDrawFilled then
    DoEllipse(ToLocal(ACenter), ToLocal(XRadius), ToLocal(YRadius), False)
  else
    DoEllipseEdge(ToLocal(ACenter), ToLocal(XRadius), ToLocal(YRadius));
end;

procedure TSimbaCanvas.DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  DoEllipse(ToLocal(ACenter), ToLocal(XRadius), ToLocal(YRadius), True);
end;

procedure TSimbaCanvas.DrawQuadArray(Quads: TQuadArray);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(Quads) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawQuad(Quads[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaCanvas.DrawBoxArray(Boxes: TBoxArray);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(Boxes) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawBox(Boxes[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaCanvas.DrawPolygonArray(Polygons: TPolygonArray);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(Polygons) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawPolygon(Polygons[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaCanvas.DrawCircleArray(Centers: TPointArray; Radius: Integer);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(Centers) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawCircle(Centers[I], Radius);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaCanvas.DrawCrossArray(Points: TPointArray; Radius: Integer);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := FDrawColor;

  for I := 0 to High(Points) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawCross(Points[I], Radius);
  end;

  DrawColor := PrevDrawColor;
end;

end.
