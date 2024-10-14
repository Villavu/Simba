{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
}
unit simba.component_imageboxcanvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, FPImage,
  simba.base, simba.image_lazbridge, simba.component_imageboxdrawers, simba.image_textdrawer;

type
  TSimbaImageBoxCanvas = class;
  PSimbaImageBoxCanvas = ^TSimbaImageBoxCanvas;

  TTextDrawer = class(TSimbaTextDrawerBase)
  protected
    FBitmap: TBitmap;
    FCurrentColor: TFPColor;
    FCurrentX, FCurrentY: Integer;

    procedure MoveToPixel(x,y: integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;
  public
    constructor Create(Bitmap: TBitmap); reintroduce;
  end;

  TSimbaImageBoxCanvas = class
  protected
    FBitmap: TBitmap;
    FPixelFormat: ELazPixelFormat;
    FOffset: TPoint;
    FRect: TRect;
    FWidth: Integer;
    FHeight: Integer;
    FData: PByte;
    FBytesPerLine: Integer;
    FTextDrawer: TTextDrawer;

    function GetDrawInfo(Color: TColor): TDrawInfo;

    function GetFontAntialiasing: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;

    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate(Rect: TRect; Width, Height: Integer);
    procedure EndUpdate;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor); overload;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawLine(Start, Stop: TPoint; Color: TColor);
    procedure DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor);

    procedure DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);

    procedure DrawBox(Box: TBox; Color: TColor);
    procedure DrawBoxFilled(Box: TBox; Color: TColor); overload;
    procedure DrawBoxFilled(Box: TBox; Color: TColor; Transparency: Single); overload;

    procedure DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);

    procedure DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
    procedure DrawPolyFilled(Poly: TPointArray; Color: TColor);

    procedure DrawQuad(Quad: TQuad; Color: TColor);
    procedure DrawQuadFilled(Quad: TQuad; Color: TColor);

    procedure DrawPoint(Point: TPoint; Color: TColor);
    procedure DrawPoints(TPA: TPointArray; Color: TColor);

    procedure DrawHeatmap(Mat: TSingleMatrix);

    property Bitmap: TBitmap read FBitmap;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

implementation

uses
  Math,
  simba.colormath, simba.vartype_box, simba.vartype_point;

procedure TTextDrawer.MoveToPixel(x, y: integer);
begin
  FCurrentX := X;
  FCurrentY := Y;
end;

function TTextDrawer.GetCurrentColor: TFPColor;
begin
  Result := FBitmap.Canvas.Colors[FCurrentX, FCurrentY];
end;

procedure TTextDrawer.SetCurrentColorAndMoveRight(const AColor: TFPColor);
begin
  FBitmap.Canvas.Colors[FCurrentX, FCurrentY] := AColor;
  Inc(FCurrentX);
end;

procedure TTextDrawer.MoveRight;
begin
  Inc(FCurrentX);
end;

function TTextDrawer.GetClipRect: TRect;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := FBitmap.Width;
  Result.Bottom := FBitmap.Height;
end;

constructor TTextDrawer.Create(Bitmap: TBitmap);
begin
  inherited Create();

  FBitmap := Bitmap;
end;

function TSimbaImageBoxCanvas.GetDrawInfo(Color: TColor): TDrawInfo;
begin
  Result.Color := Color;
  Result.Data := FData;
  Result.BytesPerLine := FBytesPerLine;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.VisibleRect := FRect;
  Result.Offset := FOffset;
end;

function TSimbaImageBoxCanvas.GetFontAntialiasing: Boolean;
begin
  Result := FTextDrawer.Antialiased;
end;

function TSimbaImageBoxCanvas.GetFontName: String;
begin
  Result := FTextDrawer.Font;
end;

function TSimbaImageBoxCanvas.GetFontSize: Single;
begin
  Result := FTextDrawer.Size;
end;

function TSimbaImageBoxCanvas.GetFontBold: Boolean;
begin
  Result := FTextDrawer.Bold;
end;

function TSimbaImageBoxCanvas.GetFontItalic: Boolean;
begin
  Result := FTextDrawer.Italic;
end;

procedure TSimbaImageBoxCanvas.SetFontAntialiasing(Value: Boolean);
begin
  FTextDrawer.Antialiased := Value;
end;

procedure TSimbaImageBoxCanvas.SetFontName(Value: String);
begin
  FTextDrawer.Font := Value;
end;

procedure TSimbaImageBoxCanvas.SetFontSize(Value: Single);
begin
  FTextDrawer.Size := Value;
end;

procedure TSimbaImageBoxCanvas.SetFontBold(Value: Boolean);
begin
  FTextDrawer.Bold := Value;
end;

procedure TSimbaImageBoxCanvas.SetFontItalic(Value: Boolean);
begin
  FTextDrawer.Italic := Value;
end;

constructor TSimbaImageBoxCanvas.Create;
begin
  inherited Create();

  FBitmap := TBitmap.Create();
  FTextDrawer := TTextDrawer.Create(FBitmap);
  FPixelFormat := LazImage_PixelFormat(FBitmap);
  if (not (FPixelFormat in [ELazPixelFormat.BGR, ELazPixelFormat.BGRA, ELazPixelFormat.ARGB])) then
    SimbaException('SimbaImageBoxCanvas pixel format not supported');
end;

destructor TSimbaImageBoxCanvas.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FTextDrawer);

  inherited Destroy();
end;

procedure TSimbaImageBoxCanvas.BeginUpdate(Rect: TRect; Width, Height: Integer);
begin
  FBitmap.BeginUpdate();
  if (FBitmap.Width < Width) or (FBitmap.Height < Height) then
    FBitmap.SetSize(
       Max(FBitmap.Width,  Width + 150), // over allocate a little
       Max(FBitmap.Height, Height + 150)
     );

  FRect := Rect;
  FWidth := Width;
  FHeight := Height;

  FOffset.X := -FRect.Left;
  FOffset.Y := -FRect.Top;

  FBytesPerLine := FBitmap.RawImage.Description.BytesPerLine;
  FData := FBitmap.RawImage.Data;
end;

procedure TSimbaImageBoxCanvas.EndUpdate;
begin
  FBitmap.EndUpdate();
end;

procedure TSimbaImageBoxCanvas.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Position.Offset(FOffset), Color);
end;

procedure TSimbaImageBoxCanvas.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Box.Offset(FOffset), Alignments, Color);
end;

function TSimbaImageBoxCanvas.TextWidth(Text: String): Integer;
begin
  Result := FTextDrawer.TextWidth(Text);
end;

function TSimbaImageBoxCanvas.TextHeight(Text: String): Integer;
begin
  Result := FTextDrawer.TextHeight(Text);
end;

function TSimbaImageBoxCanvas.TextSize(Text: String): TPoint;
begin
  Result := FTextDrawer.TextSize(Text);
end;

procedure TSimbaImageBoxCanvas.DrawLine(Start, Stop: TPoint; Color: TColor);
begin
  Start := Start.Offset(FOffset);
  Stop := Stop.Offset(FOffset);

  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawLine<TColorBGR>(Start, Stop, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawLine<TColorBGRA>(Start, Stop, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawLine<TColorARGB>(Start, Stop, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor);
begin
  Start := Start.Offset(FOffset);
  Stop := Stop.Offset(FOffset);

  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawLineGap<TColorBGR>(Start, Stop, GapSize, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawLineGap<TColorBGRA>(Start, Stop, GapSize, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawLineGap<TColorARGB>(Start, Stop, GapSize, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Radius := Radius div 2;

  DrawLine(TPoint.Create(Center.X - Radius, Center.Y - Radius), TPoint.Create(Center.X + Radius, Center.Y + Radius), Color);
  DrawLine(TPoint.Create(Center.X + Radius, Center.Y - Radius), TPoint.Create(Center.X - Radius, Center.Y + Radius), Color);
end;

procedure TSimbaImageBoxCanvas.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);
var
  Center: TPoint;
begin
  Radius := Radius div 2;
  for Center in Centers do
  begin
    DrawLine(TPoint.Create(Center.X - Radius, Center.Y - Radius), TPoint.Create(Center.X + Radius, Center.Y + Radius), Color);
    DrawLine(TPoint.Create(Center.X + Radius, Center.Y - Radius), TPoint.Create(Center.X - Radius, Center.Y + Radius), Color);
  end;
end;

procedure TSimbaImageBoxCanvas.DrawBox(Box: TBox; Color: TColor);
begin
  Box := Box.Offset(FOffset);
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
    case FPixelFormat of
      ELazPixelFormat.BGR:  specialize DoDrawBoxEdge<TColorBGR>(Box, GetDrawInfo(Color));
      ELazPixelFormat.BGRA: specialize DoDrawBoxEdge<TColorBGRA>(Box, GetDrawInfo(Color));
      ELazPixelFormat.ARGB: specialize DoDrawBoxEdge<TColorARGB>(Box, GetDrawInfo(Color));
    end;
end;

procedure TSimbaImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor);
begin
  Box := Box.Offset(FOffset);
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
    case FPixelFormat of
      ELazPixelFormat.BGR:  specialize DoDrawBoxFilled<TColorBGR>(Box, GetDrawInfo(Color));
      ELazPixelFormat.BGRA: specialize DoDrawBoxFilled<TColorBGRA>(Box, GetDrawInfo(Color));
      ELazPixelFormat.ARGB: specialize DoDrawBoxFilled<TColorARGB>(Box, GetDrawInfo(Color));
    end;
end;

procedure TSimbaImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor; Transparency: Single);
begin
  Box := Box.Offset(FOffset);
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
    case FPixelFormat of
      ELazPixelFormat.BGR:  specialize DoDrawBoxFilledEx<TColorBGR>(Box, Transparency, GetDrawInfo(Color));
      ELazPixelFormat.BGRA: specialize DoDrawBoxFilledEx<TColorBGRA>(Box, Transparency, GetDrawInfo(Color));
      ELazPixelFormat.ARGB: specialize DoDrawBoxFilledEx<TColorARGB>(Box, Transparency, GetDrawInfo(Color));
    end;
end;

procedure TSimbaImageBoxCanvas.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Center := Center.Offset(FOffset);

  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawCircle<TColorBGR>(Center, Radius, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawCircle<TColorBGRA>(Center, Radius, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawCircle<TColorARGB>(Center, Radius, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Center := Center.Offset(FOffset);

  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawCircleFilled<TColorBGR>(Center, Radius, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawCircleFilled<TColorBGRA>(Center, Radius, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawCircleFilled<TColorARGB>(Center, Radius, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
var
  I: Integer;
begin
  if (Length(Poly) <= 1) then
    Exit;

  for I := 0 to High(Poly) - 1 do
    DrawLine(Poly[I], Poly[I+1], Color);
  if Connect then
    DrawLine(Poly[High(Poly)], Poly[0], Color);
end;

procedure TSimbaImageBoxCanvas.DrawPolyFilled(Poly: TPointArray; Color: TColor);
begin
  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawPolygonFilled<TColorBGR>(Poly, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawPolygonFilled<TColorBGRA>(Poly, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawPolygonFilled<TColorARGB>(Poly, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawQuad(Quad: TQuad; Color: TColor);
begin
  DrawLine(Quad.Top, Quad.Right, Color);
  DrawLine(Quad.Right, Quad.Bottom, Color);
  DrawLine(Quad.Bottom, Quad.Left, Color);
  DrawLine(Quad.Left, Quad.Top, Color);
end;

procedure TSimbaImageBoxCanvas.DrawQuadFilled(Quad: TQuad; Color: TColor);
begin
  DrawPolyFilled([Quad.Top, Quad.Right, Quad.Bottom, Quad.Left], Color);
end;

procedure TSimbaImageBoxCanvas.DrawPoint(Point: TPoint; Color: TColor);
begin
  DrawPoints([Point], Color);
end;

procedure TSimbaImageBoxCanvas.DrawPoints(TPA: TPointArray; Color: TColor);
begin
  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawPoints<TColorBGR>(TPA, GetDrawInfo(Color));
    ELazPixelFormat.BGRA: specialize DoDrawPoints<TColorBGRA>(TPA, GetDrawInfo(Color));
    ELazPixelFormat.ARGB: specialize DoDrawPoints<TColorARGB>(TPA, GetDrawInfo(Color));
  end;
end;

procedure TSimbaImageBoxCanvas.DrawHeatmap(Mat: TSingleMatrix);
begin
  case FPixelFormat of
    ELazPixelFormat.BGR:  specialize DoDrawHeatmap<TColorBGR>(Mat, GetDrawInfo(0));
    ELazPixelFormat.BGRA: specialize DoDrawHeatmap<TColorBGRA>(Mat, GetDrawInfo(0));
    ELazPixelFormat.ARGB: specialize DoDrawHeatmap<TColorARGB>(Mat, GetDrawInfo(0));
  end;
end;

end.

