{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  A canvas over pixel data that something else allocated.
   - Draws directly onto that data, there is no back buffer.
   - Everything is locked, to ensure thread safety.
}
unit simba.canvas_external;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.colormath, simba.image, simba.canvas, simba.image_drawtext, simba.threading,
  simba.vartype_quad, simba.vartype_polygon;

type
  TSimbaExternalCanvas = class(TSimbaCanvas)
  protected
    FLock: TEnterableLock;

    function GetPixel(const X, Y: Integer): TColor; override;
    function GetAlpha(const X, Y: Integer): Byte; override;
    procedure SetPixel(const X, Y: Integer; const Color: TColor); override;
    procedure SetAlpha(const X, Y: Integer; const Value: Byte); override;
  public
    UserData: Pointer;
    AutoResize: Boolean;

    constructor Create; reintroduce;

    // set/resize the data the canvas is pointing to
    procedure SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
    procedure Resize(NewWidth, NewHeight: Integer);

    function GetPixels(Points: TPointArray): TColorArray; override;
    procedure SetPixels(Points: TPointArray; Color: TColor); overload; override;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload; override;

    procedure Fill(Color: TColor); override;
    procedure FillWithAlpha(Value: Byte); override;

    procedure Clear; overload; override;
    procedure Clear(Box: TBox); overload; override;
    procedure ClearInverted(Box: TBox); override;

    // TSimbaCanvas' drawing, with the lock held.
    procedure DrawText(Text: String; Position: TPoint); overload; override;
    procedure DrawText(Text: String; Box: TBox; Alignments: ECanvasTextAligns); overload; override;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint); override;

    procedure DrawImage(Src: PColorBGRA; SrcW, SrcH: Integer; Location: TPoint); overload; override;
    procedure DrawImage(Image: TSimbaImage; Location: TPoint); overload;
    procedure DrawHeatmap(Mat: TSingleMatrix); override;

    procedure DrawATPA(ATPA: T2DPointArray); override;
    procedure DrawTPA(TPA: TPointArray); override;

    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer); override;
    procedure DrawCross(ACenter: TPoint; Radius: Integer); override;
    procedure DrawLine(Start, Stop: TPoint); override;

    procedure DrawBox(Box: TBox); override;
    procedure DrawBoxInverted(Box: TBox); override;

    procedure DrawPolygon(Points: TPointArray); override;
    procedure DrawPolygonInverted(Points: TPointArray); override;

    procedure DrawQuad(Quad: TQuad); override;
    procedure DrawQuadInverted(Quad: TQuad); override;

    procedure DrawCircle(ACenter: TPoint; Radius: Integer); override;
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer); override;

    procedure DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer); override;
    procedure DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer); override;

    procedure DrawQuadArray(Quads: TQuadArray); override;
    procedure DrawBoxArray(Boxes: TBoxArray); override;
    procedure DrawPolygonArray(Polygons: TPolygonArray); override;
    procedure DrawCircleArray(Centers: TPointArray; Radius: Integer); override;
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer); override;
  end;

implementation

constructor TSimbaExternalCanvas.Create;
begin
  inherited Create();

  DefaultPixel := Default(TColorBGRA); // transparent black: what Clear leaves behind shows nothing
end;

procedure TSimbaExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
begin
  FLock.Enter();
  try
    SetData(Data, AWidth, AWidth, AHeight);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Resize(NewWidth, NewHeight: Integer);
begin
  FLock.Enter();
  try
    if (FData <> nil) then // nothing to resize until SetMemory
      SetData(FData, NewWidth, NewWidth, NewHeight);
  finally
    FLock.Leave();
  end;
end;

function TSimbaExternalCanvas.GetPixel(const X, Y: Integer): TColor;
begin
  FLock.Enter();
  try
    Result := inherited GetPixel(X, Y);
  finally
    FLock.Leave();
  end;
end;

function TSimbaExternalCanvas.GetAlpha(const X, Y: Integer): Byte;
begin
  FLock.Enter();
  try
    Result := inherited GetAlpha(X, Y);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  FLock.Enter();
  try
    inherited SetPixel(X, Y, Color);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  FLock.Enter();
  try
    inherited SetAlpha(X, Y, Value);
  finally
    FLock.Leave();
  end;
end;

function TSimbaExternalCanvas.GetPixels(Points: TPointArray): TColorArray;
begin
  FLock.Enter();
  try
    Result := inherited GetPixels(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetPixels(Points: TPointArray; Color: TColor);
begin
  FLock.Enter();
  try
    inherited SetPixels(Points, Color);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.SetPixels(Points: TPointArray; Colors: TColorArray);
begin
  FLock.Enter();
  try
    inherited SetPixels(Points, Colors);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Fill(Color: TColor);
begin
  FLock.Enter();
  try
    inherited Fill(Color);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.FillWithAlpha(Value: Byte);
begin
  FLock.Enter();
  try
    inherited FillWithAlpha(Value);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Clear;
begin
  FLock.Enter();
  try
    inherited Clear();
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.Clear(Box: TBox);
begin
  FLock.Enter();
  try
    inherited Clear(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.ClearInverted(Box: TBox);
begin
  FLock.Enter();
  try
    inherited ClearInverted(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawText(Text: String; Position: TPoint);
begin
  FLock.Enter();
  try
    inherited DrawText(Text, Position);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawText(Text: String; Box: TBox; Alignments: ECanvasTextAligns);
begin
  FLock.Enter();
  try
    inherited DrawText(Text, Box, Alignments);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);
begin
  FLock.Enter();
  try
    inherited DrawTextLines(Text, Position);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawImage(Src: PColorBGRA; SrcW, SrcH: Integer; Location: TPoint);
begin
  FLock.Enter();
  try
    inherited DrawImage(Src, SrcW, SrcH, Location);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawImage(Image: TSimbaImage; Location: TPoint);
begin
  DrawImage(Image.Data, Image.Width, Image.Height, Location); // takes the lock
end;

procedure TSimbaExternalCanvas.DrawHeatmap(Mat: TSingleMatrix);
begin
  FLock.Enter();
  try
    inherited DrawHeatmap(Mat);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawATPA(ATPA: T2DPointArray);
begin
  FLock.Enter();
  try
    inherited DrawATPA(ATPA);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawTPA(TPA: TPointArray);
begin
  FLock.Enter();
  try
    inherited DrawTPA(TPA);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCrosshairs(ACenter: TPoint; Size: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCrosshairs(ACenter, Size);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCross(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCross(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawLine(Start, Stop: TPoint);
begin
  FLock.Enter();
  try
    inherited DrawLine(Start, Stop);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBox(Box: TBox);
begin
  FLock.Enter();
  try
    inherited DrawBox(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBoxInverted(Box: TBox);
begin
  FLock.Enter();
  try
    inherited DrawBoxInverted(Box);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygon(Points: TPointArray);
begin
  FLock.Enter();
  try
    inherited DrawPolygon(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygonInverted(Points: TPointArray);
begin
  FLock.Enter();
  try
    inherited DrawPolygonInverted(Points);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuad(Quad: TQuad);
begin
  FLock.Enter();
  try
    inherited DrawQuad(Quad);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuadInverted(Quad: TQuad);
begin
  FLock.Enter();
  try
    inherited DrawQuadInverted(Quad);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircle(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCircle(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircleInverted(ACenter: TPoint; Radius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCircleInverted(ACenter, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawEllipse(ACenter, XRadius, YRadius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawEllipseInverted(ACenter, XRadius, YRadius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawQuadArray(Quads: TQuadArray);
begin
  FLock.Enter();
  try
    inherited DrawQuadArray(Quads);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawBoxArray(Boxes: TBoxArray);
begin
  FLock.Enter();
  try
    inherited DrawBoxArray(Boxes);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawPolygonArray(Polygons: TPolygonArray);
begin
  FLock.Enter();
  try
    inherited DrawPolygonArray(Polygons);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCircleArray(Centers: TPointArray; Radius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCircleArray(Centers, Radius);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaExternalCanvas.DrawCrossArray(Points: TPointArray; Radius: Integer);
begin
  FLock.Enter();
  try
    inherited DrawCrossArray(Points, Radius);
  finally
    FLock.Leave();
  end;
end;

end.

