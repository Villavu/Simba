{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.baseclass, simba.base, simba.image_textdrawer, simba.image_utils,
  simba.colormath, simba.colormath_distance;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaImageMirrorStyle = (WIDTH, HEIGHT, LINE);
  ESimbaImageThreshMethod = (MEAN, MIN_MAX);
  {$POP}

  TSimbaImageLineStarts = array of PColorBGRA;

  TSimbaImage = class(TSimbaBaseClass)
  protected
    FWidth: Integer;
    FHeight: Integer;
    FCenter: TPoint;

    FData: PColorBGRA;
    FDataOwner: Boolean;
    FDataSize: SizeUInt;

    FLineStarts: TSimbaImageLineStarts;

    FTextDrawer: TSimbaTextDrawer;

    procedure RaiseOutOfImageException(X, Y: Integer);
    procedure NotifyUnfreed; override;

    procedure _DrawTPA(TPA: TPointArray; Color: TColor);
    procedure _DrawTPAAlpha(TPA: TPointArray; Color: TColor; Alpha: Byte);

    procedure _DrawLine(Start, Stop: TPoint; Color: TColor);
    procedure _DrawLineAlpha(Start, Stop: TPoint; Color: TColor; Alpha: Byte);

    procedure _DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor);
    procedure _DrawLineGapAlpha(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte);

    procedure _DrawBoxFilled(Box: TBox; Color: TColor);
    procedure _DrawBoxFilledAlpha(Box: TBox; Color: TColor; Alpha: Byte);

    procedure _DrawBoxEdge(Box: TBox; Color: TColor);
    procedure _DrawBoxEdgeAlpha(Box: TBox; Color: TColor; Alpha: Byte);

    procedure _DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
    procedure _DrawCircleFilledAlpha(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);

    procedure _DrawPolygonFilled(Points: TPointArray; Color: TColor);
    procedure _DrawPolygonFilledAlpha(Points: TPointArray; Color: TColor; Alpha: Byte);

    procedure _DrawImage(Image: TSimbaImage; P: TPoint);
    procedure _DrawImageAlpha(Image: TSimbaImage; P: TPoint; Alpha: Byte);

    function GetPixel(const X, Y: Integer): TColor;
    function GetFontAntialiasing: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetLineStart(const Y: Integer): PColorBGRA;

    procedure SetPixel(X, Y: Integer; Color: TColor);
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
  public
    class var SaveUnfreedImages: ShortString;
    class function LoadFonts(Dir: String): Boolean;
    class function FontNames: TStringArray;
  public
    DefaultPixel: TColorBGRA;

    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    constructor CreateFromFile(FileName: String);
    constructor CreateFromZip(ZipFileName, ZipEntry: String);
    constructor CreateFromString(Str: String);
    constructor CreateFromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);
    constructor CreateFromWindow(Window: TWindowHandle);

    destructor Destroy; override;

    property Data: PColorBGRA read FData;
    property DataOwner: Boolean read FDataOwner write FDataOwner;
    property DataSize: SizeUInt read FDataSize;

    property LineStarts: TSimbaImageLineStarts read FLineStarts;
    property LineStart[Line: Integer]: PColorBGRA read GetLineStart;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Center: TPoint read FCenter;

    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;

    property TextDrawer: TSimbaTextDrawer read FTextDrawer;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    function InImage(const X, Y: Integer): Boolean;

    function Equals(Other: TObject): Boolean; override;
    function Equals(Other: TSimbaImage): Boolean; overload;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EDrawTextAlignSet; Color: TColor); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);

    procedure SetSize(NewWidth, NewHeight: Integer);

    procedure SetAlpha(Value: Byte); overload;
    procedure SetAlpha(Points: TPointArray; Value: Byte); overload;
    procedure SetAlpha(Color: TColor; Value: Byte); overload;

    function ResizeNN(NewWidth, NewHeight: Integer): TSimbaImage;
    function ResizeBilinear(NewWidth, NewHeight: Integer): TSimbaImage;

    function RotateNN(Radians: Single; Expand: Boolean): TSimbaImage;
    function RotateBilinear(Radians: Single; Expand: Boolean): TSimbaImage;

    function GetPixels(Points: TPointArray): TColorArray;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray);

    procedure SetExternalData(AData: PColorBGRA; AWidth, AHeight: Integer);
    procedure ResetExternalData;

    // Image
    procedure DrawImage(Image: TSimbaImage; Location: TPoint; Alpha: Byte = 0);

    // Point
    procedure DrawATPA(ATPA: T2DPointArray; Color: TColor = -1; Alpha: Byte = 0);
    procedure DrawTPA(TPA: TPointArray; Color: TColor; Alpha: Byte = 0);

    // Line
    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawLine(Start, Stop: TPoint; Color: TColor; Alpha: Byte = 0);
    procedure DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte = 0);

    // Box
    procedure DrawBox(Box: TBox; Color: TColor; Alpha: Byte = 0);
    procedure DrawBoxFilled(Box: TBox; Color: TColor; Alpha: Byte = 0);
    procedure DrawBoxInverted(B: TBox; Color: TColor; Alpha: Byte = 0);

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
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);
    procedure DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);

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

    procedure DrawHSLCircle(ACenter: TPoint; Radius: Integer);

    procedure DrawMatrix(Matrix: TIntegerMatrix); overload;
    procedure DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload;

    procedure Fill(Color: TColor; Alpha: Byte = 0);

    procedure Clear; overload;
    procedure Clear(Box: TBox); overload;
    procedure ClearInverted(Box: TBox);

    procedure SplitChannels(var B,G,R,A: TByteArray); overload;
    procedure SplitChannels(var B,G,R: TByteArray); overload;

    function GetColors: TColorArray;
    procedure ReplaceColor(OldColor, NewColor: TColor);
    procedure ReplaceColors(OldColors, NewColors: TColorArray);

    function GreyScale: TSimbaImage;
    function Brightness(Value: Integer): TSimbaImage;
    function Invert: TSimbaImage;
    function Posterize(Value: Integer): TSimbaImage;
    function Convolute(Matrix: TDoubleMatrix): TSimbaImage;
    function Mirror(Style: ESimbaImageMirrorStyle): TSimbaImage;
    function BoxBlur(Radius: Integer): TSimbaImage;
    function GaussBlur(Radius: Double): TSimbaImage;
    function Blend(Points: TPointArray; Radius: Integer): TSimbaImage;
    function Downsample(Scale: Integer): TSimbaImage;

    function Copy(X1, Y1, X2, Y2: Integer): TSimbaImage; overload;
    function Copy: TSimbaImage; overload;

    procedure Crop(X1, Y1, X2, Y2: Integer);
    procedure Pad(Amount: Integer);

    function ToLazBitmap: TBitmap;
    procedure FromLazBitmap(LazBitmap: TBitmap);

    function ToGreyMatrix: TByteMatrix;
    function ToMatrix: TIntegerMatrix; overload;
    function ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload;

    function ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: ESimbaImageThreshMethod; K: Integer): TSimbaImage;
    function ThresholdSauvola(Radius: Integer; AInvert: Boolean = False; R: Single = 128; K: Single = 0.5): TSimbaImage;

    procedure Load(FileName: String); overload;
    procedure Load(FileName: String; Area: TBox); overload;
    function Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
    function SaveToString: String;

    procedure FromStream(Stream: TStream; FileName: String);
    procedure FromString(Str: String);
    procedure FromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);

    function Compare(Other: TSimbaImage): Single;

    function PixelDifference(Other: TSimbaImage): Integer; overload;
    function PixelDifference(Other: TSimbaImage; Tolerance: Single): Integer; overload;
    function PixelDifferenceTPA(Other: TSimbaImage): TPointArray; overload;
    function PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Single): TPointArray; overload;
  end;
  TSimbaImageArray = array of TSimbaImage;

  PSimbaImage = ^TSimbaImage;
  PSimbaImageArray = ^TSimbaImageArray;

implementation

uses
  Math, FPImage,
  simba.zip, simba.vartype_floatmatrix, simba.vartype_box, simba.vartype_quad, simba.geometry, simba.nativeinterface,
  simba.vartype_ordmatrix, simba.vartype_pointarray,
  simba.image_lazbridge, simba.image_integral, simba.image_gaussblur,
  simba.image_bitmaparealoader, simba.image_stringconv, simba.containers, simba.array_algorithm;

function TSimbaImage.Save(FileName: String; OverwriteIfExists: Boolean): Boolean;
var
  Stream: TFileStream;
  WriterClass: TFPCustomImageWriterClass;
begin
  Result := False;

  if FileExists(FileName) and (not OverwriteIfExists) then
    SimbaException('TSimbaImage.Save: File already exists "%s"', [FileName]);

  WriterClass := TFPCustomImage.FindWriterFromFileName(FileName);
  if (WriterClass = nil) then
    SimbaException('TSimbaImage.Save: Unknown image format "%s"', [FileName]);

  Stream := nil;
  try
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);

    SimbaImage_ToFPImageWriter(Self, WriterClass, Stream);

    Result := True;
  finally
    Stream.Free();
  end;
end;

procedure TSimbaImage.Load(FileName: String);
var
  ReaderClass: TFPCustomImageReaderClass;
  Stream: TFileStream;
begin
  if (not FileExists(FileName)) then
    SimbaException('TSimbaImage.LoadFromFile: File "%s" does not exist', [FileName]);

  ReaderClass := TFPCustomImage.FindReaderFromFileName(FileName);
  if (ReaderClass = nil) then
    SimbaException('TSimbaImage.LoadFromFile: Unknown image format "%s"', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SimbaImage_FromFPImageReader(Self, ReaderClass, Stream);
  finally
    Stream.Free();
  end;
end;

procedure TSimbaImage.Load(FileName: String; Area: TBox);
begin
  if (not FileExists(FileName)) then
    SimbaException('TSimbaImage.LoadFromFile: File not found "%s"', [FileName]);

  if FileName.EndsWith('.bmp', False) then
    SimbaImage_LoadBitmapArea(Self, FileName, Area)
  else
  begin
    Load(FileName);

    Area := Area.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));
    if (Area.Width > 1) and (Area.Height > 1) then
      Crop(Area.X1, Area.Y1, Area.X2, Area.Y2);
  end;
end;

function TSimbaImage.Copy: TSimbaImage;
begin
  Result := TSimbaImage.Create();
  Result.SetSize(FWidth, FHeight);

  Move(FData^, Result.FData^, FWidth * FHeight * SizeOf(TColorBGRA));
end;

function TSimbaImage.Copy(X1, Y1, X2, Y2: Integer): TSimbaImage;
var
  Y: Integer;
begin
  if (not InImage(X1, Y1)) then RaiseOutOfImageException(X1, Y1);
  if (not InImage(X2, Y2)) then RaiseOutOfImageException(X2, Y2);

  Result := TSimbaImage.Create();
  Result.SetSize(X2-X1+1, Y2-Y1+1);
  for Y := Y1 to Y2 do
    Move(FData[Y * FWidth + X1], Result.FData[(Y-Y1) * Result.FWidth], Result.FWidth * SizeOf(TColorBGRA));
end;

procedure TSimbaImage.Crop(X1, Y1, X2, Y2: Integer);
var
  Y: Integer;
begin
  if (not InImage(X1, Y1)) then RaiseOutOfImageException(X1, Y1);
  if (not InImage(X2, Y2)) then RaiseOutOfImageException(X2, Y2);

  for Y := Y1 to Y2 do
    Move(FData[Y * FWidth + X1], FData[(Y-Y1) * FWidth], FWidth * SizeOf(TColorBGRA));

  SetSize(X2-X1+1, Y2-Y1+1);
end;

function TSimbaImage.ToLazBitmap: TBitmap;
begin
  Result := SimbaImage_ToLazImage(Self);
end;

function TSimbaImage.ToGreyMatrix: TByteMatrix;
var
  X, Y: Integer;
begin
  Result.SetSize(FWidth, FHeight);

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
      with FData[Y * FWidth + X] do
        Result[Y, X] := Round(R * 0.3 + G * 0.59 + B * 0.11);
end;

function TSimbaImage.ToMatrix: TIntegerMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(Width, Height);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      with FData[Y * FWidth + X] do
        Result[Y, X] := TColor(R or G shl G_BIT or B shl B_BIT);
end;

function TSimbaImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
var
  X, Y: Integer;
begin
  if (not InImage(X1, Y1)) then RaiseOutOfImageException(X1, Y1);
  if (not InImage(X2, Y2)) then RaiseOutOfImageException(X2, Y2);

  Result.SetSize(X2-X1+1, Y2-Y1+1);

  for Y := Y1 to Y2 do
    for X := X1 to X2 do
      with FData[Y * FWidth + X] do
        Result[Y-Y1, X-X1] := TColor(R or G shl G_BIT or B shl B_BIT);
end;

procedure TSimbaImage.DrawMatrix(Matrix: TIntegerMatrix);
var
  X, Y, W, H: Integer;
begin
  SetSize(Matrix.Width, Matrix.Height);

  W := FWidth - 1;
  H := FHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
      FData[Y * FWidth + X] := TColorBGRA(
        (Matrix[Y, X] shr B_BIT and $FF) or
        (Matrix[Y, X] shr G_BIT and $FF) shl G_BIT or
        (Matrix[Y, X] shr R_BIT and $FF) shl B_BIT
      );
end;

procedure TSimbaImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
var
  X,Y, W,H: TColor;
  Normed: TSingleMatrix;
  HSL: TColorHSL;
begin
  SetSize(Matrix.Width, Matrix.Height);

  Normed := Matrix.NormMinMax(0, 1);

  W := FWidth - 1;
  H := FHeight - 1;

  HSL := Default(TColorHSL);

  for Y := 0 to H do
    for X := 0 to W do
    begin
      case ColorMapID of
        0:begin //cold blue to red
            HSL.H := (1 - Normed[Y,X]) * 240;
            HSL.S := 40 + Normed[Y,X] * 60;
            HSL.L := 50;
          end;
        1:begin //black -> blue -> red
            HSL.H := (1 - Normed[Y,X]) * 240;
            HSL.S := 100;
            HSL.L := Normed[Y,X] * 50;
          end;
        2:begin //white -> blue -> red
            HSL.H := (1 - Normed[Y,X]) * 240;
            HSL.S := 100;
            HSL.L := 100 - Normed[Y,X] * 50;
          end;
        3:begin //Light (to white)
            HSL.L := (1 - Normed[Y,X]) * 100;
          end;
        4:begin //Light (to black)
            HSL.L := Normed[Y,X] * 100;
          end;
        else
          begin //Custom black to hue to white
            HSL.H := ColorMapID;
            HSL.S := 100;
            HSL.L := Normed[Y,X] * 100;
          end;
      end;

      FData[Y*FWidth+X] := HSL.ToColor.ToBGRA();
    end;
end;

function TSimbaImage.SaveToString: String;
begin
  Result := SimbaImage_ToString(Self);
end;

procedure TSimbaImage.FromString(Str: String);
begin
  SimbaImage_FromString(Self, Str);
end;

procedure TSimbaImage.FromStream(Stream: TStream; FileName: String);
var
  ReaderClass: TFPCustomImageReaderClass;
begin
  ReaderClass := TFPCustomImage.FindReaderFromFileName(FileName);
  if (ReaderClass = nil) then
    SimbaException('TSimbaImage.LoadFromStream: Unknown image format "%s"', [FileName]);

  SimbaImage_FromFPImageReader(Self, ReaderClass, Stream);
end;

procedure TSimbaImage.FromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);
var
  Y: Integer;
begin
  SetSize(AWidth, AHeight);
  if (AData = nil) then
    Exit;

  if (ADataWidth <> AWidth) then
    for Y := 0 to AHeight - 1 do
      Move(AData[Y * ADataWidth], FData[Y * FWidth], FWidth * SizeOf(TColorBGRA))
  else
    Move(AData^, FData^, FWidth * FHeight * SizeOf(TColorBGRA));
end;

// TM_CCOEFF_NORMED
// Author: slackydev
function TSimbaImage.Compare(Other: TSimbaImage): Single;
var
  invSize, sigmaR, sigmaG, sigmaB, isum, tsum: Single;
  x, y, W, H: Integer;
  tcR, tcG, tcB, icR, icG, icB: TSingleMatrix;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.Compare: Both images must be equal dimensions');

  invSize := 1 / (FWidth * FHeight);

  // compute T' for template
  tcR.SetSize(FWidth, FHeight);
  tcG.SetSize(FWidth, FHeight);
  tcB.SetSize(FWidth, FHeight);

  sigmaR := 0;
  sigmaG := 0;
  sigmaB := 0;

  W := FWidth - 1;
  H := FHeight - 1;
  for y:=0 to H do
    for x:=0 to W do
      with Other.Data[y*FWidth+x] do
      begin
        sigmaR += R;
        sigmaG += G;
        sigmaB += B;
      end;

  for y:=0 to H do
    for x:=0 to W do
      with Other.Data[y*FWidth+x] do
      begin
        tcR[y,x] := R - invSize * sigmaR;
        tcG[y,x] := G - invSize * sigmaG;
        tcB[y,x] := B - invSize * sigmaB;
      end;

  // compute I' for image
  icR.SetSize(Width, Height);
  icG.SetSize(Width, Height);
  icB.SetSize(Width, Height);

  sigmaR := 0;
  sigmaG := 0;
  sigmaB := 0;

  for y:=0 to H do
    for x:=0 to W do
      with FData[y*Width+x] do
      begin
        sigmaR += R;
        sigmaG += G;
        sigmaB += B;
      end;

  for y:=0 to H do
    for x:=0 to W do
      with FData[y*Width+x] do
      begin
        icR[y,x] := R - invsize * sigmaR;
        icG[y,x] := G - invsize * sigmaG;
        icB[y,x] := B - invsize * sigmaB;
      end;

  // ccoeff
  Result := 0;
  for y:=0 to H do
    for x:=0 to W do
      Result += ((icR[y,x] * tcR[y,x]) + (icG[y,x] * tcG[y,x]) + (icB[y,x] * tcB[y,x]));

  isum := 0;
  tsum := 0;
  for y:=0 to H do
    for x:=0 to W do
    begin
      isum += Sqr(icR[y,x]) + Sqr(icG[y,x]) + Sqr(icB[y,x]);
      tsum += Sqr(tcR[y,x]) + Sqr(tcG[y,x]) + Sqr(tcB[y,x]);
    end;
  Result := Result / Sqrt(isum * tsum);
end;

function TSimbaImage.PixelDifference(Other: TSimbaImage): Integer;
var
  I: Integer;
  P1, P2: PColorBGRA;
begin
  Result := 0;
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.PixelDifference: Both images must be equal dimensions');

  P1 := Data;
  P2 := Other.Data;
  for I := 0 to FWidth * FHeight - 1 do
  begin
    if not P1^.EqualsIgnoreAlpha(P2^) then
      Inc(Result);

    Inc(P1);
    Inc(P2);
  end;
end;

function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Single): Integer;
var
  I: Integer;
  P1, P2: PColorBGRA;
begin
  Result := 0;
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.PixelDifference: Both images must be equal dimensions');
  if (Tolerance = 0) then
    Exit(PixelDifference(Other));

  Tolerance := Sqr(Tolerance);

  P1 := Data;
  P2 := Other.Data;
  for I := 0 to FWidth * FHeight - 1 do
  begin
    if (not SimilarColors(TColor(P1^.R or P1^.G shl G_BIT or P1^.B shl B_BIT), TColor(P2^.R or P2^.G shl G_BIT or P2^.B shl B_BIT), Tolerance)) then
      Inc(Result);

    Inc(P1);
    Inc(P2);
  end;
end;

function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage): TPointArray;
var
  X, Y, W, H: Integer;
  P1, P2: PColorBGRA;
  Buffer: TSimbaPointBuffer;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.PixelDifferenceTPA: Both images must be equal dimensions');

  W := FWidth - 1;
  H := FHeight - 1;

  P1 := Data;
  P2 := Other.Data;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (not P1^.EqualsIgnoreAlpha(P2^)) then
        {%H-}Buffer.Add(X, Y);

      Inc(P1);
      Inc(P2);
    end;

  Result := Buffer.ToArray(False);
end;

function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Single): TPointArray;
var
  X, Y, W, H: Integer;
  P1, P2: PColorBGRA;
  Buffer: TSimbaPointBuffer;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.PixelDifferenceTPA: Both images must be equal dimensions');
  if (Tolerance = 0) then
    Exit(PixelDifferenceTPA(Other));
  Tolerance := Sqr(Tolerance);

  W := FWidth - 1;
  H := FHeight - 1;

  P1 := Data;
  P2 := Other.Data;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (not SimilarColors(TColor(P1^.R or P1^.G shl G_BIT or P1^.B shl B_BIT), TColor(P2^.R or P2^.G shl G_BIT or P2^.B shl B_BIT), Tolerance)) then
        Buffer.Add(X, Y);

      Inc(P1);
      Inc(P2);
    end;

  Result := Buffer.ToArray(False);
end;

procedure TSimbaImage.FromLazBitmap(LazBitmap: TBitmap);
var
  TempBitmap: TSimbaImage;
begin
  SetSize(0, 0);

  TempBitmap := LazImage_ToSimbaImage(LazBitmap);
  TempBitmap.FDataOwner := False;

  FData := TempBitmap.Data;
  FWidth := TempBitmap.Width;
  FHeight := TempBitmap.Height;

  TempBitmap.Free();
end;

procedure TSimbaImage.DrawTPA(TPA: TPointArray; Color: TColor; Alpha: Byte);
begin
  if (Alpha = 0) then
    _DrawTPA(TPA, Color)
  else
    _DrawTPAAlpha(TPA, Color, Alpha);
end;

procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray; Color: TColor; Alpha: Byte);
var
  I: Integer;
begin
  for I := 0 to High(ATPA) do
    DrawTPA(ATPA[I], GetDistinctColor(Color, I), Alpha);
end;

procedure TSimbaImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor; Alpha: Byte);
begin
  Size := Max(1, Size);

  with ACenter do
  begin
    Self.DrawLine(Point(X - Size, Y), Point(X + Size, Y), Color, Alpha);
    Self.DrawLine(Point(X, Y - Size), Point(X, Y + Size), Color, Alpha);
  end;
end;

procedure TSimbaImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  Radius := Max(1, Round(Radius/2*Sqrt(2)));

  with ACenter do
  begin
    Self.DrawLine(Point(X - Radius, Y - Radius), Point(X + Radius, Y + Radius), Color, Alpha);
    Self.DrawLine(Point(X + Radius, Y - Radius), Point(X - Radius, Y + Radius), Color, Alpha);
  end;
end;

procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor; Alpha: Byte);
begin
  if (Alpha > 0) then
    _DrawLineAlpha(Start, Stop, Color, Alpha)
  else
    _DrawLine(Start, Stop, Color);
end;

procedure TSimbaImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte);
begin
  if (Alpha > 0) then
    _DrawLineGapAlpha(Start, Stop, GapSize, Color, Alpha)
  else
    _DrawLineGap(Start, Stop, GapSize, Color);
end;

procedure TSimbaImage.DrawPolygon(Points: TPointArray; Color: TColor; Alpha: Byte);
begin
  if (Length(Points) < 3) then
    Exit;

  Self.DrawTPA(Points.Connect(), Color, Alpha);
end;

procedure TSimbaImage.DrawPolygonFilled(Points: TPointArray; Color: TColor; Alpha: Byte);
begin
  if (Length(Points) < 3) then
    Exit;

  if (Alpha > 0) then
    _DrawPolygonFilledAlpha(Points, Color, Alpha)
  else
    _DrawPolygonFilled(Points, Color);
end;

procedure TSimbaImage.DrawPolygonInverted(Points: TPointArray; Color: TColor; Alpha: Byte);
var
  B: TBox;
  BGRA: TColorBGRA;
  X, Y: Integer;
begin
  if (Length(Points) < 3) then
    Exit;

  B := Points.Bounds().Clip(TBox.Create(0, 0, FWidth-1, FHeight-1));

  Self.DrawBoxInverted(B, Color, Alpha);

  case (Alpha > 0) of
    True:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := Alpha;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInPolygon(X, Y, Points) then
              BlendPixel(@FData[Y*FWidth+X], BGRA);
      end;

    False:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := ALPHA_OPAQUE;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInPolygon(X, Y, Points) then
              FData[Y*FWidth+X] := BGRA;
      end;
  end;
end;

procedure TSimbaImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      FData[Y*FWidth+X] := BGRA;
  end;

  {$i shapebuilder_circle.inc}

begin
  if (Radius < 1) then
    Exit;
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  _BuildCircle(ACenter.X, ACenter.Y, Radius);
end;

procedure TSimbaImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
begin
  if (Radius < 1) then
    Exit;

  if (Alpha > 0) then
    _DrawCircleFilledAlpha(ACenter, Radius, Color, Alpha)
  else
    _DrawCircleFilled(ACenter, Radius, Color);
end;

procedure TSimbaImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
var
  X, Y: Integer;
  B: TBox;
  BGRA: TColorBGRA;
begin
  B.X1 := Max(ACenter.X-Radius, 0);
  B.Y1 := Max(ACenter.Y-Radius, 0);
  B.X2 := Min(ACenter.X+Radius, FWidth-1);
  B.Y2 := Min(ACenter.Y+Radius, FHeight-1);

  Self.DrawBoxInverted(B, Color, Alpha);

  case (Alpha > 0) of
    True:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := Alpha;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInCircle(X, Y, ACenter.X, ACenter.Y, Radius) then
              BlendPixel(@FData[Y*FWidth+X], BGRA);
      end;

    False:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := ALPHA_OPAQUE;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInCircle(X, Y, ACenter.X, ACenter.Y, Radius) then
              FData[Y*FWidth+X] := BGRA;
      end;
  end;
end;

procedure TSimbaImage.DrawBox(Box: TBox; Color: TColor; Alpha: Byte = 0);
begin
  if (Alpha > 0) then
    _DrawBoxEdgeAlpha(Box, Color, Alpha)
  else
    _DrawBoxEdge(Box, Color);
end;

procedure TSimbaImage.DrawBoxFilled(Box: TBox; Color: TColor; Alpha: Byte);
begin
  if (Alpha > 0) then
    _DrawBoxFilledAlpha(Box, Color, Alpha)
  else
    _DrawBoxFilled(Box, Color);
end;

procedure TSimbaImage.DrawBoxInverted(B: TBox; Color: TColor; Alpha: Byte);
begin
  Self.DrawBoxFilled(TBox.Create(0,        0,        B.X1 - 1, B.Y1 - 1   ), Color, Alpha); //Top Left
  Self.DrawBoxFilled(TBox.Create(0,        B.Y1,     B.X1 - 1, B.Y2       ), Color, Alpha); //Mid Left
  Self.DrawBoxFilled(TBox.Create(0,        B.Y2 + 1, B.X1 - 1, FHeight - 1), Color, Alpha); //Btm Left
  Self.DrawBoxFilled(TBox.Create(B.X1,     0,        B.X2,     B.Y1 - 1   ), Color, Alpha); //Top Mid
  Self.DrawBoxFilled(TBox.Create(B.X1,     B.Y2 + 1, B.X2,     FHeight - 1), Color, Alpha); //Btm Mid
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, 0,        FWidth-1, B.Y1 - 1   ), Color, Alpha); //Top Right
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, B.Y1,     FWidth-1, B.Y2       ), Color, Alpha); //Mid Right
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, B.Y2 + 1, FWidth-1, FHeight - 1), Color, Alpha); //Btm Right
end;

procedure TSimbaImage.DrawQuad(Quad: TQuad; Color: TColor; Alpha: Byte);
begin
  DrawLine(Quad.Top, Quad.Right, Color, Alpha);
  DrawLine(Quad.Right, Quad.Bottom, Color, Alpha);
  DrawLine(Quad.Bottom, Quad.Left, Color, Alpha);
  DrawLine(Quad.Left, Quad.Top, Color, Alpha);
end;

procedure TSimbaImage.DrawQuadFilled(Quad: TQuad; Color: TColor; Alpha: Byte);
begin
  DrawPolygonFilled([Quad.Top, Quad.Right, Quad.Bottom, Quad.Left], Color, Alpha);
end;

procedure TSimbaImage.DrawQuadInverted(Quad: TQuad; Color: TColor; Alpha: Byte);
var
  X, Y: Integer;
  B: TBox;
  BGRA: TColorBGRA;
begin
  B := Quad.Bounds.Clip(TBox.Create(0, 0, FWidth-1, FHeight-1));

  Self.DrawBoxInverted(B, Color, Alpha);

  case (Alpha > 0) of
    True:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := Alpha;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInQuad(X, Y, Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
              BlendPixel(@FData[Y*FWidth+X], BGRA);
      end;

    False:
      begin
        BGRA := Color.ToBGRA();
        BGRA.A := ALPHA_OPAQUE;

        for X := B.X1 to B.X2 do
          for Y := B.Y1 to B.Y2 do
            if not TSimbaGeometry.PointInQuad(X, Y, Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
              FData[Y*FWidth+X] := BGRA;
      end;
  end;
end;

procedure TSimbaImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Quads) do
    if Filled then
      DrawQuadFilled(Quads[I], GetDistinctColor(Color, I))
    else
      DrawQuad(Quads[I], GetDistinctColor(Color, I));
end;

procedure TSimbaImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Boxes) do
    if Filled then
      DrawBoxFilled(Boxes[I], GetDistinctColor(Color, I))
    else
      DrawBox(Boxes[I], GetDistinctColor(Color, I));
end;

procedure TSimbaImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Polygons) do
    if Filled then
      DrawPolygonFilled(Polygons[I], GetDistinctColor(Color, I))
    else
      DrawPolygon(Polygons[I], GetDistinctColor(Color, I));
end;

procedure TSimbaImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Centers) do
    if Filled then
      DrawCircleFilled(Centers[I], Radius, GetDistinctColor(Color, I))
    else
      DrawCircle(Centers[I], Radius, GetDistinctColor(Color, I));
end;

procedure TSimbaImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor);
var
  I: Integer;
begin
  for I := 0 to High(Points) do
    DrawCross(Points[I], Radius, GetDistinctColor(Color, I));
end;

procedure TSimbaImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
var
  HSL: TColorHSL;
  X, Y: Integer;
  Bounds: TBox;
begin
  Bounds.X1 := Max(ACenter.X - Radius, 0);
  Bounds.Y1 := Max(ACenter.Y - Radius, 0);
  Bounds.X2 := Min(ACenter.X + Radius, FWidth - 1);
  Bounds.Y2 := Min(ACenter.Y + Radius, FHeight - 1);

  for Y := Bounds.Y1 to Bounds.Y2 do
    for X := Bounds.X1 to Bounds.X2 do
    begin
      HSL.H := RadToDeg(ArcTan2(Y - ACenter.Y, X - ACenter.X));
      HSL.S := Hypot(ACenter.X - X, ACenter.Y - Y) / Radius * 100;
      HSL.L := 50;
      if (HSL.S < 100) then
        SetPixel(X, Y, HSL.ToColor());
    end;
end;

procedure TSimbaImage.Fill(Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;
  Ptr: PColorBGRA;
  Upper: PtrUInt;
begin
  BGRA := Color.ToBGRA();
  BGRA.A := ALPHA_OPAQUE;

  if (Alpha > 0) then
  begin
    BGRA.A := Alpha;

    Upper := PtrUInt(@FData[FWidth * FHeight]);
    Ptr := FData;
    while (PtrUInt(Ptr) < Upper) do
    begin
      BlendPixel(Ptr, BGRA);

      Inc(Ptr);
    end;
  end else
    FillData(FData, FWidth * FHeight, BGRA);
end;

procedure TSimbaImage.Clear;
begin
  if (FWidth * FHeight > 0) then
    FillData(FData, FWidth * FHeight, DefaultPixel);
end;

procedure TSimbaImage.Clear(Box: TBox);

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@FData[Y * FWidth + X1], Box.Width, DefaultPixel);
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));
  if (Box.Width > 1) and (Box.Height > 1) then
    _BuildBoxFilled(Box);
end;

procedure TSimbaImage.ClearInverted(Box: TBox);
begin
  Self.Clear(TBox.Create(0,          0,          Box.X1 - 1, Box.Y1 - 1 )); //Top Left
  Self.Clear(TBox.Create(0,          Box.Y1,     Box.X1 - 1, Box.Y2     )); //Mid Left
  Self.Clear(TBox.Create(0,          Box.Y2 + 1, Box.X1 - 1, FHeight - 1)); //Btm Left
  Self.Clear(TBox.Create(Box.X1,     0,          Box.X2,     Box.Y1 - 1 )); //Top Mid
  Self.Clear(TBox.Create(Box.X1,     Box.Y2 + 1, Box.X2,     FHeight - 1)); //Btm Mid
  Self.Clear(TBox.Create(Box.X2 + 1, 0,          FWidth-1,   Box.Y1 - 1 )); //Top Right
  Self.Clear(TBox.Create(Box.X2 + 1, Box.Y1,     FWidth-1,   Box.Y2     )); //Mid Right
  Self.Clear(TBox.Create(Box.X2 + 1, Box.Y2 + 1, FWidth-1,   FHeight - 1)); //Btm Right
end;

procedure TSimbaImage.SplitChannels(var B,G,R,A: TByteArray);
var
  Upper: PtrUInt;
  Src: PColorBGRA;
  DestB, DestG, DestR, DestA: PByte;
begin
  SetLength(B, FWidth*FHeight);
  SetLength(G, FWidth*FHeight);
  SetLength(R, FWidth*FHeight);
  SetLength(A, FWidth*FHeight);

  DestB := @B[0];
  DestG := @G[0];
  DestR := @R[0];
  DestA := @A[0];

  Src := FData;
  Upper := PtrUInt(FData) + FDataSize;
  while (PtrUInt(Src) < Upper) do
  begin
    DestB^ := Src^.B;
    DestG^ := Src^.G;
    DestR^ := Src^.R;
    DestA^ := Src^.A;

    Inc(Src);
    Inc(DestB);
    Inc(DestG);
    Inc(DestR);
    Inc(DestA);
  end;
end;

procedure TSimbaImage.SplitChannels(var B,G,R: TByteArray);
var
  Upper: PtrUInt;
  Src: PColorBGRA;
  DestB, DestG, DestR: PByte;
begin
  SetLength(B, FWidth*FHeight);
  SetLength(G, FWidth*FHeight);
  SetLength(R, FWidth*FHeight);

  DestB := @B[0];
  DestG := @G[0];
  DestR := @R[0];

  Src := FData;
  Upper := PtrUInt(FData) + FDataSize;
  while (PtrUInt(Src) < Upper) do
  begin
    DestB^ := Src^.B;
    DestG^ := Src^.G;
    DestR^ := Src^.R;

    Inc(Src);
    Inc(DestB);
    Inc(DestG);
    Inc(DestR);
  end;
end;

procedure TSimbaImage.DrawImage(Image: TSimbaImage; Location: TPoint; Alpha: Byte = 0);
begin
  if (Alpha = 0) then
    _DrawImage(Image, Location)
  else
    _DrawImageAlpha(Image, Location, Alpha);
end;

function TSimbaImage.GetColors: TColorArray;
var
  I: Integer;
begin
  SetLength(Result, FHeight * FWidth);
  for I := 0 to High(Result) do
    with FData[I] do
      Result[I] := TColor(R or G shl G_BIT or B shl B_BIT);
end;

function TSimbaImage.Equals(Other: TObject): Boolean;
begin
  if (Other is TSimbaImage) then
    Result := Equals(TSimbaImage(Other))
  else
    Result := inherited Equals(Other);
end;

// Compare without alpha
function TSimbaImage.Equals(Other: TSimbaImage): Boolean;
var
  I: Integer;
  Ptr, OtherPtr: PColorBGRA;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    Exit(False);

  Ptr := Data;
  OtherPtr := Other.Data;

  for I := 0 to FWidth * FHeight - 1 do
  begin
    if not Ptr^.EqualsIgnoreAlpha(OtherPtr^) then
      Exit(False);

    Inc(Ptr);
    Inc(OtherPtr);
  end;

  Result := True;
end;

class function TSimbaImage.LoadFonts(Dir: String): Boolean;
begin
  Result := SimbaFreeTypeFontLoader.LoadFonts(Dir);
end;

procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor);
var
  Old, New: TColorBGRA;
  Ptr: PColorBGRA;
  Upper: PtrUInt;
begin
  Old := OldColor.ToBGRA();
  New := NewColor.ToBGRA();

  Upper := PtrUInt(@FData[FWidth * FHeight]);
  Ptr := FData;

  while (PtrUInt(Ptr) < Upper) do
  begin
    if Ptr^.EqualsIgnoreAlpha(Old) then
      Ptr^ := New;

    Inc(Ptr);
  end;
end;

procedure TSimbaImage.ReplaceColors(OldColors, NewColors: TColorArray);
var
  I, H: Integer;
  Arr: array of record
    Old, New: TColorBGRA;
  end;
  Ptr: PColorBGRA;
  Upper: PtrUInt;
begin
  if (Length(OldColors) <> Length(NewColors)) then
    SimbaException('TSimbaImage.ReplaceColors: Color arrays must be same lengths (%d, %s)', [Length(OldColors), Length(NewColors)]);

  SetLength(Arr, Length(OldColors));
  for I := 0 to High(Arr) do
  begin
    Arr[I].Old := OldColors[I].ToBGRA();
    Arr[I].New := NewColors[I].ToBGRA();
  end;

  H := High(Arr);
  Upper := PtrUInt(@FData[FWidth * FHeight]);
  Ptr := FData;

  while (PtrUInt(Ptr) < Upper) do
  begin
    for I := 0 to H do
      if Ptr^.EqualsIgnoreAlpha(Arr[I].Old) then
        Ptr^ := Arr[I].New;

    Inc(Ptr);
  end;
end;

function TSimbaImage.RotateNN(Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;

  procedure RotateNoExpand;
  var
    X, Y, OldX, OldY, W, H: Integer;
    MidX, MidY: Single;
  begin
    Result.SetSize(FWidth, FHeight);

    MidX := (FWidth - 1) / 2;
    MidY := (FHeight - 1) / 2;

    W := FWidth - 1;
    H := FHeight - 1;
    for Y := 0 to H do
      for X := 0 to W do
      begin
        OldX := Round(MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := Round(MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));
        if (OldX >= 0) and (OldX < FWidth) and (OldY >= 0) and (OldY < FHeight) then
          Result.FData[Y * FWidth + X] := FData[OldY * FWidth + OldX];
      end;
  end;

  procedure RotateExpand;
  var
    X, Y, OldX, OldY, NewWidth, NewHeight: Integer;
    MidX, MidY: Single;
    NewBounds: TBox;
  begin
    MidX := (FWidth - 1) / 2;
    MidY := (FHeight - 1) / 2;

    NewBounds := GetRotatedSize(FWidth, FHeight, Radians);

    NewWidth := NewBounds.Width-1;
    NewHeight := NewBounds.Height-1;

    Result.SetSize(NewWidth, NewHeight);

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
      for X := 0 to NewWidth do
      begin
        OldX := Round(MidX + CosAngle * (NewBounds.X1+X - MidX) - SinAngle * (NewBounds.Y1+Y - MidY));
        OldY := Round(MidY + SinAngle * (NewBounds.X1+X - MidX) + CosAngle * (NewBounds.Y1+Y - MidY));
        if (OldX >= 0) and (OldX < FWidth) and (OldY >= 0) and (OldY < FHeight) then
          Result.FData[Y * Result.FWidth + X] := FData[OldY * FWidth + OldX];
      end;
  end;

begin
  Result := TSimbaImage.Create();

  SinCos(Radians, SinAngle, CosAngle);

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

function TSimbaImage.RotateBilinear(Radians: Single; Expand: Boolean): TSimbaImage;
var
  CosAngle, SinAngle: Single;

  function ShouldRotate(var p0, p1, p2, p3: TColorBGRA): Boolean; inline;
  begin
    if (p0.A > 0) or (p1.A > 0) or (p2.A > 0) or (p3.A > 0) then
    begin
      if (p0.A = 0) then
      begin
        if (p1.A > 0) then p0 := p1 else
        if (p2.A > 0) then p0 := p2 else
        if (p3.A > 0) then p0 := p3;
      end;
      if (p1.A = 0) then
      begin
        if (p0.A > 0) then p1 := p0 else
        if (p2.A > 0) then p1 := p2 else
        if (p3.A > 0) then p1 := p3;
      end;
      if (p2.A = 0) then
      begin
        if (p0.A > 0) then p2 := p0 else
        if (p1.A > 0) then p2 := p1 else
        if (p3.A > 0) then p2 := p3;
      end;
      if (p3.A = 0) then
      begin
        if (p0.A > 0) then p3 := p0 else
        if (p1.A > 0) then p3 := p1 else
        if (p2.A > 0) then p3 := p2;
      end;

      Result := True;
    end else
      Result := False;
  end;

  procedure RotateNoExpand;
  var
    x, y, w, h: Integer;
    OldX, OldY: Single;
    dX, dY, dxMinus1, dyMinus1: Single;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    Result.SetSize(FWidth, FHeight);
    Result.SetAlpha(0);

    MidX := (FWidth - 1) / 2;
    MidY := (FHeight - 1) / 2;

    W := FWidth - 1;
    H := FHeight - 1;
    for Y := 0 to H do
      for X := 0 to W do
      begin
        OldX := (MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := (MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));

        fX := Trunc(OldX);
        fY := Trunc(OldY);
        cX := Ceil(OldX);
        cY := Ceil(OldY);

        if (fX >= 0) and (cX >= 0) and (fX < FWidth) and (cX < FWidth) and
           (fY >= 0) and (cY >= 0) and (fY < FHeight) and (cY < FHeight) then
        begin
          dx := OldX - fX;
          dy := OldY - fY;
          dxMinus1 := 1 - dx;
          dyMinus1 := 1 - dy;

          p0 := FData[fY * FWidth + fX];
          p1 := FData[fY * FWidth + cX];
          p2 := FData[cY * FWidth + fX];
          p3 := FData[cY * FWidth + cX];

          if ShouldRotate(p0, p1, p2, p3) then
          begin
            TopR := dxMinus1 * p0.R + dx * p1.R;
            TopG := dxMinus1 * p0.G + dx * p1.G;
            TopB := dxMinus1 * p0.B + dx * p1.B;
            BtmR := dxMinus1 * p2.R + dx * p3.R;
            BtmG := dxMinus1 * p2.G + dx * p3.G;
            BtmB := dxMinus1 * p2.B + dx * p3.B;

            with Result.Data[Y * Result.Width + X] do
            begin
              R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
              G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
              B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
              A := ALPHA_OPAQUE;
            end;
          end;
        end;
      end;
  end;

  procedure RotateExpand;
  var
    NewWidth, NewHeight, X, Y: Integer;
    NewBounds: TBox;
    OldX, OldY: Single;
    dX, dY, dxMinus1, dyMinus1: Single;
    p0, p1, p2, p3: TColorBGRA;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    NewBounds := GetRotatedSize(FWidth, FHeight, Radians);
    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;
    MidX := (NewWidth - 1) / 2;
    MidY := (NewHeight - 1) / 2;

    Result.SetSize(NewWidth, NewHeight);
	  Result.SetAlpha(0);

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
      for X := 0 to NewWidth do
      begin
        OldX := (MidX + CosAngle * (X - MidX) - SinAngle * (Y - MidY));
        OldY := (MidY + SinAngle * (X - MidX) + CosAngle * (Y - MidY));

        fX := Trunc(OldX) + NewBounds.X1;
        fY := Trunc(OldY) + NewBounds.Y1;
        cX := Ceil(OldX)  + NewBounds.X1;
        cY := Ceil(OldY)  + NewBounds.Y1;

        if (fX >= 0) and (cX >= 0) and (fX < FWidth) and (cX < FWidth) and
           (fY >= 0) and (cY >= 0) and (fY < FHeight) and (cY < FHeight) then
        begin
          dx := OldX - (fX - NewBounds.X1);
          dy := OldY - (fY - NewBounds.Y1);
          dxMinus1 := 1 - dx;
          dyMinus1 := 1 - dy;

          p0 := FData[fY * FWidth + fX];
          p1 := FData[fY * FWidth + cX];
          p2 := FData[cY * FWidth + fX];
          p3 := FData[cY * FWidth + cX];

          if ShouldRotate(p0, p1, p2, p3) then
          begin
            TopR := dxMinus1 * p0.R + dx * p1.R;
            TopG := dxMinus1 * p0.G + dx * p1.G;
            TopB := dxMinus1 * p0.B + dx * p1.B;
            BtmR := dxMinus1 * p2.R + dx * p3.R;
            BtmG := dxMinus1 * p2.G + dx * p3.G;
            BtmB := dxMinus1 * p2.B + dx * p3.B;

            with Result.Data[Y * Result.Width + X] do
            begin
              R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
              G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
              B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
              A := ALPHA_OPAQUE;
            end;
          end;
        end;
      end;
  end;

begin
  Result := TSimbaImage.Create();

  SinCos(Radians, SinAngle, CosAngle);

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

function TSimbaImage.GreyScale: TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
  Lum: Byte;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  Src := FData;
  Dest := Result.Data;

  for i := (FHeight * FWidth - 1) downto 0 do
  begin
    Dest^.A := Src^.A;
    if (Dest^.A > 0) then
    begin
      Lum := Round(Src^.R * 0.3 + Src^.G * 0.59 + Src^.B * 0.11);

      Dest^.R := Lum;
      Dest^.G := Lum;
      Dest^.B := Lum;
    end;

    Inc(Src);
    Inc(Dest);
  end;
end;

function TSimbaImage.Brightness(Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  Src := FData;
  Dest := Result.Data;

  for I := (FHeight * FWidth - 1) downto 0 do
  begin
    Dest^.A := Src^.A;
    if (Dest^.A > 0) then
    begin
      Dest^.R := EnsureRange(Src^.R + Value, 0, 255);
      Dest^.G := EnsureRange(Src^.G + Value, 0, 255);
      Dest^.B := EnsureRange(Src^.B + Value, 0, 255);
    end;

    Inc(Src);
    Inc(Dest);
  end;
end;

function TSimbaImage.Invert: TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  Src := FData;
  Dest := Result.Data;

  for I := (FHeight * FWidth - 1) downto 0 do
  begin
    Dest^.A := Src^.A;
    if (Dest^.A > 0) then
    begin
      Dest^.R := not Src^.R;
      Dest^.G := not Src^.G;
      Dest^.B := not Src^.B;
    end;

    Inc(Src);
    Inc(Dest);
  end;
end;

function TSimbaImage.Posterize(Value: Integer): TSimbaImage;
var
  I: Integer;
  Src, Dest: PColorBGRA;
begin
  if not InRange(Value, 1, 255) then
    SimbaException('TSimbaImage.Posterize: Value(%d) out of range[1..255]', [Value]);

  Result := TSimbaImage.Create(FWidth, FHeight);

  Src := FData;
  Dest := Result.Data;

  for I := (FHeight * FWidth - 1) downto 0 do
  begin
    Dest^.A := Src^.A;
    if (Dest^.A > 0) then
    begin
      Dest^.R := Min(Round(Src^.R / Value) * Value, 255);
      Dest^.G := Min(Round(Src^.G / Value) * Value, 255);
      Dest^.B := Min(Round(Src^.B / Value) * Value, 255);
    end;

    Inc(Src);
    Inc(Dest);
  end;
end;

function TSimbaImage.BoxBlur(Radius: Integer): TSimbaImage;
var
  X, Y, W, H: Integer;
  XX, YY: Integer;
  Size: Integer;
  B: TBox;
  Sum: record
    R,G,B: UInt64;
  end;
  Ptr: PColorBGRA;
begin
  Result := Copy();
  if (Radius <= 1) or (not Odd(Radius)) then
    SimbaException('TSimbaImage.BlockBlur: Radius(%d) must be odd (1,3,5 etc).', [Radius]);

  Radius := Radius div 2;

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (FData[Y * FWidth + X].A > ALPHA_TRANSPARENT) then
      begin
        B.X1 := Max(X-Radius, 0);
        B.Y1 := Max(Y-Radius, 0);
        B.X2 := Min(X+Radius, W);
        B.Y2 := Min(Y+Radius, H);

        Size := 0;
        Sum.R := 0; Sum.G := 0; Sum.B := 0;

        for YY := B.Y1 to B.Y2 - 1 do
          for XX := B.X1 to B.X2 - 1 do
          begin
            Ptr := @FData[YY * FWidth + XX];
            if (Ptr^.A > 0) then
            begin
              Sum.R += Ptr^.R;
              Sum.G += Ptr^.G;
              Sum.B += Ptr^.B;

              Size += 1;
            end;
          end;

        Ptr := @Result.Data[Y * FWidth + X];
        Ptr^.R := Sum.R div Size;
        Ptr^.G := Sum.G div Size;
        Ptr^.B := Sum.B div Size;
      end;
    end;
end;

function TSimbaImage.GaussBlur(Radius: Double): TSimbaImage;
var
  B,G,R,A: TByteArray;
  Ignore: TBooleanArray;
  I: Integer;
  Ptr: PColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);
  if (FWidth * FHeight = 0) then
    Exit;

  SplitChannels(B,G,R,A);

  SetLength(Ignore, Length(A));
  for I := 0 to High(Ignore) do
    Ignore[I] := (A[I] = ALPHA_TRANSPARENT);

  imgGaussBlur(Radius, R,G,B, Ignore, FWidth, FHeight);

  Ptr := Result.Data;
  for i := 0 to (FWidth * FHeight) - 1 do
  begin
    Ptr^.A := A[I];
    if (Ptr^.A > ALPHA_TRANSPARENT) then
    begin
      Ptr^.B := B[I];
      Ptr^.G := G[I];
      Ptr^.R := R[I];
    end;

    Inc(Ptr);
  end;
end;

function TSimbaImage.Convolute(Matrix: TDoubleMatrix): TSimbaImage;
var
  X, Y, YY, XX, CX, CY: Integer;
  SrcRows, DestRows: TSimbaImageLineStarts;
  MatWidth, MatHeight, MidX, MidY: Integer;
  NewR, NewG, NewB: Double;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  SrcRows := LineStarts;
  DestRows := Result.LineStarts;

  if Matrix.GetSize(MatWidth, MatHeight) then
  begin
    MidX := MatWidth div 2;
    MidY := MatHeight div 2;

    MatWidth -= 1;
    MatHeight -= 1;

    for Y := 0 to FHeight - 1 do
      for X := 0 to FWidth - 1 do
      begin
        NewR := 0;
        NewG := 0;
        NewB := 0;

        for YY := 0 to MatHeight do
          for XX := 0 to MatWidth do
          begin
            CX := EnsureRange(X+XX-MidX, 0, FWidth-1);
            CY := EnsureRange(Y+YY-MidY, 0, FHeight-1);

            NewR += (Matrix[YY, XX] * SrcRows[CY, CX].R);
            NewG += (Matrix[YY, XX] * SrcRows[CY, CX].G);
            NewB += (Matrix[YY, XX] * SrcRows[CY, CX].B);
          end;

        DestRows[Y, X].R := Round(NewR);
        DestRows[Y, X].G := Round(NewG);
        DestRows[Y, X].B := Round(NewB);
      end;
  end;
end;

function TSimbaImage.Mirror(Style: ESimbaImageMirrorStyle): TSimbaImage;
var
  X, Y: Integer;
begin
  case Style of
    ESimbaImageMirrorStyle.WIDTH:
      begin
        Result := TSimbaImage.Create(FWidth, FHeight);

        for Y := FHeight - 1 downto 0 do
          for X := FWidth - 1 downto 0 do
            Result.FData[Y*FWidth+X] := FData[Y*FWidth+FWidth-1-X];
      end;

    ESimbaImageMirrorStyle.HEIGHT:
      begin
        Result := TSimbaImage.Create(FWidth, FHeight);

        for Y := FHeight - 1 downto 0 do
          Move(FData[Y*FWidth], Result.FData[(FHeight-1-Y) * FWidth], FWidth * SizeOf(TColorBGRA));
      end;

    ESimbaImageMirrorStyle.LINE:
      begin
        Result := TSimbaImage.Create(FHeight, FWidth);

        for Y := FHeight - 1 downto 0 do
          for X := FHeight - 1 downto 0 do
            Result.FData[X*FHeight+Y] := FData[Y*FWidth+X];
      end;

    else
      Result := nil;
  end;
end;

function TSimbaImage.Blend(Points: TPointArray; Radius: Integer): TSimbaImage;
var
  P: TPoint;
  X, Y, Count: Integer;
  Area: TBox;
  R, G, B: Int64;
  Color: TColorBGRA;
begin
  Result := Self.Copy();
  for P in Points do
    Result.Data[P.Y * FWidth + P.X].A := 0;

  for P in Points do
  begin
    Area.X1 := Max(P.X - Radius, 0);
    Area.Y1 := Max(P.Y - Radius, 0);
    Area.X2 := Min(P.X + Radius, FWidth - 1);
    Area.Y2 := Min(P.Y + Radius, FHeight - 1);

    Count := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := Area.X1 to Area.X2 do
      for Y := Area.Y1 to Area.Y2 do
      begin
        Color := Result.Data[Y * FWidth + X];
        if (Color.A = ALPHA_TRANSPARENT) then
          Continue;

        Inc(R, Color.R);
        Inc(G, Color.G);
        Inc(B, Color.B);

        Inc(Count);
      end;

    if (Count > 0) then
    begin
      Color.R := R div Count;
      Color.G := G div Count;
      Color.B := B div Count;
      Color.A := ALPHA_OPAQUE;

      Result.Data[P.Y * FWidth + P.X] := Color;
    end;
  end;
end;

function TSimbaImage.Downsample(Scale: Integer): TSimbaImage;
var
  Area: Double;

  function BlendArea(const X1, Y1, X2, Y2: Integer): TColorBGRA;
  var
    R, G, B: Integer;
    Hit, Miss: Integer;
    X, Y: Integer;
    Color: TColorBGRA;
  begin
    Miss := 0;
    Hit := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := X1 to X2 do
      for Y := Y1 to Y2 do
      begin
        Color := FData[Y * FWidth + X];

        if (Color.A = ALPHA_TRANSPARENT) then
          Inc(Miss)
        else
        begin
          Inc(Hit);

          Inc(R, Color.R);
          Inc(G, Color.G);
          Inc(B, Color.B);
        end;
      end;

    Result.R := Round((R + (R div Hit) * Miss) * Area);
    Result.G := Round((G + (G div Hit) * Miss) * Area);
    Result.B := Round((B + (B div Hit) * Miss) * Area);
    Result.A := ALPHA_OPAQUE;
  end;

var
  X, Y, W, H: Integer;
  OldX, OldY: Integer;
  Color: TColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth div Scale, FHeight div Scale);

  Area := Double(1.0) / Sqr(Scale);

  W := Result.Width - 1;
  H := Result.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := X * Scale;
      OldY := Y * Scale;

      Color := FData[OldY * FWidth + OldX];
      if (Color.A = ALPHA_TRANSPARENT) then
        Continue;

      Result.Data[Y * Result.Width + X] := BlendArea(OldX, OldY, (OldX + Scale) - 1, (OldY + Scale) - 1);
    end;
end;

function TSimbaImage.GetFontAntialiasing: Boolean;
begin
  Result := FTextDrawer.Antialiased;
end;

function TSimbaImage.GetFontName: String;
begin
  Result := FTextDrawer.Font;
end;

class function TSimbaImage.FontNames: TStringArray;
begin
  Result := SimbaFreeTypeFontLoader.FontNames;
end;

function TSimbaImage.GetFontSize: Single;
begin
  Result := FTextDrawer.Size;
end;

function TSimbaImage.GetFontBold: Boolean;
begin
  Result := FTextDrawer.Bold;
end;

function TSimbaImage.GetFontItalic: Boolean;
begin
  Result := FTextDrawer.Italic;
end;

function TSimbaImage.GetLineStart(const Y: Integer): PColorBGRA;
begin
  Result := FLineStarts[Y];
end;

procedure TSimbaImage.SetFontAntialiasing(Value: Boolean);
begin
  FTextDrawer.Antialiased := Value;
end;

procedure TSimbaImage.SetFontName(Value: String);
begin
  FTextDrawer.Font := Value;
end;

procedure TSimbaImage.SetFontSize(Value: Single);
begin
  FTextDrawer.Size := Value;
end;

procedure TSimbaImage.SetFontBold(Value: Boolean);
begin
  FTextDrawer.Bold := Value;
end;

procedure TSimbaImage.SetFontItalic(Value: Boolean);
begin
  FTextDrawer.Italic := Value;
end;

function TSimbaImage.TextWidth(Text: String): Integer;
begin
  Result := FTextDrawer.TextWidth(Text);
end;

function TSimbaImage.TextHeight(Text: String): Integer;
begin
  Result := FTextDrawer.TextHeight(Text);
end;

function TSimbaImage.TextSize(Text: String): TPoint;
begin
  Result := FTextDrawer.TextSize(Text);
end;

procedure TSimbaImage.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Position, Color);
end;

procedure TSimbaImage.DrawText(Text: String; Box: TBox; Alignments: EDrawTextAlignSet; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Box, Alignments, Color);
end;

procedure TSimbaImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
var
  I, LineHeight: Integer;
begin
  LineHeight := TextHeight('TaylorSwift') + 1;

  for I := 0 to High(Text) do
  begin
    DrawText(Text[I], Position, Color);

    Inc(Position.Y, LineHeight);
    if (Position.Y >= FHeight) then
      Break;
  end;
end;

procedure TSimbaImage.SetSize(NewWidth, NewHeight: Integer);
var
  NewData: PColorBGRA;
  I, MinW, MinH: Integer;
begin
  if (not FDataOwner) then
    SimbaException('TSimbaImage.SetSize: Cannot resize a image with external memory');

  if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
  begin
    if (NewWidth * NewHeight <> 0) then
    begin
      NewData := GetMem(NewWidth * NewHeight * SizeOf(TColorBGRA));

      FillData(NewData, NewWidth * NewHeight, DefaultPixel);
    end else
      NewData := nil;

    if Assigned(FData) and Assigned(NewData) and (FWidth * FHeight <> 0) then
    begin
      MinW := Min(NewWidth, FWidth);
      MinH := Min(NewHeight, FHeight);
      for I := 0 to MinH - 1 do
        Move(FData[I * FWidth], NewData[I * NewWidth], MinW * SizeOf(TColorBGRA));
    end;
    if Assigned(FData) then
      FreeMem(FData);

    FData := NewData;
    FDataSize := (NewWidth * NewHeight) * SizeOf(TColorBGRA);
    FWidth := NewWidth;
    FHeight := NewHeight;
    FCenter := TPoint.Create(FWidth div 2, FHeight div 2);

    SetLength(FLineStarts, FHeight);
    for I := 0 to High(FLineStarts) do
      FLineStarts[I] := @FData[FWidth * I];
  end;
end;

procedure TSimbaImage.SetAlpha(Value: Byte);
var
  Upper: PtrUInt;
  Ptr: PColorBGRA;
begin
  Upper := PtrUInt(@FData[FWidth * FHeight]);
  Ptr := FData;

  while (PtrUInt(Ptr) < Upper) do
  begin
    Ptr^.A := Value;

    Inc(Ptr);
  end;
end;

procedure TSimbaImage.SetAlpha(Points: TPointArray; Value: Byte);
var
  P: TPoint;
begin
  for P in Points do
    if (P.X >= 0) and (P.Y >= 0) and (P.X < FWidth) and (P.Y < FHeight) then
      FData[P.Y * FWidth + P.X].A := Value;
end;

procedure TSimbaImage.SetAlpha(Color: TColor; Value: Byte);
var
  BGRA: TColorBGRA;
  X, Y: Integer;
begin
  BGRA := Color.ToBGRA();

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
      if FData[Y * FWidth + X].EqualsIgnoreAlpha(BGRA) then
        FData[Y * FWidth + X].A := Value;
end;

function TSimbaImage.ResizeNN(NewWidth, NewHeight: Integer): TSimbaImage;
var
  X, Y, W, H: Integer;
begin
  Result := TSimbaImage.Create(NewWidth, NewHeight);

  W := NewWidth - 1;
  H := NewHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result.Data[Y * NewWidth + X] := FData[((Y * FHeight) div NewHeight) * FWidth + (X * FWidth) div NewWidth];
end;

function TSimbaImage.ResizeBilinear(NewWidth, NewHeight: Integer): TSimbaImage;

  function ShouldResize(var p0, p1, p2, p3: TColorBGRA): Boolean; inline;
  begin
    if (p0.A > 0) or (p1.A > 0) or (p2.A > 0) or (p3.A > 0) then
    begin
      if (p0.A = 0) then
      begin
        if (p1.A > 0) then p0 := p1 else
        if (p2.A > 0) then p0 := p2 else
        if (p3.A > 0) then p0 := p3;
      end;
      if (p1.A = 0) then
      begin
        if (p0.A > 0) then p1 := p0 else
        if (p2.A > 0) then p1 := p2 else
        if (p3.A > 0) then p1 := p3;
      end;
      if (p2.A = 0) then
      begin
        if (p0.A > 0) then p2 := p0 else
        if (p1.A > 0) then p2 := p1 else
        if (p3.A > 0) then p2 := p3;
      end;
      if (p3.A = 0) then
      begin
        if (p0.A > 0) then p3 := p0 else
        if (p1.A > 0) then p3 := p1 else
        if (p2.A > 0) then p3 := p2;
      end;

      Result := True;
    end else
      Result := False;
  end;

var
  X, Y, OldX, OldY: Integer;
  p0, p1, p2, p3: TColorBGRA;
  RatioX, RatioY, dX, dY: Single;
  Color: TColorBGRA;
  W,H: Integer;
begin
  Result := TSimbaImage.Create(NewWidth, NewHeight);

  Color := DefaultPixel;

  RatioX := (FWidth - 1) / NewWidth;
  RatioY := (FHeight - 1) / NewHeight;

  W := NewWidth - 1;
  H := NewHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := Trunc(RatioX * X);
      OldY := Trunc(RatioY * Y);

      p0 := FLineStarts[OldY,     OldX    ];
      p1 := FLineStarts[OldY,     OldX + 1];
      p2 := FLineStarts[OldY + 1, OldX    ];
      p3 := FLineStarts[OldY + 1, OldX + 1];

      if ShouldResize(p0, p1, p2, p3) then
      begin
        dX := ratioX * X - OldX;
        dY := ratioY * Y - OldY;

        Color.R := Trunc(
          p0.R * (1-dX) * (1-dY) +
          p1.R * (dX * (1-dY)) +
          p2.R * (dY * (1-dX)) +
          p3.R * (dX * dY)
        );

        Color.G := Trunc(
          p0.G * (1-dX) * (1-dY) +
          p1.G * (dX * (1-dY)) +
          p2.G * (dY * (1-dX)) +
          p3.G * (dX * dY)
        );

        Color.B := Trunc(
          p0.B * (1-dX) * (1-dY) +
          p1.B * (dX * (1-dY)) +
          p2.B * (dY * (1-dX)) +
          p3.B * (dX * dY)
        );

        Result.Data[Y * NewWidth + X] := Color;
      end;
    end;
end;

function TSimbaImage.GetPixels(Points: TPointArray): TColorArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));

  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
        RaiseOutOfImageException(X, Y);

      with FData[Y * FWidth + X] do
        Result[I] := TColor(R or G shl G_BIT or B shl B_BIT);
    end;
end;

procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);
var
  I: Integer;
begin
  if (Length(Points) <> Length(Colors)) then
    SimbaException('TSimbaImage.SetPixels: Pixel & Color arrays must be same lengths (%d, %s)', [Length(Points), Length(Colors)]);

  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
        RaiseOutOfImageException(X, Y);

      FData[Y * FWidth + X] := Colors[I].ToBGRA();
    end;
end;

procedure TSimbaImage.SetExternalData(AData: PColorBGRA; AWidth, AHeight: Integer);
begin
  SetSize(0, 0);

  FWidth := AWidth;
  FHeight := AHeight;
  FData := AData;
  FDataOwner := False;
end;

procedure TSimbaImage.ResetExternalData;
begin
  if FDataOwner then
    Exit;

  FDataOwner := True;
  FData := nil;

  SetSize(0, 0);
end;

function TSimbaImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: ESimbaImageThreshMethod; K: Integer): TSimbaImage;
var
  I: Integer;
  vMin, vMax, Threshold: UInt8;
  Counter: Int64;
  Tab: array[0..256] of UInt8;
  Upper: PtrUInt;
  SrcPtr, DestPtr: PColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  if Alpha = Beta then Exit;
  if Alpha > Beta then Swap(Alpha, Beta);

  //Finding the threshold - While at it convert image to grayscale.
  Threshold := 0;
  case Method of
    //Find the Arithmetic Mean / Average.
    ESimbaImageThreshMethod.MEAN:
      begin
        SrcPtr := FData;
        DestPtr := Result.Data;
        Upper := PtrUInt(@FData[FWidth * FHeight]);

        Counter := 0;

        while (PtrUInt(SrcPtr) < Upper) do
        begin
          DestPtr^.R := (SrcPtr^.B + SrcPtr^.G + SrcPtr^.R) div 3;

          Counter += DestPtr^.R;

          Inc(SrcPtr);
          Inc(DestPtr);
        end;

        Threshold := (Counter div ((FWidth * FHeight) - 1)) + K;
      end;

    //Middle of Min- and Max-value
    ESimbaImageThreshMethod.MIN_MAX:
      begin
        vMin := 255;
        vMax := 0;

        SrcPtr := FData;
        DestPtr := Result.Data;
        Upper := PtrUInt(@FData[FWidth * FHeight]);

        while (PtrUInt(SrcPtr) < Upper) do
        begin
          DestPtr^.R := (SrcPtr^.B + SrcPtr^.G + SrcPtr^.R) div 3;

          if (DestPtr^.R < vMin) then vMin := DestPtr^.R else
          if (DestPtr^.R > vMax) then vMax := DestPtr^.R;

          Inc(SrcPtr);
          Inc(DestPtr)
        end;

        Threshold := ((vMax + Integer(vMin)) shr 1) + K;
      end;
  end;

  if AInvert then Swap(Alpha, Beta);
  for I:=0 to Threshold - 1 do Tab[I] := Alpha;
  for I:=Threshold to 255   do Tab[I] := Beta;

  Upper := PtrUInt(@Result.Data[Result.Width * Result.Height]);
  DestPtr := Result.Data;
  while (PtrUInt(DestPtr) < Upper) do
  begin
    DestPtr^.R := Tab[DestPtr^.R];

    Inc(DestPtr);
  end;
end;

{
  Radius = Window size
  Invert = Invert output
  R      = dynamic range of standard deviation (default = 128)
  K      = constant value in range 0.2..0.5 (default = 0.5)
}
function TSimbaImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; R: Single; K: Single): TSimbaImage;
var
  Mat: TByteMatrix;
  Integral: TSimbaIntegralImageF;
  X,Y,W,H: Integer;
  Left, Right, Top, Bottom: Integer;
  Count: Integer;
  Sum, SumSquares: Double;
  Mean, Stdev, Threshold: Double;
  HitColor, MissColor: TColorBGRA;
begin
  Result := TSimbaImage.Create(FWidth, FHeight);

  Mat := Self.ToGreyMatrix();
  Mat.GetSize(W, H);

  Dec(W);
  Dec(H);

  Radius := (Radius - 1) div 2;
  Integral := TSimbaIntegralImageF.Create(Mat);

  HitColor.AsInteger := $FFFFFF;
  MissColor.AsInteger := 0;
  if AInvert then
    Swap(HitColor, MissColor);

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Left   := Max(X-Radius, 0);
      Right  := Min(X+Radius, W);
      Top    := Max(Y-Radius, 0);
      Bottom := Min(Y+Radius, H);

      //Sum := 0;
      //SumSquares := 0;
      //
      //for y := top to bottom do
      //  for x := left to right do
      //  begin
      //    Sum += mat[y,x];
      //    SumSquares += mat[y,x]*mat[y,x];
      //  end;

      Integral.Query(Left, Top, Right, Bottom, Sum, SumSquares);

      Count := (Bottom - Top + 1) * (Right - Left + 1);
      Mean := Sum / Count;
      Stdev := Sqrt((SumSquares / Count) - Sqr(Mean));
      Threshold := Mean * (1.0 + K * ((Stdev / R) - 1.0));

      if Mat[Y, X] < Threshold then
        Result.FData[Y * FWidth + X] := MissColor
      else
        Result.FData[Y * FWidth + X] := HitColor;
    end;
end;

procedure TSimbaImage.Pad(Amount: Integer);
var
  OldWidth, OldHeight: Integer;
  Y: Integer;
begin
  OldWidth := FWidth;
  OldHeight := FHeight;

  SetSize(FWidth + (Amount * 2), FHeight + (Amount * 2));

  for Y := OldHeight downto 0 do
  begin
    Move(FData[Y * FWidth], FData[(Y + Amount) * FWidth + Amount], OldWidth * SizeOf(TColorBGRA));

    // clear old pixels
    if (Y < Amount) then
      FillData(@FData[Y * FWidth], OldWidth * SizeOf(TColorBGRA), DefaultPixel)
    else
      FillData(@FData[Y * FWidth], Amount * SizeOf(TColorBGRA), DefaultPixel);
  end;
end;

procedure TSimbaImage._DrawTPA(TPA: TPointArray; Color: TColor);
var
  BGRA: TColorBGRA;
  Point: TPoint;
begin
  BGRA := Color.ToBGRA();
  BGRA.A := ALPHA_OPAQUE;

  for Point in TPA do
    if (Point.X >= 0) and (Point.Y >= 0) and (Point.X < FWidth) and (Point.Y < FHeight) then
      FData[Point.Y * FWidth + Point.X] := BGRA;
end;

procedure TSimbaImage._DrawTPAAlpha(TPA: TPointArray; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;
  Point: TPoint;
begin
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  for Point in TPA do
    if (Point.X >= 0) and (Point.Y >= 0) and (Point.X < FWidth) and (Point.Y < FHeight) then
      BlendPixel(@FData[Point.Y * FWidth + Point.X], BGRA);
end;

procedure TSimbaImage._DrawLine(Start, Stop: TPoint; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      FData[Y * FWidth + X] := BGRA;
  end;

  {$i shapebuilder_line.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := ALPHA_OPAQUE;

  _BuildLine(Start, Stop);
end;

procedure TSimbaImage._DrawLineAlpha(Start, Stop: TPoint; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer);
  begin
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      BlendPixel(@FData[Y * FWidth + X], BGRA);
  end;

  {$i shapebuilder_line.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  _BuildLine(Start, Stop);
end;

procedure TSimbaImage._DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      FData[Y * FWidth + X] := BGRA;
  end;

  {$i shapebuilder_linegap.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := ALPHA_OPAQUE;

  _BuildLineGap(Start, Stop, GapSize);
end;

procedure TSimbaImage._DrawLineGapAlpha(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer);
  begin
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      BlendPixel(@FData[Y * FWidth + X], BGRA);
  end;

  {$i shapebuilder_linegap.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  _BuildLineGap(Start, Stop, GapSize);
end;

procedure TSimbaImage._DrawBoxFilled(Box: TBox; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@FData[Y * FWidth + X1], Box.Width, BGRA);
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth-1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Color.ToBGRA();
    BGRA.A := ALPHA_OPAQUE;

    _BuildBoxFilled(Box);
  end;
end;

procedure TSimbaImage._DrawBoxFilledAlpha(Box: TBox; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    Ptr := @FData[Y * FWidth + X1];
    Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

    while (PtrUInt(Ptr) <= Upper) do
    begin
      BlendPixel(Ptr, BGRA);

      Inc(Ptr);
    end;
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Color.ToBGRA();
    BGRA.A := Alpha;

    _BuildBoxFilled(Box);
  end;
end;

procedure TSimbaImage._DrawBoxEdge(Box: TBox; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    FData[Y * FWidth + X] := BGRA;
  end;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@FData[Y * FWidth + X1], (X2 - X1) + 1, BGRA);
  end;

  {$i shapebuilder_boxedge.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Color.ToBGRA();
    BGRA.A := ALPHA_OPAQUE;

    _BuildBoxEdge(Box);
  end;
end;

procedure TSimbaImage._DrawBoxEdgeAlpha(Box: TBox; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    FData[Y * FWidth + X] := BGRA;
  end;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@FData[Y * FWidth + X1], (X2 - X1) + 1, BGRA);
  end;

  {$i shapebuilder_boxedge.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));

  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    BGRA := Color.ToBGRA();
    BGRA.A := ALPHA_OPAQUE;

    _BuildBoxEdge(Box);
  end;
end;

procedure TSimbaImage._DrawCircleFilledAlpha(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < FHeight) then
    begin
      X1 := EnsureRange(X1, 0, FWidth - 1);
      X2 := EnsureRange(X2, 0, FWidth - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := @FData[Y * FWidth + X1];
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

        while (PtrUInt(Ptr) <= Upper) do
        begin
          BlendPixel(Ptr, BGRA);

          Inc(Ptr);
        end;
      end;
    end;
  end;

  {$i shapebuilder_circlefilled.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  _BuildCircleFilled(ACenter.X, ACenter.Y, Radius);
end;

procedure TSimbaImage._DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  begin
    if (Y >= 0) and (Y < FHeight) then
    begin
      X1 := EnsureRange(X1, 0, FWidth - 1);
      X2 := EnsureRange(X2, 0, FWidth - 1);
      if ((X2 - X1) + 1 > 0) then
        FillData(@FData[Y * FWidth + X1], (X2 - X1) + 1, BGRA);
    end;
  end;

  {$i shapebuilder_circlefilled.inc}

begin
  BGRA := Color.ToBGRA(); // rgb to bgra
  BGRA.A := ALPHA_OPAQUE;

  _BuildCircleFilled(ACenter.X, ACenter.Y, Radius);
end;

procedure TSimbaImage._DrawPolygonFilledAlpha(Points: TPointArray; Color: TColor; Alpha: Byte);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  var
    Ptr: PColorBGRA;
    Upper: PtrUInt;
  begin
    // Y is already clipped in _PolygonFilled
    X1 := EnsureRange(X1, 0, FWidth - 1);
    X2 := EnsureRange(X2, 0, FWidth - 1);

    if ((X2 - X1) + 1 > 0) then
    begin
      Ptr := @FData[Y * FWidth + X1];
      Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(TColorBGRA));

      while (PtrUInt(Ptr) <= Upper) do
      begin
        BlendPixel(Ptr, BGRA);

        Inc(Ptr);
      end;
    end;
  end;

  {$i shapebuilder_polygonfilled.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := Alpha;

  _BuildPolygonFilled(Points, TRect.Create(0, 0, FWidth-1, FHeight-1), TPoint.ZERO);
end;

procedure TSimbaImage._DrawPolygonFilled(Points: TPointArray; Color: TColor);
var
  BGRA: TColorBGRA;

  procedure _Row(const Y: Integer; X1, X2: Integer);
  begin
    // Y is already clipped in _PolygonFilled
    X1 := EnsureRange(X1, 0, FWidth - 1);
    X2 := EnsureRange(X2, 0, FWidth - 1);

    if ((X2 - X1) + 1 > 0) then
      FillData(@FData[Y * FWidth + X1], (X2 - X1) + 1, BGRA);
  end;

  {$i shapebuilder_polygonfilled.inc}

begin
  BGRA := Color.ToBGRA();
  BGRA.A := ALPHA_OPAQUE;

  _BuildPolygonFilled(Points, TRect.Create(0, 0, FWidth-1, FHeight-1), TPoint.ZERO);
end;

procedure TSimbaImage._DrawImage(Image: TSimbaImage; P: TPoint);
var
  W, H: Integer;
  LoopX, LoopY, DestX, DestY: Integer;
  Color: TColorBGRA;
begin
  W := Image.Width - 1;
  H := Image.Height - 1;

  for LoopY := 0 to H do
    for LoopX := 0 to W do
    begin
      DestX := LoopX + P.X;
      DestY := LoopY + P.Y;
      if (DestX >= 0) and (DestY >= 0) and (DestX < FWidth) and (DestY < FHeight) then
      begin
        Color := Image.Data[LoopY * Image.Width + LoopX];
        Color.A := ALPHA_OPAQUE;

        FData[DestY * FWidth + DestX] := Color;
      end;
    end;
end;

procedure TSimbaImage._DrawImageAlpha(Image: TSimbaImage; P: TPoint; Alpha: Byte);
var
  LoopX, LoopY, DestX, DestY, W, H: Integer;
  Color: TColorBGRA;
begin
  W := Image.Width - 1;
  H := Image.Height - 1;

  for LoopY := 0 to H do
    for LoopX := 0 to W do
    begin
      DestX := LoopX + P.X;
      DestY := LoopY + P.Y;

      Color := Image.Data[LoopY * Image.Width + LoopX];
      Color.A := Alpha;

      BlendPixel(FData, FWidth, FHeight, DestX, DestY, Color);
    end;
end;

procedure TSimbaImage.RaiseOutOfImageException(X, Y: Integer);
begin
  SimbaException('%d,%d is outside the image bounds (0,0,%d,%d)', [X, Y, FWidth-1, FHeight-1]);
end;

procedure TSimbaImage.NotifyUnfreed;
begin
  inherited NotifyUnfreed();

  if (SaveUnfreedImages <> '') then
  try
    Save(IncludeTrailingPathDelimiter(SetDirSeparators(SaveUnfreedImages)) + IntToStr(PtrUInt(Self)) + '.bmp');
  except
    on E: Exception do
      DebugLn(E.ToString);
  end;
end;

function TSimbaImage.GetPixel(const X, Y: Integer): TColor;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfImageException(X, Y);

  with FData[Y * FWidth + X] do
    Result := R or G shl G_BIT or B shl B_BIT;
end;

procedure TSimbaImage.SetPixel(X, Y: Integer; Color: TColor);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfImageException(X, Y);

  with FData[Y * FWidth + X] do
  begin
    B := Color shr B_BIT and $FF;
    G := Color shr G_BIT and $FF;
    R := Color shr R_BIT and $FF;
    A := ALPHA_OPAQUE;
  end;
end;

function TSimbaImage.InImage(const X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight);
end;

procedure TSimbaImage.DrawLineAA(Start, Stop: TPoint; Color: TColor; Thickness: Single);
var
  RGB: TColorBGRA;
  Alpha: Byte absolute RGB.A;

  // https://zingl.github.io/bresenham.js
  procedure _LineAntialias(x0, y0, x1, y1: Integer; Thickness: Single);
  var
    dx, dy, err: Integer;
    e2, x2, y2: Integer;
    ed: Single;
    sx, sy: Integer;
  begin
    dx := Abs(x1 - x0);
    dy := Abs(y1 - y0);

    if (x0 < x1) then sx := 1 else sx := -1;
    if (y0 < y1) then sy := 1 else sy := -1;

    err := dx-dy;
    if (dx+dy = 0) then
      ed := 1
    else
      ed := Sqrt(Double(dx*dx) + Double(dy*dy));

    Thickness := (Thickness + 1) / 2;
    while True do
    begin
      Alpha := Round(255-Max(0, 255 * (Abs(err-dx+dy)/ed-Thickness+1)));
      BlendPixel(FData, FWidth, FHeight, x0, y0, RGB);

      e2 := err;
      x2 := x0;
      if (2*e2 >= -dx) then
      begin
        e2 += dy;
        y2 := y0;
        while (e2 < ed*Thickness) and ((y1 <> y2) or (dx > dy)) do
        begin
          y2 += sy;

          Alpha := Round(255-Max(0, 255 * (Abs(e2)/ed-Thickness+1)));
          BlendPixel(FData, FWidth, FHeight, x0, y2, RGB);

          e2 += dx;
        end;
        if (x0 = x1) then
          Break;

        e2 := err;
        err -= dy;
        x0 += sx;
      end;

      if (2*e2 <= dy) then
      begin
        e2 := dx-e2;
        while (e2 < ed*Thickness) and ((x1 <> x2) or (dx < dy)) do
        begin
          x2 += sx;

          Alpha := Round(255-Max(0, 255 * (Abs(e2)/ed-Thickness+1)));
          BlendPixel(FData, FWidth, FHeight, x2, y0, RGB);

          e2 += dy;
        end;
        if (y0 = y1) then
          Break;

        err += dx;
        y0 += sy;
      end;
    end;
  end;

begin
  RGB := Color.ToBGRA();

  _LineAntialias(
    Start.X, Start.Y,
    Stop.X, Stop.Y,
    Thickness
  );
end;

procedure TSimbaImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Color: TColor; Thickness: Single);
var
  RGB: TColorBGRA;
  Alpha: Byte absolute RGB.A;

  // https://zingl.github.io/bresenham.js
  procedure _EllipseAntialias(x0, y0, x1, y1: Integer; Thickness: Single);
  var
    a,b,b1: Integer;
    a2,b2: Single;
    dx,dy: Single;
    err: Single;
    dx2,dy2,e2,ed: Single;
    i: Single;
  begin
    a := Abs(x1 - x0);
    b := Abs(y1 - y0);
    if (a = 0) or (b = 0) then
      Exit;

    b1 := b and 1;
    a2 := a-2*Thickness;
    b2 := b-2*Thickness;
    dx := 4*(a-1)*b*b;
    dy := 4*(b1-1)*a*a;

    i := a+b2;
    err := b1*a*a;

    if ((Thickness-1) * (2*b-Thickness) > a*a) then
      b2 := Sqrt(a*(b-a)*i*a2) / (a-Thickness);

    if ((Thickness-1) * (2*a-Thickness) > b*b) then
    begin
      a2 := Sqrt(b*(a-b)*i*b2) / (b-Thickness);
      Thickness := (a-a2) / 2;
    end;

    if (x0 > x1) then
    begin
      x0 := x1;
      x1 += a;
    end;

    if (y0 > y1) then
      y0 := y1;

    if (b2 <= 0) then
      Thickness := a;

    e2 := Thickness - Floor(Thickness);
    Thickness := x0+Thickness-e2;
    dx2 := 4*(a2+2*e2-1)*b2*b2;
    dy2 := 4*(b1-1)*a2*a2;
    e2 := dx2*e2;
    y0 += (b+1) shr 1;
    y1 := y0-b1;
    a := 8*a*a;
    b1 := 8*b*b;
    a2 := 8*a2*a2;
    b2 := 8*b2*b2;

    repeat
      while True do
      begin
        if (err < 0) or (x0 > x1) then
        begin
          i := x0;
          Break;
        end;

        i := Min(dx,dy);
        ed := Max(dx,dy);

        if ((y0 = y1+1) and (2*err > dx) and (a > b1)) then
          ed := a/4
        else
          ed += 2*ed*i*i/(4*ed*ed+i*i+1)+1;
        i := 255*err/ed;

        Alpha := 255-Byte(Round(i));

        BlendPixel(FData, FWidth, FHeight, x0, y0, RGB);
        BlendPixel(FData, FWidth, FHeight, x0, y1, RGB);
        BlendPixel(FData, FWidth, FHeight, x1, y0, RGB);
        BlendPixel(FData, FWidth, FHeight, x1, y1, RGB);

        if (err+dy+a < dx) then
        begin
          i := x0+1;
          Break;
        end;

        x0 += 1;
        x1 -= 1;
        err -= dx;
        dx -= b1;
      end;

      Alpha := 255;

      while (i < Thickness) and (2*i <= x0+x1) do
      begin
        BlendPixel(FData, FWidth, FHeight, Round(i),       y0, RGB);
        BlendPixel(FData, FWidth, FHeight, Round(x0+x1-i), y0, RGB);
        BlendPixel(FData, FWidth, FHeight, Round(i),       y1, RGB);
        BlendPixel(FData, FWidth, FHeight, Round(x0+x1-i), y1, RGB);

        i += 1.0;
      end;

      while ((e2 > 0) and (x0+x1 >= 2*Thickness)) do
      begin
         i := Min(dx2, dy2);
         ed := Max(dx2, dy2);

         if (y0 = y1+1) and (2*e2 > dx2) and (a2 > b2) then
           ed := a2/4
         else
           ed += 2*ed*i*i/(4*ed*ed+i*i);

         Alpha := 255-Byte(Round(255-255*e2/ed));

         BlendPixel(FData, FWidth, FHeight, Round(Thickness),       y0, RGB);
         BlendPixel(FData, FWidth, FHeight, Round(x0+x1-Thickness), y0, RGB);
         BlendPixel(FData, FWidth, FHeight, Round(Thickness),       y1, RGB);
         BlendPixel(FData, FWidth, FHeight, Round(x0+x1-Thickness), y1, RGB);

         if (e2+dy2+a2 < dx2) then
           Break;

         Thickness += 1;
         e2 -= dx2;
         dx2 -= b2;
      end;

      dy2 += a2;
      e2 += dy2;
      y0 += 1;
      y1 -= 1;
      dy += a;
      err += dy;
    until (x0 >= x1);

    while (y0-y1 <= b) do
    begin
      Alpha := 255 - Byte(Round(255*4*err/b1));

      BlendPixel(FData, FWidth, FHeight, x0, y0, RGB);
      BlendPixel(FData, FWidth, FHeight, x1, y0, RGB);
      BlendPixel(FData, FWidth, FHeight, x0, y1, RGB);
      BlendPixel(FData, FWidth, FHeight, x1, y1, RGB);

      y0 += 1;
      y1 -= 1;
      dy += a;
      err += dy;
    end;
  end;

begin
  RGB := Color.ToBGRA();

  _EllipseAntialias(
    ACenter.X - XRadius, ACenter.Y - YRadius,
    ACenter.X + XRadius, ACenter.Y + YRadius,
    Thickness
  );
end;

procedure TSimbaImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Color: TColor; Thickness: Single);
begin
  DrawEllipseAA(ACenter, Radius, Radius, Color, Thickness);
end;

constructor TSimbaImage.Create;
begin
  inherited Create();

  FDataOwner := True;
  FTextDrawer := TSimbaTextDrawer.Create(Self);

  DefaultPixel.AsInteger := 0;
  DefaultPixel.A := ALPHA_OPAQUE;
end;

constructor TSimbaImage.Create(AWidth, AHeight: Integer);
begin
  Create();

  SetSize(AWidth, AHeight);
end;

constructor TSimbaImage.CreateFromFile(FileName: String);
begin
  Create();

  Load(FileName);
end;

constructor TSimbaImage.CreateFromZip(ZipFileName, ZipEntry: String);
var
  Stream: TMemoryStream;
begin
  Create();

  Stream := ZipExtractEntry(ZipFileName, ZipEntry);
  try
    FromStream(Stream, ZipEntry);
  finally
    Stream.Free();
  end;
end;

constructor TSimbaImage.CreateFromString(Str: String);
begin
  Create();

  FromString(Str);
end;

constructor TSimbaImage.CreateFromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);
begin
  Create();

  FromData(AWidth, AHeight, AData, ADataWidth);
end;

constructor TSimbaImage.CreateFromWindow(Window: TWindowHandle);
var
  B: TBox;
  ImageData: PColorBGRA = nil;
begin
  Create();

  if SimbaNativeInterface.GetWindowBounds(Window, B) and
     SimbaNativeInterface.GetWindowImage(Window, 0, 0, B.Width - 1, B.Height - 1, ImageData) then
  try
    FromData(B.Width - 1, B.Height - 1, ImageData, B.Width - 1);
  finally
    FreeMem(ImageData);
  end;
end;

destructor TSimbaImage.Destroy;
begin
  if FDataOwner then
    SetSize(0, 0);

  FreeAndNil(FTextDrawer);

  inherited Destroy();
end;

end.

