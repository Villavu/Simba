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
  simba.base, simba.baseclass, simba.image_textdrawer, simba.colormath, simba.dtm;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  EImageMirrorStyle  = (WIDTH, HEIGHT, LINE);
  EImageThreshMethod = (MEAN, MIN_MAX);
  EImageResizeAlgo   = (NEAREST_NEIGHBOUR, BILINEAR);
  EImageRotateAlgo   = (NEAREST_NEIGHBOUR, BILINEAR);
  EImageBlurAlgo     = (BOX, GAUSS);
  {$POP}

  TDetachedImageData = record
    Data: PColorBGRA;
    Width: Integer;
    Height: Integer;
  end;

  TSimbaImage = class;
  TSimbaImageLineStarts = array of PColorBGRA;
  TSimbaImageArray = array of TSimbaImage;

  TSimbaImage = class(TSimbaBaseClass)
  protected
    FWidth: Integer;
    FHeight: Integer;
    FCenter: TPoint;

    FData: PColorBGRA;
    FDataSize: SizeUInt;

    FLineStarts: TSimbaImageLineStarts;

    FTextDrawer: TSimbaTextDrawer;

    FDrawColor: TColor;
    FDrawAlpha: Byte;

    function DetachData: TDetachedImageData;

    procedure DrawData(TheData: PColorBGRA; DataW, DataH: Integer; P: TPoint);
    procedure DrawDataAlpha(TheData: PColorBGRA; DataW, DataH: Integer; P: TPoint; Alpha: Byte);

    procedure RaiseOutOfImageException(X, Y: Integer);
    procedure NotifyUnfreed; override;

    function GetPixel(const X, Y: Integer): TColor;
    function GetAlpha(const X, Y: Integer): Byte;
    function GetFontAntialiasing: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetLineStart(const Y: Integer): PColorBGRA;
    function GetDrawColorAsBGRA: TColorBGRA;

    procedure SetPixel(const X, Y: Integer; const Color: TColor);
    procedure SetAlpha(const X, Y: Integer; const Value: Byte);
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
  public
    class var SaveUnfreedImages: ShortString;
    class function LoadFontsInDir(Dir: String): Boolean;
    class function Fonts: TStringArray;
  public
    DefaultPixel: TColorBGRA;

    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    constructor Create(FileName: String); overload;
    constructor CreateFromZip(ZipFileName, ZipEntry: String);
    constructor CreateFromString(Str: String);
    constructor CreateFromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);
    constructor CreateFromWindow(Window: TWindowHandle);
    constructor CreateFromMatrix(Mat: TIntegerMatrix); overload;
    constructor CreateFromMatrix(Mat: TSingleMatrix; ColorMapType: Integer = 0); overload;
    destructor Destroy; override;

    property Data: PColorBGRA read FData;
    property DataSize: SizeUInt read FDataSize;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Center: TPoint read FCenter;

    property LineStarts: TSimbaImageLineStarts read FLineStarts;
    property LineStart[Line: Integer]: PColorBGRA read GetLineStart;

    property DrawColor: TColor read FDrawColor write FDrawColor;
    property DrawAlpha: Byte read FDrawAlpha write FDrawAlpha;
    property DrawColorAsBGRA: TColorBGRA read GetDrawColorAsBGRA;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    property Alpha[X, Y: Integer]: Byte read GetAlpha write SetAlpha;

    function InImage(const X, Y: Integer): Boolean;

    procedure SetSize(NewWidth, NewHeight: Integer);

    procedure Fill(Color: TColor);
    procedure FillWithAlpha(Value: Byte);

    procedure Clear; overload;
    procedure Clear(Box: TBox); overload;
    procedure ClearInverted(Box: TBox);

    function Copy(Box: TBox): TSimbaImage; overload;
    function Copy: TSimbaImage; overload;
    procedure Crop(Box: TBox);
    procedure Pad(Amount: Integer);
    procedure Offset(X, Y: Integer);

    function GetPixels(Points: TPointArray): TColorArray;
    procedure SetPixels(Points: TPointArray; Color: TColor); overload;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload;

    function GetColors: TColorArray; overload;
    function GetColors(Box: TBox): TColorArray; overload;

    procedure SplitChannels(var B,G,R: TByteArray);
    procedure FromChannels(const B,G,R: TByteArray; W, H: Integer);

    procedure ReplaceColor(OldColor, NewColor: TColor); overload;
    procedure ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single); overload;

    // Rotate resize flip
    function Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TSimbaImage;
    function Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TSimbaImage; overload;
    function Resize(Algo: EImageResizeAlgo; Scale: Single): TSimbaImage; overload;
    function Downsample(Scale: Integer): TSimbaImage; overload;
    function Downsample(Scale: Integer; IgnorePoints: TPointArray): TSimbaImage; overload;
    function Mirror(Style: EImageMirrorStyle): TSimbaImage;

    // text measure
    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    // text draw
    procedure DrawText(Text: String; Position: TPoint); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint);

    // Image
    procedure DrawImage(Image: TSimbaImage; Location: TPoint);

    // Point
    procedure DrawATPA(ATPA: T2DPointArray);
    procedure DrawTPA(TPA: TPointArray);

    // Line
    procedure DrawLine(Start, Stop: TPoint);
    procedure DrawLineGap(Start, Stop: TPoint; GapSize: Integer);
    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer);
    procedure DrawCross(ACenter: TPoint; Radius: Integer);

    // Box
    procedure DrawBox(Box: TBox);
    procedure DrawBoxFilled(Box: TBox);
    procedure DrawBoxInverted(B: TBox);

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
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer);
    procedure DrawCircleFilled(ACenter: TPoint; Radius: Integer);

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

    // Misc
    procedure DrawHSLCircle(ACenter: TPoint; Radius: Integer);

    // Filters
    function Convolute(Matrix: TDoubleMatrix): TSimbaImage;
    function Sobel: TSimbaImage;
    function Enhance(Enchantment: Byte; C: Single): TSimbaImage;
    function GreyScale: TSimbaImage;
    function Brightness(Value: Integer): TSimbaImage;
    function Invert: TSimbaImage;
    function Posterize(Value: Integer): TSimbaImage;
    function Threshold(Inv: Boolean; C: Integer): TSimbaImage;
    function ThresholdAdaptive(Inv: Boolean; Radius: Integer; C: Integer): TSimbaImage;
    function ThresholdAdaptiveSauvola(Inv: Boolean; Radius: Integer; C: Single): TSimbaImage;
    function Blend(Points: TPointArray; Radius: Integer): TSimbaImage; overload;
    function Blend(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TSimbaImage; overload;
    function Blur(Algo: EImageBlurAlgo; Radius: Integer): TSimbaImage;

    // Matrix
    procedure FromMatrix(Matrix: TIntegerMatrix); overload;
    procedure FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0); overload;

    function ToGreyMatrix: TByteMatrix;
    function ToMatrix: TIntegerMatrix; overload;
    function ToMatrix(Box: TBox): TIntegerMatrix; overload;

    // Load & Save
    procedure FromStream(Stream: TStream; FileName: String);
    procedure FromString(Str: String);
    procedure FromData(AWidth, AHeight: Integer; AData: PColorBGRA; ADataWidth: Integer);
    procedure Load(FileName: String); overload;
    procedure Load(FileName: String; Area: TBox); overload;
    function Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
    function SaveToString: String;

    // Difference
    function Equals(Other: TObject): Boolean; override;
    function Equals(Other: TSimbaImage): Boolean; overload;

    function Compare(Other: TSimbaImage): Single;

    function PixelDifference(Other: TSimbaImage): Integer; overload;
    function PixelDifference(Other: TSimbaImage; Tolerance: Single): Integer; overload;
    function PixelDifferenceTPA(Other: TSimbaImage): TPointArray; overload;                        
    function PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Single): TPointArray; overload;

    // Laz bridge
    function ToLazBitmap: TBitmap;
    procedure FromLazBitmap(LazBitmap: TBitmap);

    // Finders
    function MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;

    function FindColor(Color: TColor; Tolerance: Single): TPointArray; overload;
    function FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray; overload;
    function FindColor(Color: TColorTolerance): TPointArray; overload;

    function FindImage(Image: TSimbaImage; Tolerance: Single; MaxToFind: Integer): TPointArray; overload;
    function FindImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer): TPointArray; overload;
    function FindDTM(DTM: TDTM; MaxToFind: Integer): TPointArray;
    function FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer): TPointArray;

    function FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray; overload;
    function FindEdges(MinDiff: Single): TPointArray; overload;

    function PeakBrightness: Integer;
    function AverageBrightness: Integer;
  end;

  PSimbaImage = ^TSimbaImage;
  PSimbaImageArray = ^TSimbaImageArray;

implementation

uses
  Math, FPImage, BMPcomn,
  simba.vartype_floatmatrix,
  simba.vartype_ordmatrix,
  simba.vartype_pointarray,
  simba.vartype_box,
  simba.vartype_quad,
  simba.image_utils,
  simba.image_lazbridge,
  simba.image_resizerotate,
  simba.image_stringconv,
  simba.image_filters,
  simba.image_draw,
  simba.colormath_distance,
  simba.colormath_conversion,
  simba.zip,
  simba.geometry,
  simba.nativeinterface,
  simba.containers,
  simba.threading,
  simba.target;

function TSimbaImage.Copy: TSimbaImage;
begin
  Result := TSimbaImage.Create();
  Result.SetSize(FWidth, FHeight);
  Result.DefaultPixel := DefaultPixel;

  Move(FData^, Result.FData^, FWidth * FHeight * SizeOf(TColorBGRA));
end;

function TSimbaImage.Copy(Box: TBox): TSimbaImage;
var
  Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfImageException(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfImageException(Box.X2, Box.Y2);

  Result := TSimbaImage.Create();
  Result.SetSize(Box.Width, Box.Height);
  Result.DefaultPixel := DefaultPixel;
  for Y := Box.Y1 to Box.Y2 do
    Move(FData[Y * FWidth + Box.X1], Result.FData[(Y-Box.Y1) * Result.FWidth], Result.FWidth * SizeOf(TColorBGRA));
end;

procedure TSimbaImage.Crop(Box: TBox);
var
  Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfImageException(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfImageException(Box.X2, Box.Y2);

  for Y := Box.Y1 to Box.Y2 do
    Move(FData[Y * FWidth + Box.X1], FData[(Y-Box.Y1) * FWidth], FWidth * SizeOf(TColorBGRA));

  SetSize(Box.Width, Box.Height);
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
        Result[Y, X] := Round(R * 0.299 + G * 0.587 + B * 0.114);
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
      Result[Y, X] := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
end;

function TSimbaImage.ToMatrix(Box: TBox): TIntegerMatrix;
var
  X, Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfImageException(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfImageException(Box.X2, Box.Y2);

  Result.SetSize(Box.Width, Box.Height);

  for Y := Box.Y1 to Box.Y2 do
    for X := Box.X1 to Box.X2 do
      Result[Y-Box.Y1, X-Box.X1] := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
end;

procedure TSimbaImage.FromMatrix(Matrix: TIntegerMatrix);
var
  X, Y, W, H: Integer;
begin
  SetSize(Matrix.Width, Matrix.Height);

  W := FWidth - 1;
  H := FHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
      FData[Y * FWidth + X] := TSimbaColorConversion.ColorToBGRA(Matrix[Y,X], ALPHA_OPAQUE);
end;

procedure TSimbaImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0);
var
  X,Y, W,H: TColor;
  Normed: TSingleMatrix;
  HSL: TColorHSL;
begin
  SetSize(Matrix.Width, Matrix.Height);

  Normed := Matrix.NormMinMax(0, 1);
  HSL := Default(TColorHSL);
  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      case ColorMapType of
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
            HSL.H := ColorMapType;
            HSL.S := 100;
            HSL.L := Normed[Y,X] * 100;
          end;
      end;

      FData[Y * FWidth + X] := TSimbaColorConversion.ColorToBGRA(HSL.ToColor(), ALPHA_OPAQUE);
    end;
end;

procedure TSimbaImage.FromStream(Stream: TStream; FileName: String);
var
  ReaderClass: TFPCustomImageReaderClass;
begin
  ReaderClass := TFPCustomImage.FindReaderFromFileName(FileName);
  if (ReaderClass = nil) then
    SimbaException('TSimbaImage.FromStream: Unknown image format "%s"', [FileName]);

  SimbaImage_FromFPImageReader(Self, ReaderClass, Stream);
end;

procedure TSimbaImage.FromString(Str: String);
begin
  SimbaImage_FromString(Self, Str);
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

procedure TSimbaImage.Load(FileName: String);
var
  ReaderClass: TFPCustomImageReaderClass;
  Stream: TFileStream;
begin
  if (not FileExists(FileName)) then
    SimbaException('TSimbaImage.FromFile: File "%s" does not exist', [FileName]);

  ReaderClass := TFPCustomImage.FindReaderFromFileName(FileName);
  if (ReaderClass = nil) then
    SimbaException('TSimbaImage.FromFile: Unknown image format "%s"', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SimbaImage_FromFPImageReader(Self, ReaderClass, Stream);
  finally
    Stream.Free();
  end;
end;

procedure TSimbaImage.Load(FileName: String; Area: TBox);

  procedure LoadBitmapArea(Image: TSimbaImage; FileName: String; Area: TBox);
  var
    Stream: TFileStream;
    FileHeader: TBitmapFileHeader;
    Header: TBitmapInfoHeader;
    Row, PixelOffset: Integer;
    ScanLineSize: Int64;
    Buffer: PByte;
    BytesPerPixel: Byte;
    SrcPtr, DestPtr: Pointer;
    Upper, DestRowSize: PtrUInt;
  begin
    Buffer := nil;

    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      if Stream.Read(FileHeader, SizeOf(TBitmapFileHeader)) <> SizeOf(TBitmapFileHeader) then
        raise Exception.Create('Invalid file header');

      {$IFDEF ENDIAN_BIG}
      SwapBMPFileHeader(FileHeader);
      {$ENDIF}
      if (FileHeader.bfType <> BMmagic) then
        raise Exception.Create('Invalid file header magic');

      if Stream.Read(Header, SizeOf(TBitmapInfoHeader)) <> SizeOf(TBitmapInfoHeader) then
        raise Exception.Create('Invalid info header');
      {$IFDEF ENDIAN_BIG}
      SwapBMPInfoHeader(Header);
      {$ENDIF}

      case Header.BitCount of
        32: BytesPerPixel := 4;
        24: BytesPerPixel := 3;
        else
          raise Exception.Create('Not a 32 or 24 bit bitmap');
      end;

      Area := Area.Clip(TBox.Create(0, 0, Header.Width-1, Header.Height-1));
      if (Area.Width > 1) and (Area.Height > 1) then
      begin
        PixelOffset := FileHeader.bfOffset;
        ScanLineSize := Int64((Header.Width * Header.BitCount) + 31) div 32 * 4;
        Buffer := GetMem(ScanLineSize);

        Image.SetSize(Area.Width, Area.Height);
        DestPtr := Image.Data;
        DestRowSize := Area.Width * SizeOf(TColorBGRA);

        for Row := Area.Y1 to Area.Y2 do
        begin
          Stream.Position := PixelOffset + ((Header.Height - (Row + 1)) * ScanLineSize) + (Area.X1 * BytesPerPixel);
          Stream.Read(Buffer^, ScanLineSize);

          SrcPtr := Buffer;

          Upper := PtrUInt(DestPtr) + DestRowSize;
          while (PtrUInt(DestPtr) < Upper) do
          begin
            PColorRGB(DestPtr)^ := PColorRGB(SrcPtr)^; // dont care about alpha

            Inc(SrcPtr, BytesPerPixel);
            Inc(DestPtr, SizeOf(TColorBGRA));
          end;
        end;
      end;
    finally
      if (Buffer <> nil) then
        FreeMem(Buffer);

      Stream.Free();
    end;
  end;

begin
  if (not FileExists(FileName)) then
    SimbaException('TSimbaImage.FromFile: File not found "%s"', [FileName]);

  // Because bitmaps are not compressed we can load a select area without loading the entire image.
  if FileName.EndsWith('.bmp', False) then
    LoadBitmapArea(Self, FileName, Area)
  else
  begin // else just load then crop
    Load(FileName);

    Area := Area.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));
    if (Area.Width > 1) and (Area.Height > 1) then
      Crop(Area);
  end;
end;

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

function TSimbaImage.SaveToString: String;
begin
  Result := SimbaImage_ToString(Self);
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

  P1 := Data;
  P2 := Other.Data;
  for I := 0 to FWidth * FHeight - 1 do
  begin
    if (not SimilarColors(TSimbaColorConversion.BGRAToColor(P1^), TSimbaColorConversion.BGRAToColor(P2^), Tolerance)) then
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

  W := FWidth - 1;
  H := FHeight - 1;

  P1 := Data;
  P2 := Other.Data;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (not SimilarColors(TSimbaColorConversion.BGRAToColor(P1^), TSimbaColorConversion.BGRAToColor(P2^), Tolerance)) then
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

  FData := TempBitmap.Data;
  FWidth := TempBitmap.Width;
  FHeight := TempBitmap.Height;

  TempBitmap.FData := nil; // data is now ours
  TempBitmap.Free();
end;

function TSimbaImage.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.MatchColor(Color, ColorSpace, Multipliers, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindColor(Color: TColor; Tolerance: Single): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindColor(Color, Tolerance, DefaultColorSpace, DefaultMultipliers, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindColor(Color, Tolerance, ColorSpace, Multipliers, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindColor(Color: TColorTolerance): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindColor(Color.Color, Color.Tolerance, Color.ColorSpace, Color.Multipliers, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindImage(Image: TSimbaImage; Tolerance: Single; MaxToFind: Integer): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindImageEx(Image, Tolerance, DefaultColorSpace, DefaultMultipliers, MaxToFind, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindImageEx(Image, Tolerance, ColorSpace, Multipliers, MaxToFind, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindDTM(DTM: TDTM; MaxToFind: Integer): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindDTMEx(DTM, MaxToFind, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer): TPointArray;
var
  Target: TSimbaTarget;
begin
  Target.SetImage(Self);

  Result := Target.FindDTMRotatedEx(DTM, StartDegrees, EndDegrees, Step, FoundDegrees, MaxToFind, TBox.Create(-1,-1,-1,-1));
end;

function TSimbaImage.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
var
  X, Y, W, H: Integer;
  Buffer: TSimbaPointBuffer;
  First, Second, Third: TColor;
begin
  Buffer.Init();

  W := FWidth - 2;
  H := FHeight - 2;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      First  := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
      Second := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + (X+1)]);
      Third  := TSimbaColorConversion.BGRAToColor(FData[(Y+1) * FWidth + X]);

      if (not SimilarColors(First, Second, MinDiff, ColorSpace, Multipliers)) or
         (not SimilarColors(First, Third, MinDiff, ColorSpace, Multipliers)) then
        Buffer.Add(X, Y);
    end;

  Result := Buffer.ToArray(False);
end;

function TSimbaImage.FindEdges(MinDiff: Single): TPointArray;
begin
  Result := FindEdges(MinDiff, DefaultColorSpace, DefaultMultipliers);
end;

function TSimbaImage.PeakBrightness: Integer;
var
  X, Y: Integer;
begin
  Result := 0;

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
      with FData[Y * FWidth + X] do
      begin
        if (R > Result) then Result := R;
        if (G > Result) then Result := G;
        if (B > Result) then Result := B;
      end;

  Result := Round(Result / 255 * 100);
end;

function TSimbaImage.AverageBrightness: Integer;
var
  X, Y, Sum: Integer;
begin
  Result := 0;

  for Y := 0 to FHeight - 1 do
  begin
    Sum := 0;
    for X := 0 to FWidth - 1 do
      with FData[Y * FWidth + X] do
        Sum += Round((R + G + B) / 3 * 0.392);

    Result += Sum div FWidth;
  end;

  Result := Round(Result / FHeight);
end;

procedure TSimbaImage.DrawTPA(TPA: TPointArray);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawTPA(Self, TPA)
  else
    SimbaImage_DrawTPAAlpha(Self, TPA);
end;

procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(ATPA) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);
    DrawTPA(ATPA[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaImage.DrawCrosshairs(ACenter: TPoint; Size: Integer);
begin
  Size := Max(1, Size);

  with ACenter do
  begin
    Self.DrawLine(Point(X - Size, Y), Point(X + Size, Y));
    Self.DrawLine(Point(X, Y - Size), Point(X, Y + Size));
  end;
end;

procedure TSimbaImage.DrawCross(ACenter: TPoint; Radius: Integer);
begin
  Radius := Max(1, Round(Radius/2*Sqrt(2)));

  with ACenter do
  begin
    Self.DrawLine(Point(X - Radius, Y - Radius), Point(X + Radius, Y + Radius));
    Self.DrawLine(Point(X + Radius, Y - Radius), Point(X - Radius, Y + Radius));
  end;
end;

procedure TSimbaImage.DrawLine(Start, Stop: TPoint);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawLine(Self, Start, Stop)
  else
    SimbaImage_DrawLineAlpha(Self, Start, Stop);
end;

procedure TSimbaImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawLineGap(Self, Start, Stop, GapSize)
  else
    SimbaImage_DrawLineGapAlpha(Self, Start, Stop, GapSize);
end;

procedure TSimbaImage.DrawPolygon(Points: TPointArray);
begin
  if (Length(Points) >= 3) then
    Self.DrawTPA(Points.Connect());
end;

procedure TSimbaImage.DrawPolygonFilled(Points: TPointArray);
begin
  if (Length(Points) >= 3) then
    if (FDrawAlpha = ALPHA_OPAQUE) then
      SimbaImage_DrawPolygonFilled(Self, Points)
    else
      SimbaImage_DrawPolygonFilledAlpha(Self, Points)
end;

procedure TSimbaImage.DrawPolygonInverted(Points: TPointArray);
begin
  if (Length(Points) >= 3) then
  begin
    Self.DrawBoxInverted(Points.Bounds().Clip(TBox.Create(0, 0, FWidth-1, FHeight-1)));

    if (FDrawAlpha = ALPHA_OPAQUE) then
      SimbaImage_DrawPolygonInverted(Self, Points)
    else
      SimbaImage_DrawPolygonInvertedAlpha(Self, Points);
  end;
end;

procedure TSimbaImage.DrawCircle(ACenter: TPoint; Radius: Integer);
begin
  if (Radius >= 1) then
    if (FDrawAlpha = ALPHA_OPAQUE) then
      SimbaImage_DrawCircleEdge(Self, ACenter, Radius)
    else
      SimbaImage_DrawCircleEdgeAlpha(Self, ACenter, Radius);
end;

procedure TSimbaImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer);
begin
  if (Radius >= 1) then
    if (FDrawAlpha = ALPHA_OPAQUE) then
      SimbaImage_DrawCircleFilled(Self, ACenter, Radius)
    else
      SimbaImage_DrawCircleFilledAlpha(Self, ACenter, Radius);
end;

procedure TSimbaImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer);
begin
  if (Radius >= 1) then
  begin
    Self.DrawBoxInverted(
      TBox.Create(Max(ACenter.X-Radius, 0), Max(ACenter.Y-Radius, 0), Min(ACenter.X+Radius, FWidth-1), Min(ACenter.Y+Radius, FHeight-1))
    );

    if (FDrawAlpha = ALPHA_OPAQUE) then
      SimbaImage_DrawCircleInverted(Self, ACenter, Radius)
    else
      SimbaImage_DrawCircleInvertedAlpha(Self, ACenter, Radius);
  end;
end;

procedure TSimbaImage.DrawBox(Box: TBox);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawBoxEdge(Self, Box)
  else
    SimbaImage_DrawBoxEdgeAlpha(Self, Box);
end;

procedure TSimbaImage.DrawBoxFilled(Box: TBox);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawBoxFilled(Self, Box)
  else
    SimbaImage_DrawBoxFilledAlpha(Self, Box);
end;

procedure TSimbaImage.DrawBoxInverted(B: TBox);
begin
  Self.DrawBoxFilled(TBox.Create(0,        0,        B.X1 - 1, B.Y1 - 1   )); //Top Left
  Self.DrawBoxFilled(TBox.Create(0,        B.Y1,     B.X1 - 1, B.Y2       )); //Mid Left
  Self.DrawBoxFilled(TBox.Create(0,        B.Y2 + 1, B.X1 - 1, FHeight - 1)); //Btm Left
  Self.DrawBoxFilled(TBox.Create(B.X1,     0,        B.X2,     B.Y1 - 1   )); //Top Mid
  Self.DrawBoxFilled(TBox.Create(B.X1,     B.Y2 + 1, B.X2,     FHeight - 1)); //Btm Mid
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, 0,        FWidth-1, B.Y1 - 1   )); //Top Right
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, B.Y1,     FWidth-1, B.Y2       )); //Mid Right
  Self.DrawBoxFilled(TBox.Create(B.X2 + 1, B.Y2 + 1, FWidth-1, FHeight - 1)); //Btm Right
end;

procedure TSimbaImage.DrawQuad(Quad: TQuad);
begin
  DrawPolygon([Quad.Top, Quad.Right, Quad.Right, Quad.Bottom, Quad.Bottom, Quad.Left, Quad.Left, Quad.Top]);
end;

procedure TSimbaImage.DrawQuadFilled(Quad: TQuad);
begin
  DrawPolygonFilled([Quad.Top, Quad.Right, Quad.Bottom, Quad.Left]);
end;

procedure TSimbaImage.DrawQuadInverted(Quad: TQuad);
begin
  Self.DrawBoxInverted(
    Quad.Bounds.Clip(TBox.Create(0, 0, FWidth-1, FHeight-1))
  );

  if (FDrawAlpha = ALPHA_OPAQUE) then
    SimbaImage_DrawQuadInverted(Self, Quad)
  else
    SimbaImage_DrawQuadInvertedAlpha(Self, Quad);
end;

procedure TSimbaImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(Quads) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    if Filled then
      DrawQuadFilled(Quads[I])
    else
      DrawQuad(Quads[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(Boxes) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    if Filled then
      DrawBoxFilled(Boxes[I])
    else
      DrawBox(Boxes[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(Polygons) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    if Filled then
      DrawPolygonFilled(Polygons[I])
    else
      DrawPolygon(Polygons[I]);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(Centers) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    if Filled then
      DrawCircleFilled(Centers[I], Radius)
    else
      DrawCircle(Centers[I], Radius);
  end;

  DrawColor := PrevDrawColor;
end;

procedure TSimbaImage.DrawCrossArray(Points: TPointArray; Radius: Integer);
var
  I: Integer;
  PrevDrawColor: TColor;
begin
  PrevDrawColor := DrawColor;

  for I := 0 to High(Points) do
  begin
    if (PrevDrawColor = -1) then
      DrawColor := GetDistinctColor(I);

    DrawCross(Points[I], Radius);
  end;

  DrawColor := PrevDrawColor;
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

function TSimbaImage.Sobel: TSimbaImage;
begin
  Result := SimbaImage_Sobel(Self);
end;

function TSimbaImage.Enhance(Enchantment: Byte; C: Single): TSimbaImage;
begin
  Result := SimbaImage_Enhance(Self, Enchantment, C);
end;

procedure TSimbaImage.Fill(Color: TColor);
begin
  FillData(FData, FWidth * FHeight, TSimbaColorConversion.ColorToBGRA(Color, ALPHA_OPAQUE))
end;

procedure TSimbaImage.FillWithAlpha(Value: Byte);
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

procedure TSimbaImage.Clear;
begin
  if (FWidth * FHeight > 0) then
    FillData(FData, FWidth * FHeight, DefaultPixel);
end;

procedure TSimbaImage.Clear(Box: TBox);
var
  W: Integer;

  procedure _Row(const Y: Integer; const X1, X2: Integer);
  begin
    FillData(@FData[Y * FWidth + X1], W, DefaultPixel);
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Box := Box.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));
  if (Box.Width > 1) and (Box.Height > 1) then
  begin
    W := Box.Width;

    _BuildBoxFilled(Box);
  end;
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

procedure TSimbaImage.FromChannels(const B,G,R: TByteArray; W, H: Integer);
var
  Dst: PColorBGRA;
  SrcB, SrcG, SrcR: PByte;
  Upper: PtrUInt;
begin
  SetSize(W, H);
  if (Length(B) <> W*H) or (Length(G) <> W*H) or (Length(R) <> W*H) then
    SimbaException('Channel size does not match image size');

  Dst := FData;
  SrcB := @B[0];
  SrcG := @G[0];
  SrcR := @R[0];

  Upper := PtrUInt(FData) + FDataSize;
  while (PtrUInt(Dst) < Upper) do
  begin
    Dst^.A := ALPHA_OPAQUE;
    Dst^.B := SrcB^;
    Dst^.G := SrcG^;
    Dst^.R := SrcR^;

    Inc(Dst);
    Inc(SrcB);
    Inc(SrcG);
    Inc(SrcR);
  end;
end;

procedure TSimbaImage.DrawImage(Image: TSimbaImage; Location: TPoint);
begin
  if (FDrawAlpha = ALPHA_OPAQUE) then
    DrawData(Image.Data, Image.Width, Image.Height, Location)
  else
    DrawDataAlpha(Image.Data, Image.Width, Image.Height, Location, DrawAlpha);
end;

function TSimbaImage.GetColors: TColorArray;
var
  I: Integer;
begin
  SetLength(Result, FHeight * FWidth);
  for I := 0 to High(Result) do
    Result[I] := TSimbaColorConversion.BGRAToColor(FData[I]);
end;

function TSimbaImage.GetColors(Box: TBox): TColorArray;
var
  Count, X, Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfImageException(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfImageException(Box.X2, Box.Y2);

  SetLength(Result, Box.Width * Box.Height);
  Count := 0;
  for Y := Box.Y1 to Box.Y2 - 1 do
    for X := Box.X1 to Box.X2 - 1 do
    begin
      Result[Count] := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
      Inc(Count);
    end;
end;

class function TSimbaImage.LoadFontsInDir(Dir: String): Boolean;
begin
  Result := SimbaFreeTypeFontLoader.LoadFonts(Dir);
end;

procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor);
var
  Old, New: TColorBGRA;
  Ptr: PColorBGRA;
  Upper: PtrUInt;
begin
  Old := TSimbaColorConversion.ColorToBGRA(OldColor);
  New := TSimbaColorConversion.ColorToBGRA(NewColor, ALPHA_OPAQUE);

  Upper := PtrUInt(@FData[FWidth * FHeight]);
  Ptr := FData;
  while (PtrUInt(Ptr) < Upper) do
  begin
    if Ptr^.EqualsIgnoreAlpha(Old) then
      Ptr^ := New;

    Inc(Ptr);
  end;
end;

procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single);
var
  Old, New: TColorBGRA;
  Ptr: PColorBGRA;
  Upper: PtrUInt;
begin
  Old := TSimbaColorConversion.ColorToBGRA(OldColor);
  New := TSimbaColorConversion.ColorToBGRA(NewColor, ALPHA_OPAQUE);

  Upper := PtrUInt(@FData[FWidth * FHeight]);
  Ptr := FData;
  while (PtrUInt(Ptr) < Upper) do
  begin
    if SimilarColors(Old.ToColor(), Ptr^.ToColor(), Tolerance) then
      Ptr^ := New;
    Inc(Ptr);
  end;
end;

function TSimbaImage.GreyScale: TSimbaImage;
begin
  Result := SimbaImage_GreyScale(Self);
end;

function TSimbaImage.Brightness(Value: Integer): TSimbaImage;
begin
  Result := SimbaImage_Brightness(Self, Value);
end;

function TSimbaImage.Invert: TSimbaImage;
begin
  Result := SimbaImage_Invert(Self);
end;

function TSimbaImage.Posterize(Value: Integer): TSimbaImage;
begin
  Result := SimbaImage_Posterize(Self, Value);
end;

function TSimbaImage.Threshold(Inv: Boolean; C: Integer): TSimbaImage;
begin
  Result := SimbaImage_Threshold(Self, Inv, C);
end;

function TSimbaImage.ThresholdAdaptive(Inv: Boolean; Radius: Integer; C: Integer): TSimbaImage;
begin
  Result := SimbaImage_ThresholdAdaptive(Self, Inv, Radius, C);
end;

function TSimbaImage.ThresholdAdaptiveSauvola(Inv: Boolean; Radius: Integer; C: Single): TSimbaImage;
begin
  Result := SimbaImage_ThresholdAdaptiveSauvola(Self, Inv, Radius, C);
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

function TSimbaImage.Mirror(Style: EImageMirrorStyle): TSimbaImage;
var
  X, Y: Integer;
begin
  case Style of
    EImageMirrorStyle.WIDTH:
      begin
        Result := TSimbaImage.Create(FWidth, FHeight);

        for Y := FHeight - 1 downto 0 do
          for X := FWidth - 1 downto 0 do
            Result.FData[Y*FWidth+X] := FData[Y*FWidth+FWidth-1-X];
      end;

    EImageMirrorStyle.HEIGHT:
      begin
        Result := TSimbaImage.Create(FWidth, FHeight);

        for Y := FHeight - 1 downto 0 do
          Move(FData[Y*FWidth], Result.FData[(FHeight-1-Y) * FWidth], FWidth * SizeOf(TColorBGRA));
      end;

    EImageMirrorStyle.LINE:
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
begin
  Result := Blend(Points, Radius, []);
end;

function TSimbaImage.Blend(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TSimbaImage;
var
  P: TPoint;
  X, Y, Count: Integer;
  Area: TBox;
  SumR, SumG, SumB: UInt64;
  Skip: TBooleanArray;
begin
  Result := Copy();
  SetLength(Skip, FWidth*FHeight);
  for P in IgnorePoints do
    Skip[P.Y * FWidth + P.X] := True;
  for P in Points do
    Skip[P.Y * FWidth + P.X] := True;

  for P in Points do
  begin
    Area.X1 := Max(P.X - Radius, 0);
    Area.Y1 := Max(P.Y - Radius, 0);
    Area.X2 := Min(P.X + Radius, FWidth - 1);
    Area.Y2 := Min(P.Y + Radius, FHeight - 1);

    Count := 0;
    SumR := 0; SumG := 0; SumB := 0;

    for X := Area.X1 to Area.X2 do
      for Y := Area.Y1 to Area.Y2 do
      begin
        if Skip[Y * FWidth + X] then
          Continue;

        with FData[Y * FWidth + X] do
        begin
          Inc(SumR, R);
          Inc(SumG, G);
          Inc(SumB, B);
        end;
        Inc(Count);
      end;

    if (Count > 1) then
    begin
      with Result.Data[P.Y * FWidth + P.X] do
      begin
        R := SumR div Count;
        G := SumG div Count;
        B := SumB div Count;
        A := ALPHA_OPAQUE;
      end;
    end else
      Result.Data[P.Y * FWidth + P.X] := FData[P.Y * FWidth + P.X];
  end;
end;

function TSimbaImage.Downsample(Scale: Integer): TSimbaImage;
begin
  Result := SimbaImage_Downsample(Self, Scale);
end;

function TSimbaImage.Downsample(Scale: Integer; IgnorePoints: TPointArray): TSimbaImage;
begin
  Result := SimbaImage_Downsample(Self, SCale, IgnorePoints);
end;

function TSimbaImage.GetFontAntialiasing: Boolean;
begin
  Result := FTextDrawer.Antialiased;
end;

function TSimbaImage.GetFontName: String;
begin
  Result := FTextDrawer.Font;
end;

class function TSimbaImage.Fonts: TStringArray;
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

function TSimbaImage.GetDrawColorAsBGRA: TColorBGRA;
begin
  if (FDrawColor = -1) then
    Result := TSimbaColorConversion.ColorToBGRA(GetDistinctColor(0), FDrawAlpha)
  else
    Result := TSimbaColorConversion.ColorToBGRA(FDrawColor, FDrawAlpha);
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

procedure TSimbaImage.DrawText(Text: String; Position: TPoint);
begin
  FTextDrawer.DrawText(Text, Position, IfThen(DrawColor > -1, DrawColor, GetDistinctColor(0)));
end;

procedure TSimbaImage.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign);
begin
  FTextDrawer.DrawText(Text, Box, Alignments, IfThen(DrawColor > -1, DrawColor, GetDistinctColor(0)));
end;

procedure TSimbaImage.DrawTextLines(Text: TStringArray; Position: TPoint);
var
  I, LineHeight: Integer;
begin
  LineHeight := TextHeight('TaylorSwift') + 1;

  for I := 0 to High(Text) do
  begin
    DrawText(Text[I], Position);

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

function TSimbaImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TSimbaImage;
begin
  case Algo of
    EImageResizeAlgo.NEAREST_NEIGHBOUR: Result := SimbaImage_ResizeNN(Self, NewWidth, NewHeight);
    EImageResizeAlgo.BILINEAR:          Result := SimbaImage_ResizeBilinear(Self, NewWidth, NewHeight);
    else
      Result := nil;
  end;
end;

function TSimbaImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TSimbaImage;
begin
  Result := Resize(Algo, Trunc(FWidth * Scale), Trunc(FHeight * Scale));
end;

function TSimbaImage.Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TSimbaImage;
begin
  case Algo of
    EImageRotateAlgo.NEAREST_NEIGHBOUR: Result := SimbaImage_RotateNN(Self, Radians, Expand);
    EImageRotateAlgo.BILINEAR:          Result := SimbaImage_RotateBilinear(Self, Radians, Expand);
    else
      Result := nil;
  end;
end;

function TSimbaImage.Blur(Algo: EImageBlurAlgo; Radius: Integer): TSimbaImage;
begin
  case Algo of
    EImageBlurAlgo.BOX:   Result := SimbaImage_BlurBox(Self, Radius);
    EImageBlurAlgo.GAUSS: Result := SimbaImage_BlurGauss(Self, Radius);
    else
      Result := nil;
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

      Result[I] := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
    end;
end;

procedure TSimbaImage.SetPixels(Points: TPointArray; Color: TColor);
var
  BGRA: TColorBGRA;
  I: Integer;
begin
  BGRA := TSimbaColorConversion.ColorToBGRA(Color, ALPHA_OPAQUE);

  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
        RaiseOutOfImageException(X, Y);

      FData[Y * FWidth + X] := BGRA;
    end;
end;

procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);
var
  I: Integer;
begin
  if (Length(Points) <> Length(Colors)) then
    SimbaException('TSimbaImage.SetPixels: Pixel & Color arrays must be same lengths (%d, %d)', [Length(Points), Length(Colors)]);

  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
        RaiseOutOfImageException(X, Y);

      FData[Y * FWidth + X] := TSimbaColorConversion.ColorToBGRA(Colors[I], FDrawAlpha);
    end;
end;

procedure TSimbaImage.DrawData(TheData: PColorBGRA; DataW, DataH: Integer; P: TPoint);
var
  W, H: Integer;
  LoopX, LoopY, DestX, DestY: Integer;
  BGRA: TColorBGRA;
begin
  W := DataW - 1;
  H := DataH - 1;

  for LoopY := 0 to H do
    for LoopX := 0 to W do
    begin
      DestX := LoopX + P.X;
      DestY := LoopY + P.Y;
      if (DestX >= 0) and (DestY >= 0) and (DestX < FWidth) and (DestY < FHeight) then
      begin
        BGRA := TheData[LoopY * DataW + LoopX];
        BGRA.A := ALPHA_OPAQUE;

        FData[DestY * FWidth + DestX] := BGRA;
      end;
    end;
end;

procedure TSimbaImage.DrawDataAlpha(TheData: PColorBGRA; DataW, DataH: Integer; P: TPoint; Alpha: Byte);
var
  W, H: Integer;
  LoopX, LoopY, DestX, DestY: Integer;
  BGRA: TColorBGRA;
begin
  W := DataW - 1;
  H := DataH - 1;

  for LoopY := 0 to H do
    for LoopX := 0 to W do
    begin
      DestX := LoopX + P.X;
      DestY := LoopY + P.Y;
      if (DestX >= 0) and (DestY >= 0) and (DestX < FWidth) and (DestY < FHeight) then
      begin
        BGRA := TheData[LoopY * DataW + LoopX];
        BGRA.A := FDrawAlpha;

        BlendPixel(@FData[DestY * FWidth + DestX], BGRA);
      end;
    end;
end;

procedure TSimbaImage.Pad(Amount: Integer);
begin
  with DetachData() do
  try
    SetSize(Width + (Amount * 2), Height + (Amount * 2));

    DrawData(Data, Width, Height, TPoint.Create(Amount, Amount));
  finally
    FreeMem(Data);
  end;
end;

procedure TSimbaImage.Offset(X, Y: Integer);
begin
  with DetachData() do
  try
    SetSize(Width, Height);

    DrawData(Data, Width, Height, TPoint.Create(X, Y));
  finally
    FreeMem(Data);
  end;
end;

function TSimbaImage.DetachData: TDetachedImageData;
begin
  Result.Data := FData;
  Result.Width := FWidth;
  Result.Height := FHeight;

  FData := nil;
  SetSize(0, 0);
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

  Result := TSimbaColorConversion.BGRAToColor(FData[Y * FWidth + X]);
end;

function TSimbaImage.GetAlpha(const X, Y: Integer): Byte;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfImageException(X, Y);

  Result := FData[Y * FWidth + X].A;
end;

procedure TSimbaImage.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfImageException(X, Y);

  FData[Y * FWidth + X] := TSimbaColorConversion.ColorToBGRA(Color, ALPHA_OPAQUE);
end;

procedure TSimbaImage.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfImageException(X, Y);

  FData[Y * FWidth + X].A := Value;
end;

function TSimbaImage.InImage(const X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight);
end;

procedure TSimbaImage.DrawLineAA(Start, Stop: TPoint; Thickness: Single);
begin
  SimbaImage_DrawLineAA(Self, Start, Stop, Thickness);
end;

procedure TSimbaImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single);
begin
  SimbaImage_DrawEllipseAA(Self, ACenter, XRadius, YRadius, Thickness);
end;

procedure TSimbaImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Thickness: Single);
begin
  DrawEllipseAA(ACenter, Radius, Radius, Thickness);
end;

constructor TSimbaImage.Create;
begin
  inherited Create();

  FTextDrawer := TSimbaTextDrawer.Create(Self);

  DefaultPixel.AsInteger := 0;
  DefaultPixel.A := ALPHA_OPAQUE;

  FDrawColor := -1;
  FDrawAlpha := 255;
end;

constructor TSimbaImage.Create(AWidth, AHeight: Integer);
begin
  Create();

  SetSize(AWidth, AHeight);
end;

constructor TSimbaImage.Create(FileName: String);
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

constructor TSimbaImage.CreateFromMatrix(Mat: TIntegerMatrix);
begin
  Create();

  FromMatrix(Mat);
end;

constructor TSimbaImage.CreateFromMatrix(Mat: TSingleMatrix; ColorMapType: Integer);
begin
  Create();

  FromMatrix(Mat, ColorMapType);
end;

destructor TSimbaImage.Destroy;
begin
  SetSize(0, 0);

  FreeAndNil(FTextDrawer);

  inherited Destroy();
end;

end.

