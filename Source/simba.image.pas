{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base,
  simba.baseclass,
  simba.canvas,
  simba.colormath,
  simba.dtm;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  EImageMirrorStyle   = (WIDTH, HEIGHT, LINE);
  EImageResizeAlgo    = (NEAREST_NEIGHBOUR, BILINEAR, LANCZOS, BOX, HAMMING, BICUBIC);
  EImageRotateAlgo    = (NEAREST_NEIGHBOUR, BILINEAR);
  EImageBlurAlgo      = (BOX, GAUSS);
  EImageThresholdAlgo = (MEAN, WOLF, GAUSSIAN);
  {$POP}

  TSimbaImage = class;
  TSimbaImageArray = array of TSimbaImage;

  PSimbaImage = ^TSimbaImage;

  TSimbaImageCanvas = class(TSimbaCanvas)
  public
    procedure DrawImage(Image: TSimbaImage; Location: TPoint); overload;
  end;

  TSimbaImage = class(TSimbaBaseClass)
  protected
    FWidth: Integer;
    FHeight: Integer;
    FCenter: TPoint;

    FData: PColorBGRA;
    FDataOwner: Boolean;

    FCanvas: TSimbaImageCanvas;

    // The only way FData / FWidth / FHeight / FDataOwner change
    procedure UpdateData(NewData: PColorBGRA; NewWidth, NewHeight: Integer; IsDataOwner: Boolean = True); virtual;

    procedure RaiseExternalData;
    procedure RaiseOutOfBounds(X, Y: Integer); virtual;

    function GetPixelCount: Integer; inline;
    function GetBytesPerRow: SizeInt; inline;
    function GetPixelPtr(X, Y: Integer): PColorBGRA; inline;

    function GetPixel(const X, Y: Integer): TColor; inline;
    function GetAlpha(const X, Y: Integer): Byte; inline;
    procedure SetPixel(const X, Y: Integer; const Color: TColor); inline;
    procedure SetAlpha(const X, Y: Integer; const Value: Byte); inline;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    constructor Create(FileName: String); overload;
    constructor CreateFromString(Str: String);
    constructor CreateFromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer);
    destructor Destroy; override;

    property DataOwner: Boolean read FDataOwner;
    property Data: PColorBGRA read FData;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Center: TPoint read FCenter;
    // Width * Height
    property PixelCount: Integer read GetPixelCount;
    property BytesPerRow: SizeInt read GetBytesPerRow;

    // everything drawing related
    property Canvas: TSimbaImageCanvas read FCanvas;

    // The address of pixel (X, Y) in Data. Not bounds checked.
    property PixelPtr[X, Y: Integer]: PColorBGRA read GetPixelPtr;
    // Color at (X, Y) will raise on out of bounds
    property Pixel[X, Y: Integer]: TColor read GetPixel write SetPixel; default;
    // Alpha at (X, Y) will raise on out of bounds
    property Alpha[X, Y: Integer]: Byte read GetAlpha write SetAlpha;

    function GetPixels(Points: TPointArray): TColorArray;
    procedure SetPixels(Points: TPointArray; Color: TColor); overload;
    procedure SetPixels(Points: TPointArray; Colors: TColorArray); overload;

    function InImage(const X, Y: Integer): Boolean;
    function DataRange(out Lo, Hi: PColorBGRA): Boolean;
    function isBinary: Boolean;
    function isDualColor(out Color1, Color2: TColorBGRA): Boolean;

    procedure SetSize(NewWidth, NewHeight: Integer);
    // NewData = nil takes ownership back, with a fresh allocation of our own.
    procedure SetExternalData(NewData: PColorBGRA; DataWidth, DataHeight: Integer);

    function Copy(Box: TBox): TSimbaImage; overload;
    function Copy: TSimbaImage; overload;
    procedure Crop(Box: TBox);
    procedure Pad(Amount: Integer);
    procedure Offset(X, Y: Integer);

    procedure ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single = 0);
    procedure ReplaceColorBinary(AInvert: Boolean; Color: TColor; Tolerance: Single = 0); overload;
    procedure ReplaceColorBinary(AInvert: Boolean; Colors: TColorArray; Tolerance: Single = 0); overload;

    // Rotate resize flip
    function Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TSimbaImage;
    function Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TSimbaImage; overload;
    function Resize(Algo: EImageResizeAlgo; Scale: Single): TSimbaImage; overload;
    function Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer; IgnorePoints: TPointArray): TSimbaImage; overload;
    function Mirror(Style: EImageMirrorStyle): TSimbaImage;

    // Filters
    function Convolute(Matrix: TDoubleMatrix): TSimbaImage;
    function Sobel: TSimbaImage;
    function GreyScale: TSimbaImage;
    function Brightness(Value: Integer): TSimbaImage;
    function Invert: TSimbaImage;
    function Threshold(Algo: EImageThresholdAlgo; Inv: Boolean = False; Radius: Integer = 10): TSimbaImage; overload;
    function Threshold(Algo: EImageThresholdAlgo; Inv: Boolean; Radius: Integer; C: Single): TSimbaImage; overload;
    function BlendFromSurrounding(Points: TPointArray; Radius: Integer): TSimbaImage; overload;
    function BlendFromSurrounding(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TSimbaImage; overload;
    function Blur(Algo: EImageBlurAlgo; Radius: Single): TSimbaImage;

    // Load & Save
    procedure Load(FileName: String); overload;
    procedure Load(FileName: String; Area: TBox); overload;
    function Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;

    // Conversions
    function ToColors: TColorArray; overload;
    function ToColors(Box: TBox): TColorArray; overload;
    procedure ToChannels(var B,G,R: TByteArray); overload;
    procedure ToChannels(var B,G,R,A: TByteArray); overload;
    function ToGreyMatrix: TByteMatrix;
    function ToMatrix: TIntegerMatrix; overload;
    function ToMatrix(Box: TBox): TIntegerMatrix; overload;
    function ToString: String; override;
    function ToLazBitmap: TBitmap;

    procedure FromChannels(const B,G,R: TByteArray; W, H: Integer); overload;
    procedure FromChannels(const B,G,R,A: TByteArray; W, H: Integer); overload;
    procedure FromMatrix(Matrix: TIntegerMatrix); overload;
    procedure FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0); overload;
    procedure FromLazBitmap(LazBitmap: TBitmap);
    procedure FromZip(ZipFile, ZipEntry: String);
    procedure FromStream(Stream: TStream; Format: String);
    procedure FromResource(ResourceName: String);
    procedure FromString(Str: String);
    procedure FromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer);

    // Compare/Difference
    function Equals(Other: TSimbaImage): Boolean; reintroduce;
    function Compare(Other: TSimbaImage): Single;
    function PixelDifference(Other: TSimbaImage; Tolerance: Single; AOffset: TPoint): TPointArray; overload;
    function PixelDifference(Other: TSimbaImage; Tolerance: Single): TPointArray; overload;

    // Bounds [-1,-1,-1,-1] is the whole image
    function FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray; overload;
    function FindColor(Color: TColorTolerance; Bounds: TBox): TPointArray; overload;
    function FindImage(Image: TSimbaImage; Tolerance: Single; Bounds: TBox): TPoint;
    // every match
    function FindDTM(DTM: TDTM; Bounds: TBox): TPointArray;
    // each pixel's distance from Color: 0 an exact match, 100 as far as the colour space goes
    function MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TSingleMatrix;
  end;

implementation

uses
  Math,
  simba.vartype_matrix,
  simba.vartype_point,
  simba.vartype_box,
  simba.image_utils,
  simba.image_lazbridge,
  simba.image_resizerotate,
  simba.image_resample,
  simba.image_string,
  simba.image_file,
  simba.image_filters,
  simba.image_drawmatrix,
  simba.colormath_distance,
  simba.container_point,
  simba.finder_color,
  simba.finder_image,
  simba.finder_dtm;

const
  // The most pixel data one image may hold
  {$if SizeOf(SizeInt) >= 8}
  MAX_IMAGE_GB = 4;
  {$else}
  MAX_IMAGE_GB = 2;
  {$endif}

  // what every pixel an image gains without a source becomes
  OPAQUE_BLACK: TColorBGRA = (B: 0; G: 0; R: 0; A: ALPHA_OPAQUE);

procedure TSimbaImageCanvas.DrawImage(Image: TSimbaImage; Location: TPoint);
begin
  inherited DrawImage(Image.Data, Image.Width, Image.Height, Location);
end;

procedure TSimbaImage.UpdateData(NewData: PColorBGRA; NewWidth, NewHeight: Integer; IsDataOwner: Boolean);
begin
  if FDataOwner and Assigned(FData) and (FData <> NewData) then
    FreeMem(FData);

  FDataOwner := IsDataOwner;
  FData := NewData;
  FWidth := NewWidth;
  FHeight := NewHeight;
  FCenter := TPoint.Create(FWidth div 2, FHeight div 2);

  FCanvas.SetData(FData, FWidth, FWidth, FHeight);
end;

procedure TSimbaImage.RaiseExternalData;
begin
  SimbaException('Cannot replace external image data.');
end;

procedure TSimbaImage.RaiseOutOfBounds(X, Y: Integer);
begin
  SimbaException('%d,%d is outside the image bounds (0,0,%d,%d)', [X, Y, FWidth - 1, FHeight - 1]);
end;

function TSimbaImage.GetPixelCount: Integer;
begin
  Result := FWidth * FHeight;
end;

function TSimbaImage.GetBytesPerRow: SizeInt;
begin
  Result := SizeInt(FWidth * SizeOf(TColorBGRA));
end;

function TSimbaImage.GetPixelPtr(X, Y: Integer): PColorBGRA;
begin
  Result := @FData[Y * FWidth + X];
end;

function TSimbaImage.GetPixel(const X, Y: Integer): TColor;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  Result := PixelPtr[X, Y]^.ToColor;
end;

function TSimbaImage.GetAlpha(const X, Y: Integer): Byte;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  Result := PixelPtr[X, Y]^.A;
end;

procedure TSimbaImage.SetPixel(const X, Y: Integer; const Color: TColor);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  PixelPtr[X, Y]^ := Color.ToBGRA(ALPHA_OPAQUE);
end;

procedure TSimbaImage.SetAlpha(const X, Y: Integer; const Value: Byte);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    RaiseOutOfBounds(X, Y);

  PixelPtr[X, Y]^.A := Value;
end;

constructor TSimbaImage.Create;
begin
  inherited Create();

  FDataOwner := True;
  FCanvas := TSimbaImageCanvas.Create();

  UpdateData(nil, 0, 0);
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

constructor TSimbaImage.CreateFromString(Str: String);
begin
  Create();

  FromString(Str);
end;

constructor TSimbaImage.CreateFromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer);
begin
  Create();

  FromData(Src, SrcWidth, NewWidth, NewHeight);
end;

destructor TSimbaImage.Destroy;
begin
  UpdateData(nil, 0, 0);
  FreeAndNil(FCanvas);

  inherited Destroy();
end;

function TSimbaImage.GetPixels(Points: TPointArray): TColorArray;
begin
  Result := FCanvas.GetPixels(Points);
end;

procedure TSimbaImage.SetPixels(Points: TPointArray; Color: TColor);
begin
  FCanvas.SetPixels(Points, Color);
end;

procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);
begin
  FCanvas.SetPixels(Points, Colors);
end;

function TSimbaImage.InImage(const X, Y: Integer): Boolean;
begin
  Result := (UInt32(X) < UInt32(FWidth)) and (UInt32(Y) < UInt32(FHeight));
end;

function TSimbaImage.DataRange(out Lo, Hi: PColorBGRA): Boolean;
begin
  Result := (FWidth > 0) and (FHeight > 0);
  if Result then
  begin
    Lo := FData;
    Hi := @FData[PixelCount - 1];
  end;
end;

function TSimbaImage.isBinary: Boolean;
var
  Ptr, Upper: PColorBGRA;
begin
  if DataRange(Ptr, Upper) then
  begin
    while (Ptr <= Upper) and (((Ptr^.R = 0) and (Ptr^.G = 0) and (Ptr^.B = 0)) or ((Ptr^.R = 255) and (Ptr^.G = 255) and (Ptr^.B = 255))) do
      Inc(Ptr);
    Result := Ptr > Upper;
  end else
    Result := False;
end;

function TSimbaImage.isDualColor(out Color1, Color2: TColorBGRA): Boolean;
var
  Ptr, Upper: PColorBGRA;
begin
  Color1 := Default(TColorBGRA);
  Color2 := Default(TColorBGRA);

  if DataRange(Ptr, Upper) then
  begin
    Color1 := Ptr^;
    Color2 := Ptr^;
    while (Ptr <= Upper) and (Ptr^.AsInteger = Color1.AsInteger) do
      Inc(Ptr);

    // the first pixel that differs is the second color
    if (Ptr <= Upper) then
    begin
      Color2 := Ptr^;
      while (Ptr <= Upper) and ((Ptr^.AsInteger = Color1.AsInteger) or (Ptr^.AsInteger = Color2.AsInteger)) do
        Inc(Ptr);
    end;

    Result := Ptr > Upper;
  end else
    Result := False;
end;

procedure TSimbaImage.SetSize(NewWidth, NewHeight: Integer);
var
  NewData: PColorBGRA;
  Bytes: Int64;
begin
  if not FDataOwner then RaiseExternalData();

  Bytes := Int64(NewWidth * NewHeight * SizeOf(TColorBGRA));
  if (NewWidth < 0) or (NewHeight < 0) or (Bytes > MAX_IMAGE_GB * 1024 * 1024 * 1024) then
    SimbaException('TImage.SetSize: Invalid size %dx%d', [NewWidth, NewHeight]);

  if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
  begin
    if (NewWidth * NewHeight <> 0) then
    begin
      NewData := GetMem(SizeInt(NewWidth * NewHeight * SizeOf(TColorBGRA)));
      if (NewData = nil) then
        SimbaException('TImage.SetSize: Out of memory for %dx%d', [NewWidth, NewHeight]);

      FillData(NewData, NewWidth * NewHeight, OPAQUE_BLACK);
    end else
      NewData := nil;

    if Assigned(FData) and Assigned(NewData) and (PixelCount <> 0) then
      CopyRows(NewData, NewWidth * SizeOf(TColorBGRA), FData, BytesPerRow, Min(NewWidth, FWidth), Min(NewHeight, FHeight));

    UpdateData(NewData, NewWidth, NewHeight);
  end;
end;

procedure TSimbaImage.SetExternalData(NewData: PColorBGRA; DataWidth, DataHeight: Integer);
begin
  UpdateData(nil, 0, 0, NewData = nil);

  if FDataOwner then
  begin
    SetSize(DataWidth, DataHeight);
    Exit;
  end;

  UpdateData(NewData, DataWidth, DataHeight, False);
end;

function TSimbaImage.Copy(Box: TBox): TSimbaImage;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfBounds(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfBounds(Box.X2, Box.Y2);

  Result := TSimbaImage.Create();
  Result.SetSize(Box.Width, Box.Height);
  CopyRows(Result.Data, Result.BytesPerRow, PixelPtr[Box.X1, Box.Y1], BytesPerRow, Box.Width, Box.Height);
end;

function TSimbaImage.Copy: TSimbaImage;
begin
  Result := TSimbaImage.Create();
  Result.SetSize(FWidth, FHeight);

  MoveData(Result.FData, FData, PixelCount);
end;

procedure TSimbaImage.Crop(Box: TBox);
begin
  if not FDataOwner then
    RaiseExternalData();
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfBounds(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfBounds(Box.X2, Box.Y2);

  CopyRows(FData, BytesPerRow, PixelPtr[Box.X1, Box.Y1], BytesPerRow, Box.Width, Box.Height);
  SetSize(Box.Width, Box.Height);
end;

procedure TSimbaImage.Pad(Amount: Integer);
var
  NewData: PColorBGRA;
  NewWidth, NewHeight, Row: Integer;
begin
  if not FDataOwner then RaiseExternalData();

  if (Amount <= 0) then
    Exit;

  NewWidth := FWidth + (Amount * 2);
  NewHeight := FHeight + (Amount * 2);

  NewData := GetMem(SizeInt(NewWidth * NewHeight * SizeOf(TColorBGRA)));

  FillData(@NewData[0], SizeInt(Amount * NewWidth), OPAQUE_BLACK);
  FillData(@NewData[SizeInt((Amount + FHeight) * NewWidth)], SizeInt(Amount * NewWidth), OPAQUE_BLACK);
  for Row := Amount to Amount + FHeight - 1 do
  begin
    FillData(@NewData[SizeInt(Row * NewWidth)], Amount, OPAQUE_BLACK);
    FillData(@NewData[SizeInt(Row * NewWidth + Amount + FWidth)], Amount, OPAQUE_BLACK);
  end;

  CopyRows(@NewData[SizeInt(Amount * NewWidth + Amount)], NewWidth * SizeOf(TColorBGRA), FData, BytesPerRow, FWidth, FHeight);

  UpdateData(NewData, NewWidth, NewHeight);
end;

procedure TSimbaImage.Offset(X, Y: Integer);
var
  NewData: PColorBGRA;
  SrcX, SrcY, DstX, DstY, CopyW, CopyH, Row: Integer;
begin
  if not FDataOwner then RaiseExternalData();

  if (FWidth = 0) or (FHeight = 0) or ((X = 0) and (Y = 0)) then
    Exit;

  NewData := GetMem(SizeInt(FWidth * FHeight * SizeOf(TColorBGRA)));

  // the part of our pixels that is still on the image once shifted
  SrcX := Max(-X, 0);
  SrcY := Max(-Y, 0);
  DstX := Max(X, 0);
  DstY := Max(Y, 0);
  CopyW := FWidth - Abs(X);
  CopyH := FHeight - Abs(Y);

  if (CopyW <= 0) or (CopyH <= 0) then // all off the image
    FillData(NewData, PixelCount, OPAQUE_BLACK)
  else
  begin
    if (DstY > 0) then                 // above the shifted pixels
      FillData(@NewData[0], SizeInt(DstY * FWidth), OPAQUE_BLACK);
    if (DstY + CopyH < FHeight) then   // below them
      FillData(@NewData[SizeInt((DstY + CopyH) * FWidth)], SizeInt((FHeight - DstY - CopyH) * FWidth), OPAQUE_BLACK);

    for Row := DstY to DstY + CopyH - 1 do
    begin
      if (DstX > 0) then               // left gap on this row
        FillData(@NewData[SizeInt(Row * FWidth)], DstX, OPAQUE_BLACK);
      if (DstX + CopyW < FWidth) then  // right gap on this row
        FillData(@NewData[SizeInt(Row * FWidth + DstX + CopyW)], FWidth - DstX - CopyW, OPAQUE_BLACK);
    end;

    CopyRows(@NewData[SizeInt(DstY * FWidth + DstX)], BytesPerRow, PixelPtr[SrcX, SrcY], BytesPerRow, CopyW, CopyH);
  end;

  UpdateData(NewData, FWidth, FHeight);
end;

procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single);
begin
  SimbaImage_ReplaceColor(Self, OldColor, NewColor, Tolerance);
end;

procedure TSimbaImage.ReplaceColorBinary(AInvert: Boolean; Color: TColor; Tolerance: Single);
begin
  SimbaImage_ReplaceColorBinary(Self, AInvert, [Color], Tolerance);
end;

procedure TSimbaImage.ReplaceColorBinary(AInvert: Boolean; Colors: TColorArray; Tolerance: Single);
begin
  SimbaImage_ReplaceColorBinary(Self, AInvert, Colors, Tolerance);
end;

function TSimbaImage.Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TSimbaImage;
begin
  Result := nil;
  case Algo of
    EImageRotateAlgo.NEAREST_NEIGHBOUR: Result := SimbaImage_RotateNN(Self, Radians, Expand);
    EImageRotateAlgo.BILINEAR:          Result := SimbaImage_RotateBilinear(Self, Radians, Expand);
    else
      SimbaException('TImage.Rotate: Unknown algorithm (%d)', [Ord(Algo)]);
  end;
end;

function TSimbaImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TSimbaImage;
begin
  Result := SimbaImage_Resample(Self, NewWidth, NewHeight, Algo);
end;

function TSimbaImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TSimbaImage;
begin
  Result := Resize(Algo, Trunc(FWidth * Scale), Trunc(FHeight * Scale));
end;

function TSimbaImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer; IgnorePoints: TPointArray): TSimbaImage;
var
  Ignore: TBooleanArray;
  P: TPoint;
begin
  if (Length(IgnorePoints) = 0) then
    Exit(Resize(Algo, NewWidth, NewHeight)); // nothing ignored -> the fast unmasked path

  SetLength(Ignore, PixelCount);
  for P in IgnorePoints do
    if (P.X >= 0) and (P.Y >= 0) and (P.X < FWidth) and (P.Y < FHeight) then
      Ignore[P.Y * FWidth + P.X] := True;

  Result := SimbaImage_ResampleMasked(Self, NewWidth, NewHeight, Algo, Ignore);
end;

function TSimbaImage.Mirror(Style: EImageMirrorStyle): TSimbaImage;
begin
  Result := SimbaImage_Mirror(Self, Style);
end;

function TSimbaImage.Convolute(Matrix: TDoubleMatrix): TSimbaImage;
begin
  Result := SimbaImage_Convolute(Self, Matrix);
end;

function TSimbaImage.Sobel: TSimbaImage;
begin
  Result := SimbaImage_Sobel(Self);
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

function TSimbaImage.Threshold(Algo: EImageThresholdAlgo; Inv: Boolean; Radius: Integer): TSimbaImage;
begin
  Result := nil;
  case Algo of // each takes the C or K it is normally used with
    EImageThresholdAlgo.MEAN:     Result := SimbaImage_ThresholdMean(Self, Inv, Radius);
    EImageThresholdAlgo.WOLF:     Result := SimbaImage_ThresholdWolf(Self, Inv, Radius);
    EImageThresholdAlgo.GAUSSIAN: Result := SimbaImage_ThresholdGaussian(Self, Inv, Radius);
    else
      SimbaException('TImage.Threshold: Unknown algorithm (%d)', [Ord(Algo)]);
  end;
end;

function TSimbaImage.Threshold(Algo: EImageThresholdAlgo; Inv: Boolean; Radius: Integer; C: Single): TSimbaImage;
begin
  Result := nil;
  case Algo of
    EImageThresholdAlgo.MEAN:     Result := SimbaImage_ThresholdMean(Self, Inv, Radius, C);
    EImageThresholdAlgo.WOLF:     Result := SimbaImage_ThresholdWolf(Self, Inv, Radius, C);
    EImageThresholdAlgo.GAUSSIAN: Result := SimbaImage_ThresholdGaussian(Self, Inv, Radius, C);
    else
      SimbaException('TImage.Threshold: Unknown algorithm (%d)', [Ord(Algo)]);
  end;
end;

function TSimbaImage.BlendFromSurrounding(Points: TPointArray; Radius: Integer): TSimbaImage;
begin
  Result := SimbaImage_BlendFromSurrounding(Self, Points, Radius, []);
end;

function TSimbaImage.BlendFromSurrounding(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TSimbaImage;
begin
  Result := SimbaImage_BlendFromSurrounding(Self, Points, Radius, IgnorePoints);
end;

function TSimbaImage.Blur(Algo: EImageBlurAlgo; Radius: Single): TSimbaImage;
begin
  Result := nil;
  case Algo of
    EImageBlurAlgo.BOX:   Result := SimbaImage_BlurBox(Self, Radius);
    EImageBlurAlgo.GAUSS: Result := SimbaImage_BlurGauss(Self, Radius);
    else
      SimbaException('TImage.Blur: Unknown algorithm (%d)', [Ord(Algo)]);
  end;
end;

procedure TSimbaImage.Load(FileName: String);
begin
  SimbaImage_Load(Self, FileName);
end;

procedure TSimbaImage.Load(FileName: String; Area: TBox);
begin
  SimbaImage_LoadArea(Self, FileName, Area.X1, Area.Y1, Area.X2, Area.Y2);
end;

function TSimbaImage.Save(FileName: String; OverwriteIfExists: Boolean): Boolean;
begin
  Result := SimbaImage_Save(Self, FileName, OverwriteIfExists);
end;

function TSimbaImage.ToColors: TColorArray;
begin
  SetLength(Result, PixelCount);
  BGRAToColors(FData, PInt32(Result), Length(Result));
end;

function TSimbaImage.ToColors(Box: TBox): TColorArray;
var
  Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfBounds(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfBounds(Box.X2, Box.Y2);

  SetLength(Result, Box.Width * Box.Height);
  for Y := Box.Y1 to Box.Y2 do
    BGRAToColors(PixelPtr[Box.X1, Y], PInt32(@Result[(Y - Box.Y1) * Box.Width]), Box.Width);
end;

procedure TSimbaImage.ToChannels(var B,G,R: TByteArray);
begin
  SetLength(B, PixelCount);
  SetLength(G, PixelCount);
  SetLength(R, PixelCount);
  SplitChannels(FData, PixelCount, PByte(B), PByte(G), PByte(R), nil);
end;

procedure TSimbaImage.ToChannels(var B,G,R,A: TByteArray);
begin
  SetLength(B, PixelCount);
  SetLength(G, PixelCount);
  SetLength(R, PixelCount);
  SetLength(A, PixelCount);
  SplitChannels(FData, PixelCount, PByte(B), PByte(G), PByte(R), PByte(A));
end;

function TSimbaImage.ToGreyMatrix: TByteMatrix;
var
  Y: Integer;
begin
  Result.SetSize(FWidth, FHeight);

  for Y := 0 to FHeight - 1 do
    GreyData(PixelPtr[0, Y], PByte(Result[Y]), FWidth);
end;

function TSimbaImage.ToMatrix: TIntegerMatrix;
var
  Y: Integer;
begin
  Result.SetSize(Width, Height);

  for Y := 0 to FHeight - 1 do
    BGRAToColors(PixelPtr[0, Y], PInt32(Result[Y]), FWidth);
end;

function TSimbaImage.ToMatrix(Box: TBox): TIntegerMatrix;
var
  Y: Integer;
begin
  if (not InImage(Box.X1, Box.Y1)) then RaiseOutOfBounds(Box.X1, Box.Y1);
  if (not InImage(Box.X2, Box.Y2)) then RaiseOutOfBounds(Box.X2, Box.Y2);

  Result.SetSize(Box.Width, Box.Height);

  for Y := Box.Y1 to Box.Y2 do
    BGRAToColors(PixelPtr[Box.X1, Y], PInt32(Result[Y - Box.Y1]), Box.Width);
end;

function TSimbaImage.ToString: String;
begin
  Result := SimbaImage_ToString(Self);
end;

function TSimbaImage.ToLazBitmap: TBitmap;
begin
  Result := SimbaImage_ToLazImage(Self);
end;

procedure TSimbaImage.FromChannels(const B,G,R: TByteArray; W, H: Integer);
begin
  if (Length(B) <> W*H) or (Length(G) <> W*H) or (Length(R) <> W*H) then
    SimbaException('Channel size does not match image size');

  SetSize(W, H);
  MergeChannels(FData, PixelCount, PByte(B), PByte(G), PByte(R), nil, ALPHA_OPAQUE);
end;

procedure TSimbaImage.FromChannels(const B,G,R,A: TByteArray; W, H: Integer);
begin
  if (Length(B) <> W*H) or (Length(G) <> W*H) or (Length(R) <> W*H) or (Length(A) <> W*H) then
    SimbaException('Channel size does not match image size');

  SetSize(W, H);
  MergeChannels(FData, PixelCount, PByte(B), PByte(G), PByte(R), PByte(A), ALPHA_OPAQUE);
end;

procedure TSimbaImage.FromMatrix(Matrix: TIntegerMatrix);
var
  W, H: Integer;
begin
  Matrix.GetSize(W, H);
  SetSize(W, H);
  SimbaImage_DrawMatrix(Matrix, Data);
end;

procedure TSimbaImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0);
var
  W, H: Integer;
  Normed: TSingleMatrix;
begin
  Matrix.GetSize(W, H);
  SetSize(W, H);

  Normed := Matrix.Copy();
  Normed.NormMinMax(0, 1);
  SimbaImage_DrawMatrix(Normed, ColorMapType, Data);
end;

procedure TSimbaImage.FromLazBitmap(LazBitmap: TBitmap);
var
  TempBitmap: TSimbaImage;
begin
  SetSize(0, 0);

  TempBitmap := LazImage_ToSimbaImage(LazBitmap);

  UpdateData(TempBitmap.Data, TempBitmap.Width, TempBitmap.Height);

  TempBitmap.FData := nil; // data is now ours
  TempBitmap.Free();
end;

procedure TSimbaImage.FromZip(ZipFile, ZipEntry: String);
begin
  SimbaImage_LoadZip(Self, ZipFile, ZipEntry);
end;

procedure TSimbaImage.FromStream(Stream: TStream; Format: String);
begin
  SimbaImage_LoadStream(Self, Stream, Format);
end;

procedure TSimbaImage.FromResource(ResourceName: String);
begin
  SimbaImage_LoadResource(Self, ResourceName);
end;

procedure TSimbaImage.FromString(Str: String);
begin
  SimbaImage_FromString(Self, Str);
end;

procedure TSimbaImage.FromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer);
begin
  SetSize(NewWidth, NewHeight);
  if (Src = nil) then
    Exit;

  CopyRows(FData, BytesPerRow, Src, SrcWidth * SizeOf(TColorBGRA), FWidth, FHeight);
end;

// Compare without alpha
function TSimbaImage.Equals(Other: TSimbaImage): Boolean;
var
  Ptr, Upper, OtherPtr: PColorBGRA;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    Exit(False);

  Result := True;
  if DataRange(Ptr, Upper) then
  begin
    OtherPtr := Other.Data;
    while (Ptr <= Upper) do
    begin
      if not Ptr^.EqualsIgnoreAlpha(OtherPtr^) then
        Exit(False);

      Inc(Ptr);
      Inc(OtherPtr);
    end;
  end;
end;

// TM_CCOEFF_NORMED
// Author: slackydev
function TSimbaImage.Compare(Other: TSimbaImage): Single;
var
  N: Int64;
  sumIR, sumIG, sumIB, sumTR, sumTG, sumTB: Double;
  meanIR, meanIG, meanIB, meanTR, meanTG, meanTB: Double;
  dIR, dIG, dIB, dTR, dTG, dTB: Double;
  cross, isum, tsum: Double;
  Ptr, Upper, OtherPtr: PColorBGRA;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.Compare: Both images must be equal dimensions');

  if not DataRange(Ptr, Upper) then
    Exit(0);
  N := PixelCount;

  // pass 1: per-channel means for both images (Double accumulators, exact for byte data)
  sumIR := 0; sumIG := 0; sumIB := 0;
  sumTR := 0; sumTG := 0; sumTB := 0;
  OtherPtr := Other.Data;
  while (Ptr <= Upper) do
  begin
    sumIR += Ptr^.R;
    sumIG += Ptr^.G;
    sumIB += Ptr^.B;
    sumTR += OtherPtr^.R;
    sumTG += OtherPtr^.G;
    sumTB += OtherPtr^.B;
    Inc(Ptr);
    Inc(OtherPtr);
  end;
  meanIR := sumIR / N; meanIG := sumIG / N; meanIB := sumIB / N;
  meanTR := sumTR / N; meanTG := sumTG / N; meanTB := sumTB / N;

  // pass 2: TM_CCOEFF_NORMED, channels combined (centered cross-correlation / norms)
  cross := 0; isum := 0; tsum := 0;
  Ptr := Data;
  OtherPtr := Other.Data;
  while (Ptr <= Upper) do
  begin
    dIR := Ptr^.R - meanIR;
    dIG := Ptr^.G - meanIG;
    dIB := Ptr^.B - meanIB;
    dTR := OtherPtr^.R - meanTR;
    dTG := OtherPtr^.G - meanTG;
    dTB := OtherPtr^.B - meanTB;
    cross += dIR*dTR + dIG*dTG + dIB*dTB;
    isum  += dIR*dIR + dIG*dIG + dIB*dIB;
    tsum  += dTR*dTR + dTG*dTG + dTB*dTB;
    Inc(Ptr);
    Inc(OtherPtr);
  end;

  if (isum = 0) or (tsum = 0) then // a solid (zero-variance) image -> correlation undefined
    Exit(0);
  Result := cross / Sqrt(isum * tsum);
end;

function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Single; AOffset: TPoint): TPointArray;
var
  P1, P2: PColorBGRA;
  X, Y: Integer;
  Buffer: TPointBuffer;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    SimbaException('TSimbaImage.PixelDifference: Both images must be equal dimensions');

  P1 := Data;
  P2 := Other.Data;
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      if not SimilarRGB(P1^, P2^, Tolerance) then
        Buffer.Add(X + AOffset.X, Y + AOffset.Y);

      Inc(P1);
      Inc(P2);
    end;

  Result := Buffer.ToArray(False);
end;

function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Single): TPointArray;
begin
  Result := PixelDifference(Other, Tolerance, TPoint.ZERO);
end;

function TSimbaImage.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray;
begin
  Result := FindColor(TColorTolerance.Create(Color, Tolerance, EColorSpace.RGB, DefaultMultipliers), Bounds);
end;

function TSimbaImage.FindColor(Color: TColorTolerance; Bounds: TBox): TPointArray;
begin
  Result := [];

  if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    Bounds := TBox.Create(0, 0, FWidth-1, FHeight-1)
  else
    Bounds := TBox.Create(Max(Bounds.X1, 0), Max(Bounds.Y1, 0), Min(Bounds.X2, FWidth-1), Min(Bounds.Y2, FHeight-1)); // outside the image: no width

  if (Bounds.Width > 0) and (Bounds.Height > 0) then
    Result := SimbaFinder_FindColors(PixelPtr[Bounds.X1, Bounds.Y1], FWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft,
                                     Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
end;

function TSimbaImage.FindDTM(DTM: TDTM; Bounds: TBox): TPointArray;
begin
  Result := [];

  if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    Bounds := TBox.Create(0, 0, FWidth-1, FHeight-1)
  else
    Bounds := TBox.Create(Max(Bounds.X1, 0), Max(Bounds.Y1, 0), Min(Bounds.X2, FWidth-1), Min(Bounds.Y2, FHeight-1)); // outside the image: no width

  if (Bounds.Width > 0) and (Bounds.Height > 0) then
    Result := SimbaFinder_FindDTM(PixelPtr[Bounds.X1, Bounds.Y1], FWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft, DTM, -1);
end;

function TSimbaImage.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TSingleMatrix;
begin
  Result := [];

  if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    Bounds := TBox.Create(0, 0, FWidth-1, FHeight-1)
  else
    Bounds := TBox.Create(Max(Bounds.X1, 0), Max(Bounds.Y1, 0), Min(Bounds.X2, FWidth-1), Min(Bounds.Y2, FHeight-1)); // outside the image: no width

  if (Bounds.Width > 0) and (Bounds.Height > 0) then
    Result := SimbaFinder_MatchColors(PixelPtr[Bounds.X1, Bounds.Y1], FWidth, Bounds.Width, Bounds.Height, ColorSpace, Color, Multipliers);
end;

function TSimbaImage.FindImage(Image: TSimbaImage; Tolerance: Single; Bounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  Result := TPoint.Create(-1, -1);

  if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    Bounds := TBox.Create(0, 0, FWidth-1, FHeight-1)
  else
    Bounds := TBox.Create(Max(Bounds.X1, 0), Max(Bounds.Y1, 0), Min(Bounds.X2, FWidth-1), Min(Bounds.Y2, FHeight-1)); // outside the image: no width

  if (Bounds.Width > 0) and (Bounds.Height > 0) then
  begin
    TPA := SimbaFinder_FindImage(PixelPtr[Bounds.X1, Bounds.Y1], FWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft,
                                 Image, EColorSpace.RGB, Tolerance, DefaultMultipliers, 1);
    if (Length(TPA) > 0) then
      Result := TPA[0];
  end;
end;

end.


