{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  classes, sysutils, graphtype, graphics,
  simba.baseclass, simba.mufasatypes, simba.bitmap_textdrawer;

type
  PBmpMirrorStyle = ^TBmpMirrorStyle;
  TBmpMirrorStyle = (MirrorWidth, MirrorHeight, MirrorLine);

  PBmpThreshMethod = ^TBmpThreshMethod;
  TBmpThreshMethod = (TM_Mean, TM_MinMax);

  PMufasaBitmap = ^TMufasaBitmap;
  TMufasaBitmap = class(TSimbaBaseClass)
  protected
    FWidth: Integer;
    FHeight: Integer;

    FTransparentColorActive: Boolean;
    FTransparentRGB: TRGB32;
    FTransparentColor: Integer;

    FData: PRGB32;

    FExternalData: Boolean;
    FTextDrawer: TSimbaTextDrawer;
    FClient: TObject;

    procedure NotifyUnfreed; override;

    function GetPixel(X, Y: Integer): Integer;
    function GetCenter: TPoint;
    function GetFontAntialiasing: Boolean;
    function GetFontName: String;
    function GetFonts: TStringArray;
    function GetFontSize: Single;

    procedure SetPixel(X, Y: Integer; Color: Integer);
    procedure SetTransparentColor(Value: Integer);
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
  public
    class var DebugUnfreedBitmaps: ShortString;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    constructor CreateFromClient(Client: TObject); overload;
    constructor CreateFromClient(Client: TObject; Area: TBox); overload;
    constructor CreateFromFile(FileName: String);
    constructor CreateFromString(AWidth, AHeight: Integer; Str: String);
    constructor CreateFromData(AWidth, AHeight: Integer; AData: PRGB32; CopyData: Boolean = True);

    destructor Destroy; override;

    property Client: TObject read FClient write FClient;
    property Data: PRGB32 read FData write FData;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Center: TPoint read GetCenter;

    property TransparentColorActive: Boolean read FTransparentColorActive write FTransparentColorActive;
    property TransparentColor: Integer read FTransparentColor write SetTransparentColor;
    property TransparentRGB: TRGB32 read FTransparentRGB;

    property Pixel[X, Y: Integer]: Integer read GetPixel write SetPixel; default;

    property Fonts: TStringArray read GetFonts;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;

    function PointInBitmap(const P: TPoint): Boolean; overload;
    function PointInBitmap(const X, Y: Integer): Boolean; overload;

    function Equals(Other: TObject): Boolean; override;
    function Equals(Other: TMufasaBitmap): Boolean; overload;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint; Color: Integer); overload;
    procedure DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: Integer); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint; Color: Integer);

    procedure SetSize(NewWidth, NewHeight: Integer);

    procedure Resize(NewWidth, NewHeight: Integer);
    procedure ResizeBilinear(NewWidth, NewHeight: Integer);

    function GetPixels(Points: TPointArray): TIntegerArray;
    procedure SetPixels(Points: TPointArray; Colors: TIntegerArray);

    procedure SetPersistentMemory(mem: PtrUInt; NewWidth, NewHeight: Integer);
    procedure ResetPersistentMemory;

    procedure DrawATPA(ATPA: T2DPointArray); overload;
    procedure DrawATPA(ATPA: T2DPointArray; Color: Integer); overload;
    procedure DrawTPA(Points: TPointArray; Color: Integer);

    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: Integer);
    procedure DrawCross(ACenter: TPoint; Radius: Integer; Color: Integer);

    procedure DrawLine(Start, Stop: TPoint; Color: Integer);

    procedure DrawPolygon(Points: TPointArray; Color: Integer);
    procedure DrawPolygonFilled(Points: TPointArray; Color: Integer);
    procedure DrawPolygonInverted(Points: TPointArray; Color: Integer);

    procedure DrawCircle(ACenter: TPoint; Radius: Integer; Color: Integer);
    procedure DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: Integer);
    procedure DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: Integer);

    procedure DrawBox(B: TBox; Color: Integer);
    procedure DrawBoxFilled(B: TBox; Color: Integer);
    procedure DrawBoxInverted(B: TBox; Color: Integer);

    procedure DrawQuad(Quad: TQuad; Color: Integer);
    procedure DrawQuadFilled(Quad: TQuad; Color: Integer);
    procedure DrawQuadInverted(Quad: TQuad; Color: Integer);

    procedure DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer = -1);
    procedure DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer = -1);
    procedure DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer = -1);
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer; Color: Integer = -1);

    procedure DrawHSLCircle(ACenter: TPoint; Radius: Integer);

    function AverageBrightness: Integer;
    function PeakBrightness: Integer;

    procedure Fill(Color: Integer);

    procedure Clear;
    procedure Clear(Area: TBox);
    procedure ClearInverted(Area: TBox);

    procedure DrawClient(Area: TBox; Position: TPoint); overload;
    procedure DrawClient(Position: TPoint); overload;
    procedure DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);

    procedure DrawToCanvas(x,y: Integer; Canvas: TCanvas);
    function GetColors: TIntegerArray;
    procedure ReplaceColor(OldColor, NewColor: Integer);
    procedure ReplaceColors(OldColors, NewColors: TIntegerArray);
    procedure Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap); overload;
    procedure RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap); overload;
    function Rotate(Radians: Single; Expand: Boolean): TMufasaBitmap; overload;
    function RotateBilinear(Radians: Single; Expand: Boolean): TMufasaBitmap; overload;

    procedure Desaturate(TargetBitmap: TMufasaBitmap); overload;
    procedure Desaturate; overload;
    procedure GreyScale(TargetBitmap: TMufasaBitmap); overload;
    procedure GreyScale; overload;
    procedure Brightness(TargetBitmap: TMufasaBitmap; br: Integer); overload;
    procedure Brightness(br: Integer); overload;
    procedure Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload;
    procedure Contrast(co: Extended); overload;
    procedure Invert(TargetBitmap: TMufasaBitmap); overload;
    procedure Invert; overload;
    procedure Posterize(TargetBitmap: TMufasaBitmap; Po: Integer); overload;
    procedure Posterize(Po: Integer); overload;
    procedure Convolute(TargetBitmap: TMufasaBitmap; Matrix: TDoubleMatrix);

    procedure Mirror(MirrorStyle: TBmpMirrorStyle); overload;
    procedure Mirror(TargetBitmap: TMufasaBitmap; MirrorStyle: TBmpMirrorStyle); overload;

    procedure Blur(TargetBitmap: TMufasaBitmap; Block: Integer); overload;
    procedure Blur(Block: Integer); overload;

    procedure Blend(Points: TPointArray; Radius: Integer); overload;
    procedure Blend(TargetBitmap: TMufasaBitmap; Points: TPointArray; Radius: Integer); overload;

    procedure Downsample(Scale: Integer); overload;
    procedure Downsample(TargetBitmap: TMufasaBitmap; Scale: Integer); overload;

    function Copy(X1, Y1, X2, Y2: Integer): TMufasaBitmap; overload;
    function Copy: TMufasaBitmap; overload;

    procedure Crop(X1, Y1, X2, Y2: Integer);

    function ToTBitmap: TBitmap;

    function ToMatrixBGR: TIntegerMatrix;
    function ToMatrix: TIntegerMatrix; overload;
    function ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload;
    function ToGreyMatrix: TByteMatrix;
    function ToRawImage: TRawImage;

    procedure DrawMatrix(Matrix: TByteMatrix); overload;
    procedure DrawMatrix(Matrix: TIntegerMatrix); overload;
    procedure DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload;

    procedure ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: TBmpThreshMethod; K: Integer);
    procedure ThresholdSauvola(Window: Integer; AInvert: Boolean; K: Single);

    procedure Pad(Amount: Integer);
    function RowPtrs: TPRGB32Array;

    function SaveToFile(FileName: String): Boolean;
    function SaveToString: String;

    procedure LoadFromFile(FileName: String); overload;
    procedure LoadFromFile(FileName: String; Area: TBox); overload;

    procedure LoadFromClient; overload;
    procedure LoadFromClient(Area: TBox); overload;
    procedure LoadFromString(AWidth, AHeight: Integer; Str: String);
    procedure LoadFromData(AWidth, AHeight: Integer; AData: PRGB32; CopyData: Boolean = True);
    procedure LoadFromTBitmap(bmp: TBitmap);
    procedure LoadFromBitmap(Bitmap: TMufasaBitmap);
    procedure LoadFromRawImage(RawImage: TRawImage);

    function Compare(Other: TMufasaBitmap): Single;

    function PixelDifference(Other: TMufasaBitmap): Integer; overload;
    function PixelDifference(Other: TMufasaBitmap; Tolerance: Integer): Integer; overload;
    function PixelDifferenceTPA(Other: TMufasaBitmap): TPointArray; overload;
    function PixelDifferenceTPA(Other: TMufasaBitmap; Tolerance: Integer): TPointArray; overload;
    function PixelEdgesTPA(MinDiff: Integer): TPointArray;
  end;

  TMufasaBitmapArray = array of TMufasaBitmap;

type
  ESimbaBitmapException = class(Exception);

const
  sbeOutOfBounds             = 'Point (%d, %d) is not within the bounds of the bitmap (0, 0, %d, %d)';
  sbeFileNotFound            = 'File not found "%s"';
  sbeResizeExternalData      = 'Cannot resize bitmap with external data';
  sbeImageFormatNotSupported = 'Image format "%s" not supported';
  sbeInvalidClient           = 'Invalid client';
  sbeInvalidBitmapString     = 'Invalid bitmap string';
  sbeMustBeEqualDimensions   = 'Bitmaps must be equal dimensions';
  sbeMustBeEqualLengths      = 'Arrays must be equal lengths';

implementation

uses
  fpimage, math, intfgraphics, simba.overallocatearray, simba.geometry,
  simba.tpa, simba.colormath, simba.client, simba.iomanager,
  simba.bitmap_misc, simba.encoding, simba.compress, simba.math;

function GetDrawColor(Color, Index: Integer): Integer; inline;
const
  BRIGHT_COLORS: TIntegerArray = (clRed, clBlue, clGreen, clLime, clMaroon, clYellow, clAqua, clWhite, clPurple, $0099FF, $B3A2EA); // Orange, Pink
begin
  if (Color > -1) then
    Result := Color
  else
    Result := BRIGHT_COLORS[Index mod Length(BRIGHT_COLORS)];
end;

function TMufasaBitmap.SaveToFile(FileName: String): Boolean;
var
  Image: TLazIntfImage;
  Stream: TFileStream;
  WriterClass: TFPCustomImageWriterClass;
  Writer: TFPCustomImageWriter;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    Writer := nil;
    Stream := nil;
    Image  := nil;

    WriterClass := TFPCustomImage.FindWriterFromFileName(FileName);
    if (WriterClass = nil) then
      raise Exception.Create('Unknown image format: ' + FileName);
    Writer := WriterClass.Create();

    try
      Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);

      Image := TLazIntfImage.Create(Self.ToRawImage(), False);
      Image.SaveToStream(Stream, Writer);

      Result := True;
    except
    end;

    if (Writer <> nil) then
      FreeAndNil(Writer);
    if (Stream <> nil) then
      FreeAndNil(Stream);
    if (Image <> nil) then
      FreeAndNil(Image);
  end;
end;

procedure TMufasaBitmap.LoadFromFile(FileName: String);
var
  LazIntf: TLazIntfImage;
  RawImageDesc: TRawImageDescription;
begin
  try
    LazIntf := TLazIntfImage.Create(0, 0);
    RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width, LazIntf.Height); // TRGB32 image
    LazIntf.DataDescription := RawImageDesc;
    LazIntf.LoadFromFile(FileName);

    LoadFromData(LazIntf.Width, LazIntf.Height, PRGB32(LazIntf.PixelData));
  finally
    LazIntf.Free;
  end;
end;

procedure TMufasaBitmap.LoadFromFile(FileName: String; Area: TBox);
begin
  if (not FileExists(FileName)) then
    raise ESimbaBitmapException.CreateFmt(sbeFileNotFound, [FileName]);

  if FileName.EndsWith('.bmp', False) then
    LoadBitmapAreaFromFile(Self, FileName, Area)
  else
  begin
    LoadFromFile(FileName);

    Area.Clip(TBox.Create(0, 0, FWidth - 1, FHeight - 1));
    if (Area.Width > 1) and (Area.Height > 1) then
      Crop(Area.X1, Area.Y1, Area.X2, Area.Y2);
  end;
end;

function TMufasaBitmap.Copy: TMufasaBitmap;
begin
  Result := TMufasaBitmap.Create();
  Result.Client := FClient;
  Result.SetSize(FWidth, FHeight);

  Move(FData^, Result.FData^, FWidth * FHeight * SizeOf(TRGB32));
end;

function TMufasaBitmap.Copy(X1, Y1, X2, Y2: Integer): TMufasaBitmap;
var
  Y: Integer;
begin
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X1, Y1, FWidth, FHeight]);
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X2, Y2, FWidth, FHeight]);

  Result := TMufasaBitmap.Create();
  Result.Client := FClient;
  Result.SetSize(X2-X1+1, Y2-Y1+1);
  for Y := Y1 to Y2 do
    Move(FData[Y * FWidth + X1], Result.FData[(Y-Y1) * Result.FWidth], Result.FWidth * SizeOf(TRGB32));
end;

procedure TMufasaBitmap.Crop(X1, Y1, X2, Y2: Integer);
var
  Y: Integer;
begin
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X1, Y1, FWidth, FHeight]);
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X2, Y2, FWidth, FHeight]);

  for Y := Y1 to Y2 do
    Move(FData[Y * FWidth + X1], FData[(Y-Y1) * FWidth], FWidth * SizeOf(TRGB32));

  SetSize(X2-X1+1, Y2-Y1+1);
end;

function TMufasaBitmap.ToTBitmap: TBitmap;
begin
  Result := TBitmap.Create();
  Result.LoadFromRawImage(Self.ToRawImage(), False);
end;

function TMufasaBitmap.SaveToString: String;
type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R: Byte;
  end;
var
  i: Integer;
  DataStr: String;
  DataPtr: PRGB24;
begin
  SetLength(DataStr, FWidth * FHeight * 3);

  DataPtr := PRGB24(@DataStr[1]);
  for i := FWidth * FHeight - 1 downto 0 do
  begin
    DataPtr[i].R := FData[i].R;
    DataPtr[i].G := FData[i].G;
    DataPtr[i].B := FData[i].B;
  end;

  Result := 'm' + Base64Encode(CompressString(DataStr));
end;

function TMufasaBitmap.ToMatrixBGR: TIntegerMatrix;
var
  Y: Integer;
begin
  Result.SetSize(FWidth, FHeight);
  for Y := 0 to FHeight - 1 do
    Move(FData[Y * FWidth], Result[Y, 0], FWidth * SizeOf(Integer));
end;

function TMufasaBitmap.ToMatrix: TIntegerMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(Width, Height);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y][X] := BGRToRGB(FData[Y * FWidth + X]);
end;

function TMufasaBitmap.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
var
  X, Y: Integer;
begin
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X1, Y1, FWidth, FHeight]);
  if (not PointInBitmap(X1, Y1)) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X2, Y2, FWidth, FHeight]);

  Result.SetSize(X2-X1+1, Y2-Y1+1);

  for Y := Y1 to Y2 do
    for X := X1 to X2 do
      Result[Y-Y1, X-X1] := BGRToRGB(FData[Y * FWidth + X]);
end;

function TMufasaBitmap.ToRawImage: TRawImage;
begin
  Result.Init();

  Result.Description.PaletteColorCount := 0;
  Result.Description.MaskBitsPerPixel  := 0;
  Result.Description.Width             := FWidth;
  Result.Description.Height            := FHeight;

  Result.Description.Format       := ricfRGBA;
  Result.Description.ByteOrder    := riboLSBFirst;
  Result.Description.BitOrder     := riboBitsInOrder; // should be fine
  Result.Description.Depth        := 24;
  Result.Description.BitsPerPixel := 32;
  Result.Description.LineOrder    := riloTopToBottom;
  Result.Description.LineEnd      := rileDWordBoundary;

  Result.Description.RedPrec   := 8;
  Result.Description.GreenPrec := 8;
  Result.Description.BluePrec  := 8;
  Result.Description.AlphaPrec := 0;

  Result.Description.RedShift   := 16;
  Result.Description.GreenShift := 8;
  Result.Description.BlueShift  := 0;

  Result.DataSize := Result.Description.Width * Result.Description.Height * (Result.Description.BitsPerPixel shr 3);
  Result.Data := PByte(FData);
end;

procedure TMufasaBitmap.DrawMatrix(Matrix: TByteMatrix);
var
  X, Y, W, H: Integer;
begin
  SetSize(Matrix.Width, Matrix.Height);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      with FData[Y * FWidth + X] do
      begin
        R := Matrix[Y, X];
        G := Matrix[Y, X];
        B := Matrix[Y, X];
      end;
end;

procedure TMufasaBitmap.DrawMatrix(Matrix: TIntegerMatrix);
var
  X, Y, W, H: Integer;
begin
  SetSize(Matrix.Width, Matrix.Height);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      FData[Y * FWidth + X] := RGBToBGR(Matrix[Y, X]);
end;

procedure TMufasaBitmap.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload;
var
  X,Y, W,H, Color: Integer;
  _H,_S,_L: Extended;
  Normed: TSingleMatrix;
begin
  SetSize(Matrix.Width, Matrix.Height);

  Normed := Matrix.NormMinMax(0, 1);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      case ColorMapID of
        0:begin //cold blue to red
            _H := (1 - Normed[Y,X]) * 67;
            _S := 40 + Normed[Y,X] * 60;
            Color := HSLToColor(_H,_S,50);
          end;
        1:begin //black -> blue -> red
            _H := (1 - Normed[Y,X]) * 67;
            _L := Normed[Y,X] * 50;
            Color := HSLToColor(_H,100,_L);
          end;
        2:begin //white -> blue -> red
            _H := (1 - Normed[Y,X]) * 67;
            _L := 100 - Normed[Y,X] * 50;
            Color := HSLToColor(_H,100,_L);
          end;
        3:begin //Light (to white)
            _L := (1 - Normed[Y,X]) * 100;
            Color := HSLToColor(0,0,_L);
          end;
        4:begin //Light (to black)
            _L := Normed[Y,X] * 100;
            Color := HSLToColor(0,0,_L);
          end;
        else
          begin //Custom black to hue to white
            _L := Normed[Y,X] * 100;
            Color := HSLToColor(ColorMapID/3.6,100,_L);
          end;
      end;

      FData[Y*FWidth+X] := RGBToBGR(Color);
    end;
end;

function TMufasaBitmap.RowPtrs: TPRGB32Array;
var
  I: Integer;
begin
  SetLength(Result, FHeight);
  for i := 0 to FHeight - 1 do
    Result[i] := FData + FWidth * i;
end;

procedure TMufasaBitmap.LoadFromClient;
var
  W, H: Integer;
begin
  if (not (FClient is TClient)) then
    raise ESimbaBitmapException.Create(sbeInvalidClient);

  TClient(FClient).IOManager.GetDimensions(W, H);

  SetSize(W, H);
  DrawClient(TPoint.Create(0, 0));
end;

procedure TMufasaBitmap.LoadFromClient(Area: TBox);
begin
  if (not (FClient is TClient)) then
    raise ESimbaBitmapException.Create(sbeInvalidClient);

  SetSize(Area.Width, Area.Height);
  DrawClient(Area, TPoint.Create(0, 0));
end;

procedure TMufasaBitmap.LoadFromString(AWidth, AHeight: Integer; Str: String);
type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R: Byte;
  end;
var
  I: Integer;
  Source: String;
  SourcePtr: PRGB24;
  DestPtr: PRGB32;
begin
  SetSize(AWidth, AHeight);

  if (Str <> '') and (Str[1] = 'm') then
    Delete(Str, 1, 1);

  Source := DecompressString(Base64Decode(Str));
  if (Source = '') then
    raise ESimbaBitmapException.Create(sbeInvalidBitmapString);

  SourcePtr := @Source[1];
  DestPtr := PRGB32(FData);

  for I := Width * Height - 1 downto 0 do
  begin
    DestPtr[I].R := SourcePtr[I].R;
    DestPtr[I].G := SourcePtr[I].G;
    DestPtr[I].B := SourcePtr[I].B;
  end;
end;

procedure TMufasaBitmap.LoadFromData(AWidth, AHeight: Integer; AData: PRGB32; CopyData: Boolean);
begin
  if CopyData then
  begin
    SetSize(AWidth, AHeight);
    if (AData <> nil) then
      Move(AData^, FData^, AWidth * AHeight * SizeOf(TRGB32));
  end else
    SetPersistentMemory(PtrUInt(AData), AWidth, AHeight);
end;

procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage);
var
  x,y: Integer;
  _24_old_p: PByte;
  rs,gs,bs:byte;
  NewData: PRGB32;
begin
  // clear data
  Self.SetSize(0,0);

  if (RawImage.Description.BitsPerPixel <> 24) and (RawImage.Description.BitsPerPixel <> 32) then
    raise Exception.CreateFmt('TMufasaBitmap.LoadFromRawImage - BitsPerPixel is %d', [RawImage.Description.BitsPerPixel]);

  {writeln('Bits per pixel: ' + Inttostr(RawImage.Description.BitsPerPixel));   }
  if RawImage.Description.LineOrder <> riloTopToBottom then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - LineOrder is not riloTopToBottom');

 { writeln(format('LineOrder: theirs: %d, ours: %d', [RawImage.Description.LineOrder, riloTopToBottom]));  }

 { if RawImage.Description.LineEnd <> rileDWordBoundary then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - LineEnd is not rileDWordBoundary');         }

  //writeln(format('LineEnd: t', [RawImage.Description.LineEnd]));

  if RawImage.Description.Format<>ricfRGBA then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - Format is not ricfRGBA');

  // Set w,h and alloc mem.
  Self.SetSize(RawImage.Description.Width, RawImage.Description.Height);

  {writeln(format('Image size: %d, %d', [FWidth,FHeight]));  }
  rs := RawImage.Description.RedShift shr 3;
  gs := RawImage.Description.GreenShift shr 3;
  bs := RawImage.Description.BlueShift shr 3;
 { writeln(format('Shifts(R,G,B): %d, %d, %d', [rs,gs,bs]));
  writeln(format('Bits per line %d, expected: %d',
  [RawImage.Description.BitsPerLine, RawImage.Description.BitsPerPixel * self.FWidth]));
  }

  if RawImage.Description.BitsPerPixel = 32 then
    Move(RawImage.Data[0], Self.FData[0],  self.FWidth * self.FHeight * SizeOf(TRGB32))
  else
  begin
    //FillChar(Self.FData[0], self.FWidth * self.FHeight * SizeOf(TRGB32), 0);
    NewData := self.FData;

    _24_old_p := RawImage.Data;
    for y := 0 to self.FHeight -1 do
    begin
      for x := 0 to self.FWidth -1 do
      begin
        // b is the first byte in the record.
        NewData^.b := _24_old_p[bs];
        NewData^.g := _24_old_p[gs];
        NewData^.r := _24_old_p[rs];
        NewData^.a := 0;

        inc(_24_old_p, 3);
        inc(NewData);
      end;

      case RawImage.Description.LineEnd of
        rileTight, rileByteBoundary: ; // do nothing
        rileWordBoundary:
          while (_24_old_p - RawImage.Data) mod 2 <> 0 do
            inc(_24_old_p);
        rileDWordBoundary:
          while (_24_old_p - RawImage.Data) mod 4 <> 0 do
            inc(_24_old_p);
        rileQWordBoundary:
          while (_24_old_p - RawImage.Data) mod 4 <> 0 do
            inc(_24_old_p);
        rileDQWordBoundary:
          while (_24_old_p - RawImage.Data) mod 8 <> 0 do
            inc(_24_old_p);
        end;
    end;
  end;
end;

// TM_CCOEFF_NORMED
// Author: slackydev
function TMufasaBitmap.Compare(Other: TMufasaBitmap): Single;
var
  invSize, sigmaR, sigmaG, sigmaB, isum, tsum: Single;
  x, y, W, H: Integer;
  tcR, tcG, tcB, icR, icG, icB: TSingleMatrix;
begin
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualDimensions);

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

function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap): Integer;
var
  I: Integer;
  Ptr, OtherPtr: PRGB32;
begin
  Result := 0;
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualDimensions);

  Ptr := Data;
  OtherPtr := Other.Data;

  for I := 0 to FWidth * FHeight - 1 do
  begin
    if not Ptr^.EqualsIgnoreAlpha(OtherPtr^) then
      Inc(Result);

    Inc(Ptr);
    Inc(OtherPtr);
  end;
end;

function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap; Tolerance: Integer): Integer;
var
  I: Integer;
  Ptr, OtherPtr: PRGB32;
begin
  if (Tolerance = 0) then
    Exit(PixelDifference(Other));

  Result := 0;
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualDimensions);

  Tolerance := Sqr(Tolerance);

  Ptr := Data;
  OtherPtr := Other.Data;

  for I := 0 to FWidth * FHeight - 1 do
  begin
    if (RGBDistance(Ptr^, OtherPtr^) > Tolerance) then
      Inc(Result);

    Inc(Ptr);
    Inc(OtherPtr);
  end;
end;

function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap): TPointArray;
var
  X, Y, W, H: Integer;
  Index: Integer;
  Buffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Buffer.Init();
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualDimensions);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Index := Y * FWidth + X;
      if not FData[Index].EqualsIgnoreAlpha(Other.FData[Index]) then
        Buffer.Add(TPoint.Create(X, Y));
    end;

  Result := Buffer.Trim();
end;

function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap; Tolerance: Integer): TPointArray;
var
  X, Y, W, H: Integer;
  Index: Integer;
  Buffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  if (Tolerance = 0) then
    Exit(PixelDifferenceTPA(Other));

  Buffer.Init();
  if (FWidth <> Other.Width) or (FHeight <> Other.Height) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualDimensions);

  Tolerance := Sqr(Tolerance);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Index := Y * FWidth + X;
      if (RGBDistance(FData[Index], Other.FData[Index]) > Tolerance) then
        Buffer.Add(TPoint.Create(X, Y));
    end;

  Result := Buffer.Trim();
end;

function TMufasaBitmap.PixelEdgesTPA(MinDiff: Integer): TPointArray;
var
  X, Y ,W, H: Integer;
  Buffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Buffer.Init();

  MinDiff := Sqr(MinDiff);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (X+1 < W) then
        if RGBDistance(FData[Y*FWidth+X], FData[Y*FWidth+(X+1)]) > MinDiff then
        begin
          Buffer.Add(TPoint.Create(X, Y));

          Continue;
        end;

      if (Y+1 < H) then
        if RGBDistance(FData[Y*FWidth+X], FData[(Y+1)*FWidth+X]) > MinDiff then
          Buffer.Add(TPoint.Create(X, Y));
    end;

  Result := Buffer.Trim();
end;

procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);
begin
  LoadFromRawImage(bmp.RawImage);
end;

procedure TMufasaBitmap.LoadFromBitmap(Bitmap: TMufasaBitmap);
begin
  SetSize(Bitmap.Width, Bitmap.Height);

  Move(Bitmap.Data^, FData^, FWidth * FHeight * SizeOf(TRGB32));
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray);
var
  I: Integer;
begin
  for I := 0 to High(ATPA) do
    DrawTPA(ATPA[I], GetDrawColor(-1, I));
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Color: Integer);
var
  I: Integer;
begin
  for I := 0 to High(ATPA) do
    DrawTPA(ATPA[I], Color);
end;

procedure TMufasaBitmap.DrawTPA(Points: TPointArray; Color: Integer);
var
  I: Integer;
  P: TPoint;
  BGR: TRGB32;
begin
  if (Length(Points) = 0) then
    Exit;

  BGR := RGBToBGR(Color);

  for I := 0 to High(Points) do
  begin
    P := Points[I];
    if (P.X >= 0) and (P.Y >= 0) and (P.X < FWidth) and (P.Y < FHeight) then
      FData[P.Y * FWidth + P.X] := BGR;
  end;
end;

procedure TMufasaBitmap.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: Integer);
begin
  Size := Max(1, Size);

  with ACenter do
  begin
    Self.DrawLine(Point(X - Size, Y), Point(X + Size, Y), Color);
    Self.DrawLine(Point(X, Y - Size), Point(X, Y + Size), Color);
  end;
end;

procedure TMufasaBitmap.DrawCross(ACenter: TPoint; Radius: Integer; Color: Integer);
begin
  Radius := Max(1, Round(Radius/2*Sqrt(2)));

  with ACenter do
  begin
    Self.DrawLine(Point(X - Radius, Y - Radius), Point(X + Radius, Y + Radius), Color);
    Self.DrawLine(Point(X + Radius, Y - Radius), Point(X - Radius, Y + Radius), Color);
  end;
end;

procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Color: Integer);
var
  BGR: TRGB32;

  procedure PutPixel(const X, Y: Single); inline;
  var
    XX, YY: Integer;
  begin
    XX := Round(X);
    YY := Round(Y);
    if (XX >= 0) and (YY >= 0) and (XX < FWidth) and (YY < FHeight) then
      FData[YY * FWidth + XX] := BGR;
  end;

var
  DX, DY, Step, I: Integer;
  RX, RY, X, Y: Single;
begin
  BGR := RGBToBGR(Color);

  DX := (Stop.X - Start.X);
  DY := (Stop.Y - Start.Y);
  if (Abs(DX) > Abs(DY)) then
    Step := Abs(DX)
  else
    Step := Abs(DY);

  if (Step = 0) then
  begin
    RX := DX;
    RY := DY;
  end else
  begin
    RX := DX / Step;
    RY := DY / Step;
  end;
  X := Start.X;
  Y := Start.Y;

  PutPixel(X, Y);
  for I := 1 to Step do
  begin
    X := X + RX;
    Y := Y + RY;

    PutPixel(X, Y);
  end;
end;

procedure TMufasaBitmap.DrawPolygon(Points: TPointArray; Color: Integer);
begin
  Self.DrawTPA(Points.Connect(), Color);
end;

procedure TMufasaBitmap.DrawPolygonFilled(Points: TPointArray; Color: Integer);
var
  X, Y, Hi: Integer;
  Bounds: TBox;
  RGB: TRGB32;
begin
  Hi := High(Points);
  if (Hi < 0) then
    Exit;

  RGB := RGBToBGR(Color);

  Bounds := Points.Bounds();
  Bounds.Clip(Box(0, 0, FWidth-1, FHeight-1));

  for X := Bounds.X1 to Bounds.X2 do
    for Y := Bounds.Y1 to Bounds.Y2 do
      if TSimbaGeometry.PointInPolygon(Point(X, Y), Points) then
        FData[Y*FWidth+X] := RGB;
end;

procedure TMufasaBitmap.DrawPolygonInverted(Points: TPointArray; Color: Integer);
var
  X, Y, Hi: Integer;
  Bounds: TBox;
  RGB: TRGB32;
begin
  Hi := High(Points);
  if (Hi < 0) then
    Exit;

  RGB := RGBToBGR(Color);

  Bounds := Points.Bounds();
  Bounds.Clip(Box(0, 0, FWidth-1, FHeight-1));

  Self.DrawBoxInverted(Bounds, Color);

  for X := Bounds.X1 to Bounds.X2 do
    for Y := Bounds.Y1 to Bounds.Y2 do
      if not TSimbaGeometry.PointInPolygon(Point(X, Y), Points) then
        FData[Y*FWidth+X] := RGB;
end;

procedure TMufasaBitmap.DrawCircle(ACenter: TPoint; Radius: Integer; Color: Integer);
begin
  if (Radius < 1) then
    Exit;

  DrawTPA(TPointArray.CreateFromCircle(ACenter, Radius, False), Color);
end;

procedure TMufasaBitmap.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: Integer);
begin
  if (Radius < 1) then
    Exit;

  DrawTPA(TPointArray.CreateFromCircle(ACenter, Radius, True), Color);
end;

procedure TMufasaBitmap.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: Integer);
var
  X, Y: Integer;
  Bounds: TBox;
  RGB: TRGB32;
begin
  RGB := RGBToBGR(Color);

  Bounds.X1 := Max(ACenter.X-Radius, 0);
  Bounds.Y1 := Max(ACenter.Y-Radius, 0);
  Bounds.X2 := Min(ACenter.X+Radius, FWidth-1);
  Bounds.Y2 := Min(ACenter.Y+Radius, FHeight-1);

  Self.DrawBoxInverted(Bounds, Color);

  for X := Bounds.X1 to Bounds.X2 do
    for Y := Bounds.Y1 to Bounds.Y2 do
      if not TSimbaGeometry.PointInCircle(Point(X, Y), ACenter, Radius) then
        FData[Y*FWidth+X] := RGB;
end;

procedure TMufasaBitmap.DrawBox(B: TBox; Color: Integer);
begin
  Self.DrawTPA(TPointArray.CreateFromBox(B, False), Color);
end;

procedure TMufasaBitmap.DrawBoxFilled(B: TBox; Color: Integer);
var
  Y, Size: Integer;
begin
  B.Clip(Box(0, 0, FWidth-1, FHeight-1));

  if (B.X2 - B.X1 < 1) then Exit;
  if (B.Y2 - B.Y1 < 1) then Exit;

  Color := Integer(RGBToBGR(Color));
  Size := B.X2 - B.X1 + 1;
  for Y := B.Y1 to B.Y2 do
    FillDWord(FData[Y * FWidth + B.X1], Size, Color);
end;

procedure TMufasaBitmap.DrawBoxInverted(B: TBox; Color: Integer);
begin
  Self.DrawBoxFilled(Box(0,    0,    B.X1,     B.Y1),      Color); //Top Left
  Self.DrawBoxFilled(Box(0,    B.Y1, B.X1,     B.Y2),      Color); //Mid Left
  Self.DrawBoxFilled(Box(0,    B.Y1, B.X1,     FHeight-1), Color); //Btm Left
  Self.DrawBoxFilled(Box(B.X1, 0,    B.X2,     B.Y1),      Color); //Top Mid
  Self.DrawBoxFilled(Box(B.X1, B.Y2, B.X2,     FHeight-1), Color); //Btm Mid
  Self.DrawBoxFilled(Box(B.X2, 0,    FWidth-1, B.Y1),      Color); //Top Right
  Self.DrawBoxFilled(Box(B.X2, B.Y1, FWidth-1, B.Y2),      Color); //Mid Right
  Self.DrawBoxFilled(Box(B.X2, B.Y1, FWidth-1, FHeight-1), Color); //Btm Right
end;

procedure TMufasaBitmap.DrawQuad(Quad: TQuad; Color: Integer);
begin
  DrawLine(Quad.Top, Quad.Right, Color);
  DrawLine(Quad.Right, Quad.Bottom, Color);
  DrawLine(Quad.Bottom, Quad.Left, Color);
  DrawLine(Quad.Left, Quad.Top, Color);
end;

procedure TMufasaBitmap.DrawQuadFilled(Quad: TQuad; Color: Integer);
var
  X, Y: Integer;
  Bounds: TBox;
  RGB: TRGB32;
begin
  RGB := RGBToBGR(Color);

  Bounds := Quad.Bounds;
  Bounds.Clip(Box(0, 0, FWidth-1, FHeight-1));

  for X := Bounds.X1 to Bounds.X2 do
    for Y := Bounds.Y1 to Bounds.Y2 do
      if TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
        FData[Y*FWidth+X] := RGB;
end;

procedure TMufasaBitmap.DrawQuadInverted(Quad: TQuad; Color: Integer);
var
  X, Y: Integer;
  Bounds: TBox;
  RGB: TRGB32;
begin
  RGB := RGBToBGR(Color);

  Bounds := Quad.Bounds;
  Bounds.Clip(Box(0, 0, FWidth-1, FHeight-1));

  Self.DrawBoxInverted(Bounds, Color);

  for X := Bounds.X1 to Bounds.X2 do
    for Y := Bounds.Y1 to Bounds.Y2 do
      if not TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Quad.Top, Quad.Right, Quad.Bottom, Quad.Left) then
        FData[Y*FWidth+X] := RGB;
end;

procedure TMufasaBitmap.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Boxes) do
    if Filled then
      DrawBoxFilled(Boxes[I], GetDrawColor(Color, I))
    else
      DrawBox(Boxes[I], GetDrawColor(Color, I));
end;

procedure TMufasaBitmap.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Polygons) do
    if Filled then
      DrawPolygonFilled(Polygons[I], GetDrawColor(Color, I))
    else
      DrawPolygon(Polygons[I], GetDrawColor(Color, I));
end;

procedure TMufasaBitmap.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Points) do
    if Filled then
      DrawCircleFilled(Points[I], Radius, GetDrawColor(Color, I))
    else
      DrawCircle(Points[I], Radius, GetDrawColor(Color, I));
end;

procedure TMufasaBitmap.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Points) do
    DrawCross(Points[I], Radius, GetDrawColor(Color, I));
end;

procedure TMufasaBitmap.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
var
  H, S, L: Single;
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
      H := Degrees(ArcTan2(Y - ACenter.Y, X - ACenter.X)) / 3.6;
      S := Hypot(ACenter.X - X, ACenter.Y - Y);
      L := 50;
      if S / Radius * 100 < 100 then
        SetPixel(X, Y, HSLToColor(H, S / Radius * 100, L));
    end;
end;

procedure TMufasaBitmap.DrawToCanvas(x,y: Integer; Canvas: TCanvas);
var
  Bitmap: Graphics.TBitmap;
begin
  Bitmap := Self.ToTBitmap;
  Canvas.Draw(x,y,Bitmap);
  Bitmap.Free();
end;

function TMufasaBitmap.PeakBrightness: Integer;
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

procedure TMufasaBitmap.Fill(Color: Integer);
begin
  FillDWord(FData[0], FWidth * FHeight, RGBToBGR(Color).AsInteger);
end;

procedure TMufasaBitmap.Clear;
begin
  DrawBoxFilled(Box(0, 0, FWidth-1, FHeight-1), FTransparentColor);
end;

procedure TMufasaBitmap.Clear(Area: TBox);
begin
  DrawBoxFilled(Area, FTransparentColor);
end;

procedure TMufasaBitmap.ClearInverted(Area: TBox);
begin
  DrawBoxInverted(Area, FTransparentColor);
end;

procedure TMufasaBitmap.DrawClient(Area: TBox; Position: TPoint);
var
  Y: Integer;
  ClientData: TRetData;
  W, H: Integer;
  XOffset, YOffset: Integer;
begin
  if (not (FClient is TClient)) then
    raise ESimbaBitmapException.Create(sbeInvalidClient);

  ClientData := TClient(FClient).IOManager.ReturnData(Area.X1, Area.Y1, Area.Width, Area.Height);

  if (ClientData.Ptr <> nil) then
  begin
    W := Area.Width;
    H := Area.Height;
    XOffset := 0;
    YOffset := 0;

    if (Position.X < 0) then
    begin
      XOffset := Abs(Position.X);
      W -= XOffset;
      Position.X := 0;
    end;

    if (Position.Y < 0) then
    begin
      YOffset := Abs(Position.Y);
      H -= YOffset;
      Position.Y := 0;
    end;

    if (Position.X + W > FWidth) then  W := W - (Position.X + W - FWidth);
    if (Position.Y + H > FHeight) then H := H - (Position.Y + H - FHeight);

    for Y := 0 to H - 1 do
      Move(ClientData.Ptr[(Y+YOffset) * ClientData.RowLen + XOffset], FData[(Position.Y + Y) * FWidth + Position.X], W * SizeOf(TRGB32));
  end;
end;

procedure TMufasaBitmap.DrawClient(Position: TPoint);
var
  W, H: Integer;
begin
  if (not (FClient is TClient)) then
    raise ESimbaBitmapException.Create(sbeInvalidClient);

  TClient(FClient).IOManager.GetDimensions(W, H);

  DrawClient(TBox.Create(0,0,W-1,H-1), TPoint.Create(0, 0));
end;

procedure TMufasaBitmap.DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);
var
  X, Y, W, H: Integer;
  DestX, DestY: Integer;
  Color: TRGB32;
begin
  W := Bitmap.Width - 1;
  H := Bitmap.Height - 1;

  for X := 0 to W do
    for Y := 0 to H do
    begin
      DestX := X + Position.X;
      DestY := Y + Position.Y;

      if (DestX >= 0) and (DestY >= 0) and (DestX < FWidth) and (DestY < FHeight) then
      begin
        Color := Bitmap.Data[Y * Bitmap.Width + X];
        if (not Self.TransparentColorActive) or (not Color.EqualsIgnoreAlpha(FTransparentRGB)) then
          FData[DestY * FWidth + DestX] := Bitmap.Data[Y * Bitmap.Width + X];
      end;
    end;
end;

function TMufasaBitmap.AverageBrightness: Integer;
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

function TMufasaBitmap.GetColors: TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, FHeight * FWidth);
  for I := 0 to High(Result) do
    Result[I] := BGRToRGB(FData[I]);
end;

function TMufasaBitmap.Equals(Other: TObject): Boolean;
begin
  if (Other is TMufasaBitmap) then
    Result := Equals(TMufasaBitmap(Other))
  else
    Result := inherited Equals(Other);
end;

function TMufasaBitmap.Equals(Other: TMufasaBitmap): Boolean;
begin
  Result := (FWidth  = Other.FWidth) and
            (FHeight = Other.FHeight) and
            (CompareMem(FData, Other.FData, FWidth * FHeight * SizeOf(TRGB32)));
end;

procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: Integer);
var
  I: Integer;
  OldRGB, NewRGB: TRGB32;
begin
  OldRGB := RGBToBGR(OldColor);
  NewRGB := RGBToBGR(NewColor);

  for I := 0 to FWidth * FHeight - 1 do
    if FData[I].EqualsIgnoreAlpha(OldRGB) then
      FData[I] := NewRGB;
end;

procedure TMufasaBitmap.ReplaceColors(OldColors, NewColors: TIntegerArray);
var
  I, J, H: Integer;
  OldRGBs, NewRGBs: TRGB32Array;
begin
  if (Length(OldColors) <> Length(NewColors)) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualLengths);

  SetLength(OldRGBs, Length(OldColors));
  SetLength(NewRGBs, Length(NewColors));

  H := High(OldColors);
  for I := 0 to High(OldColors) do
  begin
    OldRGBs[I] := RGBToBGR(OldColors[I]);
    NewRGBs[I] := RGBToBGR(NewColors[I]);
  end;

  for I := 0 to FWidth * FHeight - 1 do
    for J := 0 to H do
      if FData[I].EqualsIgnoreAlpha(OldRGBs[J]) then
      begin
        FData[I] := NewRGBs[J];

        Break;
      end;
end;

function GetRotatedSize(W, H: Integer; Angle: Single): TBox;
var
  B: TPointArray;
begin
  B := [
    TSimbaGeometry.RotatePoint(Point(0, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, 0), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(0, 0), Angle, W div 2, H div 2)
  ];

  Result := B.Bounds();
end;

procedure TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);
var
  CosAngle, SinAngle: Single;

  procedure RotateNoExpand;
  var
    X, Y, OldX, OldY, W, H: Integer;
    MidX, MidY: Single;
  begin
    TargetBitmap.SetSize(FWidth, FHeight);

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
          TargetBitmap.FData[Y * FWidth + X] := FData[OldY * FWidth + OldX];
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

    TargetBitmap.SetSize(NewWidth, NewHeight);

    Dec(NewWidth);
    Dec(NewHeight);
    for Y := 0 to NewHeight do
      for X := 0 to NewWidth do
      begin
        OldX := Round(MidX + CosAngle * (NewBounds.X1+X - MidX) - SinAngle * (NewBounds.Y1+Y - MidY));
        OldY := Round(MidY + SinAngle * (NewBounds.X1+X - MidX) + CosAngle * (NewBounds.Y1+Y - MidY));
        if (OldX >= 0) and (OldX < FWidth) and (OldY >= 0) and (OldY < FHeight) then
          TargetBitmap.FData[Y * TargetBitmap.FWidth + X] := FData[OldY * FWidth + OldX];
      end;
  end;

begin
  CosAngle := Cos(Radians);
  SinAngle := Sin(Radians);

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

procedure TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);
var
  CosAngle, SinAngle: Single;

  procedure RotateNoExpand;
  var
    x, y, w, h: Integer;
    OldX, OldY: Single;
    dX, dY, dxMinus1, dyMinus1: Single;
    p0, p1, p2, p3: TRGB32;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    TargetBitmap.SetSize(FWidth, FHeight);

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

          TopR := dxMinus1 * p0.R + dx * p1.R;
          TopG := dxMinus1 * p0.G + dx * p1.G;
          TopB := dxMinus1 * p0.B + dx * p1.B;
          BtmR := dxMinus1 * p2.R + dx * p3.R;
          BtmG := dxMinus1 * p2.G + dx * p3.G;
          BtmB := dxMinus1 * p2.B + dx * p3.B;

          with TargetBitmap.FData[Y * TargetBitmap.FWidth + X] do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := 0;
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
    p0, p1, p2, p3: TRGB32;
    topR, topG, topB, BtmR, btmG, btmB: Single;
    fX, fY, cX, cY: Integer;
    MidX, MidY: Single;
  begin
    NewBounds := GetRotatedSize(FWidth, FHeight, Radians);
    NewWidth := NewBounds.Width - 1;
    NewHeight := NewBounds.Height - 1;
    MidX := (NewWidth - 1) / 2;
    MidY := (NewHeight - 1) / 2;

    TargetBitmap.SetSize(NewWidth, NewHeight);

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

          TopR := dxMinus1 * p0.R + dx * p1.R;
          TopG := dxMinus1 * p0.G + dx * p1.G;
          TopB := dxMinus1 * p0.B + dx * p1.B;
          BtmR := dxMinus1 * p2.R + dx * p3.R;
          BtmG := dxMinus1 * p2.G + dx * p3.G;
          BtmB := dxMinus1 * p2.B + dx * p3.B;

          with TargetBitmap.FData[Y * TargetBitmap.FWidth + X] do
          begin
            R := EnsureRange(Round(dyMinus1 * TopR + dy * BtmR), 0, 255);
            G := EnsureRange(Round(dyMinus1 * TopG + dy * BtmG), 0, 255);
            B := EnsureRange(Round(dyMinus1 * TopB + dy * BtmB), 0, 255);
            A := 0;
          end;
        end;
      end;
  end;

begin
  CosAngle := Cos(Radians);
  SinAngle := Sin(Radians);

  case Expand of
    True:  RotateExpand();
    False: RotateNoExpand();
  end;
end;

function TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean): TMufasaBitmap;
begin
  Result := TMufasaBitmap.Create();

  Self.Rotate(Radians, Expand, Result);
end;

function TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean): TMufasaBitmap;
begin
  Result := TMufasaBitmap.Create();

  Self.RotateBilinear(Radians, Expand, Result);
end;

procedure TMufasaBitmap.Desaturate;
var
  I: Integer;
  He,Se,Le: extended;
  Ptr: PRGB32;
begin
  Ptr := FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,Ptr^.R,Ptr^.G,Ptr^.B);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap);
var
  I: Integer;
  He,Se,Le: extended;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    RGBToHSL(PtrOld^.R,PtrOld^.G,PtrOld^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,PtrNew^.R,PtrNew^.G,PtrNew^.B);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap);
var
  I: Integer;
  Lum: byte;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    Lum := Round(PtrOld^.r * 0.3 + PtrOld^.g * 0.59 + PtrOld^.b * 0.11);
    PtrNew^.r := Lum;
    PtrNew^.g := Lum;
    PtrNew^.b := Lum;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale;
var
  I: Integer;
  Lum: Byte;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    Lum := Round(Ptr^.r * 0.3 + Ptr^.g * 0.59 + Ptr^.b * 0.11);
    Ptr^.r := Lum;
    Ptr^.g := Lum;
    Ptr^.b := Lum;
    inc(ptr);
  end;
end;

function BrightnessAdjust(Col:  byte; br: Integer): byte;inline;
var
  temp: Integer;
begin
  Temp := Col + Br;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  Result := temp;
end;
procedure TMufasaBitmap.Brightness(br: Integer);
var
  I: Integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    Ptr^.r := BrightnessAdjust(Ptr^.r,br);
    Ptr^.g := BrightnessAdjust(Ptr^.g,br);
    Ptr^.b := BrightnessAdjust(Ptr^.b,br);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Integer);
var
  I: Integer;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    PtrNew^.r := BrightnessAdjust(PtrOld^.r,br);
    PtrNew^.g := BrightnessAdjust(PtrOld^.g,br);
    PtrNew^.b := BrightnessAdjust(PtrOld^.b,br);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

const
  Grey = 128;
function ContrastAdjust(Col:  byte; co: extended): byte;inline;
var
  temp: Integer;
begin
  Temp := floor((col - Grey) * co) + grey;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  Result := temp;
end;

procedure TMufasaBitmap.Contrast(co: Extended);
var
  I: Integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    Ptr^.r := ContrastAdjust(Ptr^.r,co);
    Ptr^.g := ContrastAdjust(Ptr^.g,co);
    Ptr^.b := ContrastAdjust(Ptr^.b,co);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended);
var
  I: Integer;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    PtrNew^.r := ContrastAdjust(PtrOld^.r,co);
    PtrNew^.g := ContrastAdjust(PtrOld^.g,co);
    PtrNew^.b := ContrastAdjust(PtrOld^.b,co);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Invert;
var
  i: Integer;
begin
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    Self.FData[i].r := not Self.FData[i].r;
    Self.FData[i].g := not Self.FData[i].g;
    Self.Fdata[i].b := not Self.FData[i].b;
  end;
end;

procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);
var
  I: Integer;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    PtrNew^.r := not PtrOld^.r;
    PtrNew^.g := not PtrOld^.g;
    PtrNew^.b := not PtrOld^.b;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Integer);
var
  I: Integer;
  PtrOld,PtrNew: PRGB32;
begin
  if not InRange(Po,1,255) then
    raise ESimbaBitmapException.CreateFmt('Value(%d) out of range[1..255]', [Po]);

  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin
    PtrNew^.r := min(Round(PtrOld^.r / po) * Po, 255);
    PtrNew^.g := min(Round(PtrOld^.g / po) * Po, 255);
    PtrNew^.b := min(Round(PtrOld^.b / po) * Po, 255);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(Po: Integer);
var
  I: Integer;
  Ptr: PRGB32;
begin
  if not InRange(Po,1,255) then
    raise ESimbaBitmapException.CreateFmt('Value(%d) out of range[1..255]', [Po]);

  Ptr := Self.FData;

  for i := (FHeight*FWidth-1) downto 0 do
  begin
    ptr^.r := min(Round(ptr^.r / po) * Po, 255);
    ptr^.g := min(Round(ptr^.g / po) * Po, 255);
    ptr^.b := min(Round(ptr^.b / po) * Po, 255);

    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Blur(TargetBitmap: TMufasaBitmap; Block: Integer); overload;
var
  X, Y, W, H: Integer;
  Size: Integer;
  B: TBox;
  SumTable: TRGBSumTable;
begin
  Size := Sqr(Block);
  if (Size <= 1) or (Block mod 2 = 0) then
    Exit;

  TargetBitmap.SetSize(FWidth, FHeight);

  Block := Block div 2;
  SumTable := TRGBSumTable.Create(Self);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      B.X1 := Max(X-Block, 0);
      B.Y1 := Max(Y-Block, 0);
      B.X2 := Min(X+Block, W);
      B.Y2 := Min(Y+Block, H);

      Size := B.Area;
      with SumTable.Query(B) do
        TargetBitmap.Data[Y * FWidth + X] := RGBToBGR(R div Size, G div Size, B div Size);
    end;
end;

procedure TMufasaBitmap.Blur(Block: Integer); overload;
var
  X, Y, W, H: Integer;
  Size: Integer;
  B: TBox;
  SumTable: TRGBSumTable;
begin
  Size := (Block * Block);
  if (Size <= 1) or (Block mod 2 = 0) then
    Exit;

  Block := Block div 2;
  SumTable := TRGBSumTable.Create(Self);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      B.X1 := Max(X-Block, 0);
      B.Y1 := Max(Y-Block, 0);
      B.X2 := Min(X+Block, W);
      B.Y2 := Min(Y+Block, H);

      Size := B.Area;
      with SumTable.Query(B) do
        FData[Y * FWidth + X] := RGBToBGR(R div Size, G div Size, B div Size);
    end;
end;

procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: TDoubleMatrix);
var
  x,y,yy,xx,cx,cy: Integer;
  Row,RowT: TPRGB32Array;
  mW,mH,midx,midy:Integer;
  valR,valG,valB: Extended;

  procedure ForceInBounds(x,y, Wid,Hig: Integer; out cx,cy: Integer); inline;
  begin
    cx := x;
    cy := y;
    if cx >= Wid then   cx := Wid-1
    else if cx < 0 then cx := 0;
    if cy >= Hig then   cy := Hig-1
    else if cy < 0 then cy := 0;
  end;

begin
  TargetBitmap.SetSize(Self.FWidth,Self.FHeight);
  Row := RowPtrs;
  RowT := TargetBitmap.RowPtrs; //Target

  mW := High(Matrix[0]);
  mH := High(Matrix);
  midx := (mW+1) div 2;
  midy := (mH+1) div 2;
  for y:=0 to Self.FHeight-1 do
    for x:=0 to Self.FWidth-1 do
    begin
      valR := 0;
      valG := 0;
      valB := 0;
      for yy:=0 to mH do
        for xx:=0 to mW do
        begin
          ForceInBounds(x+xx-midx, y+yy-midy, Self.FWidth, Self.FHeight, cx, cy);
          valR := valR + (Matrix[yy][xx] * Row[cy][cx].R);
          valG := valG + (Matrix[yy][xx] * Row[cy][cx].G);
          valB := valB + (Matrix[yy][xx] * Row[cy][cx].B);
        end;
      RowT[y][x].R := Round(valR);
      RowT[y][x].G := Round(valG);
      RowT[y][x].B := Round(valB);
    end;
end;

procedure TMufasaBitmap.Mirror(MirrorStyle: TBmpMirrorStyle);
var
  NewWidth, NewHeight: Integer;
  NewData: PRGB32;
  X, Y: Integer;
begin
  NewWidth := FWidth;
  NewHeight := FHeight;
  if (MirrorStyle = MirrorLine) then
    Swap(NewWidth, NewHeight);

  NewData := AllocMem(NewWidth * NewHeight * SizeOf(TRGB32));

  case MirrorStyle of
    MirrorWidth:
      for Y := FHeight-1 downto 0 do
        for X := FWidth-1 downto 0 do
          NewData[Y*FWidth+X] := FData[Y*FWidth+FWidth-1-X];

    MirrorHeight:
      for Y := FHeight-1 downto 0 do
        Move(FData[Y*FWidth], NewData[(FHeight-1-Y) * FWidth], FWidth * SizeOf(TRGB32));

    MirrorLine:
      for Y := FHeight-1 downto 0 do
        for X := FHeight-1 downto 0 do
          NewData[X*FHeight+Y] := FData[Y*FWidth+X];
  end;

  if Assigned(FData) then
    FreeMem(FData);

  FData := NewData;
  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TMufasaBitmap.Mirror(TargetBitmap: TMufasaBitmap; MirrorStyle: TBmpMirrorStyle);
begin
  TargetBitmap.LoadFromBitmap(Self);
  TargetBitmap.Mirror(MirrorStyle);
end;

procedure TMufasaBitmap.Blend(Points: TPointArray; Radius: Integer);
var
  P: TPoint;
  X, Y, Count: Integer;
  Area: TBox;
  R, G, B: Integer;
  Color: TRGB32;
begin
  for P in Points do
  begin
    Area.X1 := Max(P.X - Radius, 0);
    Area.Y1 := Max(P.Y - Radius, 0);
    Area.X2 := Min(P.X + Radius, FWidth-1);
    Area.Y2 := Min(P.Y + Radius, FHeight-1);

    Count := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := Area.X1 to Area.X2 do
      for Y := Area.Y1 to Area.Y2 do
      begin
        Color := FData[Y * FWidth + X];
        if FTransparentColorActive and Color.EqualsIgnoreAlpha(FTransparentRGB) then
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

      FData[P.Y * FWidth + P.X] := Color;
    end;
  end;
end;

procedure TMufasaBitmap.Blend(TargetBitmap: TMufasaBitmap; Points: TPointArray; Radius: Integer);
begin
  TargetBitmap.LoadFromBitmap(Self);
  TargetBitmap.Blend(Points, Radius);
end;

procedure TMufasaBitmap.Downsample(Scale: Integer);
var
  Area: Double;

  function BlendArea(X1, Y1, X2, Y2: Integer): TRGB32; inline;
  var
    R, G, B: Integer;
    Hit, Miss: Integer;
    X, Y: Integer;
    Color: TRGB32;
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

        if FTransparentColorActive and Color.EqualsIgnoreAlpha(FTransparentRGB) then
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
    Result.A := 0;
  end;

var
  X, Y, W, H: Integer;
  OldX, OldY: Integer;
  NewWidth, NewHeight: Integer;
  NewData: PRGB32;
  Color: TRGB32;
begin
  if (Scale <= 1) then
    Exit;

  Area := Double(1.0) / Sqr(Scale);

  NewWidth := FWidth div Scale;
  NewHeight := FHeight div Scale;
  NewData := AllocMem(NewWidth * NewHeight * SizeOf(TRGB32));

  W := NewWidth - 1;
  H := NewHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := X * Scale;
      OldY := Y * Scale;

      Color := FData[OldY * FWidth + OldX];
      if FTransparentColorActive and Color.EqualsIgnoreAlpha(FTransparentRGB) then
        Continue;

      NewData[Y * NewWidth + X] := BlendArea(OldX, OldY, (OldX + Scale) - 1, (OldY + Scale) - 1);
    end;

  if Assigned(FData) then
    FreeMem(FData);

  FData := NewData;
  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TMufasaBitmap.Downsample(TargetBitmap: TMufasaBitmap; Scale: Integer);
begin
  TargetBitmap.LoadFromBitmap(Self);
  TargetBitmap.Downsample(Scale);
end;

function TMufasaBitmap.GetCenter: TPoint;
begin
  Result.X := FWidth div 2;
  Result.Y := FHeight div 2;
end;

function TMufasaBitmap.GetFontAntialiasing: Boolean;
begin
  Result := FTextDrawer.Antialiased;
end;

function TMufasaBitmap.GetFontName: String;
begin
  Result := FTextDrawer.FontName;
end;

function TMufasaBitmap.GetFonts: TStringArray;
begin
  Result := FTextDrawer.Fonts;
end;

function TMufasaBitmap.GetFontSize: Single;
begin
  Result := FTextDrawer.FontSize;
end;

procedure TMufasaBitmap.SetFontAntialiasing(Value: Boolean);
begin
  FTextDrawer.Antialiased := Value;
end;

procedure TMufasaBitmap.SetFontName(Value: String);
begin
  FTextDrawer.FontName := Value;
end;

procedure TMufasaBitmap.SetFontSize(Value: Single);
begin
  FTextDrawer.FontSize := Value;
end;

function TMufasaBitmap.TextWidth(Text: String): Integer;
begin
  Result := FTextDrawer.TextWidth(Text);
end;

function TMufasaBitmap.TextHeight(Text: String): Integer;
begin
  Result := FTextDrawer.TextHeight(Text);
end;

function TMufasaBitmap.TextSize(Text: String): TPoint;
begin
  Result := FTextDrawer.TextSize(Text);
end;

procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: Integer);
begin
  FTextDrawer.DrawText(Text, Position, Color);
end;

procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: Integer);
begin
  FTextDrawer.DrawText(Text, Box, ACenter, Color);
end;

procedure TMufasaBitmap.DrawTextLines(Text: TStringArray; Position: TPoint; Color: Integer);
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

procedure TMufasaBitmap.SetSize(NewWidth, NewHeight: Integer);
var
  NewData: PRGB32;
  i,minw,minh: Integer;
begin
  if FExternalData then
    raise ESimbaBitmapException.Create(sbeResizeExternalData);

  if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
  begin
    if NewWidth*NewHeight <> 0 then
      NewData := AllocMem(NewWidth * NewHeight * SizeOf(TRGB32))
    else
      NewData := nil;

    if Assigned(FData) and Assigned(NewData) and (FWidth*FHeight <> 0) then
    begin
      minw := Min(NewWidth,FWidth);
      minh := Min(NewHeight,FHeight);
      for i := 0 to minh - 1 do
        Move(FData[i*FWidth],Newdata[i*NewWidth],minw * SizeOf(TRGB32));
    end;
    if Assigned(FData) then
      FreeMem(FData);

    FData := NewData;
    FWidth := NewWidth;
    FHeight := NewHeight;
  end;
end;

procedure TMufasaBitmap.Resize(NewWidth, NewHeight: Integer);
var
  NewData: PRGB32;
  x,y: Integer;
begin
  if FExternalData then
    raise ESimbaBitmapException.Create(sbeResizeExternalData);

  if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
  begin
    if NewWidth*NewHeight <> 0 then
      NewData := AllocMem(NewWidth * NewHeight * SizeOf(TRGB32))
    else
      NewData := nil;

    if Assigned(FData) and Assigned(NewData) and (FWidth*FHeight <> 0) then
    begin
      for y := 0 to NewHeight - 1 do
        for x := 0 to NewWidth -1 do
          NewData[y*NewWidth + x] := FData[((y * FHeight) div NewHeight) * FWidth+ (x * FWidth) div NewWidth];
    end;
    if Assigned(FData) then
      FreeMem(FData);

    FData := NewData;
    FWidth := NewWidth;
    FHeight := NewHeight;
  end;
end;

procedure TMufasaBitmap.ResizeBilinear(NewWidth, NewHeight: Integer);
var
  X,Y,OldX,OldY: Integer;
  p0,p1,p2,p3: TRGB32;
  ratioX,ratioY,dx,dy: Single;
  SourceRows: TPRGB32Array;
  Color: TRGB32;
  NewData: PRGB32;
  W,H: Integer;
begin
  if FExternalData then
    raise ESimbaBitmapException.Create(sbeResizeExternalData);

  NewData := AllocMem(NewWidth * NewHeight * SizeOf(TRGB32));
  SourceRows := RowPtrs;

  ratioX := (FWidth - 1) / NewWidth;
  ratioY := (FHeight - 1) / NewHeight;

  W := NewWidth - 1;
  H := NewHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      OldX := Trunc(ratioX * X);
      OldY := Trunc(ratioY * Y);

      p0 := SourceRows[OldY][OldX];
      p1 := SourceRows[OldY][OldX+1];
      p2 := SourceRows[OldY+1][OldX];
      p3 := SourceRows[OldY+1][OldX+1];

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

      NewData[Y * NewWidth + X] := Color;
    end;

  if Assigned(FData) then
    FreeMem(FData);

  FData := NewData;
  FWidth := NewWidth;
  FHeight := NewHeight;
end;

function TMufasaBitmap.GetPixels(Points: TPointArray): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if not PointInBitmap(X, Y) then
        raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X, Y, FWidth, FHeight]);

      Result[I] := BGRToRGB(FData[Y * FWidth + X]);
    end;
end;

procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);
var
  I: Integer;
begin
  if (Length(Points) <> Length(Colors)) then
    raise ESimbaBitmapException.Create(sbeMustBeEqualLengths);

  for I := 0 to High(Points) do
    with Points[I] do
    begin
      if not PointInBitmap(X, Y) then
        raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X, Y, FWidth, FHeight]);

      FData[Y * FWidth + X] := RGBToBGR(Colors[I]);
    end;
end;

procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: TBmpThreshMethod; K: Integer);
var
  i,size: Integer;
  upper: PtrUInt;
  vMin,vMax,threshold: UInt8;
  Counter: Int64;
  Tab: array[0..256] of UInt8;
  ptr: PRGB32;
begin
  if Alpha = Beta then Exit;
  if Alpha > Beta then Swap(Alpha, Beta);

  size := (Self.Width * Self.Height) - 1;
  upper := PtrUInt(@Self.FData[size]);
  //Finding the threshold - While at it convert image to grayscale.
  Threshold := 0;
  case Method of
    //Find the Arithmetic Mean / Average.
    TM_Mean:
    begin
      Counter := 0;
      ptr := Self.FData;
      while PtrUInt(Ptr) <= upper do
      begin
        Ptr^.B := (Ptr^.B + Ptr^.G + Ptr^.R) div 3;
        Counter += Ptr^.B;
        Inc(Ptr);
      end;
      Threshold := (Counter div size) + K;
    end;

    //Middle of Min- and Max-value
    TM_MinMax:
    begin
      vMin := 255;
      vMax := 0;
      ptr := Self.FData;
      while PtrUInt(Ptr) <= upper do
      begin
        ptr^.B := (ptr^.B + ptr^.G + ptr^.R) div 3;
        if ptr^.B < vMin then
          vMin := ptr^.B
        else if ptr^.B > vMax then
          vMax := ptr^.B;
        Inc(ptr);
      end;
      Threshold := ((vMax+Integer(vMin)) shr 1) + K;
    end;
  end;

  if AInvert then Swap(Alpha, Beta);
  for i:=0 to (Threshold-1) do Tab[i] := Alpha;
  for i:=Threshold to 255 do Tab[i] := Beta;

  ptr := Self.FData;
  while PtrUInt(Ptr) <= upper do
  begin
    ptr^.R := Tab[Ptr^.B];
    ptr^.G := 0;
    ptr^.B := 0;
    ptr^.A := 0;
    Inc(ptr);
  end;
end;

procedure TMufasaBitmap.ThresholdSauvola(Window: Integer; AInvert: Boolean; K: Single);
var
  SumTable: TIntegerMatrix;
  SumTableSquared: TIntegerMatrix;

  function CreateSumTable(Matrix: TByteMatrix; Squared: Boolean): TIntegerMatrix;
  var
    W, H, X, Y: Integer;
  begin
    Result.SetSize(Matrix.Width, Matrix.Height);

    W := Matrix.Width - 1;
    H := Matrix.Height - 1;

    for Y := 0 to W do
      if Squared then
        Result[0, Y] := Sqr(Matrix[0, Y])
      else
        Result[0, Y] := Matrix[0, Y];

    for Y := 1 to H do
      for X := 0 to W do
        if Squared then
          Result[Y, X] := Sqr(Matrix[Y, X]) + Result[Y-1, X]
        else
          Result[Y, X] := Matrix[Y, X] + Result[Y-1, X];

    for Y := 0 to H do
      for X := 1 to W do
        Result[Y, X] += Result[Y, X-1];
  end;

  function QuerySumTable(Table: TIntegerMatrix; X1, Y1, X2, Y2: Integer): Integer;
  begin
    Result := Table[Y2, X2];

    if (Y1 > 0) then
      Result := Result - Table[Y1 - 1, X2];
    if (X1 > 0) then
      Result := Result - Table[Y2, X1 - 1];
    if (Y1 > 0) and (X1 > 0) then
      Result := Result + Table[Y1 - 1, X1 - 1];
  end;

  function GetMean(B: TBox): Single;
  begin
    Result := QuerySumTable(SumTable, B.X1, B.Y1, B.X2, B.Y2) / B.Area();
  end;

  function GetStdDev(B: TBox; Mean: Double): Single;
  begin
    Result := QuerySumTable(SumTableSquared, B.X1, B.Y1, B.X2, B.Y2) - (Sqr(Mean) * B.Area);
    Result := Sqrt(Result / B.Area);
  end;

var
  X, Y, W, H: Integer;
  Mean, StdDev, Thresh: Single;
  B: TBox;
  Matrix: TByteMatrix;
  Background, Foreground: TRGB32;
begin
  Background := RGBToBGR(0);
  Foreground := RGBToBGR(255);
  if AInvert then
    Swap(Background, Foreground);

  Matrix := ToGreyMatrix();

  SumTable := CreateSumTable(Matrix, False);
  SumTableSquared := CreateSumTable(Matrix, True);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      B.X1 := Max(X-Window, 0);
      B.Y1 := Max(Y-Window, 0);
      B.X2 := Min(X+Window, W);
      B.Y2 := Min(Y+Window, H);

      Mean   := GetMean(B);
      StdDev := GetStdDev(B, Mean);
      Thresh := Mean * (1.0 + K * ((StdDev / 128.0) - 1.0));

      if Matrix[Y, X] < Thresh then
        FData[Y*FWidth+X] := Foreground
      else
        FData[Y*FWidth+X] := Background;
    end;
end;

procedure TMufasaBitmap.Pad(Amount: Integer);
var
  OldWidth, OldHeight: Integer;
  Y: Integer;
begin
  OldWidth := FWidth;
  OldHeight := FHeight;

  SetSize(FWidth + (Amount * 2), FHeight + (Amount * 2));

  for Y := OldHeight downto 0 do
  begin
    Move(FData[Y * FWidth], FData[(Y + Amount) * FWidth + Amount], OldWidth * SizeOf(TRGB32));

    // clear old pixels
    if Y < Amount then
      FillByte(FData[Y * FWidth], OldWidth * SizeOf(TRGB32), 0)
    else
      FillByte(FData[Y * FWidth], Amount * SizeOf(TRGB32), 0);
  end;
end;

function TMufasaBitmap.ToGreyMatrix: TByteMatrix;
var
  X, Y: Integer;
begin
  Result.SetSize(FWidth, FHeight);

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
      with FData[Y * FWidth + X] do
        Result[Y, X] := Round(R * 0.3 + G * 0.59 + B * 0.11);
end;

procedure TMufasaBitmap.SetPersistentMemory(mem: PtrUInt; NewWidth, NewHeight: Integer);
begin
  SetSize(0, 0);

  FExternalData := True;
  FWidth := NewWidth;
  FHeight := NewHeight;
  FData := PRGB32(mem);
end;

procedure TMufasaBitmap.ResetPersistentMemory;
begin
  if FExternalData then
  begin
    FExternalData := False;
    FData := nil;

    SetSize(0, 0);
  end;
end;

procedure TMufasaBitmap.NotifyUnfreed;
begin
  inherited NotifyUnfreed();

  if (DebugUnfreedBitmaps <> '') then
  try
    SaveToFile(IncludeTrailingPathDelimiter(SetDirSeparators(DebugUnfreedBitmaps)) + IntToStr(PtrUInt(Self)) + '.bmp');
  except
    on E: Exception do
      SimbaDebugLn(E.ToString);
  end;
end;

function TMufasaBitmap.GetPixel(X, Y: Integer): Integer;
begin
  if not PointInBitmap(X, Y) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X, Y, FWidth, FHeight]);

  Result := BGRToRGB(FData[Y * FWidth + X]);
end;

procedure TMufasaBitmap.SetPixel(X, Y: Integer; Color: Integer);
begin
  if not PointInBitmap(X, Y) then
    raise ESimbaBitmapException.CreateFmt(sbeOutOfBounds, [X, Y, FWidth, FHeight]);

  FData[Y * FWidth + X] := RGBToBGR(Color);
end;

procedure TMufasaBitmap.SetTransparentColor(Value: Integer);
begin
  FTransparentColor := Value;
  FTransparentRGB := RGBToBGR(Value)
end;

function TMufasaBitmap.PointInBitmap(const P: TPoint): Boolean;
begin
  Result := (P.X >= 0) and (P.Y >= 0) and (P.X < FWidth) and (P.Y < FHeight);
end;

function TMufasaBitmap.PointInBitmap(const X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight);
end;

constructor TMufasaBitmap.Create;
begin
  inherited Create();

  FTextDrawer := TSimbaTextDrawer.Create(Self);

  TransparentColorActive := True;
  TransparentColor := 0;
end;

constructor TMufasaBitmap.Create(AWidth, AHeight: Integer);
begin
  Create();

  SetSize(AWidth, AHeight);
end;

constructor TMufasaBitmap.CreateFromClient(Client: TObject);
begin
  Create();

  FClient := Client;

  LoadFromClient();
end;

constructor TMufasaBitmap.CreateFromClient(Client: TObject; Area: TBox);
begin
  Create();

  FClient := Client;

  LoadFromClient(Area);
end;

constructor TMufasaBitmap.CreateFromFile(FileName: String);
begin
  Create();

  LoadFromFile(FileName);
end;

constructor TMufasaBitmap.CreateFromString(AWidth, AHeight: Integer; Str: String);
begin
  Create();

  LoadFromString(AWidth, AHeight, Str);
end;

constructor TMufasaBitmap.CreateFromData(AWidth, AHeight: Integer; AData: PRGB32; CopyData: Boolean);
begin
  Create();

  LoadFromData(AWidth, AHeight, AData, CopyData);
end;

destructor TMufasaBitmap.Destroy;
begin
  if not FExternalData then
    SetSize(0, 0);

  if (FTextDrawer <> nil) then
    FreeAndNil(FTextDrawer);

  inherited Destroy();
end;

end.

