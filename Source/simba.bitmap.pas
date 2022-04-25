{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap;

{$i simba.inc}

interface

uses
  classes, sysutils, graphtype, graphics,
  simba.mufasatypes, simba.bitmap_textdrawer;

type
  TMBitmaps = class;

  PBmpMirrorStyle = ^TBmpMirrorStyle;
  TBmpMirrorStyle  = (MirrorWidth, MirrorHeight, MirrorLine);

  PBmpThreshMethod =  ^TBmpThreshMethod;
  TBmpThreshMethod = (TM_Mean, TM_MinMax);

  PBmpResizeMethod = ^TBmpResizeMethod;
  TBmpResizeMethod = (RM_Nearest, RM_Bilinear);

  PMufasaBitmap = ^TMufasaBitmap;
  TMufasaBitmap = class(TObject)
  protected
    FWidth,FHeight: Int32;
    FTransparentColor: TRGB32;
    FTransparentSet: Boolean;
    FIndex: Int32;
    FName: String;
    FList: TMBitmaps;
    FData: PRGB32;
    { True if we do not own FData }
    FExternData: Boolean;
    FTextDrawer: TSimbaTextDrawer;

    function GetCenter: TPoint;
    function GetFontAntialiasing: Boolean;
    function GetFontName: String;
    function GetFonts: TStringArray;
    function GetFontSize: Single;
    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
  public
    property Name: String read FName write FName;
    property Index: Int32 read FIndex write FIndex;
    property Data: PRGB32 read FData write FData;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;
    property Center: TPoint read GetCenter;

    property Fonts: TStringArray read GetFonts;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;

    function TextWidth(Text: String): Int32;
    function TextHeight(Text: String): Int32;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: TColor); overload;

    procedure SetSize(AWidth,AHeight: Int32);
    procedure StretchResize(AWidth,AHeight: Int32);
    procedure ResizeEx(Method: TBmpResizeMethod; NewWidth, NewHeight: Int32);

    procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: Int32);
    procedure ResetPersistentMemory;

    function PointInBitmap(x,y: Int32): Boolean;
    procedure ValidatePoint(x,y: Int32);
    function SaveToFile(FileName: String) :Boolean;
    procedure LoadFromFile(const FileName: String);
    procedure Rectangle(B: TBox; Color: TColor);
    procedure Rectangle(const Box: TBox; const Color: Int32; const Transparency: Extended); overload;
    procedure FloodFill(const StartPT: TPoint; const SearchCol, ReplaceCol: TColor);
    procedure FastSetPixel(X, Y: Int32; Color: TColor);
    procedure FastSetPixels(Points: TPointArray; Colors: TIntegerArray);
    procedure DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray); overload;
    procedure DrawATPA(ATPA: T2DPointArray); overload;
    procedure DrawTPA(Points: TPointArray; Color: TColor);
    procedure DrawPolyFilled(Poly: TPointArray; Invert: Boolean; Color: TColor);
    procedure DrawCircleFilled(X, Y, Radius: Int32; Invert: Boolean; Color: TColor);
    procedure DrawBoxFilled(B: TBox; Invert: Boolean; Color: TColor);
    procedure DrawToCanvas(x,y: Int32; Canvas: TCanvas);
    procedure LineTo(Src,Dst: TPoint;Color: TColor);
    function FindColors(out Points: TPointArray; Color: Integer): Boolean; overload;
    function FindColors(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload;
    function FastGetPixel(x,y: Int32): TColor;
    function FastGetPixels(Points: TPointArray): TIntegerArray;
    function GetAreaColors(xs,ys,xe,ye: Int32): T2DIntegerArray;
    function GetColors: TIntegerArray;
    function GetHSLValues(xs, ys, xe, ye: Int32): T2DHSLArray;
    procedure FastDrawClear(Color: TColor);
    procedure FastDrawTransparent(x, y: Int32; TargetBitmap: TMufasaBitmap);
    procedure FastReplaceColor(OldColor, NewColor: TColor);
    procedure CopyClientToBitmap(MWindow: TObject;Resize: Boolean; xs, ys, xe, ye: Int32); overload;
    procedure CopyClientToBitmap(MWindow: TObject;Resize: Boolean;x,y: Int32; xs, ys, xe, ye: Int32); overload;
    procedure RotateBitmap(angle: Extended; TargetBitmap: TMufasaBitmap);
    procedure RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap);
    procedure Desaturate(TargetBitmap: TMufasaBitmap); overload;
    procedure Desaturate; overload;
    procedure GreyScale(TargetBitmap: TMufasaBitmap);overload;
    procedure GreyScale;
    procedure Brightness(TargetBitmap: TMufasaBitmap; br: Int32); overload;
    procedure Brightness(br: Int32); overload;
    procedure Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload;
    procedure Contrast(co: Extended); overload;
    procedure Invert(TargetBitmap: TMufasaBitmap); overload;
    procedure Invert; overload;
    procedure Posterize(TargetBitmap: TMufasaBitmap; Po: Int32); overload;
    procedure Posterize(Po: Int32); overload;
    procedure Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);

    procedure Blend(Points: TPointArray; Size: Int32);
    function  CompareAt(Other: TMufasaBitmap; Pt: TPoint; Tol: Int32): Extended;
    procedure Downsample(DownScale: Int32; TargetBitmap: TMufasaBitmap); overload;
    function Downsample(DownScale: Int32; BlendTransparentColor: Boolean = True): TMufasaBitmap; overload;
    function Copy(const xs,ys,xe,ye: Int32): TMufasaBitmap; overload;
    function Copy: TMufasaBitmap; overload;
    procedure Blur(const Block, xs, ys, xe, ye: Int32); overload;
    procedure Blur(const Block: Int32); overload;
    procedure Crop(const xs, ys, xe, ye: Int32);

    function ToTBitmap: TBitmap;
    function ToString: String; override;
    function ToMatrixBGR: TIntegerMatrix;
    function ToMatrix: TIntegerMatrix;
    function ToGreyMatrix: TByteMatrix;
    function ToRawImage: TRawImage;

    procedure DrawMatrix(const Matrix: TIntegerMatrix);
    procedure DrawMatrix(const Matrix: TSingleMatrix; ColorMapID: Int32 = 0); overload;
    procedure ThresholdAdaptive(Alpha, Beta: Byte; DoInvert: Boolean; Method: TBmpThreshMethod; C: Int32);
    procedure Pad(Amount: Int32);
    function RowPtrs: TPRGB32Array;
    procedure LoadFromMemory(Memory: PRGB32; AWidth, AHeight: Int32);
    procedure LoadFromTBitmap(bmp: TBitmap);
    procedure LoadFromRawImage(RawImage: TRawImage);
    function CreateTMask: TMask;
    procedure ResizeBilinear(NewW, NewH: Int32);
    procedure SetTransparentColor(Col: TColor);
    function GetTransparentColor: TColor;
    property TransparentColorSet: Boolean read FTransparentSet;
    property TransparentRGB: TRGB32 read FTransparentColor;
    property List: TMBitmaps read FList write FList;
    procedure SetAlphaValue(const value: byte);
    constructor Create;
    destructor Destroy;override;
  end;
  TMufasaBmpArray = Array of TMufasaBitmap;
  
  PMBitmaps = ^TMBitmaps;
  TMBitmaps = class(TObject)
  protected
    Client: TObject;
    FreeSpots: Array of Int32;
    BmpArray: TMufasaBmpArray;
    BmpsCurr, BmpsHigh, FreeSpotsHigh, FreeSpotsLen: Int32;
    function GetNewIndex: Int32;
  public
    function GetBMP(Index: Int32): TMufasaBitmap;
    property Bmp[Index: Int32]: TMufasaBitmap read GetBMP; default;
    function CreateBMP(w, h: Int32): Int32;
    function ExistsBMP(Index: Int32): Boolean;
    function AddBMP(_bmp: TMufasaBitmap): Int32;
    function CopyBMP( Bitmap: Int32): Int32;
    function CreateMirroredBitmap(bitmap: Int32; MirrorStyle: TBmpMirrorStyle): Int32;
    function CreateBMPFromFile(const Path: String): Int32;
    function CreateBMPFromString(width,height: Int32; Data: String): Int32;overload;
    function CreateBMPFromString(BmpName: String; width, height: Int32; Data: String): Int32;overload;
    function RemoveBMP(Number: Int32): TMufasaBitmap;
    constructor Create(Owner: TObject);
    destructor Destroy;override;
  end;

  TBitmapDataFormat = (
    dfBGR,   // color bitmap 8-8-8 B-G-R
    dfRGB,   // color bitmap 8-8-8 R-G-B
    dfARGB,  // color bitmap with alpha channel first 8-8-8-8 A-R-G-B
    dfRGBA,  // color bitmap with alpha channel last 8-8-8-8 R-G-B-A
    dfABGR,  // color bitmap with alpha channel first 8-8-8-8 A-B-G-R
    dfBGRA   // color bitmap with alpha channel last 8-8-8-8 B-G-R-A);
  );

  TBitmap_Helper = class helper for TBitmap
    function DataFormat: TBitmapDataFormat;
  end;

  function CalculatePixelShift(Bmp1,Bmp2: TMufasaBitmap; CompareBox: TBox): Int32;
  function CalculatePixelShiftTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray): Int32;
  function CalculatePixelTolerance(Bmp1,Bmp2: TMufasaBitmap; CompareBox: TBox; CTS: Int32): extended;
  function CalculatePixelToleranceTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray; CTS: Int32): extended;

implementation

uses
  math, intfgraphics,
  simba.tpa, simba.stringutil, simba.colormath, simba.iomanager, simba.overallocatearray,
  simba.helpers_matrix;

function TBitmap_Helper.DataFormat: TBitmapDataFormat;
var
  Desc: TRawImageDescription;
begin
  Desc := RawImage.Description;

  if Desc.BitsPerPixel = 32 then
  begin
    if Desc.ByteOrder = riboMSBFirst then
    begin
      if (Desc.AlphaShift = 0) and (Desc.RedShift = 16) and (Desc.GreenShift = 8) and (Desc.BlueShift = 0) then
        Result := dfARGB
      else
      if (Desc.AlphaShift = 24) and (Desc.RedShift = 16) and (Desc.GreenShift = 8) and (Desc.BlueShift = 0) then
        Result := dfARGB
      else
      if (Desc.AlphaShift = 0) and (Desc.RedShift = 24) and (Desc.GreenShift = 16) and (Desc.BlueShift = 8) then
        Result := dfRGBA
      else
      if (Desc.AlphaShift = 0) and (Desc.RedShift = 8) and (Desc.GreenShift = 16)  and (Desc.BlueShift = 24) then
        Result := dfBGRA
      else
        raise Exception.Create('Unknown bitmap format(32): ' + Desc.AsString);
    end else
    begin
      if (Desc.AlphaShift = 0) and (Desc.RedShift = 16) and (Desc.GreenShift = 8) and (Desc.BlueShift = 0) then
        Result := dfBGRA
      else
      if (Desc.AlphaShift = 24) and (Desc.RedShift = 16) and (Desc.GreenShift = 8) and (Desc.BlueShift = 0) then
        Result := dfBGRA
      else
      if (Desc.AlphaShift = 0) and (Desc.RedShift = 8) and (Desc.GreenShift = 16) and (Desc.BlueShift = 24) then
        Result := dfARGB
      else
      if (Desc.AlphaShift = 24) and (Desc.RedShift = 0) and (Desc.GreenShift = 8) and (Desc.BlueShift = 16) then
        Result := dfRGBA
      else
        raise Exception.Create('Unknown bitmap format(32): ' + Desc.AsString);
    end;
  end else
  if Desc.BitsPerPixel = 24 then
  begin
    if Desc.ByteOrder = riboMSBFirst then
    begin
      if (Desc.RedShift = 24) and (Desc.GreenShift = 16) and (Desc.BlueShift = 8) then
        Result := dfRGB
      else
      if (Desc.RedShift = 8) and (Desc.GreenShift = 16) and (Desc.BlueShift = 24) then
        Result := dfBGR
      else
        raise Exception.Create('Unknown bitmap format(24): ' + Desc.AsString);
    end else
    begin
      if (Desc.RedShift = 16) and (Desc.GreenShift = 8) and (Desc.BlueShift = 0) then
        Result := dfBGR
      else
      if (Desc.RedShift = 0) and (Desc.GreenShift = 8) and (Desc.BlueShift = 16) then
        Result := dfRGB
      else
        raise Exception.Create('Unknown bitmap format(24): ' + Desc.AsString);
    end;
  end;
end;

function CalculatePixelShift(Bmp1, Bmp2: TMufasaBitmap; CompareBox: TBox): Int32;
var
  x,y: Int32;
  w1,w2: Int32;
begin
  Bmp1.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp2.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  Result := 0;
  for y := CompareBox.y1 to CompareBox.y2 do
    for x := CompareBox.x1 to CompareBox.x2 do
      if LongWord(Bmp1.FData[y * w1 + x]) <> LongWord(Bmp2.Fdata[y * w2 + x]) then
        inc(Result);
end;

function CalculatePixelShiftTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray): Int32;
var
  i: Int32;
  bounds: TBox;
  w1,w2: Int32;
begin
  bounds := GetTPABounds(CPoints);
  Bmp1.ValidatePoint(bounds.x1,bounds.y1);
  Bmp1.ValidatePoint(bounds.x2,bounds.y2);
  Bmp2.ValidatePoint(bounds.x1,bounds.y1);
  Bmp2.ValidatePoint(bounds.x2,bounds.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.width;
  w2 := bmp2.width;
  Result := 0;
  for i := 0 to High(CPoints) do
    if LongWord(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x]) <>
        LongWord(Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x]) then
      inc(Result);
end;

//CTS 0 counts the average difference in R,G,B per pixel
//CTS 1 counts the average difference using SQRT(Sqr(r) + sqr(g)+sqr(b));
function CalculatePixelTolerance(Bmp1, Bmp2: TMufasaBitmap; CompareBox: TBox;
  CTS: Int32): extended;
var
  x,y: Int32;
  w1,w2: Int32;
  Diff: int64;
begin
  Bmp1.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  Result := 0;
  if not InRange(CTS,0,1) then
    raise Exception.CreateFmt('CTS Passed to CalculateTolerance must be in [0..1], it currently is %d',[CTS]);
  case CTS of
    0: begin
          Diff := 0;
          for y := CompareBox.y1 to CompareBox.y2 do
            for x := CompareBox.x1 to CompareBox.x2 do
            begin
              Diff := Diff + abs(Bmp1.FData[y * w1 + x].r-Bmp2.Fdata[y * w2 + x].r) +
                             abs(Bmp1.FData[y * w1 + x].g-Bmp2.Fdata[y * w2 + x].g) +
                             abs(Bmp1.FData[y * w1 + x].b-Bmp2.Fdata[y * w2 + x].b);
            end;
          Result := Diff / (3 * (CompareBox.x2 - CompareBox.x1 + 1) * (CompareBox.y2-CompareBox.y1 + 1)); //We want the value for the whole Pixel; so divide by 3 (RGB)
        end;
    1: begin
          for y := CompareBox.y1 to CompareBox.y2 do
            for x := CompareBox.x1 to CompareBox.x2 do
              Result := Result + Sqrt(Sqr(Bmp1.FData[y * w1 + x].r-Bmp2.Fdata[y * w2 + x].r) +
                                      Sqr(Bmp1.FData[y * w1 + x].g-Bmp2.Fdata[y * w2 + x].g) +
                                      Sqr(Bmp1.FData[y * w1 + x].b-Bmp2.Fdata[y * w2 + x].b));
          Result := Result / ((CompareBox.x2 - CompareBox.x1 + 1) * (CompareBox.y2-CompareBox.y1 + 1)); //We want the value for the whole Pixel;
        end;
  end;
end;

function CalculatePixelToleranceTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray;
  CTS: Int32): extended;
var
  i: Int32;
  bounds: TBox;
  w1,w2: Int32;
  Diff: int64;
begin
  bounds := GetTPABounds(CPoints);
  Bmp1.ValidatePoint(bounds.x1,bounds.y1);
  Bmp1.ValidatePoint(bounds.x2,bounds.y2);
  Bmp2.ValidatePoint(bounds.x1,bounds.y1);
  Bmp2.ValidatePoint(bounds.x2,bounds.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  Result := 0;
  if not InRange(CTS,0,1) then
    raise Exception.CreateFmt('CTS Passed to CalculateTolerance must be in [0..1], it currently is %d',[CTS]);
  case CTS of
    0: begin
          Diff := 0;
          for i := 0 to High(CPoints) do
            begin
              Diff := Diff + abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].r-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].r) +
                             abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].g-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].g) +
                             abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].b-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].b);
            end;
          Result := Diff / (3 * (bounds.x2 - bounds.x1 + 1) * (bounds.y2-bounds.y1 + 1)); //We want the value for the whole Pixel; so divide by 3 (RGB)
        end;
    1: begin

          for i := 0 to High(CPoints) do
            Result := Result + Sqrt(Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].r-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].r) +
                                    Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].g-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].g) +
                                    Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].b-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].b));
          Result := Result / ((bounds.x2 - bounds.x1 + 1) * (bounds.y2-bounds.y1 + 1)); //We want the value for the whole Pixel;
        end;
  end;
end;

{ TMBitmaps }

function TMBitmaps.GetNewIndex: Int32;
begin
  if BmpsCurr < BmpsHigh then
  begin;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end else if (FreeSpotsHigh > -1) then
  begin;
    Result := FreeSpots[FreeSpotsHigh];
    dec(FreeSpotsHigh);
  end else
  begin;
    SetLength(BmpArray, BmpsHigh + 6);
    BmpsHigh := BmpsHigh + 5;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end;
end;

function TMBitmaps.GetBMP(Index: Int32): TMufasaBitmap;
begin
  Result := nil;
  if (Index >= 0) and (Index <= BmpsCurr) then
    if BmpArray[Index] <> nil then
      Result := BmpArray[Index];
  if Result = nil then
    raise Exception.CreateFmt('The bitmap[%d] does not exist',[Index]);
end;

function TMBitmaps.CreateBMP(w,h: Int32): Int32;
var
  Bitmap: TMufasaBitmap;
begin
  Bitmap := TMufasaBitmap.Create;
  Bitmap.SetSize(w,h);
  Result := addBMP(Bitmap);
end;

function TMBitmaps.AddBMP(_bmp: TMufasaBitmap): Int32;
begin
  Result := GetNewIndex;

  BmpArray[Result] := _bmp;
  BmpArray[Result].Index := Result;
  BmpArray[Result].List := Self;
end;

function TMBitmaps.CopyBMP(Bitmap: Int32): Int32;
var
  InputBMP: TMufasaBitmap;
  OutputBMP: TMUfasaBitmap;
begin
  InputBMP := GetBMP(Bitmap);
  Result := CreateBMP(InputBmp.Width,InputBMP.Height);
  OutputBMP := GetBMP(Result);
  Move(InputBMP.FData[0],OutPutBMP.FData[0],InputBMP.Width * InputBMP.Height * SizeOf(TRGB32));
end;

function TMBitmaps.CreateMirroredBitmap(bitmap: Int32;
  MirrorStyle: TBmpMirrorStyle): Int32;
var
  w,h: Int32;
  y,x: Int32;
  Source,Dest: PRGB32;
begin
  Source := Bmp[Bitmap].FData;
  w := BmpArray[Bitmap].Width;
  h := BmpArray[Bitmap].Height;
  if MirrorStyle = MirrorLine then
    Result := CreateBMP(h,w)
  else
    Result := CreateBMP(w,h);
  Dest := BmpArray[Result].FData;
  case MirrorStyle of
    MirrorWidth:  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[y*w+x] := Source[y*w+w-1-x];
    MirrorHeight: for y := (h-1) downto 0 do
                    Move(Source[y*w],Dest[(h-1 - y) * w],w*SizeOf(TRGB32));
    MirrorLine:  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[x*h+y] := Source[y*w+x];

  end;
//Can be optmized, this is just proof of concept
end;

function TMBitmaps.CreateBMPFromFile(const Path: String): Int32;
begin
  Result := CreateBMP(0,0);
  try
    BmpArray[Result].LoadFromFile(Path);
  except
    BmpArray[Result].Free();
    Result := -1; // meh
    raise;
  end;
end;

function HexToInt(HexNum: String): LongInt;inline;
begin
   Result:=StrToInt('$' + HexNum);
end;

function TMBitmaps.ExistsBMP(Index: Int32): Boolean;
begin
  Result := false;
  if (Index >= 0) and (Index <= BmpsCurr) then
    Result := Assigned(BmpArray[Index]);
end;

function TMBitmaps.CreateBMPFromString(Width, Height: Int32; Data: String): Int32;
var
  i: Int32;
  Source: String;
  SourcePtr: PRGB24;
  DestPtr: PRGB32;
begin
  Result := CreateBMP(Width, Height);

  if (Data <> '') then
  begin
    Source := '';
    if Data[1] = 'm' then
      Source := DecompressString(Base64Decode(Data.Remove(0, 1)), False);

    if Source = '' then
      raise Exception.Create('Invalid bitmap String');

    SourcePtr := @Source[1];
    DestPtr := PRGB32(BmpArray[Result].FData);

    for i := Width * Height - 1 downto 0 do
    begin
      DestPtr[i].R := SourcePtr[i].R;
      DestPtr[i].G := SourcePtr[i].G;
      DestPtr[i].B := SourcePtr[i].B;
    end;
  end;
end;

function TMBitmaps.CreateBMPFromString(BmpName: String; width, height: Int32; Data: String): Int32;
begin
  Result := Self.CreateBMPFromString(width,height,data);
  Bmp[Result].Name:= BmpName;
end;

function TMBitmaps.RemoveBMP(Number: Int32): TMufasaBitmap;
begin
  Result := GetBMP(Number);
  if (Number < BmpsCurr) then
  begin
    Inc(FreeSpotsHigh);
    if (FreeSpotsHigh = FreeSpotsLen) then
    begin
      Inc(FreeSpotsLen);
      SetLength(FreeSpots, FreeSpotsLen);
    end;

    FreeSpots[FreeSpotsHigh] := Number;
  end else
    Dec(BmpsCurr);

  BMPArray[Number] := nil;
  Result.Index := -1;
  Result.List := nil;
end;

function TMufasaBitmap.SaveToFile(FileName: String): Boolean;
var
  Image: TLazIntfImage;
begin
  if ExtractFileExt(FileName) = '' then
    FileName := FileName + '.bmp';

  Image := TLazIntfImage.Create(Self.ToRawImage(), False);

  try
    if not Image.SaveToFile(FileName) then
      raise Exception.CreateFmt('TMufasaBitmap.SaveToFile: Image format "%s" is not supported', [ExtractFileExt(FileName)]);

    Result := True;
  finally
    Image.Free();
  end;
end;

procedure TMufasaBitmap.LoadFromFile(const FileName: String);
var
  LazIntf: TLazIntfImage;
  RawImageDesc: TRawImageDescription;
begin
  try
    LazIntf := TLazIntfImage.Create(0,0);
    RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width,LazIntf.Height);
    LazIntf.DataDescription := RawImageDesc;
    LazIntf.LoadFromFile(FileName);
    if Assigned(FData) then
      FreeMem(FData);
    Self.FWidth := LazIntf.Width;
    Self.FHeight := LazIntf.Height;
    FData := GetMem(Self.FWidth*Self.FHeight*SizeOf(TRGB32));
    Move(LazIntf.PixelData[0],FData[0],FWidth*FHeight*sizeOf(TRGB32));
  finally
    LazIntf.Free;
  end;
end;

procedure TMufasaBitmap.Rectangle(B: TBox; Color: TColor);
var
  Y, Size: Int32;
begin
  B.X1 := Max(0, B.X1);
  B.Y1 := Max(0, B.Y1);
  B.X2 := Min(FWidth-1, B.X2);
  B.Y2 := Min(FHeight-1, B.Y2);

  if (B.X2 - B.X1 < 0) then Exit;
  if (B.Y2 - B.Y1 < 0) then Exit;

  Color := TColor(RGBToBGR(Color));
  Size := B.X2 - B.X1 + 1;
  for Y := B.Y1 to B.Y2 do
    FillDWord(FData[Y * FWidth + B.X1], Size, Color);
end;

procedure TMufasaBitmap.FloodFill(const StartPT: TPoint; const SearchCol,
  ReplaceCol: TColor);
var
  Stack: TPointArray;
  SIndex: Int32;
  CurrX,CurrY: Int32;
  Search,Replace: LongWord;
procedure AddToStack(x,y: Int32);inline;
begin
  if LongWord(FData[y * FWidth + x]) = Search then
  begin
    LongWord(FData[y * FWidth + x]) := Replace;
    Stack[SIndex].x := x;
    Stack[SIndex].y := y;
    inc(SIndex);
  end;
end;
begin
  ValidatePoint(StartPT.x,StartPT.y);
  Search := LongWord(RGBToBGR(SearchCol));
  Replace := LongWord(RGBToBGR(ReplaceCol));
  SetAlphaValue(0);
  if LongWord(FData[StartPT.y * FWidth + StartPT.x]) <> Search then //Only add items to the stack that are the searchcol.
    Exit;
  SetLength(Stack,FWidth * FHeight);
  SIndex := 0;
  AddToStack(StartPT.x,StartPT.y);
  SIndex := 0;
  while (SIndex >= 0) do
  begin;
    CurrX := Stack[SIndex].x;
    Curry := Stack[SIndex].y;
    if (CurrX > 0) and (CurrY > 0)         then AddToStack(CurrX - 1, CurrY - 1);
    if (CurrX > 0)                         then AddToStack(CurrX - 1, CurrY);
    if (CurrX > 0) and (CurrY + 1 < FHeight)     then AddToStack(CurrX - 1, CurrY + 1);
    if (CurrY + 1 < FHeight)                     then AddToStack(CurrX   ,  CurrY + 1);
    if (CurrX + 1 < FWidth) and (CurrY + 1 < FHeight) then AddToStack(CurrX + 1, CurrY + 1);
    if (CurrX + 1 < FWidth)                     then AddToStack(CurrX + 1, CurrY    );
    if (CurrX + 1 < FWidth) and (CurrY > 0)     then AddToStack(CurrX + 1, CurrY - 1);
    if (CurrY > 0)                         then AddToStack(CurrX    , CurrY - 1);
    Dec(SIndex);
  end;
end;

function TMufasaBitmap.Copy: TMufasaBitmap;
begin
  Result := TMufasaBitmap.Create;
  Result.SetSize(self.Width, self.Height);
  Move(self.FData[0], Result.FData[0],self.FWidth * self.FHeight * SizeOf(TRGB32));
end;

function TMufasaBitmap.Copy(const xs, ys, xe, ye: Int32): TMufasaBitmap;
var
  i: Int32;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  Result := TMufasaBitmap.Create;
  Result.SetSize(xe-xs+1, ye-ys+1);
  for i := ys to ye do
    Move(self.FData[i * self.FWidth + xs], Result.FData[(i-ys) * Result.FWidth],Result.Width * SizeOf(TRGB32));
end;

procedure TMufasaBitmap.Crop(const xs, ys, xe, ye: Int32);
var
  i: Int32;
begin
  if (not Self.PointInBitmap(xs, ys)) or (not Self.PointInBitmap(xe, ye)) then
     raise Exception.Create('TMufasaBitmap.Crop(): The bounds you pased to crop exceed the bitmap bounds');

  if (xs > xe) or (ys > ye) then
    raise Exception.CreateFmt('TMufasaBitmap.Crop(): the bounds you passed doesn''t have normal bounds (%d,%d): (%d,%d)', [xs, ys, xe, ye]);

  for i := ys to ye do
    Move(self.FData[i * self.width + xs], self.FData[(i-ys) * self.width], self.width * SizeOf(TRGB32));

  self.SetSize(xe-xs+1, ye-ys+1);
end;

function TMufasaBitmap.ToTBitmap: TBitmap;
begin
  Result.LoadFromRawImage(Self.ToRawImage(), False);
end;

function TMufasaBitmap.ToString: String;
var
  i: Int32;
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

  Result := 'm' + Base64Encode(CompressString(DataStr, False));
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
  X, Y, W, H: Int32;
begin
  SetLength(Result, FHeight, FWidth);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y][X] := BGRToRGB(FData[Y * FWidth + X]);
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

procedure TMufasaBitmap.DrawMatrix(const Matrix: TIntegerMatrix);
var
  X, Y, W, H: Int32;
begin
  SetSize(Matrix.Width, Matrix.Height);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
      FData[Y * FWidth + X] := RGBToBGR(Matrix[Y][X]);
end;

procedure TMufasaBitmap.DrawMatrix(const Matrix: TSingleMatrix; ColorMapID: Int32 = 0); overload;
var
  X,Y, W,H, Color: Int32;
  _H,_S,_L: Extended;
  Normed: TSingleMatrix;
begin
  SetSize(Matrix.Width, Matrix.Height);

  Normed := Matrix.NormMinMax(0, 1);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H-1 do
    for X := 0 to W-1 do
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
      Self.FastSetPixel(X,Y,Color);
    end;       
end;

function TMufasaBitmap.RowPtrs: TPRGB32Array;
var
  I: Int32;
begin;
  SetLength(Result, FHeight);
  for i := 0 to FHeight - 1 do
    Result[i] := FData + FWidth * i;
end;

procedure TMufasaBitmap.LoadFromMemory(Memory: PRGB32; AWidth, AHeight: Int32);
begin
  SetSize(AWidth, AHeight);

  Move(Memory^, FData^, AWidth * AHeight * SizeOf(TRGB32));
end;

procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage);
var
  x,y: Int32;
  _24_old_p: PByte;
  rs,gs,bs:byte;
  NewData: PRGB32;
begin
  // clear data
  Self.SetSize(0,0);

  if (RawImage.Description.BitsPerPixel <> 24) and (RawImage.Description.BitsPerPixel <> 32) then
    raise Exception.CreateFMT('TMufasaBitmap.LoadFromRawImage - BitsPerPixel is %d', [RawImage.Description.BitsPerPixel]);

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

procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);
begin
  LoadFromRawImage(bmp.RawImage);
end;

procedure TMufasaBitmap.FastSetPixel(X, Y: Int32; Color: TColor);
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    FData[Y * FWidth + X] := RGBToBGR(Color);
end;

procedure TMufasaBitmap.FastSetPixels(Points: TPointArray; Colors: TIntegerArray);
var
  I: Int32;
  X, Y: Int32;
begin
  if (Length(Points) = 0) or (Length(Colors) = 0) then
    Exit;

  for I := 0 to High(Points) do
  begin
    X := Points[I].X;
    Y := Points[I].Y;
    if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
      FData[Y * FWidth + X] := RGBToBGR(Colors[Min(I, High(Colors))]);
  end;
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray);
var
  I: Int32;
begin
  if (Length(ATPA) = 0) or (Length(Colors) = 0) then
    Exit;

  for I := 0 to High(ATPA) do
    DrawTPA(ATPA[I], Colors[Min(I, High(Colors))]);
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray);
var
  I: Int32;
begin
  for I := 0 to High(ATPA) do
    DrawTPA(ATPA[I], Random($FFFFFF));
end;

procedure TMufasaBitmap.DrawTPA(Points: TPointArray; Color: TColor);
var
  Ptr: PPoint;
  Upper: PtrUInt;
  BGR: TRGB32;
begin
  if (Length(Points) = 0) then
    Exit;

  BGR := RGBToBGR(Color);

  Ptr := @Points[0];
  Upper := PtrUInt(@Points[High(Points)]);

  while (PtrUInt(Ptr) <= Upper) do
  begin
    if (Ptr^.X >= 0) and (Ptr^.Y >= 0) and (Ptr^.X < Self.FWidth) and (Ptr^.Y < Self.FHeight) then
      FData[Ptr^.Y * Self.FWidth + Ptr^.X] := BGR;

    Inc(Ptr);
  end;
end;

procedure TMufasaBitmap.DrawPolyFilled(Poly: TPointArray; Invert: Boolean; Color: TColor);
var
  X, Y, Hi: Int32;

  function InPoly: Boolean; inline;
  var
    wn, I, J:Int32;
  begin
    wn := 0;
    J := Hi;

    for I := 0 to Hi do
    begin
      if (Poly[I].Y <= Y) then
      begin
        if (Poly[J].Y > Y) then
          if (((Poly[J].X-Poly[I].X) * (Y-Poly[I].Y) - (X-Poly[I].X) * (Poly[J].Y-Poly[I].Y)) > 0) then
            Inc(wn);
      end else
        if Poly[J].Y <= Y then
          if (((Poly[J].X-Poly[I].X) * (Y-Poly[I].Y) - (X-Poly[I].X) * (Poly[J].Y-Poly[I].Y)) < 0) then
            Dec(wn);
      J := I;
    end;

    Result := (wn <> 0);
  end;

var
  W, H: Int32;
  Bounds: TBox;
  RGB: TRGB32;
begin
  Hi := High(Poly);
  if (Hi < 0) then
    Exit;

  RGB := RGBToBGR(Color);

  W := FWidth-1;
  H := FHeight-1;

  Bounds := GetTPABounds(Poly);
  Bounds.X1 := Max(0, Bounds.X1);
  Bounds.Y1 := Max(0, Bounds.Y1);
  Bounds.X2 := Min(W, Bounds.X2);
  Bounds.Y2 := Min(H, Bounds.Y2);

  case Invert of
    False:
      for X := Bounds.X1 to Bounds.X2 do
        for Y := Bounds.Y1 to Bounds.Y2 do
          if InPoly then
            FData[Y*FWidth+X] := RGB;

    True:
      begin
        Self.DrawBoxFilled(Bounds, True, Color);

        for X := Bounds.X1 to Bounds.X2 do
          for Y := Bounds.Y1 to Bounds.Y2 do
            if not InPoly then
              FData[Y*FWidth+X] := RGB;
      end;
  end;
end;

procedure TMufasaBitmap.DrawCircleFilled(X, Y, Radius: Int32; Invert: Boolean; Color: TColor);
var
  RGB: TRGB32;
  Bounds: TBox;
  LoopX, LoopY: Int32;
begin
  if (Radius <= 0) then
    Exit;

  RGB := RGBToBGR(Color);

  Bounds.X1 := Max(X-Radius, 0);
  Bounds.Y1 := Max(Y-Radius, 0);
  Bounds.X2 := Min(X+Radius, FWidth-1);
  Bounds.Y2 := Min(Y+Radius, FHeight-1);

  case Invert of
    False:
      for LoopX := Bounds.X1 to Bounds.X2 do
        for LoopY := Bounds.Y1 to Bounds.Y2 do
          if Hypot(LoopX - X, LoopY - Y) <= Radius then
            FData[LoopY * FWidth + LoopX] := RGB;

    True:
      begin
        Self.DrawBoxFilled(Bounds, True, Color);

        for LoopX := Bounds.X1 to Bounds.X2 do
          for LoopY := Bounds.Y1 to Bounds.Y2 do
            if Hypot(LoopX - X, LoopY - Y) >= Radius then
              FData[LoopY * FWidth + LoopX] := RGB;
      end;
  end;
end;

procedure TMufasaBitmap.DrawBoxFilled(B: TBox; Invert: Boolean; Color: TColor);
var
  Y, Size: Int32;
begin
  B.X1 := Max(0, B.X1);
  B.Y1 := Max(0, B.Y1);
  B.X2 := Min(FWidth-1, B.X2);
  B.Y2 := Min(FHeight-1, B.Y2);

  case Invert of
    True:
      begin
        B := B.Expand(1);

        Self.DrawBoxFilled(Box(0,    0,    B.X1,     B.Y1),      False, Color); //Top Left
        Self.DrawBoxFilled(Box(0,    B.Y1, B.X1,     B.Y2),      False, Color); //Mid Left
        Self.DrawBoxFilled(Box(0,    B.Y1, B.X1,     FHeight-1), False, Color); //Btm Left
        Self.DrawBoxFilled(Box(B.X1, 0,    B.X2,     B.Y1),      False, Color); //Top Mid
        Self.DrawBoxFilled(Box(B.X1, B.Y2, B.X2,     FHeight-1), False, Color); //Btm Mid
        Self.DrawBoxFilled(Box(B.X2, 0,    FWidth-1, B.Y1),      False, Color); //Top Right
        Self.DrawBoxFilled(Box(B.X2, B.Y1, FWidth-1, B.Y2),      False, Color); //Mid Right
        Self.DrawBoxFilled(Box(B.X2, B.Y1, FWidth-1, FHeight-1), False, Color); //Btm Right
      end;

    False:
      begin
        if (B.X2 - B.X1 < 0) then Exit;
        if (B.Y2 - B.Y1 < 0) then Exit;

        Color := TColor(RGBToBGR(Color));
        Size := B.X2 - B.X1 + 1;
        for Y := B.Y1 to B.Y2 do
          FillDWord(FData[Y * FWidth + B.X1], Size, Color);
      end;
  end;
end;

procedure TMufasaBitmap.DrawToCanvas(x,y: Int32; Canvas: TCanvas);
var
  Bitmap: Graphics.TBitmap;
begin
  Bitmap := Self.ToTBitmap;
  Canvas.Draw(x,y,Bitmap);
  Bitmap.free;
end;

procedure TMufasaBitmap.LineTo(Src, Dst: TPoint;Color: TColor);
var
  TPA: TPointArray;
begin
  TPA:=TPAFromLine(src.x,src.y,dst.x,dst.y);
 // if (not Assigned(TPA)) or (Length(TPA)< 2) then exit;
  Self.DrawTPA(TPA,Color);
end;

function RGB32ToTColor(constref RGB: TRGB32): TColor; inline;
begin
  Result := TColor(RGB) and $FFFFFF; // Remove alpha value
end;

function TMufasaBitmap.FindColors(out Points: TPointArray; Color: Integer): Boolean;
var
  X, Y, W, H: Int32;
  P: TPoint;
  Arr: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Arr.Init();

  Color := RGB32ToTColor(RGBToBGR(Color));

  W := FWidth - 1;
  H := FHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      if RGB32ToTColor(Self.FData[Y * FWidth + X]) = Color then
      begin
        P.X := X;
        P.Y := Y;

        Arr.Add(P);
      end;
    end;

  Points := Arr.Trim();

  Result := Arr.Count > 0;
end;

function TMufasaBitmap.FindColors(out Points: TPointArray; Color, Tolerance: Integer): Boolean;
var
  X, Y, W, H: Int32;
  P: TPoint;
  RGB: TRGB32;
  Arr: specialize TSimbaOverAllocateArray<TPoint>;
begin
  Arr.Init();

  RGB := RGBToBGR(Color);
  Tolerance := Sqr(Tolerance);

  W := FWidth - 1;
  H := FHeight - 1;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      with Self.FData[Y * FWidth + X] do
        if (Sqr(R - RGB.R) + Sqr(G - RGB.G) + Sqr(B - RGB.B) <= Tolerance) then
        begin
          P.X := X;
          P.Y := Y;

          Arr.Add(P);
        end;
    end;

  Points := Arr.Trim();

  Result := Arr.Count > 0;
end;

function TMufasaBitmap.FastGetPixel(x, y: Int32): TColor;
begin
  ValidatePoint(x,y);
  Result := BGRToRGB(FData[y*FWidth+x]);
end;

function TMufasaBitmap.FastGetPixels(Points: TPointArray): TIntegerArray;
var
  i,len: Int32;
  Box : TBox;
begin
  len := high(Points);
  Box := GetTPABounds(Points);
  if (Box.x1 < 0) or (Box.y1 < 0) or (Box.x2 >= self.FWidth) or (Box.y2 >= self.FHeight) then
    raise Exception.Create('The Points you passed to FastGetPixels exceed the bitmap''s bounds');
  SetLength(Result,len+1);
  for i := 0 to len do
    Result[i] := BGRToRGB(FData[Points[i].y*FWidth + Points[i].x]);
end;

function TMufasaBitmap.GetAreaColors(xs, ys, xe, ye: Int32): T2DIntegerArray;
var
  x,y: Int32;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  SetLength(Result,xe-xs+1,ye-ys+1);
  for x := xs to xe do
    for y := ys to ye do
      Result[x-xs][y-ys] := BGRToRGB(FData[y*FWidth+x]);
end;

function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: Int32): T2DHSLArray;
var
  x, y: Int32;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  SetLength(Result,ye-ys+1,xe-xs+1);
  for y := ys to ye do
    for x := xs to xe do
    begin                                                   { REWRITE THIS }
      RGBToHSL(FData[y*FWidth+x].R, FData[y*FWidth+x].G, FData[y*FWidth+x].B,
               Result[y-ys][x-xs].H, Result[y-ys][x-xs].S,
               Result[y-ys][x-xs].L);
    end;
end;

function TMufasaBitmap.GetColors: TIntegerArray;
var
  size, i: Int32;
begin
  size := (self.height * self.width);
  SetLength(Result, size);
  dec(size);

  for i := 0 to size do
    Result[i] := BGRToRGB(self.FData[i]);
end;

procedure TMufasaBitmap.SetTransparentColor(Col: TColor);
begin
  self.FTransparentSet:= True;
  self.FTransparentColor:= RGBToBGR(Col);
end;

function TMufasaBitmap.GetTransparentColor: TColor;
begin
  if FTransparentSet then
    Result := BGRToRGB(FTransparentColor)
  else
    raise Exception.CreateFmt('Transparent color for Bitmap[%d] isn''t set',[index]);
end;

procedure TMufasaBitmap.SetAlphaValue(const value: byte);
var
  i: Int32;
begin
  for i := FWidth * FHeight - 1 downto 0 do
    FData[i].A:= Value;
end;

procedure TMufasaBitmap.FastDrawClear(Color: TColor);
var
  i: Int32;
  Rec: TRGB32;
begin
  Rec := RGBToBGR(Color);
  if FHeight > 0 then
  begin;
    for i := (FWidth-1) downto 0 do
      FData[i] := Rec;
    for i := (FHeight-1) downto 1 do
      Move(FData[0],FData[i*FWidth],FWidth*SizeOf(TRGB32));
  end;
end;

procedure TMufasaBitmap.FastDrawTransparent(x, y: Int32;
  TargetBitmap: TMufasaBitmap);
var
  MinW,MinH,TargetW,TargetH: Int32;
  loopx,loopy: Int32;
begin
  TargetBitmap.ValidatePoint(x,y);
  TargetW := TargetBitmap.Width;
  TargetH := TargetBitmap.height;
  MinW := Min(FWidth-1,TargetW-x-1);
  MinH := Min(FHeight-1,TargetH-y-1);
  if FTransparentSet then
  begin;
    for loopy := 0 to MinH do
      for loopx := 0 to MinW do
      begin;
        FData[loopy * FWidth + loopx].A := 0;
        if LongWord(FData[loopy * FWidth + loopx]) <> LongWord(FTransparentColor) then
          TargetBitmap.FData[(loopy + y) * TargetW + loopx + x] := FData[Loopy * FWidth + loopx];
      end;
  end
  else
    for loopy := 0 to MinH do
      Move(FData[loopy*FWidth],TargetBitmap.FData[(loopy+y) * TargetW + x],(MinW+1) * SizeOf(TRGB32));

end;

procedure TMufasaBitmap.FastReplaceColor(OldColor, NewColor: TColor);
var
  OldCol,NewCol: TRGB32;
  i: Int32;
begin
  OldCol := RGBToBGR(OldColor);
  NewCol := RGBToBGR(NewColor);
  for i := FWidth*FHeight-1 downto 0 do
  begin
    FData[i].a := 0;
    if LongWord(FData[i]) = LongWord(OldCol) then
      FData[i] := NewCol;
  end;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject;Resize: Boolean; xs, ys, xe, ye: Int32);
var
  y: Int32;
  wi,hi: Int32;
  RetData: TRetData;
  TargetWidth, TargetHeight: Int32;
begin
  if Resize then
    Self.SetSize(xe-xs+1,ye-ys+1);

  wi := Min(xe-xs + 1,Self.FWidth);
  hi := Min(ye-ys + 1,Self.FHeight);

  TIOManager(MWindow).GetDimensions(TargetWidth, TargetHeight);
  if (xs + wi > TargetWidth) or (ys + hi > TargetHeight) then
  begin
    WriteLn('Warning! The area passed to `CopyClientToBitmap` exceeds the clients bounds');

    xe := Min(xe, TargetWidth - 1);
    ye := Min(ye, TargetHeight - 1);
  end;

  RetData := TIOManager(MWindow).ReturnData(xs,ys,wi,hi);

  if (RetData = NullReturnData) then
  begin
    WriteLn('Warning! ReturnData returned null');
  end else
  begin
    for y := 0 to (hi-1) do
      Move(RetData.Ptr[y * RetData.RowLen], FData[y * self.FWidth], wi * SizeOf(TRGB32));

   // TIOManager(MWindow).FreeReturnData();
  end;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: Boolean; x, y: Int32; xs, ys, xe, ye: Int32);
var
  yy: Int32;
  wi,hi: Int32;
  RetData: TRetData;
  TargetWidth, TargetHeight: Int32;
begin
  if Resize then
    Self.SetSize(xe-xs+1 + x,ye-ys+1 + y);

  ValidatePoint(x,y);
  wi := Min(xe-xs + 1 + x,Self.FWidth)-x;
  hi := Min(ye-ys + 1 + y,Self.FHeight)-y;

  TIOManager(MWindow).GetDimensions(TargetWidth, TargetHeight);
  if (xs + wi > TargetWidth) or (ys + hi > TargetHeight) then
  begin
    if (FList <> nil) and (FList.Client <> nil) then
      WriteLn('Warning! The area passed to `CopyClientToBitmap` exceeds the clients bounds');

    xe := Min(xe, TargetWidth - 1);
    ye := Min(ye, TargetHeight - 1);
  end;

  RetData := TIOManager(MWindow).ReturnData(xs,ys,wi,hi);

  if (RetData = NullReturnData) then
  begin
    if (FList <> nil) and (FList.Client <> nil) then
      WriteLn('Warning! ReturnData returned null');
  end else
  begin
    for yy := 0 to (hi-1) do
      Move(RetData.Ptr[yy * (RetData.RowLen)], FData[(yy + y) * self.FWidth + x], wi * SizeOf(TRGB32));

    //TIOManager(MWindow).FreeReturnData();
  end;
end;

procedure __RotateNoExpand(Bitmap: TMufasaBitmap; Angle: Extended; TargetBitmap: TMufasaBitmap);
var
  x,y,mx,my,i,j,wid,hei: Int32;
  cosa,sina: Single;
begin
  TargetBitmap.SetSize(Bitmap.Width, Bitmap.Height);

  mx := (Bitmap.Width div 2);
  my := (Bitmap.Height div 2);
  cosa := cos(angle);
  sina := sin(angle);
  wid := (Bitmap.Width - 1);
  hei := (Bitmap.Height - 1);

  for i:=0 to hei do
    for j:=0 to wid do
    begin
      x := Round(mx + cosa * (j - mx) - sina * (i - my));
      y := Round(my + sina * (j - mx) + cosa * (i - my));
      if (x >= 0) and (x < wid) and (y >= 0) and (y < hei) then
        TargetBitmap.FData[i * Bitmap.Width + j] := Bitmap.FData[y * Bitmap.Width + x];
    end;
end;

function RotatePointEdited(p: TPoint; angle, mx, my: Extended): TPoint;

begin
  Result.X := Ceil(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Ceil(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

//Scar rotates unit circle-wise.. Oh, scar doesnt update the bounds, so kinda crops ur image.
procedure TMufasaBitmap.RotateBitmap(angle: Extended;TargetBitmap: TMufasaBitmap);
var
  NewW,NewH: Int32;
  CosAngle,SinAngle: extended;
  MinX,MinY,MaxX,MaxY: Int32;
  i: Int32;
  x,y: Int32;
  OldX,OldY: Int32;
  MiddlePoint: TPoint;
  NewCorners: array[1..4] of TPoint; //(xs,ye);(xe,ye);(xe,ys);(xs,ys)
begin
  MiddlePoint := Point((FWidth-1) div 2,(FHeight-1) div 2);
  CosAngle := Cos(Angle);
  SinAngle := Sin(Angle);
  MinX := MaxInt;
  MinY := MaxInt;
  MaxX := 0;
  MaxY := 0;
  NewCorners[1]:= RotatePointEdited(Point(0,FHeight-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[2]:= RotatePointEdited(Point(FWidth-1,FHeight-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[3]:= RotatePointEdited(Point(FWidth-1,0),angle,middlepoint.x,middlepoint.y);
  NewCorners[4]:= RotatePointEdited(Point(0,0),angle,middlepoint.x,middlepoint.y);
  for i := 1 to 4 do
  begin;
    if NewCorners[i].x > MaxX then
      MaxX := NewCorners[i].x;
    if NewCorners[i].Y > MaxY then
      MaxY := NewCorners[i].y;
    if NewCorners[i].x < MinX then
      MinX := NewCorners[i].x;
    if NewCorners[i].y < MinY then
      MinY := NewCorners[i].y;
  end;
  //mDebugLn(Format('Min: (%d,%d) Max: (%d,%d)',[MinX,MinY,MaxX,MaxY]));
  NewW := MaxX - MinX+1;
  NewH := MaxY - MinY+1;
 // mDebugLn(format('New bounds: %d,%d',[NewW,NewH]));
  TargetBitmap.SetSize(NewW,NewH);
  for y := NewH - 1 downto 0 do
    for x := NewW - 1 downto 0 do
    begin;
      Oldx := Round(MiddlePoint.x + CosAngle * (x + MinX-MiddlePoint.x) - SinAngle * (y + MinY - MiddlePoint.y));
      Oldy := Round(MiddlePoint.y + SinAngle * (x + MinX-MiddlePoint.x) + CosAngle * (y + MinY-MiddlePoint.y));
      if not ((Oldx <0) or (Oldx >= FWidth) or (Oldy < 0) or (Oldy >= FHeight)) then
        TargetBitmap.FData[ y * NewW + x] := Self.FData[OldY * FWidth + OldX];
    end;
end;

procedure __RotateBINoExpand(Bitmap: TMufasaBitmap; Angle: Single; TargetBitmap: TMufasaBitmap);
var
  i,j,k,RR,GG,BB,mx,my,fX,fY,cX,cY,wid,hei: Int32;
  rX,rY,dX,dY,cosa,sina:Single;
  p0,p1,p2,p3: TRGB32;
  topR,topG,topB,BtmR,btmG,btmB:Single;
begin
  TargetBitmap.SetSize(Bitmap.Width, Bitmap.Height);

  cosa := Cos(Angle);
  sina := Sin(Angle);
  mX := Bitmap.Width div 2;
  mY := Bitmap.Height div 2;
  wid := (Bitmap.Width - 1);
  hei := (Bitmap.Height - 1);

  for i := 0 to hei do begin
    for j := 0 to wid do begin
      rx := (mx + cosa * (j - mx) - sina * (i - my));
      ry := (my + sina * (j - mx) + cosa * (i - my));

      fX := Trunc(rX);
      fY := Trunc(rY);
      cX := Ceil(rX);
      cY := Ceil(rY);

      if not((fX < 0) or (cX < 0) or (fX > wid) or (cX > wid) or
             (fY < 0) or (cY < 0) or (fY > hei) or (cY > hei)) then
      begin
        dx := rX - fX;
        dy := rY - fY;

        p0 := Bitmap.FData[fY * Bitmap.Width + fX];
        p1 := Bitmap.FData[fY * Bitmap.Width + cX];
        p2 := Bitmap.FData[cY * Bitmap.Width + fX];
        p3 := Bitmap.FData[cY * Bitmap.Width + cX];

        TopR := (1 - dx) * p0.R + dx * p1.R;
        TopG := (1 - dx) * p0.G + dx * p1.G;
        TopB := (1 - dx) * p0.B + dx * p1.B;
        BtmR := (1 - dx) * p2.R + dx * p3.R;
        BtmG := (1 - dx) * p2.G + dx * p3.G;
        BtmB := (1 - dx) * p2.B + dx * p3.B;

        RR := Round((1 - dy) * TopR + dy * BtmR);
        GG := Round((1 - dy) * TopG + dy * BtmG);
        BB := Round((1 - dy) * TopB + dy * BtmB);

        if (RR < 0) then RR := 0
        else if (RR > 255)then RR := 255;
        if (GG < 0) then GG := 0
        else if (GG > 255)then GG := 255;
        if (BB < 0) then BB := 0
        else if (BB > 255)then BB := 255;

        k := i * Bitmap.Width + j;
        TargetBitmap.FData[k].r := RR;
        TargetBitmap.FData[k].g := GG;
        TargetBitmap.FData[k].b := BB;
      end;
    end;
  end;
end;

procedure __RotateBIExpand(Bitmap: TMufasaBitmap; Angle: Single; TargetBitmap: TMufasaBitmap);

  function __GetNewSizeRotated(W,H:Int32; Angle:Single): TBox;
    function Rotate(p:TPoint; angle:Single; mx,my:Int32): TPoint;
    begin
      Result.X := Round(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
      Result.Y := Round(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y - my));
    end;
  var B: TPointArray;
  begin
    SetLength(B, 4);
    FillChar(Result, SizeOf(TBox), 0);
    Result.X1 := $FFFFFF;
    Result.Y1 := $FFFFFF;
    B[0]:= Rotate(Point(0,h),angle, W div 2, H div 2);
    B[1]:= Rotate(Point(w,h),angle, W div 2, H div 2);
    B[2]:= Rotate(Point(w,0),angle, W div 2, H div 2);
    B[3]:= Rotate(Point(0,0),angle, W div 2, H div 2);
    Result := GetTPABounds(B);
  end;

var
  i,j,RR,GG,BB,mx,my,nW,nH,fX,fY,cX,cY,wid,hei,k: Int32;
  rX,rY,dX,dY,cosa,sina:Single;
  topR,topG,topB,BtmR,btmG,btmB:Single;
  p0,p1,p2,p3: TRGB32;
  NewB:TBox;
begin
  NewB := __GetNewSizeRotated(Bitmap.Width, Bitmap.Height,Angle);
  nW := (NewB.x2 - NewB.x1) + 1;
  nH := (NewB.y2 - NewB.y1) + 1;
  mX := nW div 2;
  mY := nH div 2;
  wid := (Bitmap.Width - 1);
  hei := (Bitmap.Height - 1);
  TargetBitmap.SetSize(nW, nH);
  cosa := Cos(Angle);
  sina := Sin(Angle);
  nW -= 1; nH -= 1;

  for i := 0 to nH do begin
    for j := 0 to nW do begin
      rx := (mx + cosa * (j - mx) - sina * (i - my));
      ry := (my + sina * (j - mx) + cosa * (i - my));

      fX := (Trunc(rX)+ NewB.x1);
      fY := (Trunc(rY)+ NewB.y1);
      cX := (Ceil(rX) + NewB.x1);
      cY := (Ceil(rY) + NewB.y1);

      if not((fX < 0) or (cX < 0) or (fX >= wid) or (cX >= wid) or
             (fY < 0) or (cY < 0) or (fY >= hei) or (cY >= hei)) then
      begin
        dx := rX - (fX - NewB.x1);
        dy := rY - (fY - NewB.y1);

        p0 := Bitmap.FData[fY * Bitmap.Width + fX];
        p1 := Bitmap.FData[fY * Bitmap.Width + cX];
        p2 := Bitmap.FData[cY * Bitmap.Width + fX];
        p3 := Bitmap.FData[cY * Bitmap.Width + cX];

        TopR := (1 - dx) * p0.R + dx * p1.R;
        TopG := (1 - dx) * p0.G + dx * p1.G;
        TopB := (1 - dx) * p0.B + dx * p1.B;
        BtmR := (1 - dx) * p2.R + dx * p3.R;
        BtmG := (1 - dx) * p2.G + dx * p3.G;
        BtmB := (1 - dx) * p2.B + dx * p3.B;

        RR := Round((1 - dy) * TopR + dy * BtmR);
        GG := Round((1 - dy) * TopG + dy * BtmG);
        BB := Round((1 - dy) * TopB + dy * BtmB);

        if (RR < 0) then RR := 0
        else if (RR > 255) then RR := 255;
        if (GG < 0) then GG := 0
        else if (GG > 255) then GG := 255;
        if (BB < 0) then BB := 0
        else if (BB > 255) then BB := 255;

        k := i * TargetBitmap.Width + j;
        TargetBitmap.FData[k].r := RR;
        TargetBitmap.FData[k].g := GG;
        TargetBitmap.FData[k].b := BB;
      end;
    end;
  end;
end;

procedure TMufasaBitmap.RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap);
begin
  case Expand of
    True:
      case Smooth of
        True:
          __RotateBIExpand(Self, Angle, TargetBitmap);
        False:
          Self.RotateBitmap(Angle, TargetBitmap);
      end;
    False:
      case Smooth of
        True:
          __RotateBINoExpand(Self, Angle, TargetBitmap);
        False:
          __RotateNoExpand(Self, Angle, TargetBitmap);
      end;
  end;
end;

procedure TMufasaBitmap.Desaturate;
var
  I: Int32;
  He,Se,Le: extended;
  Ptr: PRGB32;
begin
  Ptr := FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,Ptr^.R,Ptr^.G,Ptr^.B);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap);
var
  I: Int32;
  He,Se,Le: extended;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    RGBToHSL(PtrOld^.R,PtrOld^.G,PtrOld^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,PtrNew^.R,PtrNew^.G,PtrNew^.B);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap);
var
  I: Int32;
  Lum: byte;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
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
  I: Int32;
  Lum: Byte;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    Lum := Round(Ptr^.r * 0.3 + Ptr^.g * 0.59 + Ptr^.b * 0.11);
    Ptr^.r := Lum;
    Ptr^.g := Lum;
    Ptr^.b := Lum;
    inc(ptr);
  end;
end;

function BrightnessAdjust(Col:  byte; br: Int32): byte;inline;
var
  temp: Int32;
begin;
  Temp := Col + Br;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  Result := temp;
end;
procedure TMufasaBitmap.Brightness(br: Int32);
var
  I: Int32;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    Ptr^.r := BrightnessAdjust(Ptr^.r,br);
    Ptr^.g := BrightnessAdjust(Ptr^.g,br);
    Ptr^.b := BrightnessAdjust(Ptr^.b,br);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Int32);
var
  I: Int32;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
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
  temp: Int32;
begin;
  Temp := floor((col - Grey) * co) + grey;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  Result := temp;
end;

procedure TMufasaBitmap.Contrast(co: Extended);
var
  I: Int32;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    Ptr^.r := ContrastAdjust(Ptr^.r,co);
    Ptr^.g := ContrastAdjust(Ptr^.g,co);
    Ptr^.b := ContrastAdjust(Ptr^.b,co);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended);
var
  I: Int32;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    PtrNew^.r := ContrastAdjust(PtrOld^.r,co);
    PtrNew^.g := ContrastAdjust(PtrOld^.g,co);
    PtrNew^.b := ContrastAdjust(PtrOld^.b,co);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Invert;
var
  i: Int32;
begin
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    Self.FData[i].r := not Self.FData[i].r;
    Self.FData[i].g := not Self.FData[i].g;
    Self.Fdata[i].b := not Self.FData[i].b;
  end;
end;

procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);
var
  I: Int32;
  PtrOld,PtrNew: PRGB32;
begin
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    PtrNew^.r := not PtrOld^.r;
    PtrNew^.g := not PtrOld^.g;
    PtrNew^.b := not PtrOld^.b;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Int32);
var
  I: Int32;
  PtrOld,PtrNew: PRGB32;
begin
  if not InRange(Po,1,255) then
    raise Exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  TargetBitmap.SetSize(FWidth,FHeight);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
    PtrNew^.r := min(Round(PtrOld^.r / po) * Po, 255);
    PtrNew^.g := min(Round(PtrOld^.g / po) * Po, 255);
    PtrNew^.b := min(Round(PtrOld^.b / po) * Po, 255);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(Po: Int32);
var
  I: Int32;
  Ptr: PRGB32;
  {a:Int32; }
begin
  if not InRange(Po,1,255) then
    raise Exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  Ptr := Self.FData;
  for i := (FHeight*FWidth-1) downto 0 do
  begin;
   { a := round(ptr^.r / po);
    a := a * po;
    ptr^.r := min(a,255);
    a := round(ptr^.g / po);
    a := a * po;
    ptr^.g := min(a,255);
    a := round(ptr^.b / po);
    a := a * po;
    ptr^.b := min(a,255);      }
    ptr^.r := min(Round(ptr^.r / po) * Po, 255);
    ptr^.g := min(Round(ptr^.g / po) * Po, 255);
    ptr^.b := min(Round(ptr^.b / po) * Po, 255);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Blur(const Block, xs, ys, xe, ye: Int32);
var
  wid,hei,x,y,mid,fx,fy,size:Int32;
  red,green,blue,lx,ly,hx,hy,sl:Int32;
  bmp: TMufasaBitmap;
begin
  Size := (Block*Block);

  if (Size<=1) or (Block mod 2 = 0) then
    Exit;

  if (not Self.PointInBitmap(xs, ys)) or (not Self.PointInBitmap(xe, ye)) then
    raise Exception.Create('TMufasaBitmap.Blur(): The bounds you pased to blur exceed the bitmap bounds');

  bmp := Self.Copy(xs, ys, xe, ye);
  wid := (bmp.Width - 1);
  hei := (bmp.Height - 1);
  mid := (Block div 2);

  try
    for y:=0 to hei do
    begin
      ly := Max(0,y-mid);
      hy := Min(hei,y+mid);
      for x:=0 to wid do
      begin
        lx := Max(0,x-mid);
        hx := Min(wid,x+mid);
        size := 0;
        red := 0; green := 0; blue := 0;

        for fy:=ly to hy do
          for fx:=lx to hx do
          begin
            sl := (fy * bmp.Width +fx);
            inc(red, bmp.FData[sl].R);
            inc(green, bmp.FData[sl].G);
            inc(blue, bmp.FData[sl].B);
            inc(size);
          end;

         sl := ((y+xs)*self.Width+(x+ys));
         Self.FData[sl].R := (red div size);
         Self.FData[sl].G := (green div size);
         Self.FData[sl].B := (blue div size);
      end;
    end;
  finally
    bmp.free();
  end;
end;

procedure TMufasaBitmap.Blur(const Block: Int32); overload;
begin
  Self.Blur(Block, 0, 0, self.width -1, self.height -1);
end;

procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);
var
  x,y,yy,xx,cx,cy: Int32;
  Row,RowT: TPRGB32Array;
  mW,mH,midx,midy:Int32;
  valR,valG,valB: Extended;

  procedure ForceInBounds(x,y, Wid,Hig: Int32; out cx,cy: Int32); Inline;
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
      RowT[y][x].R := round(valR);
      RowT[y][x].G := round(valG);
      RowT[y][x].B := round(valB);;
    end;
end;

procedure TMufasaBitmap.Blend(Points: TPointArray; Size: Int32);
var
  P: TPoint;
  X, Y, Count: Int32;
  Area: TBox;
  R, G, B: Int32;
  BGR: TRGB32;
begin
  for P in Points do
  begin
    Area.X1 := Max(P.X - Size, 0);
    Area.Y1 := Max(P.Y - Size, 0);
    Area.X2 := Min(P.X + Size, FWidth-1);
    Area.Y2 := Min(P.Y + Size, FHeight-1);

    Count := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := Area.X1 to Area.X2 do
      for Y := Area.Y1 to Area.Y2 do
      begin
        BGR := FData[Y * FWidth + X];
        BGR.A := 0;
        if (BGR = FTransparentColor) then
          Continue;

        Inc(R, BGR.R);
        Inc(G, BGR.G);
        Inc(B, BGR.B);

        Inc(Count);
      end;

    if Count > 0 then
    begin
      BGR.R := R div Count;
      BGR.G := G div Count;
      BGR.B := B div Count;

      FData[P.Y * FWidth + P.X] := BGR;
    end;
  end;
end;

function TMufasaBitmap.CompareAt(Other: TMufasaBitmap; Pt: TPoint; Tol: Int32): Extended;
var
  x,y,tw,th,SAD: Int32;
  c1,c2: TRGB32;
begin
  tw := Other.Width;
  th := Other.Height;
  if (tw = 0) or (th = 0) or (tw+pt.x > Self.FWidth) or (th+pt.y > Self.FHeight) then
    Exit(0);

  SAD := 0;
  for y:=0 to th-1 do
    for x:=0 to tw-1 do
    begin
      c1 := Self.FData [(y + pt.y) * FWidth + x + pt.x];
      c2 := Other.FData[y * tw + x];
      if Sqr(c1.R-c2.R) + Sqr(c1.G-c2.G) + Sqr(c1.B-c2.B) > Sqr(Tol) then
        Inc(SAD);
    end;
  
  Result := 1 - SAD / (FWidth*FHeight);
end;

procedure TMufasaBitmap.Downsample(DownScale: Int32; TargetBitmap: TMufasaBitmap);
var
  invArea: Double;
  function BlendArea(x1,y1: Int32): TRGB32; inline;
  var
    x,y:Int32;
    R:Int32=0; G:Int32=0; B:Int32=0;
  begin
    for y:=y1 to y1+DownScale-1 do
      for x:=x1 to x1+DownScale-1 do
      begin
        Inc(R, Self.FData[y*FWidth+x].R);
        Inc(G, Self.FData[y*FWidth+x].G);
        Inc(B, Self.FData[y*FWidth+x].B);
      end;
    Result.R := Round(R * invArea);
    Result.G := Round(G * invArea);
    Result.B := Round(B * invArea);
  end;
var
  x,y,nw,nh:Int32;
begin
  if (FWidth = 0) or (FHeight = 0) or (DownScale <= 0) then 
    Exit;
  
  nw := FWidth div DownScale;
  nh := FHeight div DownScale;
  invArea := Double(1.0) / Sqr(DownScale);
  TargetBitmap.SetSize(nW, nH);
  for y:=0 to nh-1 do
    for x:=0 to nw-1 do
      TargetBitmap.FData[y*nw+x] := BlendArea(x*DownScale, y*DownScale);
end;

function TMufasaBitmap.Downsample(DownScale: Int32; BlendTransparentColor: Boolean): TMufasaBitmap;
var
  Area: Double;

  function BlendArea(X1, Y1, X2, Y2: Int32): TRGB32; inline;
  var
    R, G, B: Int32;
    Hit, Miss: Int32;
    X, Y: Int32;
    Pixel: TRGB32;
  begin
    Miss := 0;
    Hit := 0;

    R := 0;
    G := 0;
    B := 0;

    for X := X1 to X2-1 do
      for Y := Y1 to Y2-1 do
      begin
        Pixel := FData[Y * FWidth + X];

        if (not BlendTransparentColor) and (Pixel = FTransparentColor) then
          Inc(Miss)
        else
        begin
          Inc(Hit);

          Inc(R, Pixel.R);
          Inc(G, Pixel.G);
          Inc(B, Pixel.B);
        end;
      end;

    Result.R := Round((R + (R div Hit) * Miss) * Area);
    Result.G := Round((G + (G div Hit) * Miss) * Area);
    Result.B := Round((B + (B div Hit) * Miss) * Area);
    Result.A := 0;
  end;

var
  X, Y, W, H: Int32;
  XX, YY: Int32;
  Pixel: TRGB32;
begin
  if DownScale <= 1 then
    Exit(Self.Copy());

  Area := Double(1.0) / Sqr(DownScale);

  Result := TMufasaBitmap.Create();
  Result.SetSize(FWidth div DownScale, FHeight div DownScale);

  W := Result.Width - 1;
  H := Result.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      XX := X * DownScale;
      YY := Y * DownScale;

      if not BlendTransparentColor then
      begin
        Pixel := FData[YY * FWidth + XX];
        if (Pixel = FTransparentColor) then
          Continue;
      end;

      Result.FData[Y * Result.Width + X] := BlendArea(XX, YY, XX + DownScale, YY + DownScale);
    end;
end;

function TMufasaBitmap.CreateTMask: TMask;
var
  x,y: Int32;
  dX,dY: Int32;
begin
  Result.BlackHi:= -1;
  Result.WhiteHi:= -1;
  Result.w := Self.Width;
  Result.h := Self.Height;
  SetLength(Result.Black,FWidth*FHeight);
  SetLength(Result.White,FWidth*FHeight);
  dX := FWidth-1;
  dY := FHeight-1;
  for y := 0 to dY do
    for x := 0 to dX do
    //Check for non-white/black pixels? Not for now atleast.
      if FData[y*FWidth+x].r = 255 then
      begin;
        inc(Result.WhiteHi);
        Result.White[Result.WhiteHi].x := x;
        Result.White[Result.WhiteHi].y := y;
      end else
      begin;
        inc(Result.BlackHi);
        Result.Black[Result.BlackHi].x := x;
        Result.Black[Result.BlackHi].y := y;
      end;
  SetLength(Result.Black,Result.BlackHi+1);
  SetLength(Result.White,Result.WhiteHi+1);
end;

procedure TMufasaBitmap.ResizeBilinear(NewW, NewH: Int32);
var
  x,y,i,j: Int32;
  p0,p1,p2,p3: TRGB32;
  ratioX,ratioY,dx,dy: Single;
  Temp: TMufasaBitmap;
  RR,GG,BB: Single;
  Row,RowT: TPRGB32Array;
begin
  Temp := Self.Copy();
  RowT:= Temp.RowPtrs;
  ratioX := (Self.Width-1) / NewW;
  ratioY := (Self.Height-1) / NewH;
  Self.SetSize(NewW, NewH);
  Row := Self.RowPtrs;
  Dec(NewW);
  for i:=0 to NewH-1 do
  for j:=0 to NewW do
  begin
    x := Trunc(ratioX * j);
    y := Trunc(ratioY * i);
    dX := ratioX * j - x;
    dY := ratioY * i - y;

    p0 := RowT[y][x];
    p1 := RowT[y][x+1];
    p2 := RowT[y+1][x];
    p3 := RowT[y+1][x+1];

    RR := p0.R * (1-dX) * (1-dY) +
          p1.R * (dX * (1-dY)) +
          p2.R * (dY * (1-dX)) +
          p3.R * (dX * dY);

    GG := p0.G * (1-dX) * (1-dY) +
          p1.G * (dX * (1-dY)) +
          p2.G * (dY * (1-dX)) +
          p3.G * (dX * dY);

    BB := p0.B * (1-dX) * (1-dY) +
          p1.B * (dX * (1-dY)) +
          p2.B * (dY * (1-dX)) +
          p3.B * (dX * dY);

    Row[i][j].R := Trunc(RR);
    Row[i][j].G := Trunc(GG);
    Row[i][j].B := Trunc(BB);
  end;
  Temp.Free();
end;

procedure TMufasaBitmap.Rectangle(const Box: TBox; const Color: Int32; const Transparency: Extended); overload;
var
  RR, GG, BB: Byte;
  Line, x, y: Longword;
begin
  Self.ValidatePoint(Box.X1, Box.Y1);
  Self.ValidatePoint(Box.X2, Box.Y2);

  if (Transparency > 1.00) then
  begin
    Self.Rectangle(Box, Color);
    Exit();
  end;

  if (Transparency = 0.00) then
    Exit();

  ColorToRGB(Color, RR, GG, BB);

  for y := Box.Y1 to Box.Y2 do
  begin
    Line := (y * Self.Width) + Box.x1;
    for x := Box.X1 to Box.X2 do
    begin
      Self.FData[Line].r := Round((Self.FData[Line].r * (1.0 - Transparency)) + (RR * Transparency));
      Self.FData[Line].g := Round((Self.FData[Line].g * (1.0 - Transparency)) + (GG * Transparency));
      Self.FData[Line].b := Round((Self.FData[Line].b * (1.0 - Transparency)) + (BB * Transparency));
      Inc(Line);
    end;
  end;
end;

constructor TMBitmaps.Create(Owner: TObject);
begin
  inherited Create;
  SetLength(BmpArray,50);
  SetLength(FreeSpots, 50);
  FreeSpotsLen := 50;
  BmpsHigh := 49;
  BmpsCurr := -1;
  FreeSpotsHigh := -1;
  Self.Client := Owner;
end;

destructor TMBitmaps.Destroy;
var
  I: Int32;
  WriteStr: String;
begin
  WriteStr := '[';
  for I := 0 to BmpsCurr do
    if Assigned(BmpArray[I]) then
    begin;
      if BmpArray[I].Name = '' then
        WriteStr := WriteStr + IntToStr(I) + ', '
      else
        WriteStr := WriteStr + bmpArray[I].Name + ', ';

      BmpArray[I].Free();
    end;

  if WriteStr <> '[' then  //Has unfreed bitmaps
  begin
    SetLength(WriteStr, Length(WriteStr) - 1);
    WriteStr[Length(writeStr)] := ']';

    WriteLn(Format('The following bitmaps were not freed: %s', [WriteStr]));
  end;

  SetLength(BmpArray, 0);
  SetLength(FreeSpots, 0);

  inherited Destroy;
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

function TMufasaBitmap.TextWidth(Text: String): Int32;
begin
  Result := FTextDrawer.TextWidth(Text);
end;

function TMufasaBitmap.TextHeight(Text: String): Int32;
begin
  Result := FTextDrawer.TextHeight(Text);
end;

function TMufasaBitmap.TextSize(Text: String): TPoint;
begin
  Result := FTextDrawer.TextSize(Text);
end;

procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Position, Color);
end;

procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: TColor);
begin
  FTextDrawer.DrawText(Text, Box, ACenter, Color);
end;

{ TMufasaBitmap }
procedure TMufasaBitmap.SetSize(AWidth, AHeight: Int32);
var
  NewData: PRGB32;
  i,minw,minh: Int32;
begin
  if FExternData then
    raise Exception.Create('Cannot resize a bitmap with FExternData = True!');

  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    if AWidth*AHeight <> 0 then
      NewData := AllocMem(AWidth * AHeight * SizeOf(TRGB32))
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (FWidth*FHeight <> 0) then
    begin;
      minw := Min(AWidth,FWidth);
      minh := Min(AHeight,FHeight);
      for i := 0 to minh - 1 do
        Move(FData[i*FWidth],Newdata[i*AWidth],minw * SizeOf(TRGB32));
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    FWidth := AWidth;
    FHeight := AHeight;
  end;
end;

procedure TMufasaBitmap.StretchResize(AWidth, AHeight: Int32);
var
  NewData: PRGB32;
  x,y: Int32;
begin
  if FExternData then
    raise Exception.Create('Cannot resize a bitmap with FExternData = True!');

  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    if AWidth*AHeight <> 0 then
      NewData := AllocMem(AWidth * AHeight * SizeOf(TRGB32))
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (FWidth*FHeight <> 0) then
    begin
      for y := 0 to AHeight - 1 do
        for x := 0 to AWidth -1 do
          NewData[y*AWidth + x] := FData[((y * FHeight)div aheight) * FWidth+ (x * FWidth) div awidth];
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    FWidth := AWidth;
    FHeight := AHeight;
  end;
end;

procedure TMufasaBitmap.ResizeEx(Method: TBmpResizeMethod; NewWidth, NewHeight: Int32);
begin
  if (Self.FExternData) then
    raise Exception.Create('Cannot resize a bitmap with FExternData = True!');

  case Method of
    RM_Nearest: Self.StretchResize(NewWidth, NewHeight);
    RM_Bilinear: Self.ResizeBilinear(NewWidth, NewHeight);
  end;
end;

procedure Swap(var a, b: byte); inline;
var
  t: Byte;
begin
  t := a;
  a := b;
  b := t;
end;

procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; DoInvert: Boolean; Method: TBmpThreshMethod; C: Int32);
var
  i,size: Int32;
  upper: PtrUInt;
  vMin,vMax,threshold: UInt8;
  Counter: Int64;
  Tab: Array [0..256] of UInt8;
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
      Threshold := (Counter div size) + C;
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
      Threshold := ((vMax+Int32(vMin)) shr 1) + C;
    end;
  end;

  if DoInvert then Swap(Alpha, Beta);
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

procedure TMufasaBitmap.Pad(Amount: Int32);
var
  OldWidth, OldHeight: Int32;
  Y: Int32;
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
  X, Y: Int32;
begin
  SetLength(Result, FHeight, FWidth);

  for Y := 0 to FHeight-1 do
    for X := 0 to FWidth-1 do
      with FData[Y * FWidth + X] do
        Result[Y, X] := Round(R * 0.3 + G * 0.59 + B * 0.11);
end;

procedure TMufasaBitmap.SetPersistentMemory(mem: PtrUInt; awidth, aheight: Int32);
begin
  SetSize(0, 0);

  FExternData := True;
  FWidth := awidth;
  FHeight := aheight;
  FData := PRGB32(mem);
end;

procedure TMufasaBitmap.ResetPersistentMemory;
begin
  if not FExternData then
    raise Exception.Create('ResetPersistentMemory: Bitmap is not persistent (FExternData = False)');

  FExternData := False;
  FData := nil;

  SetSize(0, 0);
end;

function TMufasaBitmap.PointInBitmap(x, y: Int32): Boolean;
begin
  Result := ((x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight));
end;

procedure TMufasaBitmap.ValidatePoint(x, y: Int32);
begin
  if not(PointInBitmap(x,y)) then
    raise Exception.CreateFmt('You are accessing an invalid point, (%d,%d) at bitmap[%d]',[x,y,index]);
end;

constructor TMufasaBitmap.Create;
begin
  inherited Create();

  FName := '';
  FIndex := -1;

  FTextDrawer := TSimbaTextDrawer.Create(Self);
  FTransparentColor := Default(TRGB32); // Black

  SetSize(0,0);
end;

destructor TMufasaBitmap.Destroy;
begin
  if Assigned(List) then
    List.RemoveBMP(Index);
  if Assigned(FTextDrawer) then
    FTextDrawer.Free();
  if Assigned(FData) and not FExternData then
    FreeMem(FData);

  inherited Destroy();
end;

end.

