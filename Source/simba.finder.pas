{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder;

{$i simba.inc}

interface

uses
  simba.colormath, classes, sysutils, simba.bitmap, simba.dtm, simba.mufasatypes, simba.matchtemplate,
  simba.finder_color;

type
  PMFinder = ^TMFinder;
  TMFinder = class(TObject)
  private
    Client: TObject;
    hueMod, satMod: Extended;
    CTS3Modifier: Extended;
    CTS: Integer;
  public
    function GetData(out Data: TRetData; var X1, Y1, X2, Y2: Integer): Boolean;
    function GetBitmap(out Bitmap: TMufasaBitmap; X1, Y1, X2, Y2: Integer; CopyData: Boolean = True): Boolean;

    function SimilarColors(Color1, Color2, Tolerance: Integer): Boolean;

    function FindColor(out X, Y: Integer; Color: Integer; Area: TBox): Boolean;
    function FindColorTolerance(out X, Y: Integer; Color, Tolerance: Integer; Area: TBox): Boolean;
    function FindColors(out TPA: TPointArray; Color: Integer; Area: TBox): Boolean;
    function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; Area: TBox; MaxToFind: Integer = 0): Boolean;

    function CountColorTolerance(Color: Integer; Tolerance: Integer; Area: TBox): Integer;
    function CountColor(Color: Integer; Area: TBox): Integer;

    function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; MaxToFind: Integer = 0): Boolean;
    function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox): Boolean;
    function FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; Tolerance: Integer; MaxToFind: Integer = 0): Boolean;
    function FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox; Tolerance: Integer): Boolean;
    function FindDTMs(DTM: TDTM; out Points: TPointArray; Area: TBox; MaxToFind: Integer = 0): Boolean;
    function FindDTM(DTM: TDTM; out x, y: Integer; Area: TBox): Boolean;
    function FindDTMsRotated(DTM: TDTM; out Points: TPointArray; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: TDoubleArray; MaxToFind: Integer = 0): Boolean;
    function FindDTMRotated(DTM: TDTM; out x, y: Integer; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: Double): Boolean;

    function FindTemplateEx(TemplImage: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; Area: TBox; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
    function FindTemplate(TemplImage: TMufasaBitmap; out X,Y: Integer; Formula: ETMFormula; Area: TBox;  MinMatch: Extended; DynamicAdjust: Boolean): Boolean;

    function GetColors(TPA: TPointArray): TIntegerArray;
    function GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
    function GetColor(X, Y: Integer): Integer;

    function AverageBrightness(Area: TBox): Integer;
    function PeakBrightness(Area: TBox): Integer;

    function GetPixelDifference(Area: TBox; WaitTime: Integer): Integer; overload;
    function GetPixelDifference(Area: TBox; Tolerance: Integer; WaitTime: Integer): Integer; overload;
    function GetPixelDifferenceTPA(Area: TBox; WaitTime: Integer): TPointArray; overload;
    function GetPixelDifferenceTPA(Area: TBox; Tolerance: Integer; WaitTime: Integer): TPointArray; overload;

    // tol speeds
    procedure SetToleranceSpeed(nCTS: Integer);
    function GetToleranceSpeed: Integer;
    procedure SetToleranceSpeed2Modifiers(const nHue, nSat: Extended);
    procedure GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);
    procedure SetToleranceSpeed3Modifier(modifier: Extended);
    function GetToleranceSpeed3Modifier: Extended;

    constructor Create(aClient: TObject);
  end;

implementation

uses
  math,
  simba.client, simba.tpa, simba.finder_dtm, simba.finder_bitmap, simba.colormath_same;

constructor TMFinder.Create(aClient: TObject);
begin
  inherited Create();

  Self.Client := aClient;
  Self.CTS := 1;
  Self.hueMod := 0.2;
  Self.satMod := 0.2;
  Self.CTS3Modifier := 1;
end;

procedure TMFinder.SetToleranceSpeed(nCTS: Integer);
begin
  if (nCTS < 0) or (nCTS > 3) then
    raise Exception.CreateFmt('The given CTS ([%d]) is invalid.',[nCTS]);
  Self.CTS := nCTS;
end;

function TMFinder.GetToleranceSpeed: Integer;
begin
  Result := Self.CTS;
end;

procedure TMFinder.SetToleranceSpeed2Modifiers(const nHue, nSat: Extended);
begin
  Self.hueMod := nHue;
  Self.satMod := nSat;
end;

procedure TMFinder.GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);
begin
  hMod := Self.hueMod;
  sMod := Self.satMod;
end;

procedure TMFinder.SetToleranceSpeed3Modifier(modifier: Extended);
begin
  CTS3Modifier := modifier;
end;

function TMFinder.GetToleranceSpeed3Modifier: Extended;
begin
  Result := CTS3Modifier;
end;

function TMFinder.GetData(out Data: TRetData; var X1, Y1, X2, Y2: Integer): Boolean;
var
  ClientWidth, ClientHeight: Integer;
begin
  TClient(Self.Client).IOManager.GetDimensions(ClientWidth, ClientHeight);

  if (X1 = -1) and (Y1 = -1) and (X2 = -1) and (Y2 = -1) then
  begin
    X1 := 0;
    Y1 := 0;
    X2 := ClientWidth - 1;
    Y2 := ClientHeight - 1;
  end else
  begin
    if (X1 < 0) then X1 := 0;
    if (Y1 < 0) then Y1 := 0;
    if (X2 >= ClientWidth)  then X2 := ClientWidth - 1;
    if (Y2 >= ClientHeight) then Y2 := ClientHeight - 1;
  end;

  Data := TClient(Self.Client).IOManager.ReturnData(X1, Y1, X2 - X1 + 1, Y2 - Y1 + 1);
  if (Data.Ptr = nil) then
    SimbaDebugLn(ESimbaDebugLn.YELLOW, 'No image data returned. Is the target resizing?');

  Result := Data.Ptr <> nil;
end;

function TMFinder.GetBitmap(out Bitmap: TMufasaBitmap; X1, Y1, X2, Y2: Integer; CopyData: Boolean): Boolean;
var
  ImageData: TRetData;
begin
  if GetData(ImageData, X1, Y1, X2, Y2) then
    Bitmap := TMufasaBitmap.CreateFromData(X2-X1+1, Y2-Y1+1, ImageData.Ptr, CopyData)
  else
    Bitmap := nil;

  Result := Bitmap <> nil;
end;

function TMFinder.GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
var
  ClientWidth, ClientHeight: Integer;
begin
  TClient(Self.Client).IOManager.GetDimensions(ClientWidth, ClientHeight);

  if (X1 = -1) and (Y1 = -1) and (X2 = -1) and (Y2 = -1) then
  begin
    X1 := 0;
    Y1 := 0;
    X2 := ClientWidth - 1;
    Y2 := ClientHeight - 1;
  end else
  begin
    if (X1 < 0) then X1 := 0;
    if (Y1 < 0) then Y1 := 0;
    if (X2 >= ClientWidth)  then X2 := ClientWidth - 1;
    if (Y2 >= ClientHeight) then Y2 := ClientHeight - 1;
  end;

  Result := TClient(Self.Client).IOManager.ReturnMatrix(X1, Y1, X2 - X1 + 1, Y2 - Y1 + 1);
  if (Result.Area = 0) then
    SimbaDebugLn(ESimbaDebugLn.YELLOW, 'No image data returned. Is the target resizing?');
end;

function TMFinder.SimilarColors(Color1, Color2, Tolerance: Integer) : Boolean;
begin
  case Self.CTS of
    0: Result := TSameColorCTS0.Create(Color1, Tolerance).IsSame(RGBToBGR(Color2));
    1: Result := TSameColorCTS1.Create(Color1, Tolerance).IsSame(RGBToBGR(Color2));
    2: Result := TSameColorCTS2.Create(Color1, Tolerance, HueMod, SatMod).IsSame(RGBToBGR(Color2));
    3: Result := TSameColorCTS3.Create(Color1, Tolerance, CTS3Modifier).IsSame(RGBToBGR(Color2));
  end;
end;

function TMFinder.CountColorTolerance(Color: Integer; Tolerance: Integer; Area: TBox): Integer;
var
  ImageData: TRetData;
  Buffer: TFindColorBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;

    if (Tolerance = 0) then
      Result := Buffer.Count(Color)
    else
    case Self.CTS of
      0: Result := Buffer.CountCTS0(Color, Tolerance);
      1: Result := Buffer.CountCTS1(Color, Tolerance);
      2: Result := Buffer.CountCTS2(Color, Tolerance, HueMod, SatMod);
      3: Result := Buffer.CountCTS3(Color, Tolerance, CTS3Modifier);
    end;
  end else
    Result := 0;
end;

function TMFinder.CountColor(Color: Integer; Area: TBox): Integer;
begin
  Result := CountColorTolerance(Color, 0, Area);
end;

function TMFinder.FindColor(out X, Y: Integer; Color: Integer; Area: TBox): Boolean;
begin
  Result := FindColorTolerance(X, Y, Color, 0, Area);
end;

function TMFinder.FindColorTolerance(out X, Y: Integer; Color, Tolerance: Integer; Area: TBox): Boolean;
var
  Points: TPointArray;
begin
  Result := FindColorsTolerance(Points, Color, Tolerance, Area, 1);

  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; Area: TBox; MaxToFind: Integer = 0): Boolean;
var
  ImageData: TRetData;
  Buffer: TFindColorBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;
    Buffer.Offset := Area.TopLeft;

    if (Tolerance = 0) then
      Result := Buffer.Find(Points, Color, MaxToFind)
    else
    begin
      case Self.CTS of
        0: Result := Buffer.FindCTS0(Points, Color, Tolerance, MaxToFind);
        1: Result := Buffer.FindCTS1(Points, Color, Tolerance, MaxToFind);
        2: Result := Buffer.FindCTS2(Points, Color, Tolerance, HueMod, SatMod, MaxToFind);
        3: Result := Buffer.FindCTS2(Points, Color, Tolerance, CTS3Modifier, MaxToFind);
      end;
    end;
  end else
    Result := False;
end;

function TMFinder.FindColors(out TPA: TPointArray; Color: Integer; Area: TBox): Boolean;
begin
  Result := FindColorsTolerance(TPA, Color, 0, Area);
end;

function TMFinder.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; MaxToFind: Integer): Boolean;
var
  ImageData: TRetData;
  Buffer: TFindBitmapBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;
    Buffer.Offset := Area.TopLeft;

    Result := Buffer.Find(Bitmap, Points, MaxToFind);
  end else
    Result := False;
end;

function TMFinder.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox): Boolean;
var
  Points: TPointArray;
begin
  Result := FindBitmaps(Bitmap, Points, Area, 1);

  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

function TMFinder.FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; Tolerance: Integer; MaxToFind: Integer): Boolean;
var
  ImageData: TRetData;
  Buffer: TFindBitmapBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;
    Buffer.Offset := Area.TopLeft;

    Result := Buffer.Find(Bitmap, Points, Tolerance, MaxToFind);
  end else
    Result := False;
end;

function TMFinder.FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox; Tolerance: Integer): Boolean;
var
  Points: TPointArray;
begin
  Result := FindBitmapsTolerance(Bitmap, Points, Area, Tolerance, 1);

  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

{
  Tries to find the given Bitmap / template using MatchTemplate
}
function TMFinder.FindTemplateEx(TemplImage: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; Area: TBox; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
var
  Y: Integer;
  Image, Templ: TIntegerMatrix;
  xcorr: TSingleMatrix;
  ImageData : TRetData;
  maxLo, maxHi: Single;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    SetLength(Image, Area.Height, Area.Width);
    SetLength(Templ, TemplImage.Height, TemplImage.Width);

    for Y := 0 to Area.Height - 1 do
      Move(ImageData.Ptr[Y * ImageData.RowLen], Image[Y, 0], Area.Width * SizeOf(TRGB32));
    for Y := 0 to TemplImage.Height - 1 do
      Move(TemplImage.Data[Y * TemplImage.Width], Templ[Y, 0], TemplImage.Width * SizeOf(TRGB32));

    xcorr := MatchTemplate(Image, Templ, Formula);

    if Formula in [TM_SQDIFF, TM_SQDIFF_NORMED] then
    begin
      if DynamicAdjust then
      begin
        xcorr.MinMax(maxLo, maxHi);
        MinMatch := Min(MinMatch, maxLo + 0.1e-2);
      end;
      TPA := xcorr.Indices(MinMatch, __LE__).Offset(Area.TopLeft);
    end else
    begin
      if DynamicAdjust then
      begin
        xcorr.MinMax(maxLo, maxHi);
        MinMatch := Max(MinMatch, maxHi - 0.1e-2);
      end;
      TPA := xcorr.Indices(MinMatch, __GE__).Offset(Area.TopLeft);
    end;

    Result := Length(TPA) > 0;
  end else
    Result := False;
end;

function TMFinder.FindTemplate(TemplImage: TMufasaBitmap; out X, Y: Integer; Formula: ETMFormula; Area: TBox; MinMatch: Extended; DynamicAdjust: Boolean): Boolean;
var
  TPA: TPointArray;
begin
  Result := Self.FindTemplateEx(TemplImage, TPA, Formula, Area, MinMatch, DynamicAdjust);

  if Result then
  begin
    X := TPA[0].x;
    Y := TPA[0].y;
  end else
  begin
    X := -1;
    Y := -1;
  end;
end;

function TMFinder.FindDTMs(DTM: TDTM; out Points: TPointArray; Area: TBox; MaxToFind: Integer): Boolean;
var
  ImageData: TRetData;
  Buffer: TFindDTMBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;
    Buffer.Offset := Area.TopLeft;

    Points := Buffer.FindDTMs(DTM);

    Result := Length(Points) > 0;
  end else
    Result := False;
end;

function TMFinder.FindDTM(DTM: TDTM; out x, y: Integer; Area: TBox): Boolean;
var
  Points: TPointArray;
begin
  Result := Self.FindDTMs(DTM, Points, Area, 1);
  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

function TMFinder.FindDTMsRotated(DTM: TDTM; out Points: TPointArray; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: TDoubleArray; MaxToFind: Integer): Boolean;
var
  ImageData: TRetData;
  Buffer: TFindDTMBuffer;
begin
  if GetData(ImageData, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Buffer.Data := ImageData.Ptr;
    Buffer.Width := ImageData.RowLen;
    Buffer.SearchWidth := Area.Width;
    Buffer.SearchHeight := Area.Height;
    Buffer.Offset := Area.TopLeft;

    Points := Buffer.FindDTMsRotated(DTM, sAngle, eAngle, aStep, aFound);

    Result := Length(Points) > 0;
  end else
    Result := False;
end;

function TMFinder.FindDTMRotated(DTM: TDTM; out x, y: Integer; Area: TBox; sAngle, eAngle, aStep: Double; out aFound: Double): Boolean;
var
  Points: TPointArray;
  Angles: TDoubleArray;
begin
  Result := FindDTMsRotated(DTM, Points, Area, sAngle, eAngle, aStep, Angles, 1);
  if Result then
  begin
    X := Points[0].x;
    Y := Points[0].y;
    aFound := Angles[0];
  end;
end;

function TMFinder.GetColors(TPA: TPointArray): TIntegerArray;
var
  Bounds: TBox;
  I, X, Y: Integer;
  ImageData: TRetData;
begin
  SetLength(Result, Length(TPA));

  Bounds := TPA.Bounds();
  if GetData(ImageData, Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2) then
  begin
    for I := 0 to High(TPA) do
    begin
      X := TPA[I].X - Bounds.X1;
      Y := TPA[I].Y - Bounds.Y1;

      Result[I] := BGRToRGB(ImageData.Ptr[Y * ImageData.RowLen + X]);
    end;
  end else
    Result := nil;
end;

function TMFinder.GetColor(X, Y: Integer): Integer;
begin
  Result := TClient(Client).IOManager.GetColor(X, Y);
end;

function TMFinder.AverageBrightness(Area: TBox): Integer;
var
  Bitmap: TMufasaBitmap;
begin
  if GetBitmap(Bitmap, Area.X1, Area.Y1, Area.X2, Area.Y2, False) then
  begin
    Result := Bitmap.AverageBrightness();

    Bitmap.Free();
  end;
end;

function TMFinder.PeakBrightness(Area: TBox): Integer;
var
  Bitmap: TMufasaBitmap;
begin
  if GetBitmap(Bitmap, Area.X1, Area.Y1, Area.X2, Area.Y2, False) then
  begin
    Result := Bitmap.PeakBrightness();

    Bitmap.Free();
  end;
end;

function TMFinder.GetPixelDifference(Area: TBox; WaitTime: Integer): Integer;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := 0;

  if GetBitmap(BitmapBefore, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Sleep(WaitTime);
    if GetBitmap(BitmapAfter, Area.X1, Area.Y1, Area.X2, Area.Y2) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifference(BitmapAfter);
  end;

  if (BitmapBefore <> nil) then
    BitmapBefore.Free();
  if (BitmapAfter <> nil) then
    BitmapAfter.Free();
end;

function TMFinder.GetPixelDifference(Area: TBox; Tolerance: Integer; WaitTime: Integer): Integer;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := 0;

  if GetBitmap(BitmapBefore, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Sleep(WaitTime);
    if GetBitmap(BitmapAfter, Area.X1, Area.Y1, Area.X2, Area.Y2) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifference(BitmapAfter, Tolerance);
  end;

  if (BitmapBefore <> nil) then
    BitmapBefore.Free();
  if (BitmapAfter <> nil) then
    BitmapAfter.Free();
end;

function TMFinder.GetPixelDifferenceTPA(Area: TBox; WaitTime: Integer): TPointArray;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := nil;

  if GetBitmap(BitmapBefore, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Sleep(WaitTime);
    if GetBitmap(BitmapAfter, Area.X1, Area.Y1, Area.X2, Area.Y2) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifferenceTPA(BitmapAfter);
  end;

  if (BitmapBefore <> nil) then
    BitmapBefore.Free();
  if (BitmapAfter <> nil) then
    BitmapAfter.Free();
end;

function TMFinder.GetPixelDifferenceTPA(Area: TBox; Tolerance: Integer; WaitTime: Integer): TPointArray;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := nil;

  if GetBitmap(BitmapBefore, Area.X1, Area.Y1, Area.X2, Area.Y2) then
  begin
    Sleep(WaitTime);
    if GetBitmap(BitmapAfter, Area.X1, Area.Y1, Area.X2, Area.Y2) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifferenceTPA(BitmapAfter, Tolerance);
  end;

  if (BitmapBefore <> nil) then
    BitmapBefore.Free();
  if (BitmapAfter <> nil) then
    BitmapAfter.Free();
end;

end.
