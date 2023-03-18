{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Find things on a target.
}
unit simba.finder;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.colormath_conversion, simba.bitmap, simba.dtm,
  simba.finder_color, simba.finder_bitmap, simba.finder_dtm;

type
  PColorTolerance = ^TColorTolerance;
  TColorTolerance = record
    Color: TColor;
    Tolerance: Single;
    ColorSpace: EColorSpace;
    Multipliers: TChannelMultipliers;
  end;

  {$PUSH}
  {$SCOPEDENUMS ON}
  ETargetType = (NONE, BITMAP, WINDOW);
  {$POP}

  PSimbaFinder = ^TSimbaFinder;
  TSimbaFinder = record
  private
    FTargetType: ETargetType;
    FTarget: record
      Bitmap: TMufasaBitmap;
      Window: TWindowHandle;
    end;
    FColorFinder: TColorFinder;
    FBitmapFinder: TBitmapFinder;
    FDTMFinder: TDTMFinder;

    function DoFindDTM(DTM: TDTM; Bounds: TBox; MaxToFind: Integer): TPointArray;
    function DoFindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox; MaxToFind: Integer): TPointArray;
    function DoFindBitmap(Bitmap: TMufasaBitmap; Bounds: TBox; MaxToFind: Integer): TPointArray;

    function DoFindColor(Bounds: TBox): TPointArray;
    function DoCountColor(Bounds: TBox): Integer;

    function ValidateTargetBounds(var Bounds: TBox): Boolean;
    function GetTargetData(var Bounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
    procedure FreeTargetData(var Data: PColorBGRA);

    function GetDataAsBitmap(var Bounds: TBox; out Bitmap: TMufasaBitmap): Boolean;
  public
    procedure SetTarget(Bitmap: TMufasaBitmap); overload;
    procedure SetTarget(Window: TWindowHandle); overload;

    function FindEdges(MinDiff: Single; Bounds: TBox): TPointArray; overload;
    function FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TPointArray; overload;

    function FindDTM(DTM: TDTM; MaxToFind: Integer; Bounds: TBox): TPointArray;
    function FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; Bounds: TBox): TPointArray;

    function FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer; Bounds: TBox): TPointArray; overload;
    function FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox): TPointArray; overload;

    function MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TSingleMatrix;

    function FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray; overload;
    function FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TPointArray; overload;
    function FindColor(Color: TColorTolerance; Bounds: TBox): TPointArray; overload;

    function CountColor(Color: TColor; Tolerance: Single; Bounds: TBox): Integer; overload;
    function CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): Integer; overload;
    function CountColor(Color: TColorTolerance; Bounds: TBox): Integer; overload;

    function GetColor(X, Y: Integer): TColor;
    function GetColors(Points: TPointArray): TColorArray;
    function GetColorsMatrix(Bounds: TBox): TIntegerMatrix;

    function GetPixelDifference(WaitTime: Integer; Area: TBox): Integer; overload;
    function GetPixelDifference(WaitTime: Integer; Tolerance: Integer; Area: TBox): Integer; overload;
    function GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox): TPointArray; overload;
    function GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Integer; Area: TBox): TPointArray; overload;

    function AverageBrightness(Area: TBox): Integer;
    function PeakBrightness(Area: TBox): Integer;

    class operator Initialize(var Self: TSimbaFinder);
  end;

implementation

uses
  simba.nativeinterface, simba.tpa;

function TSimbaFinder.DoFindDTM(DTM: TDTM; Bounds: TBox; MaxToFind: Integer): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    Result := FDTMFinder.Find(DTM, Data, DataWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft, MaxToFind);
  finally
    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.DoFindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox; MaxToFind: Integer): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    Result := FDTMFinder.FindRotated(DTM, StartDegrees, EndDegrees, Step, FoundDegrees, Data, DataWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft, MaxToFind);
  finally
    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.DoFindBitmap(Bitmap: TMufasaBitmap; Bounds: TBox; MaxToFind: Integer): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    Result := FBitmapFinder.Find(Bitmap, Data, DataWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft, MaxToFind);
  finally
    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.DoFindColor(Bounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    Result := FColorFinder.Find(Data, DataWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft);
  finally
    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.DoCountColor(Bounds: TBox): Integer;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := 0;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    Result := FColorFinder.Count(Data, DataWidth, Bounds.Width, Bounds.Height);
  finally
    FreeTargetData(Data);
  end;
end;

procedure TSimbaFinder.FreeTargetData(var Data: PColorBGRA);
begin
  if (FTargetType in [ETargetType.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaFinder.GetDataAsBitmap(var Bounds: TBox; out Bitmap: TMufasaBitmap): Boolean;
var
  Data: PColorBGRA;
  DataWidth: Integer;
  Y: Integer;
begin
  Result := GetTargetData(Bounds, Data, DataWidth);
  if Result then
  begin
    Bitmap := TMufasaBitmap.Create(Bounds.Width, Bounds.Height);
    for Y := 0 to Bitmap.Height - 1 do
      Move(Data[Y * DataWidth], Bitmap.Data[Y * Bitmap.Width], Bitmap.Width * SizeOf(TColorBGRA));

    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.ValidateTargetBounds(var Bounds: TBox): Boolean;
var
  Width, Height: Integer;
  B: TBox;
begin
  Result := False;

  case FTargetType of
    ETargetType.BITMAP:
      begin
        Result := True;

        Width := FTarget.Bitmap.Width;
        Height := FTarget.Bitmap.Height;
      end;

    ETargetType.WINDOW:
      begin
        Result := SimbaNativeInterface.GetWindowBounds(FTarget.Window, B);

        Width := B.Width;
        Height := B.Height;
      end;

    else
      raise Exception.Create('TSimbaFinder: Target not set');
  end;

  if Result then
  begin
    if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    begin
      Bounds.X1 := 0;
      Bounds.Y1 := 0;
      Bounds.X2 := Width -1;
      Bounds.Y2 := Height - 1;
    end else
    begin
      if (Bounds.X1 < 0)       then Bounds.X1 := 0;
      if (Bounds.Y1 < 0)       then Bounds.Y1 := 0;
      if (Bounds.X2 >= Width)  then Bounds.X2 := Width - 1;
      if (Bounds.Y2 >= Height) then Bounds.Y2 := Height - 1;
    end;

    Result := (Bounds.Width > 0) and (Bounds.Height > 0);
  end;
end;

function TSimbaFinder.GetTargetData(var Bounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
begin
  Result := False;
  Data := nil;

  if ValidateTargetBounds(Bounds) then
  begin
    case FTargetType of
      ETargetType.BITMAP:
        begin
          DataWidth := FTarget.Bitmap.Width;
          Data := FTarget.Bitmap.Data + (Bounds.Y1 * FTarget.Bitmap.Width) + Bounds.X1;

          Result := True;
        end;

      ETargetType.WINDOW:
        begin
          DataWidth := Bounds.Width;

          Result := SimbaNativeInterface.GetWindowImage(FTarget.Window, Bounds.X1, Bounds.Y1, Bounds.Width, Bounds.Height, Data);
        end;
      else
        raise Exception.Create('TSimbaFinder: Target not set');
    end;
  end;
end;

procedure TSimbaFinder.SetTarget(Bitmap: TMufasaBitmap);
begin
  FTargetType := ETargetType.BITMAP;
  FTarget.Bitmap := Bitmap;
end;

procedure TSimbaFinder.SetTarget(Window: TWindowHandle);
begin
  FTargetType := ETargetType.WINDOW;
  FTarget.Window := Window;
end;

function TSimbaFinder.FindEdges(MinDiff: Single; Bounds: TBox): TPointArray;
var
  Bitmap: TMufasaBitmap;
begin
  Result := nil;

  if GetDataAsBitmap(Bounds, Bitmap) then
  try
    Result := Bitmap.FindEdges(MinDiff);
  finally
    Bitmap.Free();
  end;
end;

function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TPointArray;
var
  Bitmap: TMufasaBitmap;
begin
  Result := nil;

  if GetDataAsBitmap(Bounds, Bitmap) then
  try
    Result := Bitmap.FindEdges(MinDiff, ColorSpace, Multipliers);
  finally
    Bitmap.Free();
  end;
end;

function TSimbaFinder.FindDTM(DTM: TDTM; MaxToFind: Integer; Bounds: TBox): TPointArray;
begin
  Result := DoFindDTM(DTM, Bounds, MaxToFind);
end;

function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; Bounds: TBox): TPointArray;
begin
  Result := DoFindDTMRotated(DTM, StartDegrees, EndDegrees, Step, FoundDegrees, Bounds, MaxToFind);
end;

function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer; Bounds: TBox): TPointArray;
begin
  FBitmapFinder.Setup(EColorSpace.RGB, Tolerance, DefaultMultipliers);

  Result := DoFindBitmap(Bitmap, Bounds, MaxToFind);
end;

function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox): TPointArray;
begin
  FBitmapFinder.Setup(ColorSpace, Tolerance, Multipliers);

  Result := DoFindBitmap(Bitmap, Bounds, MaxToFind);
end;

function TSimbaFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TSingleMatrix;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  try
    FColorFinder.Setup(ColorSpace, Color, 0, Multipliers);

    Result := FColorFinder.Match(Data, DataWidth, Bounds.Width, Bounds.Height);
  finally
    FreeTargetData(Data);
  end;
end;

function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, Tolerance, DefaultMultipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(ColorSpace, Color, Tolerance, Multipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColor(Color: TColorTolerance; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, Tolerance, DefaultMultipliers);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(ColorSpace, Color, Tolerance, Multipliers);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColorTolerance; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.GetColor(X, Y: Integer): TColor;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  B.X1 := X; B.Y1 := Y;
  B.X2 := X; B.Y2 := Y;

  if GetTargetData(B, Data, DataWidth) then
    Result := Data^.ToColor()
  else
    Result := -1;
end;

function TSimbaFinder.GetColors(Points: TPointArray): TColorArray;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
  I, X, Y, Count: Integer;
begin
  Result := nil;

  B := Points.Bounds();
  if GetTargetData(B, Data, DataWidth) then
  begin
    SetLength(Result, Length(Points));

    Count := 0;
    for I := 0 to High(Points) do
    begin
      X := Points[I].X - B.X1;
      Y := Points[I].Y - B.Y1;

      Result[Count] := Data[Y * DataWidth + X].ToColor();
      Inc(Count);
    end;
    SetLength(Result, Count);
  end;
end;

function TSimbaFinder.GetColorsMatrix(Bounds: TBox): TIntegerMatrix;
var
  Data: PColorBGRA;
  DataWidth: Integer;
  Width, Height, X, Y: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  begin
    Width := Bounds.Width - 1;
    Height := Bounds.Height - 1;

    Result.SetSize(Width + 1, Height + 1);

    for Y := 0 to Height do
      for X := 0 to Width do
        Result[Y, X] := Data[Y * DataWidth + X].ToColor();
  end;
end;

function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Area: TBox): Integer;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := 0;

  BitmapBefore := nil;
  BitmapAfter := nil;

  if GetDataAsBitmap(Area, BitmapBefore) then
  try
    Sleep(WaitTime);
    if GetDataAsBitmap(Area, BitmapAfter) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifference(BitmapAfter);
  finally
    if (BitmapBefore <> nil) then
      BitmapBefore.Free();
    if (BitmapAfter <> nil) then
      BitmapAfter.Free();
  end;
end;

function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Tolerance: Integer; Area: TBox): Integer;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := 0;

  BitmapBefore := nil;
  BitmapAfter := nil;

  if GetDataAsBitmap(Area, BitmapBefore) then
  try
    Sleep(WaitTime);
    if GetDataAsBitmap(Area, BitmapAfter) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifference(BitmapAfter, Tolerance);
  finally
    if (BitmapBefore <> nil) then
      BitmapBefore.Free();
    if (BitmapAfter <> nil) then
      BitmapAfter.Free();
  end;
end;

function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox): TPointArray;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := nil;

  BitmapBefore := nil;
  BitmapAfter := nil;

  if GetDataAsBitmap(Area, BitmapBefore) then
  try
    Sleep(WaitTime);
    if GetDataAsBitmap(Area, BitmapAfter) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifferenceTPA(BitmapAfter);
  finally
    if (BitmapBefore <> nil) then
      BitmapBefore.Free();
    if (BitmapAfter <> nil) then
      BitmapAfter.Free();
  end;
end;

function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Integer; Area: TBox): TPointArray;
var
  BitmapBefore, BitmapAfter: TMufasaBitmap;
begin
  Result := nil;

  BitmapBefore := nil;
  BitmapAfter := nil;

  if GetDataAsBitmap(Area, BitmapBefore) then
  try
    Sleep(WaitTime);
    if GetDataAsBitmap(Area, BitmapAfter) and (BitmapBefore.Width = BitmapAfter.Width) and (BitmapBefore.Height = BitmapAfter.Height) then
      Result := BitmapBefore.PixelDifferenceTPA(BitmapAfter, Tolerance);
  finally
    if (BitmapBefore <> nil) then
      BitmapBefore.Free();
    if (BitmapAfter <> nil) then
      BitmapAfter.Free();
  end;
end;

function TSimbaFinder.AverageBrightness(Area: TBox): Integer;
var
  Bitmap: TMufasaBitmap;
begin
  Result := 0;

  if GetDataAsBitmap(Area, Bitmap) then
  try
    Result := Bitmap.AverageBrightness();
  finally
    Bitmap.Free();
  end;
end;

function TSimbaFinder.PeakBrightness(Area: TBox): Integer;
var
  Bitmap: TMufasaBitmap;
begin
  Result := 0;

  if GetDataAsBitmap(Area, Bitmap) then
  try
    Result := Bitmap.PeakBrightness();
  finally
    Bitmap.Free();
  end;
end;

class operator TSimbaFinder.Initialize(var Self: TSimbaFinder);
begin
  Self := Default(TSimbaFinder);
end;

end.

