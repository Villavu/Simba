{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Finds things in pixels. Where the pixels come from is the subclass's:
  an image (TSimbaImageFinder) or a target (TSimbaTarget).
}
unit simba.finder;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.baseclass,
  simba.colormath,
  simba.dtm,
  simba.finder_pixels;

type
  // A Bounds of [-1,-1,-1,-1] is everything.
  // It knows pixels only, not TSimbaImage (which has a finder): an image to find is given as its data and size.
  PSimbaFinder = ^TSimbaFinder;
  TSimbaFinder = class(TSimbaBaseClass)
  public
    // The pixels of ABounds, which comes back clamped to what there is. False when nothing is left.
    function GetImageData(var ABounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean; virtual; abstract;
    procedure FreeImageData(var Data: PColorBGRA); virtual; abstract;

    // color
    // each pixel's distance from Color: 0 an exact match, 100 as far as the colour space goes
    function MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TSingleMatrix;

    function FindColor(Color: TColor; Tolerance: Single; ABounds: TBox): TPointArray; overload;
    function FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray; overload;

    function CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer; overload;
    function CountColor(Color: TColorTolerance; ABounds: TBox): Integer; overload;

    function HasColor(Color: TColor; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean; overload;
    function HasColor(Color: TColorTolerance; MinCount: Integer; ABounds: TBox): Boolean; overload;

    function GetColor(P: TPoint): TColor;
    function GetColors(Points: TPointArray): TColorArray;
    function GetColorsMatrix(ABounds: TBox): TIntegerMatrix;

    // image: ImageWidth x ImageHeight pixels with no gap between rows
    function FindImage(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ABounds: TBox): TPoint; overload;
    function FindImage(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPoint; overload;
    function FindImageEx(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; MaxToFind: Integer; ABounds: TBox): TPointArray; overload;
    function FindImageEx(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; ABounds: TBox): TPointArray; overload;

    // template
    function FindTemplate(Templ: PColorBGRA; TemplWidth, TemplHeight: Integer; out Match: Single; ABounds: TBox): TPoint;

    // dtm
    function FindDTM(DTM: TDTM; ABounds: TBox): TPoint;
    function FindDTMEx(DTM: TDTM; MaxToFind: Integer; ABounds: TBox): TPointArray;
    function FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; ABounds: TBox): TPoint;
    function FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; ABounds: TBox): TPointArray;

    // other
    function FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray; overload;
    function FindEdges(MinDiff: Single; ABounds: TBox): TPointArray; overload;

    function GetBrightness(Algo: EBrightnessAlgo; ABounds: TBox): Integer;
  end;

implementation

uses
  simba.vartype_box,
  simba.vartype_pointarray,
  simba.finder_color,
  simba.finder_image,
  simba.finder_dtm;

function TSimbaFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TSingleMatrix;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_MatchColors(Data, DataWidth, ABounds.Width, ABounds.Height, ColorSpace, Color, Multipliers);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ABounds: TBox): TPointArray;
begin
  Result := FindColor(TColorTolerance.Create(Color, Tolerance), ABounds);
end;

function TSimbaFinder.FindColor(Color: TColorTolerance; ABounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindColors(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ABounds: TBox): Integer;
begin
  Result := CountColor(TColorTolerance.Create(Color, Tolerance), ABounds);
end;

function TSimbaFinder.CountColor(Color: TColorTolerance; ABounds: TBox): Integer;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := 0;

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_CountColors(Data, DataWidth, ABounds.Width, ABounds.Height, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer; ABounds: TBox): Boolean;
begin
  Result := HasColor(TColorTolerance.Create(Color, Tolerance), MinCount, ABounds);
end;

function TSimbaFinder.HasColor(Color: TColorTolerance; MinCount: Integer; ABounds: TBox): Boolean;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := False;

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_CountColors(Data, DataWidth, ABounds.Width, ABounds.Height, Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers, MinCount) >= MinCount;
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.GetColor(P: TPoint): TColor;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := -1;

  B := TBox.Create(P.X, P.Y, P.X, P.Y);
  if GetImageData(B, Data, DataWidth) then
  try
    Result := Data^.ToColor();
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.GetColors(Points: TPointArray): TColorArray;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];
  if (Length(Points) = 0) then
    Exit;

  B := Points.Bounds;
  if GetImageData(B, Data, DataWidth) then
  try
    Result := SimbaFinder_GetColors(Data, DataWidth, B.Width, B.Height, B.TopLeft, Points);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.GetColorsMatrix(ABounds: TBox): TIntegerMatrix;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_GetColorsMatrix(Data, DataWidth, ABounds.Width, ABounds.Height);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindImageEx(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; MaxToFind: Integer; ABounds: TBox): TPointArray;
begin
  Result := FindImageEx(Image, ImageWidth, ImageHeight, Tolerance, DefaultColorSpace, DefaultMultipliers, MaxToFind, ABounds);
end;

function TSimbaFinder.FindImageEx(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; ABounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindImage(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, Image, ImageWidth, ImageHeight, ColorSpace, Tolerance, Multipliers, MaxToFind);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindImage(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ABounds: TBox): TPoint;
begin
  Result := FindImage(Image, ImageWidth, ImageHeight, Tolerance, DefaultColorSpace, DefaultMultipliers, ABounds);
end;

function TSimbaFinder.FindImage(Image: PColorBGRA; ImageWidth, ImageHeight: Integer; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindImageEx(Image, ImageWidth, ImageHeight, Tolerance, ColorSpace, Multipliers, 1, ABounds);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaFinder.FindTemplate(Templ: PColorBGRA; TemplWidth, TemplHeight: Integer; out Match: Single; ABounds: TBox): TPoint;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := TPoint.Create(-1, -1);
  Match := 0;

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindTemplate(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, Templ, TemplWidth, TemplHeight, Match);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer; ABounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindDTM(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, DTM, MaxToFind);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer; ABounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];
  FoundDegrees := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindDTMRotated(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, DTM, StartDegrees, EndDegrees, Step, FoundDegrees, MaxToFind);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindDTM(DTM: TDTM; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindDTMEx(DTM, 1, ABounds);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; ABounds: TBox): TPoint;
var
  TPA: TPointArray;
begin
  TPA := FindDTMRotatedEx(DTM, StartDegrees, EndDegrees, Step, FoundDegrees, 1, ABounds);
  if (Length(TPA) > 0) then
    Result := TPA[0]
  else
    Result := TPoint.Create(-1, -1);
end;

function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; ABounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := [];

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_FindEdges(Data, DataWidth, ABounds.Width, ABounds.Height, ABounds.TopLeft, MinDiff, ColorSpace, Multipliers);
  finally
    FreeImageData(Data);
  end;
end;

function TSimbaFinder.FindEdges(MinDiff: Single; ABounds: TBox): TPointArray;
begin
  Result := FindEdges(MinDiff, DefaultColorSpace, DefaultMultipliers, ABounds);
end;

function TSimbaFinder.GetBrightness(Algo: EBrightnessAlgo; ABounds: TBox): Integer;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  Result := 0;

  if GetImageData(ABounds, Data, DataWidth) then
  try
    Result := SimbaFinder_GetBrightness(Data, DataWidth, ABounds.Width, ABounds.Height, Algo);
  finally
    FreeImageData(Data);
  end;
end;

end.
