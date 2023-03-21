{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_bitmap;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Math,
  simba.mufasatypes, simba.bitmap, simba.colormath, simba.colormath_distance;

type
  TBitmapFinder = record
  private
  type
    TCompareColorFunc = function(const C1: Pointer; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
  private
    FColorSpace: EColorSpace;
    FCompareColorFunc: TCompareColorFunc;
    FTolerance: Single;
    FMultipliers: TChannelMultipliers;
    FMaxDistance: Single;
  public
    procedure Setup(Formula: EColorSpace; Tolerance: Single; Multiplier: TChannelMultipliers);

    function GetBitmapColors(Bitmap: TMufasaBitmap; out ColorSize: Integer): PByte;
    function Find(Bitmap: TMufasaBitmap; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; Offset: TPoint; MaxToFind: Integer = -1): TPointArray;

    class operator Initialize(var Self: TBitmapFinder);
  end;

implementation

uses
  simba.overallocatearray, simba.colormath_distance_unrolled;

procedure TBitmapFinder.Setup(Formula: EColorSpace; Tolerance: Single; Multiplier: TChannelMultipliers);
begin
  FColorSpace := Formula;
  FTolerance := Tolerance;
  FMultipliers := Multiplier;

  case Formula of
    EColorSpace.RGB:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceRGB_UnRolled);
        FMaxDistance := DistanceRGB_Max(Multiplier);
      end;

    EColorSpace.HSV:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceHSV_UnRolled);
        FMaxDistance := DistanceHSV_Max(Multiplier);
      end;

    EColorSpace.HSL:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceHSL_UnRolled);
        FMaxDistance := DistanceHSL_Max(Multiplier);
      end;

    EColorSpace.XYZ:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceXYZ_UnRolled);
        FMaxDistance := DistanceXYZ_Max(Multiplier);
      end;

    EColorSpace.LAB:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceLAB_UnRolled);
        FMaxDistance := DistanceLAB_Max(Multiplier);
      end;

    EColorSpace.LCH:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceLCH_UnRolled);
        FMaxDistance := DistanceLCH_Max(Multiplier);
      end;

    EColorSpace.DeltaE:
      begin
        FCompareColorFunc := TCompareColorFunc(@DistanceDeltaE_UnRolled);
        FMaxDistance := DistanceDeltaE_Max(Multiplier);
      end;
  end;
end;

// Pre calculate color space
// and "transparent" (aka ignore) colors.
function TBitmapFinder.GetBitmapColors(Bitmap: TMufasaBitmap; out ColorSize: Integer): PByte;
var
  I: Integer;
  Source: PColorBGRA;
  Dest, DestFix: PByte;
begin
  case FColorSpace of
    EColorSpace.RGB:    ColorSize := SizeOf(TColorRGB) + 1;
    EColorSpace.HSV:    ColorSize := SizeOf(TColorHSV) + 1;
    EColorSpace.HSL:    ColorSize := SizeOf(TColorHSL) + 1;
    EColorSpace.XYZ:    ColorSize := SizeOf(TColorXYZ) + 1;
    EColorSpace.LCH:    ColorSize := SizeOf(TColorLCH) + 1;
    EColorSpace.LAB:    ColorSize := SizeOf(TColorLAB) + 1;
    EColorSpace.DeltaE: ColorSize := SizeOf(TColorLAB) + 1;
  end;

  // packed record Transparent: ByteBool; Color: TColorXXX; end;
  Result := AllocMem((Bitmap.Width * Bitmap.Height) * ColorSize);

  Source := Bitmap.Data;
  Dest := Result;
  for I := 0 to (Bitmap.Width * Bitmap.Height) - 1 do
  begin
    if (Bitmap.TransparentColorActive and Source^.EqualsIgnoreAlpha(Bitmap.TransparentRGB)) then
      PByteBool(Dest)^ := True
    else
    begin
      DestFix := Dest + 1; // temp fix, https://gitlab.com/freepascal.org/fpc/source/-/commit/851af5033fb80d4e19c4a7b5c44d50a36f456374
      case FColorSpace of
        EColorSpace.RGB:    PColorRGB(DestFix)^ := Source^.ToRGB();
        EColorSpace.HSV:    PColorHSV(DestFix)^ := Source^.ToHSV();
        EColorSpace.HSL:    PColorHSL(DestFix)^ := Source^.ToHSL();
        EColorSpace.XYZ:    PColorXYZ(DestFix)^ := Source^.ToXYZ();
        EColorSpace.LCH:    PColorLCH(DestFix)^ := Source^.ToLCH();
        EColorSpace.LAB:    PColorLAB(DestFix)^ := Source^.ToLAB();
        EColorSpace.DeltaE: PColorLAB(DestFix)^ := Source^.ToLAB();
      end;
    end;

    Inc(Source);
    Inc(Dest, ColorSize);
  end;
end;

function TBitmapFinder.Find(Bitmap: TMufasaBitmap; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; Offset: TPoint; MaxToFind: Integer): TPointArray;
var
  BitmapColors: PByte;
  BitmapColorSize: Integer;

  function IsTransparent(const BitmapPtr: Pointer): Boolean; inline;
  begin
    Result := PByteBool(BitmapPtr)^;
  end;

  function Match(const BufferPtr: TColorBGRA; const BitmapPtr: PByte): Boolean; inline;
  begin
    Result := (Self.FCompareColorFunc(BitmapPtr + 1, BufferPtr, FMultipliers) / FMaxDistance * 100 <= FTolerance);
  end;

  function Hit(BufferPtr: PColorBGRA): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PByte;
  begin
    BitmapPtr := BitmapColors;

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        if (not IsTransparent(BitmapPtr)) and (not Match(BufferPtr^, BitmapPtr)) then
          Exit(False);

        Inc(BitmapPtr, BitmapColorSize);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferWidth - Bitmap.Width);
    end;

    Result := True;
  end;

var
  X, Y: Integer;
  RowPtr: PColorBGRA;
  PointBuffer: TSimbaPointBuffer;
label
  Finished;
begin
  Result := nil;
  if IsZero(FMaxDistance) or (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  BitmapColors := GetBitmapColors(Bitmap, BitmapColorSize);
  try
    Dec(SearchWidth, Bitmap.Width);
    Dec(SearchHeight, Bitmap.Height);

    for Y := 0 to SearchHeight do
    begin
      RowPtr := @Buffer[Y * BufferWidth];

      for X := 0 to SearchWidth do
      begin
        if Hit(RowPtr) then
        begin
          PointBuffer.Add(X + Offset.X, Y + Offset.Y);
          if (PointBuffer.Count = MaxToFind) then
            goto Finished;
        end;

        Inc(RowPtr);
      end;
    end;

    Finished:

    Result := PointBuffer.Trim();
  finally
    FreeMem(BitmapColors);
  end;
end;

class operator TBitmapFinder.Initialize(var Self: TBitmapFinder);
begin
  Self := Default(TBitmapFinder);
end;

end.

