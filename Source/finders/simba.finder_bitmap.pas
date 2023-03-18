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
  simba.mufasatypes, simba.bitmap, simba.colormath_conversion;

type
  TBitmapFinder = record
  private
  type
    TCompareColorFunc = function(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
  private
    FCompareColorFunc: TCompareColorFunc;
    FTolerance: Single;
    FMultipliers: TChannelMultipliers;
    FMaxDistance: Single;
  public
    procedure Setup(Formula: EColorSpace; Tolerance: Single; Multiplier: TChannelMultipliers);

    function Find(Bitmap: TMufasaBitmap; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; Offset: TPoint; MaxToFind: Integer = -1): TPointArray;

    class operator Initialize(var Self: TBitmapFinder);
  end;

implementation

uses
  simba.colormath_distance, simba.overallocatearray;

function _DistanceRGB(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceRGB(Color1.ToRGB(), Color2.ToRGB(), Mul);
end;

function _DistanceHSV(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceHSV(Color1.ToHSV(), Color2.ToHSV(), Mul);
end;

function _DistanceHSL(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceHSL(Color1.ToHSL(), Color2.ToHSL(), Mul);
end;

function _DistanceXYZ(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceXYZ(Color1.ToXYZ(), Color2.ToXYZ(), Mul);
end;

function _DistanceLAB(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceLAB(Color1.ToLAB(), Color2.ToLAB(), Mul);
end;

function _DistanceLCH(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceLCH(Color1.ToLCH(), Color2.ToLCH(), Mul);
end;

function _DistanceDeltaE(const Color1, Color2: TColorBGRA; const Mul: TChannelMultipliers): Single;
begin
  Result := DistanceDeltaE(Color1.ToLAB(), Color2.ToLAB(), Mul);
end;

procedure TBitmapFinder.Setup(Formula: EColorSpace; Tolerance: Single; Multiplier: TChannelMultipliers);
begin
  FTolerance := Tolerance;
  FMultipliers := Multiplier;

  case Formula of
    EColorSpace.RGB:
      begin
        FCompareColorFunc := @_DistanceRGB;
        FMaxDistance := DistanceRGB_Max(Multiplier);
      end;

    EColorSpace.HSV:
      begin
        FCompareColorFunc := @_DistanceHSV;
        FMaxDistance := DistanceHSV_Max(Multiplier);
      end;

    EColorSpace.HSL:
      begin
        FCompareColorFunc := @_DistanceHSL;
        FMaxDistance := DistanceHSL_Max(Multiplier);
      end;

    EColorSpace.XYZ:
      begin
        FCompareColorFunc := @_DistanceXYZ;
        FMaxDistance := DistanceXYZ_Max(Multiplier);
      end;

    EColorSpace.LAB:
      begin
        FCompareColorFunc := @_DistanceLAB;
        FMaxDistance := DistanceLAB_Max(Multiplier);
      end;

    EColorSpace.LCH:
      begin
        FCompareColorFunc := @_DistanceLCH;
        FMaxDistance := DistanceLCH_Max(Multiplier);
      end;

    EColorSpace.DeltaE:
      begin
        FCompareColorFunc := @_DistanceDeltaE;
        FMaxDistance := DistanceDeltaE_Max(Multiplier);
      end;
  end;
end;

function TBitmapFinder.Find(Bitmap: TMufasaBitmap; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; Offset: TPoint; MaxToFind: Integer): TPointArray;

  function Match(const BufferPtr, BitmapPtr: TColorBGRA): Boolean; inline;
  begin
    Result := (Bitmap.TransparentColorActive and BitmapPtr.EqualsIgnoreAlpha(Bitmap.TransparentRGB)) or
              (Self.FCompareColorFunc(BufferPtr, BitmapPtr, FMultipliers) / FMaxDistance * 100 <= FTolerance);
  end;

  function Hit(BufferPtr: PColorBGRA): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PColorBGRA;
  begin
    BitmapPtr := Bitmap.Data;

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        if (not Match(BufferPtr^, BitmapPtr^)) then
          Exit(False);

        Inc(BitmapPtr);
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
end;

class operator TBitmapFinder.Initialize(var Self: TBitmapFinder);
begin
  Self := Default(TBitmapFinder);
end;

end.

