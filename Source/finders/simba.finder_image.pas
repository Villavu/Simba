{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_image;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.colormath,
  simba.image;

function SimbaFinder_FindImage(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                               Image: TSimbaImage; ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers;
                               MaxToFind: Integer = -1): TPointArray;

function SimbaFinder_FindTemplate(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                  Templ: TSimbaImage; out Match: Single): TPoint;

implementation

uses
  simba.container_point,
  simba.finder_pixels,
  simba.matchtemplate,
  simba.multiprocessing,
  simba.colormath_distance,
  simba.colormath_distance_unrolled,
  simba.vartype_pointarray,
  simba.vartype_matrix,
  simba.colormath_conversion;

const
  BitmapColorSize = SizeOf(TColorHSL) + SizeOf(Boolean);

// Pre calculate color space
// and "transparent" (aka ignore) colors.
function ConvertBitmapColors(Image: TSimbaImage; ColorSpace: EColorSpace): PByte;
var
  Source, SourceEnd: PColorBGRA;
  Dest, DestFix: PByte;
begin
  // packed record Transparent: Boolean; Color: TColorXXX; end;
  Result := GetMem(Image.PixelCount * BitmapColorSize);

  Source := Image.Data;
  SourceEnd := Source + Image.PixelCount;
  Dest := Result;

  while (Source < SourceEnd) do
  begin
    PBoolean(Dest)^ := Source^.A = ALPHA_TRANSPARENT;

    if not PBoolean(Dest)^ then
    begin
      DestFix := Dest + 1; // temp fix, https://gitlab.com/freepascal.org/fpc/source/-/commit/851af5033fb80d4e19c4a7b5c44d50a36f456374
      case ColorSpace of
        EColorSpace.RGB:
          begin
            PColorRGB(DestFix)^.R := Source^.R;
            PColorRGB(DestFix)^.G := Source^.G;
            PColorRGB(DestFix)^.B := Source^.B;
          end;
        EColorSpace.HSV:    PColorHSV(DestFix)^ := TSimbaColorConversion.RGBToHSV(Source^.R, Source^.G, Source^.B);
        EColorSpace.HSL:    PColorHSL(DestFix)^ := TSimbaColorConversion.RGBToHSL(Source^.R, Source^.G, Source^.B);
        EColorSpace.XYZ:    PColorXYZ(DestFix)^ := TSimbaColorConversion.RGBToXYZ(Source^.R, Source^.G, Source^.B);
        EColorSpace.LCH:    PColorLCH(DestFix)^ := TSimbaColorConversion.RGBToLCH(Source^.R, Source^.G, Source^.B);
        EColorSpace.LAB,
        EColorSpace.DeltaE: PColorLAB(DestFix)^ := TSimbaColorConversion.RGBToLAB(Source^.R, Source^.G, Source^.B);
      end;
    end;

    Inc(Source);
    Inc(Dest, BitmapColorSize);
  end;
end;

function FindImageSlice(SliceFound: PInt32; SliceIndex, MaxToFind: Integer; Image: TSimbaImage; ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
var
  BitmapColors, BitmapEnd: PByte;

  CompareFunc: TColorDistanceFunc;
  MaxDistance: Single;

  function IsTransparent(const BitmapPtr: Pointer): Boolean; inline;
  begin
    Result := PBoolean(BitmapPtr)^;
  end;

  function Match(const BufferPtr: TColorBGRA; const BitmapPtr: PByte): Boolean; inline;
  begin
    Result := (CompareFunc(BitmapPtr + 1, @BufferPtr, Multipliers) / MaxDistance * 100 <= Tolerance);
  end;

  function Hit(BufferPtr: PColorBGRA): Boolean;
  var
    BitmapPtr: PByte;
    RowEnd: PColorBGRA;
  begin
    BitmapPtr := BitmapColors;

    while (BitmapPtr < BitmapEnd) do
    begin
      RowEnd := BufferPtr + Image.Width;
      while (BufferPtr < RowEnd) do
      begin
        if (not IsTransparent(BitmapPtr)) and (not Match(BufferPtr^, BitmapPtr)) then
          Exit(False);

        Inc(BitmapPtr, BitmapColorSize);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferWidth - Image.Width);
    end;

    Result := True;
  end;

  function Enough: Boolean;
  var
    I, Found: Integer;
  begin
    Found := 0;
    for I := 0 to SliceIndex do
      Found += SliceFound[I];
    Result := Found >= MaxToFind;
  end;

var
  X, Y: Integer;
  RowPtr: PColorBGRA;
  PointBuffer: TPointBuffer;
begin
  Result := [];

  case ColorSpace of
    EColorSpace.RGB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceRGB_UnRolled);
        MaxDistance := DistanceRGB_Max(Multipliers);
      end;

    EColorSpace.HSV:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSV_UnRolled);
        MaxDistance := DistanceHSV_Max(Multipliers);
      end;

    EColorSpace.HSL:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceHSL_Unrolled);
        MaxDistance := DistanceHSL_Max(Multipliers);
      end;

    EColorSpace.XYZ:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceXYZ_UnRolled);
        MaxDistance := DistanceXYZ_Max(Multipliers);
      end;

    EColorSpace.LAB:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLAB_UnRolled);
        MaxDistance := DistanceLAB_Max(Multipliers);
      end;

    EColorSpace.LCH:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceLCH_UnRolled);
        MaxDistance := DistanceLCH_Max(Multipliers);
      end;

    EColorSpace.DeltaE:
      begin
        CompareFunc := TColorDistanceFunc(@DistanceDeltaE_UnRolled);
        MaxDistance := DistanceDeltaE_Max(Multipliers);
      end;
  end;

  BitmapColors := ConvertBitmapColors(Image, ColorSpace);
  BitmapEnd := BitmapColors + Image.PixelCount * BitmapColorSize;

  try
    Dec(SearchWidth, Image.Width);
    Dec(SearchHeight, Image.Height);

    for Y := 0 to SearchHeight do
    begin
      RowPtr := @Buffer[Y * BufferWidth];

      for X := 0 to SearchWidth do
      begin
        if Hit(RowPtr) then
        begin
          PointBuffer.Add(X + OffsetX, Y + OffsetY);

          Inc(SliceFound[SliceIndex]);
        end;

        Inc(RowPtr);
      end;

      if (MaxToFind > 0) and Enough() then
        Break;
    end;

    Result := PointBuffer.ToArray(False);
  finally
    FreeMem(BitmapColors);
  end;
end;

function SimbaFinder_FindImage(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                               Image: TSimbaImage; ColorSpace: EColorSpace; Tolerance: Single; Multipliers: TChannelMultipliers;
                               MaxToFind: Integer): TPointArray;
var
  SliceResults: T2DPointArray;
  SliceFound: array of Int32;

  // a slice covers the rows a match can start on, plus the image's height below them
  procedure Execute(const Index, Lo, Hi: Integer);
  begin
    SliceResults[Index] := FindImageSlice(
      @SliceFound[0], Index, MaxToFind,
      Image, ColorSpace, Tolerance, Multipliers,
      @Data[Lo * PixelsPerRow], PixelsPerRow, AWidth, (Hi - Lo) + Image.Height, Offset.X, Offset.Y + Lo
    );
  end;

begin
  Result := [];
  if (Data = nil) or (Image = nil) or (Image.Width < 1) or (Image.Height < 1) or (Image.Width > AWidth) or (Image.Height > AHeight) then
    Exit;

  SetLength(SliceResults, SimbaMultiprocessing.ThreadsForArea(AWidth, AHeight)); // Cannot exceed this
  SetLength(SliceFound, Length(SliceResults));
  SimbaMultiprocessing.Run(Length(SliceResults), 0, AHeight - Image.Height, @Execute);

  Result := SliceResults.Merge();
  if (MaxToFind > -1) and (Length(Result) > MaxToFind) then
    SetLength(Result, MaxToFind);
end;

function SimbaFinder_FindTemplate(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                  Templ: TSimbaImage; out Match: Single): TPoint;
var
  Mat: TSingleMatrix;
  Best: TPoint;
begin
  Match := 0;
  Result := TPoint.Create(-1, -1);
  if (Data = nil) or (Templ = nil) or (Templ.Width < 1) or (Templ.Height < 1) or (Templ.Width > AWidth) or (Templ.Height > AHeight) then
    Exit;

  Mat := MatchTemplate(SimbaFinder_GetColorsMatrix(Data, PixelsPerRow, AWidth, AHeight), Templ.ToMatrix(), TM_CCOEFF_NORMED);

  Best := Mat.ArgMax;
  Match := Mat[Best.Y, Best.X];
  Result := Best + Offset;
end;

end.

