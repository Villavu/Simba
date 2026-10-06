{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  BoxBlur derived from the Python Imaging Library (Pillow) used under the HPND License:
  Copyright (c) 1997-2011 by Secret Labs AB
  Copyright (c) 1995-2011 by Fredrik Lundh and contributors
  Copyright (c) 2010 by Jeffrey A. Clark and contributors
}
unit simba.image_filters;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image,
  simba.colormath;

// Each pixel replaced by its grey value (Opaque).
function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;

// Value added to each channel clamped to 0..255 (Opaque).
function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;

// bitwise NOT on every channel (Opaque).
function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;

// Edge strength: the 3x3 Sobel gradient of the grey values, as grey (Opaque).
function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;

// Each pixel the Matrix weighted sum of the pixels around it, reading the nearest pixel off the edges. Opaque.
function SimbaImage_Convolute(Image: TSimbaImage; Matrix: TDoubleMatrix): TSimbaImage;

// Box blur of the given radius (Opaque).
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;

// Gaussian blur of the given radius, three box passes. Alpha is dropped.
function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;

// the window's mean, minus C.
function SimbaImage_ThresholdMean(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single = 10): TSimbaImage;

// Wolf-Jolion threshold, K is how hard it pulls.
function SimbaImage_ThresholdWolf(Image: TSimbaImage; Invert: Boolean; Radius: Integer; K: Single = 0.25): TSimbaImage;

// the window's Gaussian weighted mean, minus C.
function SimbaImage_ThresholdGaussian(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single = 10): TSimbaImage;

// Each point in Points replaced by the average colour of the pixels within Radius of it.
// Neither Points no IgnorePoints are ever sampled from.
function SimbaImage_BlendFromSurrounding(Image: TSimbaImage; const Points: TPointArray; Radius: Integer; const IgnorePoints: TPointArray): TSimbaImage;

// In place: pixels within Tolerance of OldColor become NewColor, keeping their alpha.
procedure SimbaImage_ReplaceColor(Image: TSimbaImage; OldColor, NewColor: TColor; Tolerance: Single = 0);

// In place: every pixel becomes white when it is within Tolerance of any of Colors and black when it is not.
procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Colors: TColorArray; Tolerance: Single = 0);

implementation

uses
  Math,
  simba.colormath_distance,
  simba.image_utils,
  simba.vartype_box,
  simba.vartype_matrix,
  simba.vartype_pointarray;

const
  BINARY_COLORS: array[Boolean] of UInt32 = ($FF000000, $FFFFFFFF); // black & white with alpha=255

function SimbaImage_GreyScale(Image: TSimbaImage): TSimbaImage;
var
  Src, Upper, Dest: PColorBGRA;
  Grey: Byte;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if not Image.DataRange(Src, Upper) then
    Exit;

  Dest := Result.Data;
  while (Src <= Upper) do
  begin
    Grey := PixelToGrey(Src^);

    // no alpha here: Result came from Create, which is opaque
    Dest^.R := Grey;
    Dest^.G := Grey;
    Dest^.B := Grey;

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Brightness(Image: TSimbaImage; Value: Integer): TSimbaImage;
var
  Src, Upper, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if not Image.DataRange(Src, Upper) then
    Exit;

  Dest := Result.Data;
  while (Src <= Upper) do
  begin
    Dest^.R := EnsureRange(Src^.R + Value, 0, 255);
    Dest^.G := EnsureRange(Src^.G + Value, 0, 255);
    Dest^.B := EnsureRange(Src^.B + Value, 0, 255);

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Invert(Image: TSimbaImage): TSimbaImage;
var
  Src, Upper, Dest: PColorBGRA;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if not Image.DataRange(Src, Upper) then
    Exit;

  Dest := Result.Data;
  while (Src <= Upper) do
  begin
    PUInt32(Dest)^ := (not PUInt32(Src)^) or $FF000000; // untouch alpha

    Inc(Src);
    Inc(Dest);
  end;
end;

function SimbaImage_Sobel(Image: TSimbaImage): TSimbaImage;
var
  X, Y, Width, Height, LeftCol, RightCol, TopRow, BottomRow: Integer;
  Grey: TByteArray;
  Src, Upper, Dest: PColorBGRA;
  Above, Middle, Below: PByte;
  Magnitude: Byte;
begin
  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if not Image.DataRange(Src, Upper) then
    Exit;

  Width := Image.Width;
  Height := Image.Height;

  SetLength(Grey, Image.PixelCount);
  GreyData(Image.Data, @Grey[0], Image.PixelCount);

  for Y := 1 to Height - 2 do
  begin
    Above := @Grey[(Y - 1) * Width];
    Middle := @Grey[Y * Width];
    Below := @Grey[(Y + 1) * Width];
    Dest := Result.PixelPtr[1, Y];

    // Difference across the window, weighted towards its middle:
    //   RightCol - LeftCol = [ -1 0 +1 ]   BottomRow - TopRow = [ -1 -2 -1 ]
    //                        [ -2 0 +2 ]                        [  0  0  0 ]
    //                        [ -1 0 +1 ]                        [ +1 +2 +1 ]
    for X := 1 to Width - 2 do
    begin
      LeftCol   := Above[X - 1] + 2 * Middle[X - 1] + Below[X - 1];
      RightCol  := Above[X + 1] + 2 * Middle[X + 1] + Below[X + 1];
      TopRow    := Above[X - 1] + 2 * Above[X]      + Above[X + 1];
      BottomRow := Below[X - 1] + 2 * Below[X]      + Below[X + 1];
      Magnitude := EnsureRange(Trunc(Sqrt(Sqr(RightCol - LeftCol) + Sqr(BottomRow - TopRow))), 0, 255);

      Dest^.B := Magnitude;
      Dest^.G := Magnitude;
      Dest^.R := Magnitude;

      Inc(Dest);
    end;
  end;
end;

function SimbaImage_Convolute(Image: TSimbaImage; Matrix: TDoubleMatrix): TSimbaImage;
var
  X, Y, MatY, MatX: Integer;
  MatWidth, MatHeight, MatMidX, MatMidY, Width, Height: Integer;
  SumR, SumG, SumB, Weight: Double;
  SrcRow, Dest: PColorBGRA;
  MatRow: PDouble;
begin
  if not Matrix.GetSize(MatWidth, MatHeight) then
    SimbaException('TImage.Convolute: The matrix is empty');

  Width := Image.Width;
  Height := Image.Height;
  Result := TSimbaImage.Create(Width, Height);

  MatMidX := MatWidth div 2;
  MatMidY := MatHeight div 2;

  Dest := Result.Data;
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      SumR := 0;
      SumG := 0;
      SumB := 0;

      for MatY := 0 to MatHeight - 1 do
      begin
        MatRow := @Matrix[MatY, 0];
        SrcRow := Image.Data + Int64(EnsureRange(Y + MatY - MatMidY, 0, Height - 1) * Width);

        for MatX := 0 to MatWidth - 1 do
        begin
          Weight := MatRow[MatX];

          with SrcRow[EnsureRange(X + MatX - MatMidX, 0, Width - 1)] do
          begin
            SumR += Weight * R;
            SumG += Weight * G;
            SumB += Weight * B;
          end;
        end;
      end;

      with Dest^ do
      begin
        R := EnsureRange(Round(SumR), 0, 255);
        G := EnsureRange(Round(SumG), 0, 255);
        B := EnsureRange(Round(SumB), 0, 255);
      end;

      Inc(Dest);
    end;
end;

// Box blur Pillow style (BoxBlur.c)
function SimbaImage_BlurBox(Image: TSimbaImage; Radius: Single): TSimbaImage;

  // One row. The window is carried along as a running sum, so each pixel costs one add and one subtract:
  // the weights split it into the whole pixels inside the window and the two fractional ones just outside.
  procedure LineBoxBlur(Dest, Source: PByte; LastX, WindowRadius: Integer; InsideWeight, OutsideWeight: UInt32);
  var
    X, CoveredPixels: Integer;
    SumB, SumG, SumR: UInt32;
    Entering, Leaving, Outside, LastPixel, EnteringEnd: PByte;
  begin
    LastPixel := Source + LastX * 4;

    // the window one step before the row starts: the first pixel repeated off the left, then the row up to it
    SumB := Source[0] * UInt32(WindowRadius + 1);
    SumG := Source[1] * UInt32(WindowRadius + 1);
    SumR := Source[2] * UInt32(WindowRadius + 1);

    CoveredPixels := Min(WindowRadius, LastX + 1); // a window wider than the row is the last pixel repeated off the right
    Entering := Source;
    EnteringEnd := Source + CoveredPixels * 4;
    while (Entering < EnteringEnd) do
    begin
      SumB += Entering[0];
      SumG += Entering[1];
      SumR += Entering[2];
      Inc(Entering, 4);
    end;
    SumB += LastPixel[0] * UInt32(WindowRadius - CoveredPixels);
    SumG += LastPixel[1] * UInt32(WindowRadius - CoveredPixels);
    SumR += LastPixel[2] * UInt32(WindowRadius - CoveredPixels);

    Entering := Source + Min(WindowRadius, LastX) * 4;     // entering the window
    Leaving := Source;                                     // leaving it, and the outside pixel on the left
    Outside := Source + Min(WindowRadius + 1, LastX) * 4;  // the outside pixel on the right
    for X := 0 to LastX do
    begin
      SumB := SumB + Entering[0] - Leaving[0];
      SumG := SumG + Entering[1] - Leaving[1];
      SumR := SumR + Entering[2] - Leaving[2];
      // each of these is Round(Sum / Size)
      Dest[0] := UInt32(SumB * InsideWeight + (Leaving[0] + Outside[0]) * OutsideWeight + (1 shl 23)) shr 24;
      Dest[1] := UInt32(SumG * InsideWeight + (Leaving[1] + Outside[1]) * OutsideWeight + (1 shl 23)) shr 24;
      Dest[2] := UInt32(SumR * InsideWeight + (Leaving[2] + Outside[2]) * OutsideWeight + (1 shl 23)) shr 24;

      if (Entering < LastPixel) then
        Inc(Entering, 4);
      if (Outside < LastPixel) then
        Inc(Outside, 4);
      if (X > WindowRadius) then
        Inc(Leaving, 4);
      Inc(Dest, 4);
    end;
  end;

  // LineBoxBlur down every row. The vertical pass is this again, on the transposed image.
  procedure HorizBoxBlur(Source, Dest: PColorBGRA; Width, Height, WindowRadius: Integer; InsideWeight, OutsideWeight: UInt32);
  begin
    while (Height > 0) do
    begin
      LineBoxBlur(PByte(Dest), PByte(Source), Width - 1, WindowRadius, InsideWeight, OutsideWeight);

      Inc(Source, Width);
      Inc(Dest, Width);
      Dec(Height);
    end;
  end;

  // Cache-blocked (tiled) transpose
  procedure Transpose(Source, Dest: PColorBGRA; Width, Height: Integer);
  const
    BLOCK = 8;
  var
    X, Y, XEnd, YEnd: Integer;
    SourceRow, SourceRowEnd, DestColumn, SourcePixel, SourcePixelEnd, DestPixel: PColorBGRA;
  begin
    Y := 0;
    while (Y < Height) do
    begin
      YEnd := Y + BLOCK;
      if (YEnd > Height) then
        YEnd := Height;

      X := 0;
      while (X < Width) do
      begin
        XEnd := X + BLOCK;
        if (XEnd > Width) then
          XEnd := Width;

        SourceRow := @Source[Y * Width + X];
        SourceRowEnd := @Source[YEnd * Width + X];
        DestColumn := @Dest[X * Height + Y];
        while (PtrUInt(SourceRow) < PtrUInt(SourceRowEnd)) do
        begin
          SourcePixel := SourceRow;
          DestPixel := DestColumn;
          SourcePixelEnd := @SourceRow[XEnd - X];
          while (PtrUInt(SourcePixel) < PtrUInt(SourcePixelEnd)) do
          begin
            PUInt32(DestPixel)^ := PUInt32(SourcePixel)^ or $FF000000; // leave alpha alone
            Inc(SourcePixel);
            Inc(DestPixel, Height);
          end;
          Inc(SourceRow, Width);
          Inc(DestColumn);
        end;

        X := X + BLOCK;
      end;

      Y := Y + BLOCK;
    end;
  end;

var
  Width, Height, WindowRadius: Integer;
  InsideWeight, OutsideWeight: UInt32;
  Scratch: array of TColorBGRA;
begin
  if (Radius < 0) then
    SimbaException('Blur radius must be >= 0');

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Width := Image.Width;
  Height := Image.Height;
  WindowRadius := Trunc(Radius);
  InsideWeight := Trunc((1 shl 24) / (2 * Radius + 1));
  OutsideWeight := ((1 shl 24) - (2 * WindowRadius + 1) * InsideWeight) div 2;

  SetLength(Scratch, Width * Height);
  HorizBoxBlur(Image.Data,  @Scratch[0], Width, Height, WindowRadius, InsideWeight, OutsideWeight); // horizontal
  Transpose(@Scratch[0],    Result.Data, Width, Height);                                            // -> Height x Width
  HorizBoxBlur(Result.Data, @Scratch[0], Height, Width, WindowRadius, InsideWeight, OutsideWeight); // horizontal on transposed (= vertical)
  Transpose(@Scratch[0],    Result.Data, Height, Width);                                            // -> Width x Height
end;

// 3-box approximation of a Gaussian blur (Ivan Kutski @ https://blog.ivank.net/fastest-gaussian-blur.html)
procedure GaussBlurApprox(var Data: TByteArray; Width, Height: Integer; Sigma: Single);

  // A row at a time, keeping the window's running sum. Where the window hangs off an end it repeats that
  // end's value, which is why the row is walked in three parts.
  procedure BlurRows(const Source: TByteArray; var Dest: TByteArray; Width, Height, Radius: Integer);
  var
    Row, Offset, FirstValue, LastValue, Sum, Recip: Integer;
    RowStart, Leaving, Entering, DestPtr, SegmentEnd: PByte;
  begin
    if (Radius > (Width - 1) div 2) then
      Radius := (Width - 1) div 2;
    if (Radius > 8224) then
      Radius := 8224;
    // each store below is (Sum + Size div 2) div Size, for a window Size of 2 * Radius + 1
    Recip := ((1 shl 22) + Radius) div (2 * Radius + 1); // + Radius rounds it; 22 bits is all Int32 holds

    for Row := 0 to Height - 1 do
    begin
      RowStart   := @Source[Row * Width];
      DestPtr    := @Dest[Row * Width];
      FirstValue := RowStart^;
      LastValue  := (RowStart + Width - 1)^;
      Leaving    := RowStart;
      Entering   := RowStart + Radius;

      Sum := (Radius + 1) * FirstValue;
      for Offset := 0 to Radius - 1 do
        Sum := Sum + (RowStart + Offset)^;

      SegmentEnd := DestPtr + (Radius + 1);              // left edge: window hangs off the start
      while (DestPtr < SegmentEnd) do
      begin
        Sum := Sum + Entering^ - FirstValue;
        DestPtr^ := (Sum * Recip + (1 shl 21)) shr 22;

        Inc(Entering);
        Inc(DestPtr);
      end;

      SegmentEnd := DestPtr + (Width - 2 * Radius - 1);  // window fully inside the row
      while (DestPtr < SegmentEnd) do
      begin
        Sum := Sum + Entering^ - Leaving^;
        DestPtr^ := (Sum * Recip + (1 shl 21)) shr 22;

        Inc(Leaving);
        Inc(Entering);
        Inc(DestPtr);
      end;

      SegmentEnd := DestPtr + Radius;                    // right edge: window hangs off the end
      while (DestPtr < SegmentEnd) do
      begin
        Sum := Sum + LastValue - Leaving^;
        DestPtr^ := (Sum * Recip + (1 shl 21)) shr 22;

        Inc(Leaving);
        Inc(DestPtr);
      end;
    end;
  end;

  // A row at a time, keeping every column's running sum
  procedure BlurCols(const Source: TByteArray; var Dest: TByteArray; Width, Height, Radius: Integer);
  var
    Sums: TIntegerArray;
    Row, Offset, Recip: Integer;
    SrcRow, EnteringRow, LeavingRow, DestRow: PByte;
    Sum, SumEnd: PInteger;
  begin
    if (Radius > (Height - 1) div 2) then
      Radius := (Height - 1) div 2;
    if (Radius > 8224) then
      Radius := 8224;
    // each store below is (Sum + Size div 2) div Size, for a window Size of 2 * Radius + 1
    Recip := ((1 shl 22) + Radius) div (2 * Radius + 1); // + Radius rounds it

    SetLength(Sums, Width);
    Sum := @Sums[0];
    SumEnd := Sum + Width;

    SrcRow := @Source[0];
    while (Sum < SumEnd) do
    begin
      Sum^ := (Radius + 1) * SrcRow^;
      Inc(Sum);
      Inc(SrcRow);
    end;
    for Offset := 0 to Radius - 1 do
    begin
      Sum := @Sums[0];
      SrcRow := @Source[Offset * Width];
      while (Sum < SumEnd) do
      begin
        Sum^ := Sum^ + SrcRow^;
        Inc(Sum);
        Inc(SrcRow);
      end;
    end;

    for Row := 0 to Height - 1 do
    begin
      if (Row + Radius <= Height - 1) then
        EnteringRow := @Source[(Row + Radius) * Width]
      else
        EnteringRow := @Source[(Height - 1) * Width];
      if (Row <= Radius) then
        LeavingRow := @Source[0]
      else
        LeavingRow := @Source[(Row - Radius - 1) * Width];
      DestRow := @Dest[Row * Width];

      Sum := @Sums[0];
      while (Sum < SumEnd) do
      begin
        Sum^ := Sum^ + EnteringRow^ - LeavingRow^;
        DestRow^ := (Sum^ * Recip + (1 shl 21)) shr 22;

        Inc(Sum);
        Inc(EnteringRow);
        Inc(LeavingRow);
        Inc(DestRow);
      end;
    end;
  end;

  function BoxesForGauss(Sigma: Double): TIntegerArray;
  var
    NarrowWidth, NarrowCount: Integer;
    WideVariance, VariancePerBox: Int64;
    WantedVariance: Double;
  begin
    NarrowWidth := Floor(Sqrt(4 * Sigma * Sigma + 1)); // ideal box width for 3 boxes, forced odd
    if (NarrowWidth mod 2 = 0) then
      Dec(NarrowWidth);

    WantedVariance := 12 * Sigma * Sigma;
    WideVariance   := Int64(3 * (NarrowWidth + 1) * (NarrowWidth + 3));
    VariancePerBox := 4 * (NarrowWidth + 1);
    NarrowCount    := Round((WideVariance - WantedVariance) / VariancePerBox);

    Result := [
      IfThen(NarrowCount > 0, NarrowWidth, NarrowWidth + 2),
      IfThen(NarrowCount > 1, NarrowWidth, NarrowWidth + 2),
      IfThen(NarrowCount > 2, NarrowWidth, NarrowWidth + 2)
    ];
  end;

var
  Scratch: TByteArray;
  Boxes: TIntegerArray;
  I: Integer;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;

  SetLength(Scratch, Width * Height);
  if (Sigma > Max(Width, Height)) then
    Sigma := Max(Width, Height);
  Boxes := BoxesForGauss(Sigma);

  for I := 0 to 2 do
  begin
    BlurRows(Data, Scratch, Width, Height, (Boxes[I] - 1) div 2);
    BlurCols(Scratch, Data, Width, Height, (Boxes[I] - 1) div 2);
  end;
end;

function SimbaImage_BlurGauss(Image: TSimbaImage; Radius: Single): TSimbaImage;
var
  Blue, Green, Red: TByteArray;
begin
  if (Radius < 0) then
    SimbaException('Blur radius must be >= 0');

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Image.ToChannels(Blue, Green, Red);

  GaussBlurApprox(Red, Image.Width, Image.Height, Radius);
  GaussBlurApprox(Green, Image.Width, Image.Height, Radius);
  GaussBlurApprox(Blue, Image.Width, Image.Height, Radius);

  Result.FromChannels(Blue, Green, Red, Result.Width, Result.Height);
end;

type
  // a column's window: its left and right columns in the integral image, and its width
  TThresholdColumn = record
    Left, Right: Integer;
    Width: Double;
  end;

function SimbaImage_ThresholdMean(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single): TSimbaImage;
var
  Width, Height, Stride, X, Y, Top, Bottom: Integer;
  Src, Dest, DestEnd: PColorBGRA;
  Sums: TDoubleArray;
  SumRow, SumTop, SumBottom, SmoothTop, SmoothBottom: PDouble;
  Columns, SmoothColumns: array of TThresholdColumn;
  Column, SmoothColumn: ^TThresholdColumn;
  RowSum, WindowRows, SmoothRows, Threshold, Smoothed: Double;
begin
  if (Radius < 1) then
    SimbaException('TImage.Threshold: Radius(%d) must be at least 1', [Radius]);

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Width := Image.Width;
  Height := Image.Height;
  Stride := Width + 1;
  Radius := Min(Radius, Max(Width, Height));

  SetLength(Sums, Stride * (Height + 1));
  for Y := 0 to Height - 1 do
  begin
    Src := Image.PixelPtr[0, Y];
    SumRow := @Sums[(Y + 1) * Stride + 1];
    RowSum := 0;
    for X := 0 to Width - 1 do
    begin
      RowSum := RowSum + PixelToGrey(Src[X]);
      SumRow[X] := SumRow[X - Stride] + RowSum;
    end;
  end;

  // every column's window edges and width, the same for every row
  SetLength(Columns, Width);
  SetLength(SmoothColumns, Width);
  for X := 0 to Width - 1 do
  begin
    Columns[X].Left := Max(X - Radius, 0);
    Columns[X].Right := Min(X + Radius, Width - 1) + 1;
    Columns[X].Width := Columns[X].Right - Columns[X].Left;
    SmoothColumns[X].Left := Max(X - 1, 0);
    SmoothColumns[X].Right := Min(X + 1, Width - 1) + 1;
    SmoothColumns[X].Width := SmoothColumns[X].Right - SmoothColumns[X].Left;
  end;

  for Y := 0 to Height - 1 do
  begin
    Top := Max(Y - Radius, 0);
    Bottom := Min(Y + Radius, Height - 1) + 1;
    WindowRows := Bottom - Top;
    SumTop := @Sums[Top * Stride];
    SumBottom := @Sums[Bottom * Stride];

    // the pixel's own 3x3 rows
    Top := Max(Y - 1, 0);
    Bottom := Min(Y + 1, Height - 1) + 1;
    SmoothRows := Bottom - Top;
    SmoothTop := @Sums[Top * Stride];
    SmoothBottom := @Sums[Bottom * Stride];

    Dest := Result.PixelPtr[0, Y];
    DestEnd := Dest + Width;
    Column := @Columns[0];
    SmoothColumn := @SmoothColumns[0];
    while (Dest < DestEnd) do
    begin
      Threshold := (SumBottom[Column^.Right] - SumTop[Column^.Right] - SumBottom[Column^.Left] + SumTop[Column^.Left]) / (WindowRows * Column^.Width) - C;
      Smoothed := (SmoothBottom[SmoothColumn^.Right] - SmoothTop[SmoothColumn^.Right] - SmoothBottom[SmoothColumn^.Left] + SmoothTop[SmoothColumn^.Left]) / (SmoothRows * SmoothColumn^.Width);

      if Invert then
        Dest^.AsInteger := BINARY_COLORS[Smoothed <= Threshold]
      else
        Dest^.AsInteger := BINARY_COLORS[Smoothed >= Threshold];

      Inc(Dest);
      Inc(Column);
      Inc(SmoothColumn);
    end;
  end;
end;

// Wolf-Jolion: mean - K * (1 - deviation / the image's largest deviation) * (mean - the darkest grey)
function SimbaImage_ThresholdWolf(Image: TSimbaImage; Invert: Boolean; Radius: Integer; K: Single): TSimbaImage;
var
  Width, Height, Stride, X, Y, Top, Bottom: Integer;
  MinGrey: Byte;
  Src, Dest, DestEnd: PColorBGRA;
  Grey: TByteArray;
  GreyRow: PByte;
  Sums, SqrSums: TDoubleArray;
  SumRow, SqrSumRow, SumTop, SumBottom, SqrSumTop, SqrSumBottom: PDouble;
  Columns: array of TThresholdColumn;
  Column, ColumnEnd: ^TThresholdColumn;
  RowSum, SqrRowSum, WindowRows, WindowPixels, Mean, Variance, Deviation, MaxVariance, InvMaxDeviation, Threshold: Double;
begin
  if (Radius < 1) then
    SimbaException('TImage.Threshold: Radius(%d) must be at least 1', [Radius]);

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if (Result.Width = 0) or (Result.Height = 0) then
    Exit;

  Width := Image.Width;
  Height := Image.Height;
  Stride := Width + 1;
  Radius := Min(Radius, Max(Width, Height));

  SetLength(Grey, Width * Height);
  SetLength(Sums, Stride * (Height + 1));
  SetLength(SqrSums, Stride * (Height + 1));
  MinGrey := 255;
  for Y := 0 to Height - 1 do
  begin
    Src := Image.PixelPtr[0, Y];
    GreyRow := @Grey[Y * Width];
    SumRow := @Sums[(Y + 1) * Stride + 1];
    SqrSumRow := @SqrSums[(Y + 1) * Stride + 1];
    RowSum := 0;
    SqrRowSum := 0;
    for X := 0 to Width - 1 do
    begin
      GreyRow[X] := PixelToGrey(Src[X]);
      MinGrey := Min(MinGrey, GreyRow[X]);
      RowSum := RowSum + GreyRow[X];
      SqrRowSum := SqrRowSum + Sqr(Int32(GreyRow[X]));
      SumRow[X] := SumRow[X - Stride] + RowSum;
      SqrSumRow[X] := SqrSumRow[X - Stride] + SqrRowSum;
    end;
  end;

  SetLength(Columns, Width);
  for X := 0 to Width - 1 do
  begin
    Columns[X].Left := Max(X - Radius, 0);
    Columns[X].Right := Min(X + Radius, Width - 1) + 1;
    Columns[X].Width := Columns[X].Right - Columns[X].Left;
  end;

  // the threshold scales by the largest window deviation in the image, so measure that first
  MaxVariance := 0;
  for Y := 0 to Height - 1 do
  begin
    Top := Max(Y - Radius, 0);
    Bottom := Min(Y + Radius, Height - 1) + 1;
    WindowRows := Bottom - Top;
    SumTop := @Sums[Top * Stride];
    SumBottom := @Sums[Bottom * Stride];
    SqrSumTop := @SqrSums[Top * Stride];
    SqrSumBottom := @SqrSums[Bottom * Stride];

    Column := @Columns[0];
    ColumnEnd := Column + Width;
    while (Column < ColumnEnd) do
    begin
      WindowPixels := WindowRows * Column^.Width;
      Mean := (SumBottom[Column^.Right] - SumTop[Column^.Right] - SumBottom[Column^.Left] + SumTop[Column^.Left]) / WindowPixels;
      Variance := (SqrSumBottom[Column^.Right] - SqrSumTop[Column^.Right] - SqrSumBottom[Column^.Left] + SqrSumTop[Column^.Left])
                  / WindowPixels - Sqr(Mean);
      MaxVariance := Max(MaxVariance, Variance);

      Inc(Column);
    end;
  end;
  InvMaxDeviation := 0;
  if (MaxVariance > 0) then
    InvMaxDeviation := 1 / Sqrt(MaxVariance);

  for Y := 0 to Height - 1 do
  begin
    Top := Max(Y - Radius, 0);
    Bottom := Min(Y + Radius, Height - 1) + 1;
    WindowRows := Bottom - Top;
    SumTop := @Sums[Top * Stride];
    SumBottom := @Sums[Bottom * Stride];
    SqrSumTop := @SqrSums[Top * Stride];
    SqrSumBottom := @SqrSums[Bottom * Stride];

    Dest := Result.PixelPtr[0, Y];
    DestEnd := Dest + Width;
    GreyRow := @Grey[Y * Width];
    Column := @Columns[0];
    while (Dest < DestEnd) do
    begin
      WindowPixels := WindowRows * Column^.Width;
      Mean := (SumBottom[Column^.Right] - SumTop[Column^.Right] - SumBottom[Column^.Left] + SumTop[Column^.Left]) / WindowPixels;
      Variance := (SqrSumBottom[Column^.Right] - SqrSumTop[Column^.Right] - SqrSumBottom[Column^.Left] + SqrSumTop[Column^.Left])
                  / WindowPixels - Sqr(Mean);
      if (Variance > 0) then
        Deviation := Sqrt(Variance)
      else
        Deviation := 0;
      Threshold := Mean - K * (1 - Deviation * InvMaxDeviation) * (Mean - MinGrey);

      if Invert then
        Dest^.AsInteger := BINARY_COLORS[GreyRow^ <= Threshold]
      else
        Dest^.AsInteger := BINARY_COLORS[GreyRow^ >= Threshold];

      Inc(Dest);
      Inc(GreyRow);
      Inc(Column);
    end;
  end;
end;

// The threshold is the Gaussian blur of the grey values minus C
function SimbaImage_ThresholdGaussian(Image: TSimbaImage; Invert: Boolean; Radius: Integer; C: Single): TSimbaImage;
var
  MinDifference: Integer;
  Src, Upper, Dest: PColorBGRA;
  Grey, Blurred: TByteArray;
  GreyPtr, GreyEnd, BlurPtr: PByte;
begin
  if (Radius < 1) then
    SimbaException('TImage.Threshold: Radius(%d) must be at least 1', [Radius]);

  Result := TSimbaImage.Create(Image.Width, Image.Height);
  if not Image.DataRange(Src, Upper) then
    Exit;

  SetLength(Grey, Image.PixelCount);
  GreyData(Image.Data, @Grey[0], Length(Grey));

  Blurred := Copy(Grey);
  GaussBlurApprox(Blurred, Image.Width, Image.Height, 0.3 * Radius + 0.5); // OpenCV's sigma for a (2 * Radius + 1) window

  if Invert then
    MinDifference := Floor(EnsureRange(-C, -256, 256)) + 1
  else
    MinDifference := Ceil(EnsureRange(-C, -256, 256));

  Dest := Result.Data;
  GreyPtr := @Grey[0];
  GreyEnd := GreyPtr + Length(Grey);
  BlurPtr := @Blurred[0];
  while (GreyPtr < GreyEnd) do
  begin
    Dest^.AsInteger := BINARY_COLORS[(Integer(GreyPtr^) - BlurPtr^ >= MinDifference) xor Invert];

    Inc(GreyPtr);
    Inc(BlurPtr);
    Inc(Dest);
  end;
end;

procedure SimbaImage_ReplaceColor(Image: TSimbaImage; OldColor, NewColor: TColor; Tolerance: Single);
var
  OldBGRA: TColorBGRA;
  NewRGB: UInt32;
  Ptr, Upper: PColorBGRA;
begin
  if not Image.DataRange(Ptr, Upper) then
    Exit;

  OldBGRA := OldColor.ToBGRA();
  NewRGB := NewColor.ToBGRA().AsInteger and $00FFFFFF;
  while (Ptr <= Upper) do
  begin
    if SimilarRGB(OldBGRA, Ptr^, Tolerance) then
      Ptr^.AsInteger := NewRGB or (Ptr^.AsInteger and $FF000000);

    Inc(Ptr);
  end;
end;

procedure SimbaImage_ReplaceColorBinary(Image: TSimbaImage; Invert: Boolean; Colors: TColorArray; Tolerance: Single);
label
  Next;
var
  SearchColors: array of TColorBGRA;
  I: Integer;
  Ptr, Upper: PColorBGRA;
  Hit, Miss: UInt32;
begin
  if not Image.DataRange(Ptr, Upper) then
    Exit;

  Hit := BINARY_COLORS[not Invert] and $00FFFFFF; // mask out alpha changes
  Miss := BINARY_COLORS[Invert] and $00FFFFFF;

  SetLength(SearchColors, Length(Colors));
  for I := 0 to High(Colors) do
    SearchColors[I] := Colors[I].ToBGRA();

  while (Ptr <= Upper) do
  begin
    for I := 0 to High(SearchColors) do
      if SimilarRGB(SearchColors[I], Ptr^, Tolerance) then
      begin
        Ptr^.AsInteger := Hit or (Ptr^.AsInteger and $FF000000);
        goto Next;
      end;
    Ptr^.AsInteger := Miss or (Ptr^.AsInteger and $FF000000);
    Next:
    Inc(Ptr);
  end;
end;

function SimbaImage_BlendFromSurrounding(Image: TSimbaImage; const Points: TPointArray; Radius: Integer; const IgnorePoints: TPointArray): TSimbaImage;
var
  P: TPoint;
  Cols, Rows, Count, Width, Height, MaskWidth, WindowWidth: Integer;
  ImageBox, Window, MaskBox: TBox;
  SumR, SumG, SumB: UInt64;
  Skip: TBooleanArray;
  SrcData, SrcPtr: PColorBGRA;
  SkipPtr: PBoolean;
begin
  Result := Image.Copy();
  Result.Canvas.FillWithAlpha(ALPHA_OPAQUE);

  Width := Image.Width;
  Height := Image.Height;
  SrcData := Image.Data;
  if (Length(Points) = 0) or (Width < 1) or (Height < 1) then
    Exit;
  Radius := Min(Max(Radius, 0), Max(Width, Height));

  ImageBox := TBox.Create(0, 0, Width - 1, Height - 1);
  MaskBox := Points.Bounds;
  if (MaskBox.X2 < 0) or (MaskBox.Y2 < 0) or (MaskBox.X1 >= Width) or (MaskBox.Y1 >= Height) then
    Exit;

  MaskBox := MaskBox.Clip(ImageBox).Expand(Radius, ImageBox);
  MaskWidth := MaskBox.Width;
  SetLength(Skip, Int64(MaskWidth * MaskBox.Height));
  for P in IgnorePoints do
    if MaskBox.Contains(P) then
      Skip[(P.Y - MaskBox.Y1) * MaskWidth + (P.X - MaskBox.X1)] := True;
  for P in Points do
    if MaskBox.Contains(P) then
      Skip[(P.Y - MaskBox.Y1) * MaskWidth + (P.X - MaskBox.X1)] := True;

  for P in Points do
    if ImageBox.Contains(P) then
    begin
      Window.X1 := Max(P.X - Radius, 0);
      Window.Y1 := Max(P.Y - Radius, 0);
      Window.X2 := Min(P.X + Radius, Width - 1);
      Window.Y2 := Min(P.Y + Radius, Height - 1);

      Count := 0;
      SumR := 0;
      SumG := 0;
      SumB := 0;

      WindowWidth := Window.Width;
      SrcPtr := @SrcData[Int64(Window.Y1 * Width + Window.X1)];
      SkipPtr := @Skip[Int64((Window.Y1 - MaskBox.Y1) * MaskWidth + (Window.X1 - MaskBox.X1))];
      Rows := Window.Height;
      while (Rows > 0) do
      begin
        Cols := WindowWidth;
        while (Cols > 0) do
        begin
          if not SkipPtr^ then
          begin
            Inc(SumR, SrcPtr^.R);
            Inc(SumG, SrcPtr^.G);
            Inc(SumB, SrcPtr^.B);
            Inc(Count);
          end;

          Inc(SkipPtr);
          Inc(SrcPtr);
          Dec(Cols);
        end;

        Inc(SrcPtr, Width - WindowWidth);
        Inc(SkipPtr, MaskWidth - WindowWidth);
        Dec(Rows);
      end;

      if (Count > 0) then
        with Result.PixelPtr[P.X, P.Y]^ do
        begin
          R := (SumR + Count div 2) div Count;
          G := (SumG + Count div 2) div Count;
          B := (SumB + Count div 2) div Count;
        end;
    end;
end;

end.

