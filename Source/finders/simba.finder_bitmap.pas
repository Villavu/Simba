{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_bitmap;

{$DEFINE SIMBA_O4}
{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.bitmap;

type
  TFindBitmapBuffer = record
    Ptr: PRGB32;
    PtrInc: Integer;

    X1, Y1, X2, Y2: Integer;

    function Find(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
    function FindAll(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;
  end;

implementation

uses
  simba.colormath, simba.overallocatearray;

function TFindBitmapBuffer.Find(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
var
  BitmapWidth, BitmapHeight: Integer;
  BufferInc: Integer;
  LoopX, LoopY: Integer;

  function Match(const BufferPtr, BitmapPtr: TRGB32): Boolean; inline;
  begin
    Result := (Bitmap.TransparentColorActive and BitmapPtr.EqualsIgnoreAlpha(Bitmap.TransparentRGB)) or
              (RGBDistance(BufferPtr, BitmapPtr) <= Tolerance);
  end;

  function Hit(BufferPtr: PRGB32): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PRGB32;
  begin
    BitmapPtr := Bitmap.Data;

    for Y := 0 to BitmapHeight do
    begin
      for X := 0 to BitmapWidth do
      begin
        if (not Match(BufferPtr^, BitmapPtr^)) then
          Exit(False);

        Inc(BitmapPtr);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferInc);
    end;

    Result := True;
  end;

begin
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit(False);

  Tolerance := Sqr(Tolerance);

  BufferInc := ((X2 - X1) + 1) - Bitmap.Width;
  PtrInc := PtrInc + (Bitmap.Width - 1);

  BitmapWidth := Bitmap.Width - 1;
  BitmapHeight := Bitmap.Height - 1;

  Dec(X2, BitmapWidth);
  Dec(Y2, BitmapHeight);

  for LoopY := Y1 to Y2 do
  begin
    for LoopX := X1 to X2 do
    begin
      if Hit(Ptr) then
      begin
        X := LoopX;
        Y := LoopY;

        Exit(True);
      end;

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  X := 0;
  Y := 0;

  Result := False;
end;

function TFindBitmapBuffer.FindAll(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;
var
  BitmapWidth, BitmapHeight: Integer;
  BufferInc: Integer;
  Matrix: TIntegerMatrix;
  LoopX, LoopY: Integer;

  function Match(const BufferPtr, BitmapPtr: TRGB32): Boolean; inline;
  begin
    Result := (Bitmap.TransparentColorActive and BitmapPtr.EqualsIgnoreAlpha(Bitmap.TransparentRGB)) or
              (RGBDistance(BufferPtr, BitmapPtr) <= Tolerance);
  end;

  function Hit(BufferPtr: PRGB32): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PRGB32;
  begin
    BitmapPtr := Bitmap.Data;

    for Y := 0 to BitmapHeight do
    begin
      for X := 0 to BitmapWidth do
      begin
        if (Matrix[LoopY + Y, LoopX + X] = 1) or (not Match(BufferPtr^, BitmapPtr^)) then
          Exit(False);

        Inc(BitmapPtr);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, BufferInc);
    end;

    Result := True;
  end;

var
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
begin
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit(False);

  PointBuffer.Init(32);
  Tolerance := Sqr(Tolerance);

  Matrix.SetSize((X2-X1)+1, (Y2-Y1)+1);

  BufferInc := ((X2 - X1) + 1) - Bitmap.Width;
  PtrInc := PtrInc + (Bitmap.Width - 1);

  BitmapWidth := Bitmap.Width - 1;
  BitmapHeight := Bitmap.Height - 1;

  Dec(X2, BitmapWidth);
  Dec(Y2, BitmapHeight);

  for LoopY := Y1 to Y2 do
  begin
    for LoopX := X1 to X2 do
    begin
      if Hit(Ptr) then
      begin
        Matrix.Fill(Box(LoopX, LoopY, LoopX + Bitmap.Width, LoopY + Bitmap.Height), 1);

        PointBuffer.Add(TPoint.Create(LoopX, LoopY));
      end;

      Inc(Ptr);
    end;

    Inc(Ptr, PtrInc);
  end;

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

end.

