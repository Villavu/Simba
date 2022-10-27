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
    Data: PRGB32;
    Width: Integer;

    SearchWidth: Integer;
    SearchHeight: Integer;

    function Find(Bitmap: TMufasaBitmap; out Points: TPointArray; MaxToFind: Integer): Boolean; overload;
    function Find(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer; MaxToFind: Integer): Boolean; overload;

    class operator Initialize(var Self: TFindBitmapBuffer);
  end;

implementation

uses
  simba.colormath, simba.overallocatearray;

function TFindBitmapBuffer.Find(Bitmap: TMufasaBitmap; out Points: TPointArray; MaxToFind: Integer): Boolean;

  function Match(const BufferPtr, BitmapPtr: TRGB32): Boolean; inline;
  begin
    Result := (Bitmap.TransparentColorActive and BitmapPtr.EqualsIgnoreAlpha(Bitmap.TransparentRGB)) or (BufferPtr.EqualsIgnoreAlpha(BitmapPtr));
  end;

  function Hit(BufferPtr: PRGB32): Boolean;
  var
    X, Y: Integer;
    BitmapPtr: PRGB32;
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

      Inc(BufferPtr, Self.Width - Bitmap.Width);
    end;

    Result := True;
  end;

var
  X, Y: Integer;
  RowPtr: PRGB32;
  Buffer: TSimbaPointBuffer;
label
  Finished;
begin
  Points := nil;
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit(False);

  Dec(SearchWidth, Bitmap.Width);
  Dec(SearchHeight, Bitmap.Height);

  for Y := 0 to SearchHeight do
  begin
    RowPtr := @Self.Data[Y * Self.Width];

    for X := 0 to SearchWidth do
    begin
      if Hit(RowPtr) then
      begin
        Buffer.Add(X, Y);
        if (Buffer.Count = MaxToFind) then
          goto Finished;
      end;

      Inc(RowPtr);
    end;
  end;

  Finished:

  Points := Buffer.Trim();

  Result := Length(Points) > 0;
end;

function TFindBitmapBuffer.Find(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer; MaxToFind: Integer): Boolean;

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

    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        if (not Match(BufferPtr^, BitmapPtr^)) then
          Exit(False);

        Inc(BitmapPtr);
        Inc(BufferPtr);
      end;

      Inc(BufferPtr, Self.Width - Bitmap.Width);
    end;

    Result := True;
  end;

var
  X, Y: Integer;
  RowPtr: PRGB32;
  Buffer: TSimbaPointBuffer;
label
  Finished;
begin
  Points := nil;
  if (Bitmap = nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit(False);

  Tolerance := Sqr(Tolerance);

  Dec(SearchWidth, Bitmap.Width);
  Dec(SearchHeight, Bitmap.Height);

  for Y := 0 to SearchHeight do
  begin
    RowPtr := @Self.Data[Y * Self.Width];

    for X := 0 to SearchWidth do
    begin
      if Hit(RowPtr) then
      begin
        Buffer.Add(X, Y);
        if (Buffer.Count = MaxToFind) then
          goto Finished;
      end;

      Inc(RowPtr);
    end;
  end;

  Finished:

  Points := Buffer.Trim();

  Result := Length(Points) > 0;
end;

class operator TFindBitmapBuffer.Initialize(var Self: TFindBitmapBuffer);
begin
  Self := Default(TFindBitmapBuffer);
end;

end.

