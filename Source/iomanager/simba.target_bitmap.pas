{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_bitmap;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.target, simba.bitmap, simba.mufasatypes;

type
  PBitmapTarget = ^TBitmapTarget;
  TBitmapTarget = class(TTarget)
  protected
    FBitmap: TMufasaBitmap;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    constructor Create(Bitmap: TMufasaBitmap);
    destructor Destroy; override;
  end;

implementation

constructor TBitmapTarget.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  FBitmap := Bitmap;
end;

destructor TBitmapTarget.Destroy;
begin
  inherited Destroy();
end;

procedure TBitmapTarget.GetTargetBounds(out Bounds: TBox);
begin
  Bounds.X1 := 0;
  Bounds.Y1 := 0;
  Bounds.X2 := FBitmap.Width;
  Bounds.Y2 := FBitmap.Height;
end;

function TBitmapTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    Result.Ptr := @FBitmap.Data[Y * Bounds.X2 + X];
    Result.RowLen := Bounds.X2;
    Result.IncPtrWith := Bounds.X2 - Width;
  end else
    Result := NullReturnData;
end;

function TBitmapTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Loop: Int32;
begin
  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    Result := GetMem(Width * Height * SizeOf(TRGB32));
    for Loop := 0 to Height - 1 do
      Move(FBitmap.Data[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
  end else
    Result := nil;
end;

end.

