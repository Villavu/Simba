unit simba.target_bitmap;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.target, bitmaps, mufasatypes;

type
  PBitmapTarget = ^TBitmapTarget;
  TBitmapTarget = class(TTarget)
  protected
    FWidth: Int32;
    FHeight: Int32;
    FBitmap: TMufasaBitmap;
  public
    procedure GetTargetDimensions(out Width, Height: Int32); override;

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

procedure TBitmapTarget.GetTargetDimensions(out Width, Height: Int32);
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.X2 - FImageClientArea.X1;
    Height := FImageClientArea.Y2 - FImageClientArea.Y1;
  end else
  begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
  end;
end;

function TBitmapTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    Result.Ptr := @FBitmap.FData[Y * Bounds.X1 + X];
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
  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    Result := GetMem(Width * Height * SizeOf(TRGB32));
    for Loop := 0 to Height - 1 do
      Move(FBitmap.FData[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
  end else
    Result := nil;
end;

end.

