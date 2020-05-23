unit simba.target_raw;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.target;

type
  PRawTarget = ^TRawTarget;
  TRawTarget = class(TTarget)
  protected
    FWidth: Int32;
    FHeight: Int32;
    FData: PRGB32;
    FManageData: Boolean;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    constructor Create(Data: PRGB32; Width, Height: Int32; Copy: Boolean = False);
    destructor Destroy; override;
 end;

implementation

constructor TRawTarget.Create(Data: PRGB32; Width, Height: Int32; Copy: Boolean);
begin
  inherited Create();

  FWidth := Width;
  FHeight := Height;
  FManageData := Copy;

  if Copy then
  begin
    FData := GetMem(FWidth * FHeight * SizeOf(TRGB32));

    Move(Data^, FData^, FWidth * FHeight * SizeOf(TRGB32));
  end else
    FData := Data;
end;

destructor TRawTarget.Destroy;
begin
  if FManageData then
    FreeMem(FData);

  inherited Destroy();
end;

procedure TRawTarget.GetTargetBounds(out Bounds: TBox);
begin
  Bounds.X1 := 0;
  Bounds.Y1 := 0;
  Bounds.X2 := FWidth;
  Bounds.Y2 := FHeight;
end;

function TRawTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    Result.Ptr := @FData[Y * Bounds.X2 + X];
    Result.RowLen := Bounds.X2;
    Result.IncPtrWith := Bounds.X2 - Width;
  end else
    Result := NullReturnData;
end;

function TRawTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
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
      Move(FData[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
  end else
    Result := nil;
end;

end.

