unit simba.target_raw;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  mufasatypes, simba.target;

type
  PRawTarget = ^TRawTarget;
  TRawTarget = class(TTarget)
  protected
    FWidth: Int32;
    FHeight: Int32;
    FData: PRGB32;
    FManageData: Boolean;
  public
    procedure GetTargetDimensions(out Width, Height: Int32); override;

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

procedure TRawTarget.GetTargetDimensions(out Width, Height: Int32);
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.X2 - FImageClientArea.X1 + 1;
    Height := FImageClientArea.Y2 - FImageClientArea.Y1 + 1;
  end else
  begin
    Width := FWidth;
    Height := FHeight;
  end;
end;

function TRawTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    Result.Ptr := @FData[Y * Bounds.X1 + X];
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
  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    Result := GetMem(Width * Height * SizeOf(TRGB32));
    for Loop := 0 to Height - 1 do
      Move(FData[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
  end else
    Result := nil;
end;

end.

