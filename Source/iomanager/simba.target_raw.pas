{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_raw;

{$i simba.inc}

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

    constructor Create(Source: PRGB32; Width, Height: Int32; Copy: Boolean = False);
    destructor Destroy; override;
 end;

implementation

constructor TRawTarget.Create(Source: PRGB32; Width, Height: Int32; Copy: Boolean);
begin
  inherited Create();

  FWidth := Width;
  FHeight := Height;
  FManageData := Copy;

  if Copy then
  begin
    FData := GetMem(FWidth * FHeight * SizeOf(TRGB32));

    Move(Source^, FData^, FWidth * FHeight * SizeOf(TRGB32));
  end else
    FData := Source;
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
  if ValidateImageCapture(X, Y, Width, Height, Bounds) then
  begin
    Result.Ptr := @FData[Y * Bounds.X2 + X];
    Result.RowLen := Bounds.X2;
    Result.IncPtrWith := Bounds.X2 - Width;
  end else
    Result := Default(TRetData);
end;

function TRawTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Loop: Int32;
begin
  if ValidateImageCapture(X, Y, Width, Height, Bounds) then
  begin
    Result := GetMem(Width * Height * SizeOf(TRGB32));
    for Loop := 0 to Height - 1 do
      Move(FData[(Y + Loop) * Bounds.X2 + X], Result[Loop * Width], Width * SizeOf(TRGB32));
  end else
    Result := nil;
end;

end.

