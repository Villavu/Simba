{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matrix_bool;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TBooleanMatrixHelper = type helper for TBooleanMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

implementation

function TBooleanMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TBooleanMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TBooleanMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TBooleanMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TBooleanMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

end.

