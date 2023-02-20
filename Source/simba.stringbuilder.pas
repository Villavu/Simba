{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed string builder.
}
unit simba.stringbuilder;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  TSimbaStringBuilder = record
  private
    FData: String;
    FCount: Integer;

    function GetString: String;
  public
    class operator Initialize(var Self: TSimbaStringBuilder);

    procedure Append(const Str: String);
    property Str: String read GetString;
  end;

implementation

class operator TSimbaStringBuilder.Initialize(var Self: TSimbaStringBuilder);
begin
  Self := Default(TSimbaStringBuilder);
end;

function TSimbaStringBuilder.GetString: String;
begin
  Result := Copy(FData, 1, FCount);
end;

procedure TSimbaStringBuilder.Append(const Str: String);
var
  Len: Integer;
begin
  Len := Length(Str);

  if (Len > 0) then
  begin
    if (FCount + Len >= Length(FData)) then
      SetLength(FData, (Length(FData) * 2) + 1024 + Len);
    Move(Str[1], FData[1 + FCount], Len * SizeOf(Char));

    Inc(FCount, Len);
  end;
end;

end.

