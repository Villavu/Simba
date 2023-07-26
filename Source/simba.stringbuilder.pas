{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed string builder.
}
{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
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
    function GetPeek: Char;
  public
    class operator Initialize(var Self: TSimbaStringBuilder);

    procedure Append(const Str: String);
    procedure AppendLine(const Str: String = '');
    property Str: String read GetString;
    property Count: Integer read FCount;
    property Peek: Char read GetPeek;
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

function TSimbaStringBuilder.GetPeek: Char;
begin
  if (FCount = 0) then
    Exit(#0);
  Result := FData[FCount + 1];
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

procedure TSimbaStringBuilder.AppendLine(const Str: String);
begin
  Append(Str + LineEnding);
end;

end.

