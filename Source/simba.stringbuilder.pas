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

    procedure EnsureGrowth(const Len: Integer);

    function GetString: String;
    function GetPeek: Char;
  public
    class operator Initialize(var Self: TSimbaStringBuilder);

    procedure AppendBuf(const Buffer; Count: Integer);
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

procedure TSimbaStringBuilder.AppendBuf(const Buffer; Count: Integer);
begin
  if (Count > 0) then
  begin
    EnsureGrowth(Count);

    Move(Buffer, FData[1 + FCount], Count);
    Inc(FCount, Count);
  end;
end;

procedure TSimbaStringBuilder.EnsureGrowth(const Len: Integer);
var
  NewLen: Integer;
begin
  if (FCount + Len >= Length(FData)) then
  begin
    NewLen := Length(FData) + Len;
    if (NewLen < 256) then
      NewLen := 256
    else
    if (NewLen > 256000) then
      NewLen := NewLen * 4
    else
      NewLen := NewLen * 2;

    SetLength(FData, NewLen);
  end;
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
begin
  if (Length(Str) > 0) then
    AppendBuf(Str[1], Length(Str));
end;

procedure TSimbaStringBuilder.AppendLine(const Str: String);
begin
  Append(Str + LineEnding);
end;

end.

