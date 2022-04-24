{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.datetime;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

procedure ConvertTime(Time: Int64; var h, m, s: Int32);
procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Int32);
function FormatMilliseconds(Milliseconds: Int64; Fmt: String): String; overload;
function FormatMilliseconds(Milliseconds: Double; Fmt: String): String; overload;
function HighResolutionTime: Double;

implementation

uses
  simba.nativeinterface;

function FormatMilliseconds(Milliseconds: Int64; Fmt: String): String;

  procedure Build(Arg: String; Value, Padding: Int64);
  begin
    if Result.Contains(Arg) then
      Result := Result.Replace(Arg, IntToStr(Value).PadLeft(2, '0'));
  end;

var
  Time, Days, Hours, Mins, Seconds: Int64;
begin
  Result := Fmt;

  Time := Milliseconds;

  if Result.Contains('d') then
  begin
    Days  := Time div 86400000;
    Time  := Time mod 86400000;
  end;
  if Result.Contains('h') then
  begin
    Hours := Time div 3600000;
    Time  := Time mod 3600000;
  end;
  if Result.Contains('m') then
  begin
    Mins  := Time div 60000;
    Time  := Time mod 60000;
  end;

  Seconds      := Time div 1000;
  Milliseconds := Time mod 1000;

  Build('ms', MilliSeconds, 3);
  Build('d', Days, 2);
  Build('h', Hours, 2);
  Build('m', Mins, 2);
  Build('s', Seconds, 2);
end;

function FormatMilliseconds(Milliseconds: Double; Fmt: String): String;
begin
  Result := FormatMilliseconds(Round(Milliseconds), Fmt);
end;

function HighResolutionTime: Double;
begin
  Result := SimbaNativeInterface.HighResolutionTime;
end;

procedure ConvertTime(Time: Int64; var h, m, s: Int32);
var
  x: Int64;
begin
  x := time;
  h := x div (3600000);
  x := x mod (3600000);
  m := x div (60000);
  x := x mod (60000);
  s := x div (1000);
end;

procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Int32);
var
  x: Int64;
begin
  x := time;
  y := x div (31536000000); // 1000 * 60 * 60 * 24 * 365 (1 year or 365 days)
  x := x mod (31536000000);
  m := x div (2592000000); // 1000 * 60 * 60 * 24 * 30 (1 month or 30 days)
  x := x mod (2592000000);
  w := x div (604800000); // 1000 * 60 * 60 * 24 * 7 (1 week or 7 days)
  x := x mod (604800000);
  d := x div (86400000); // 1000 * 60 * 60 * 24 (1 day or 24 hours)
  x := x mod (86400000);
  h := x div (3600000); // 1000 * 60 * 60 (1 hour or 60 minutes)
  x := x mod (3600000);
  min := x div (60000); // 1000 * 60 (1 minute or 60 seconds)
  x := x mod (60000);
  s := x div (1000); // 1000 (1 second)
  x := x mod (1000);
end;

end.

