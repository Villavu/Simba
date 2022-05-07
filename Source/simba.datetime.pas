{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.datetime;

{$i simba.inc}

interface

uses
  classes, sysutils;

procedure ConvertTime(Time: Int64; var h, m, s: Int32);
procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Int32);
function FormatMilliseconds(Milliseconds: Int64; Fmt: String): String; overload;
function FormatMilliseconds(Milliseconds: Double; Fmt: String): String; overload;
function HighResolutionTime: Double;

implementation

uses
  simba.nativeinterface;

function FormatMilliseconds(Milliseconds: Int64; Fmt: String): String;
var
  Time, Days, Hours, Mins, Seconds: Int64;
  Arg: String;
begin
  Result := '';

  Time := Milliseconds;

  for Arg in Fmt.Split(':') do
    case Arg of
      'd':
        begin
          Days := Time div 86400000;
          Time := Time mod 86400000;

          if (Result <> '') then
            Result += ':' + IntToStr(Days).PadLeft(2, '0')
          else
            Result += IntToStr(Days).PadLeft(2, '0');
        end;

      'h':
        begin
          Hours := Time div 3600000;
          Time  := Time mod 3600000;

          if (Result <> '') then
            Result += ':' + IntToStr(Hours).PadLeft(2, '0')
          else
            Result += IntToStr(Hours).PadLeft(2, '0');
        end;

      'm':
        begin
          Mins := Time div 60000;
          Time := Time mod 60000;

          if (Result <> '') then
            Result += ':' + IntToStr(Mins).PadLeft(2, '0')
          else
            Result += IntToStr(Mins).PadLeft(2, '0');
        end;

      's':
        begin
          Seconds := Time div 1000;
          Time    := Time mod 1000;

          if (Result <> '') then
            Result += ':' + IntToStr(Seconds).PadLeft(2, '0')
          else
            Result += IntToStr(Seconds).PadLeft(2, '0');
        end;

      'ms':
        begin
          if (Result <> '') then
            Result += ':' + IntToStr(Time).PadLeft(3, '0')
          else
            Result += IntToStr(Time).PadLeft(3, '0');
        end;
    end;
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

