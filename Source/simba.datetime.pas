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
function TimeStamp(Time: Int64; IncludeMilliseconds: Boolean = False): String;
function HighResolutionTime: Double;

implementation

uses
  simba.nativeinterface;

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

function TimeStamp(Time: Int64; IncludeMilliseconds: Boolean): String;
var
  Hours, Mins, Secs, Milliseconds: Int32;
begin
  Hours := Time div 3600000;
  Time  := Time mod 3600000;
  Mins  := Time div 60000;
  Time  := Time mod 60000;
  Secs  := Time div 1000;
  Milliseconds  := Time mod 1000;

  if IncludeMilliseconds then
    Result := Format('[%.2d:%.2d:%.2d:%.3d]', [Hours, Mins, Secs, Milliseconds])
  else
    Result := Format('[%.2d:%.2d:%.2d]', [Hours, Mins, Secs]);
end;

end.

