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

procedure ConvertTime(Time: Int64; var h, m, s: Integer);
procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Integer);
function FormatMilliseconds(Time: Double; Fmt: String): String; overload;
function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;
function HighResolutionTime: Double;

implementation

uses
  simba.nativeinterface;

// Author: slacky
// https://pastebin.com/zwue0VCt
function FormatMilliseconds(Time: Double; Fmt: string): String;
type
  TFormatEnum = (
    fmtYearsNoPad,   fmtYears,
    fmtMonthsNoPad,  fmtMonths,
    fmtWeeksNoPad,   fmtWeeks,
    fmtDaysNoPad,    fmtDays,
    fmtHoursNoPad,   fmtHours,
    fmtMinutesNoPad, fmtMinutes,
    fmtSecondsNoPad, fmtSeconds,
    fmtMillisecondsNoPad, fmtMilliseconds
  );
var
  p, q: Integer;
  flags: set of TFormatEnum;
  flag: TFormatEnum;
  function Next(step: Integer=1): Char;
  begin
    Inc(p, step);
    if p <= Length(Fmt) then
      Exit(Fmt[p]);
    Result := #0;
  end;
var
  timeLeft: Double;
  years, months, weeks, days, hours, minutes, sec, ms: Double;
begin
  // with this we create the needed flags
  Fmt += #0;
  Flags := [];
  p := 1;
  while (p <= Length(fmt)) and (Fmt[p] <> #0) do
  begin
    q := p;
    case Fmt[p] of
      '\': Next(2);
      'Y': begin
             while Next() = 'Y' do ;
             case p-q of
               1: Include(Flags, fmtYearsNoPad);
               2: Include(Flags, fmtYears);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'M': begin
             while Next() = 'M' do ;
             case p-q of
               1: Include(Flags, fmtMonthsNoPad);
               2: Include(Flags, fmtMonths);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'W': begin
             while Next() = 'W' do ;
             case p-q of
               1: Include(Flags, fmtWeeksNoPad);
               2: Include(Flags, fmtWeeks);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'D': begin
             while Next() = 'D' do ;
             case p-q of
               1: Include(Flags, fmtDaysNoPad);
               2: Include(Flags, fmtDays);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'h': begin
             while Next() = 'h' do ;
             case p-q of
               1: Include(Flags, fmtHoursNoPad);
               2: Include(Flags, fmtHours);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'm': begin
             while Next() = 'm' do ;
             case p-q of
               1: Include(Flags, fmtMinutesNoPad);
               2: Include(Flags, fmtMinutes);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      's': begin
             while Next() = 's' do ;
             case p-q of
               1: Include(Flags, fmtSecondsNoPad);
               2: Include(Flags, fmtSeconds);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      'u': begin
             while Next() = 'u' do ;
             case p-q of
               1: Include(Flags, fmtMillisecondsNoPad);
               2: Include(Flags, fmtMilliseconds);
               else raise Exception.CreateFmt('FormatMilliseconds: Illegal format "%s"', [Fmt]);
             end;
           end;
      else
        Next();
    end;
  end;

  // with this we reduce in the order of flags
  timeLeft := time;
  for flag in flags do
  begin
    case flag of
      fmtYearsNoPad, fmtYears:
        begin
          years := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 365);
          timeLeft -= years * 365 * 24 * 60 * 60 * 1000;
        end;
      fmtMonthsNoPad,  fmtMonths:
        begin
          months := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 30.5);
          timeLeft -= months * 30.5 * 24 * 60 * 60 * 1000;
        end;
      fmtWeeksNoPad,  fmtWeeks:
        begin
          weeks := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 7);
          timeLeft -= weeks * 7 * 24 * 60 * 60 * 1000;
        end;
      fmtDaysNoPad, fmtDays:
        begin
          days := Trunc(timeLeft / 1000 / 60 / 60 / 24);
          timeLeft -= days * 24 * 60 * 60 * 1000;
        end;
      fmtHoursNoPad, fmtHours:
        begin
          hours := Trunc(timeLeft / 1000 / 60 / 60);
          timeLeft -= hours * 60 * 60 * 1000;
        end;
      fmtMinutesNoPad, fmtMinutes:
        begin
          minutes := Trunc(timeLeft / 1000 / 60);
          timeLeft -= minutes * 60 * 1000;
        end;
      fmtSecondsNoPad, fmtSeconds:
        begin
          sec := Trunc(timeLeft / 1000);
          timeLeft -= sec * 1000;
        end;
      fmtMillisecondsNoPad, fmtMilliseconds:
        begin
          ms := timeLeft;
          timeLeft -= ms * 1000;
        end;
    end;
  end;

  // now we rebuild the result based on the format
  Result := '';
  p := 1;
  while (p <= Length(fmt)) and (Fmt[p] <> #0) do
  begin
    q := p;
    case Fmt[p] of
      '\': begin
             Next();
             Result += Fmt[p];
             Next();
           end;
      'Y': begin
             while Next() = 'Y' do ;
             if fmtYearsNoPad in flags then
               Result += IntToStr(Trunc(years))
             else begin
               if years < 10 then Result += '0';
               Result += IntToStr(Trunc(years));
             end;
           end;
      'M': begin
             while Next() = 'M' do ;
             if fmtMonthsNoPad in flags then
               Result += IntToStr(Trunc(months))
             else begin
               if months < 10 then Result += '0';
               Result += IntToStr(Trunc(months));
             end;
           end;
      'W': begin
             while Next() = 'W' do ;
             if fmtWeeksNoPad in flags then
               Result += IntToStr(Trunc(weeks))
             else begin
               if weeks < 10 then Result += '0';
               Result += IntToStr(Trunc(weeks));
             end;
           end;
      'D': begin
             while Next() = 'D' do ;
             if fmtDaysNoPad in flags then
               Result += IntToStr(Trunc(days))
             else begin
               if days < 10 then Result += '0';
               Result += IntToStr(Trunc(days));
             end;
           end;
      'h': begin
             while Next() = 'h' do ;
             if fmtHoursNoPad in flags then
               Result += IntToStr(Trunc(hours))
             else begin
               if hours < 10 then Result += '0';
               Result += IntToStr(Trunc(hours));
             end;
           end;
      'm': begin
             while Next() = 'm' do ;
             if fmtMinutesNoPad in flags then
               Result += IntToStr(Trunc(minutes))
             else begin
               if minutes < 10 then Result += '0';
               Result += IntToStr(Trunc(minutes));
             end;
           end;
      's': begin
             while Next() = 's' do ;
             if fmtSecondsNoPad in flags then
               Result += IntToStr(Trunc(sec))
             else begin
               if sec < 10 then Result += '0';
               Result += IntToStr(Trunc(sec));
             end;
           end;
      'u': begin
             while Next() = 'u' do ;
             if fmtMillisecondsNoPad in flags then
               Result += IntToStr(Trunc(ms))
             else begin
               if ms < 1000 then Result += '0';
               if ms < 100 then  Result += '0';
               if ms < 10 then   Result += '0';

               Result += IntToStr(Trunc(ms));
             end;
           end;
      else
      begin
        Result += Fmt[p];
        Next();
      end;
    end;
  end;
end;

function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;
var
  timeLeft: Double;
  years, months, weeks, days, hours, minutes, sec, ms: Double;
begin
  timeLeft := Time;
  years := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 365);
  timeLeft -= years * 365 * 24 * 60 * 60 * 1000;

  months := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 30.5);
  timeLeft -= months * 30.5 * 24 * 60 * 60 * 1000;

  weeks := Trunc(timeLeft / 1000 / 60 / 60 / 24 / 7);
  timeLeft -= months * 7 * 24 * 60 * 60 * 1000;

  days := Trunc(timeLeft / 1000 / 60 / 60 / 24);
  timeLeft -= days * 24 * 60 * 60 * 1000;

  hours := Trunc(timeLeft / 1000 / 60 / 60);
  timeLeft -= hours * 60 * 60 * 1000;

  minutes := Trunc(timeLeft / 1000 / 60);
  timeLeft -= minutes * 60 * 1000;

  sec := Trunc(timeLeft / 1000);
  timeLeft -= sec * 1000;

  ms := timeLeft;
  timeLeft -= ms * 1000;
  if not TimeSymbols then
  begin
    if      years  > 0 then Result := FormatMilliseconds(time, 'YY:MM:WW:DD:hh:mm:ss:uu')
    else if months > 0 then Result := FormatMilliseconds(time, 'MM:WW:DD:hh:mm:ss:uu')
    else if weeks  > 0 then Result := FormatMilliseconds(time, 'WW:DD:hh:mm:ss:uu')
    else if days   > 0 then Result := FormatMilliseconds(time, 'DD:hh:mm:ss:uu')
    else if hours  > 0 then Result := FormatMilliseconds(time, 'hh:mm:ss:uu')
    else if minutes> 0 then Result := FormatMilliseconds(time, 'mm:ss:uu')
    else if sec    > 0 then Result := FormatMilliseconds(time, 'ss:uu')
    else                    Result := FormatMilliseconds(time, 'uu');
  end else
  begin
    if      years  > 0 then Result := FormatMilliseconds(time, 'YY\y:MM\m:WW\w:DD\d:hh\h:mm\m:ss\s:uu\m\s')
    else if months > 0 then Result := FormatMilliseconds(time, 'MM\m:WW\w:DD\d:hh\h:mm\m:ss\s:uu\m\s')
    else if weeks  > 0 then Result := FormatMilliseconds(time, 'WW\w:DD\d:hh\h:mm\m:ss\s:uu\m\s')
    else if days   > 0 then Result := FormatMilliseconds(time, 'DD\d:hh\h:mm\m:ss\s:uu\m\s')
    else if hours  > 0 then Result := FormatMilliseconds(time, 'hh\h:mm\m:ss\s:uu\m\s')
    else if minutes> 0 then Result := FormatMilliseconds(time, 'mm\m:ss\s:uu\m\s')
    else if sec    > 0 then Result := FormatMilliseconds(time, 'ss\s:uu\m\s')
    else                    Result := FormatMilliseconds(time, 'uu\m\s');
  end;
end;

function HighResolutionTime: Double;
begin
  Result := SimbaNativeInterface.HighResolutionTime;
end;

procedure ConvertTime(Time: Int64; var h, m, s: Integer);
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

procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Integer);
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

