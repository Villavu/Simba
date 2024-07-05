{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.datetime;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TDateTimeHelper = type helper for TDateTime
  private
    function GetDate: TDateTime;
    function GetTime: TDateTime;
    function GetDay: Integer;
    function GetHour: Integer;
    function GetMillisecond: Integer;
    function GetMinute: Integer;
    function GetMonth: Integer;
    function GetSecond: Integer;
    function GetYear: Integer;

    procedure SetDate(AValue: TDateTime);
    procedure SetTime(AValue: TDateTime);
    procedure SetDay(AValue: Integer);
    procedure SetHour(AValue: Integer);
    procedure SetMillisecond(AValue: Integer);
    procedure SetMinute(AValue: Integer);
    procedure SetMonth(AValue: Integer);
    procedure SetSecond(AValue: Integer);
    procedure SetYear(AValue: Integer);
  public
    class function Create(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer): TDateTime; static; overload;
    class function Create(AYear, AMonth, ADay: Integer): TDateTime; static; overload;
    class function Create(AHour, AMin, ASecond, AMillisecond: Integer): TDateTime; static; overload;
    class function CreateFromUnix(UnixTime: Int64): TDateTime; static;
    class function CreateFromString(Value: String): TDateTime; static; overload;
    class function CreateFromString(Fmt, Value: String): TDateTime; static; overload;

    class function Now: TDateTime; static;
    class function NowUTC: TDateTime; static;

    function ToUnix(IsUTC: Boolean = True): Int64;
    function ToString(Fmt: String): String;
    function ToString: String;

    function AddYears(Amount: Integer = 1): TDateTime;
    function AddMonths(Amount: Integer = 1): TDateTime;
    function AddDays(Amount: Integer = 1): TDateTime;
    function AddHours(Amount: Int64 = 1): TDateTime;
    function AddMinutes(Amount: Int64 = 1): TDateTime;
    function AddSeconds(Amount: Int64 = 1): TDateTime;
    function AddMilliseconds(Amount: Int64 = 1): TDateTime;

    function YearsBetween(Other: TDateTime): Integer;
    function MonthsBetween(Other: TDateTime): Integer;
    function WeeksBetween(Other: TDateTime): Integer;
    function DaysBetween(Other: TDateTime): Integer;
    function HoursBetween(Other: TDateTime): Int64;
    function MinutesBetween(Other: TDateTime): Int64;
    function SecondsBetween(Other: TDateTime): Int64;
    function MilliSecondsBetween(Other: TDateTime): Int64;

    property Date: TDateTime read GetDate write SetDate;
    property Time: TDateTime read GetTime write SetTime;

    property Year: Integer read GetYear write SetYear;
    property Month: Integer read GetMonth write SetMonth;
    property Day: Integer read GetDay write SetDay;
    property Hour: Integer read GetHour write SetHour;
    property Minute: Integer read GetMinute write SetMinute;
    property Second: Integer read GetSecond write SetSecond;
    property Millisecond: Integer read GetMillisecond write SetMillisecond;
  end;

function MillisecondsToTime(Time: UInt64; out Days, Hours, Mins, Secs: Integer): Integer; overload;
function MillisecondsToTime(Time: UInt64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer; overload;

function FormatMilliseconds(Time: Double; Fmt: String): String; overload;
function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;

function HighResolutionTime: Double;

implementation

uses
  DateUtils,
  simba.nativeinterface;

function TDateTimeHelper.GetDate: TDateTime;
begin
  Result := DateOf(Self);
end;

function TDateTimeHelper.GetDay: Integer;
begin
  Result := DayOf(Self);
end;

function TDateTimeHelper.GetHour: Integer;
begin
  Result := HourOf(Self);
end;

function TDateTimeHelper.GetMillisecond: Integer;
begin
  Result := MilliSecondOf(Self);
end;

function TDateTimeHelper.GetMinute: Integer;
begin
  Result := MinuteOf(Self);
end;

function TDateTimeHelper.GetMonth: Integer;
begin
  Result := MonthOf(Self);
end;

function TDateTimeHelper.GetSecond: Integer;
begin
  Result := SecondOf(Self);
end;

function TDateTimeHelper.GetTime: TDateTime;
begin
  Result := TimeOf(Self);
end;

function TDateTimeHelper.GetYear: Integer;
begin
  Result := YearOf(Self);
end;

procedure TDateTimeHelper.SetDate(AValue: TDateTime);
begin
  ReplaceDate(Self, AValue);
end;

procedure TDateTimeHelper.SetTime(AValue: TDateTime);
begin
  ReplaceTime(Self, AValue);
end;

procedure TDateTimeHelper.SetDay(AValue: Integer);
begin
  Self := RecodeDay(Self, AValue);
end;

procedure TDateTimeHelper.SetHour(AValue: Integer);
begin
  Self := RecodeHour(Self, AValue);
end;

procedure TDateTimeHelper.SetMillisecond(AValue: Integer);
begin
  Self := RecodeMilliSecond(Self, AValue);
end;

procedure TDateTimeHelper.SetMinute(AValue: Integer);
begin
  Self := RecodeMinute(Self, AValue);
end;

procedure TDateTimeHelper.SetMonth(AValue: Integer);
begin
  Self := RecodeMonth(Self, AValue);
end;

procedure TDateTimeHelper.SetSecond(AValue: Integer);
begin
  Self := RecodeSecond(Self, AValue);
end;

procedure TDateTimeHelper.SetYear(AValue: Integer);
begin
  Self := RecodeYear(Self, AValue);
end;

class function TDateTimeHelper.Create(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer): TDateTime;
begin
  Result := EncodeDateTime(AYear, AMonth, ADay, Ahour, AMinute, ASecond, AMillisecond);
end;

class function TDateTimeHelper.Create(AYear, AMonth, ADay: Integer): TDateTime;
begin
  Result := EncodeDate(AYear, AMonth, ADay);
end;

class function TDateTimeHelper.Create(AHour, AMin, ASecond, AMillisecond: Integer): TDateTime;
begin
  Result := EncodeTime(AHour, AMin, ASecond, AMillisecond);
end;

class function TDateTimeHelper.CreateFromUnix(UnixTime: Int64): TDateTime;
begin
  Result := UnixToDateTime(UnixTime);
end;

class function TDateTimeHelper.CreateFromString(Value: String): TDateTime;
begin
  Result := StrToDateTime(Value);
end;

class function TDateTimeHelper.CreateFromString(Fmt, Value: String): TDateTime;
begin
  case Fmt of
    'iso':
      if not TryISOStrToDateTime(Value, Result) then
        raise EConvertError.CreateFmt('Invalid ISO value ""', [Value]);
    'iso8601':
      Result := ISO8601ToDate(Value);
    'unix':
      Result := UnixToDateTime(StrToInt64(Value));
    else
      Result := ScanDateTime(Fmt, Value);
  end;
end;

class function TDateTimeHelper.Now: TDateTime;
begin
  Result := SysUtils.Now();
end;

class function TDateTimeHelper.NowUTC: TDateTime;
begin
  Result := IncMinute(SysUtils.Now(), GetLocalTimeOffset());
end;

function TDateTimeHelper.ToUnix(IsUTC: Boolean): Int64;
begin
  Result := DateTimeToUnix(Self, IsUTC);
end;

function TDateTimeHelper.ToString(Fmt: String): String;
begin
  Result := FormatDateTime(Fmt, Self);
end;

function TDateTimeHelper.ToString: String;
begin
  Result := DateToStr(Self);
end;

function TDateTimeHelper.AddYears(Amount: Integer): TDateTime;
begin
  Result := IncYear(Amount);
end;

function TDateTimeHelper.AddMonths(Amount: Integer): TDateTime;
begin
  Result := IncMonth(Amount);
end;

function TDateTimeHelper.AddDays(Amount: Integer): TDateTime;
begin
  Result := IncDay(Amount);
end;

function TDateTimeHelper.AddHours(Amount: Int64): TDateTime;
begin
  Result := IncHour(Amount);
end;

function TDateTimeHelper.AddMinutes(Amount: Int64): TDateTime;
begin
  Result := IncMinute(Amount);
end;

function TDateTimeHelper.AddSeconds(Amount: Int64): TDateTime;
begin
  Result := IncSecond(Amount);
end;

function TDateTimeHelper.AddMilliseconds(Amount: Int64): TDateTime;
begin
  Result := IncMilliSecond(Amount);
end;

function TDateTimeHelper.YearsBetween(Other: TDateTime): Integer;
begin
  Result := DateUtils.YearsBetween(Self, Other);
end;

function TDateTimeHelper.MonthsBetween(Other: TDateTime): Integer;
begin
  Result := DateUtils.MonthsBetween(Self, Other);
end;

function TDateTimeHelper.WeeksBetween(Other: TDateTime): Integer;
begin
  Result := DateUtils.WeeksBetween(Self, Other);
end;

function TDateTimeHelper.DaysBetween(Other: TDateTime): Integer;
begin
  Result := DateUtils.DaysBetween(Self, Other);
end;

function TDateTimeHelper.HoursBetween(Other: TDateTime): Int64;
begin
  Result := DateUtils.HoursBetween(Self, Other);
end;

function TDateTimeHelper.MinutesBetween(Other: TDateTime): Int64;
begin
  Result := DateUtils.MinutesBetween(Self, Other);
end;

function TDateTimeHelper.SecondsBetween(Other: TDateTime): Int64;
begin
  Result := DateUtils.SecondsBetween(Self, Other);
end;

function TDateTimeHelper.MilliSecondsBetween(Other: TDateTime): Int64;
begin
  Result := DateUtils.MilliSecondsBetween(Self, Other);
end;

function MillisecondsToTime(Time: UInt64; out Days, Hours, Mins, Secs: Integer): Integer;
begin
  Days := Time div 86400000; // 1000 * 60 * 60 * 24 (1 day or 24 hours)
  Time := Time mod 86400000;
  Hours:= Time div 3600000; // 1000 * 60 * 60 (1 hour or 60 minutes)
  Time := Time mod 3600000;
  Mins := Time div 60000; // 1000 * 60 (1 minute or 60 seconds)
  Time := Time mod 60000;
  Secs := Time div 1000; // 1000 (1 second)
  Time := Time mod 1000;

  Result := Time;
end;

function MillisecondsToTime(Time: UInt64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer;
begin
  Years  := Time div 31536000000; // 1000 * 60 * 60 * 24 * 365 (1 year or 365 days)
  Time   := Time mod 31536000000;
  Months := Time div 2592000000; // 1000 * 60 * 60 * 24 * 30 (1 month or 30 days)
  Time   := Time mod 2592000000;
  Weeks  := Time div 604800000; // 1000 * 60 * 60 * 24 * 7 (1 week or 7 days)
  Time   := Time mod 604800000;
  Days   := Time div 86400000; // 1000 * 60 * 60 * 24 (1 day or 24 hours)
  Time   := Time mod 86400000;
  Hours  := Time div 3600000; // 1000 * 60 * 60 (1 hour or 60 minutes)
  Time   := Time mod 3600000;
  Mins   := Time div 60000; // 1000 * 60 (1 minute or 60 seconds)
  Time   := Time mod 60000;
  Secs   := Time div 1000; // 1000 (1 second)
  Time   := Time mod 1000;

  Result := Time;
end;

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
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'M': begin
             while Next() = 'M' do ;
             case p-q of
               1: Include(Flags, fmtMonthsNoPad);
               2: Include(Flags, fmtMonths);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'W': begin
             while Next() = 'W' do ;
             case p-q of
               1: Include(Flags, fmtWeeksNoPad);
               2: Include(Flags, fmtWeeks);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'D': begin
             while Next() = 'D' do ;
             case p-q of
               1: Include(Flags, fmtDaysNoPad);
               2: Include(Flags, fmtDays);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'h': begin
             while Next() = 'h' do ;
             case p-q of
               1: Include(Flags, fmtHoursNoPad);
               2: Include(Flags, fmtHours);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'm': begin
             while Next() = 'm' do ;
             case p-q of
               1: Include(Flags, fmtMinutesNoPad);
               2: Include(Flags, fmtMinutes);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      's': begin
             while Next() = 's' do ;
             case p-q of
               1: Include(Flags, fmtSecondsNoPad);
               2: Include(Flags, fmtSeconds);
               else SimbaException('Illegal format "%s"', [Fmt]);
             end;
           end;
      'u': begin
             while Next() = 'u' do ;
             case p-q of
               1: Include(Flags, fmtMillisecondsNoPad);
               2: Include(Flags, fmtMilliseconds);
               else SimbaException('Illegal format "%s"', [Fmt]);
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
               Result += IntToStr(Trunc(years{%H-}))
             else begin
               if years < 10 then Result += '0';
               Result += IntToStr(Trunc(years));
             end;
           end;
      'M': begin
             while Next() = 'M' do ;
             if fmtMonthsNoPad in flags then
               Result += IntToStr(Trunc(months{%H-}))
             else begin
               if months < 10 then Result += '0';
               Result += IntToStr(Trunc(months));
             end;
           end;
      'W': begin
             while Next() = 'W' do ;
             if fmtWeeksNoPad in flags then
               Result += IntToStr(Trunc(weeks{%H-}))
             else begin
               if weeks < 10 then Result += '0';
               Result += IntToStr(Trunc(weeks));
             end;
           end;
      'D': begin
             while Next() = 'D' do ;
             if fmtDaysNoPad in flags then
               Result += IntToStr(Trunc(days{%H-}))
             else begin
               if days < 10 then Result += '0';
               Result += IntToStr(Trunc(days));
             end;
           end;
      'h': begin
             while Next() = 'h' do ;
             if fmtHoursNoPad in flags then
               Result += IntToStr(Trunc(hours{%H-}))
             else begin
               if hours < 10 then Result += '0';
               Result += IntToStr(Trunc(hours));
             end;
           end;
      'm': begin
             while Next() = 'm' do ;
             if fmtMinutesNoPad in flags then
               Result += IntToStr(Trunc(minutes{%H-}))
             else begin
               if minutes < 10 then Result += '0';
               Result += IntToStr(Trunc(minutes));
             end;
           end;
      's': begin
             while Next() = 's' do ;
             if fmtSecondsNoPad in flags then
               Result += IntToStr(Trunc(sec{%H-}))
             else begin
               if sec < 10 then Result += '0';
               Result += IntToStr(Trunc(sec));
             end;
           end;
      'u': begin
             while Next() = 'u' do ;
             if fmtMillisecondsNoPad in flags then
               Result += IntToStr(Trunc(ms{%H-}))
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

end.

