unit simba.import_datetime;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportDateTime(Compiler: TSimbaScript_Compiler);

implementation

uses
  DateUtils,
  lptypes,
  simba.datetime, simba.nativeinterface;

(*
Date & Time
===========
*)

(*
TDateTime.Create
----------------
> function TDateTime.Create(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer): TDateTime; static;
*)
procedure _LapeDateTime_Create1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

(*
TDateTime.Create
----------------
> function TDateTime.Create(AYear, AMonth, ADay: Integer): TDateTime; static;

Creates just the date part.
*)
procedure _LapeDateTime_Create2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TDateTime.Create
----------------
> function TDateTime.Create(AHour, AMin, ASecond, AMillisecond: Integer): TDateTime; static;

Creates just the time part.
*)
procedure _LapeDateTime_Create3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TDateTime.CreateFromUnix
------------------------
> function TDateTime.CreateFromUnix(UnixTime: Int64): TDateTime; static;
*)
procedure _LapeDateTime_CreateFromUnix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.CreateFromUnix(PInteger(Params^[0])^);
end;

(*
TDateTime.CreateFromString
--------------------------
> function TDateTime.CreateFromString(Value: String): TDateTime; static;
*)
procedure _LapeDateTime_CreateFromString1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.CreateFromString(PString(Params^[0])^);
end;

(*
TDateTime.CreateFromString
--------------------------
> function TDateTime.CreateFromString(Fmt, Value: String): TDateTime; static;
*)
procedure _LapeDateTime_CreateFromString2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.CreateFromString(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
TDateTime.ToUnix
----------------
> function TDateTime.ToUnix(IsUTC: Boolean = True): Int64;
*)
procedure _LapeDateTime_ToUnix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PDateTime(Params^[0])^.ToUnix(PBoolean(Params^[1])^);
end;

(*
TDateTime.ToString
------------------
> function TDateTime.ToString(Fmt: String): String;
*)
procedure _LapeDateTime_ToString1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDateTime(Params^[0])^.ToString(PString(Params^[1])^);
end;

(*
TDateTime.ToString
------------------
> function TDateTime.ToString: String;
*)
procedure _LapeDateTime_ToString2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDateTime(Params^[0])^.ToString();
end;

(*
TDateTime.AddYears
------------------
> function TDateTime.AddYears(Amount: Integer = 1): TDateTime;
*)
procedure _LapeDateTime_AddYears(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddYears(PInteger(Params^[1])^);
end;

(*
TDateTime.AddMonths
-------------------
> function TDateTime.AddMonths(Amount: Integer = 1): TDateTime;
*)
procedure _LapeDateTime_AddMonths(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddMonths(PInteger(Params^[1])^);
end;

(*
TDateTime.AddDays
-----------------
> function TDateTime.AddDays(Amount: Integer = 1): TDateTime;
*)
procedure _LapeDateTime_AddDays(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddDays(PInteger(Params^[1])^);
end;

(*
TDateTime.AddHours
------------------
> function TDateTime.AddHours(Amount: Int64 = 1): TDateTime;
*)
procedure _LapeDateTime_AddHours(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddHours(PInteger(Params^[1])^);
end;

(*
TDateTime.AddMinutes
--------------------
> function TDateTime.AddMinutes(Amount: Int64 = 1): TDateTime;
*)
procedure _LapeDateTime_AddMinutes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddMinutes(PInteger(Params^[1])^);
end;

(*
TDateTime.AddSeconds
--------------------
> function TDateTime.AddSeconds(Amount: Int64 = 1): TDateTime;
*)
procedure _LapeDateTime_AddSeconds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddSeconds(PInteger(Params^[1])^);
end;

(*
TDateTime.AddMilliseconds
-------------------------
> function TDateTime.AddMilliseconds(Amount: Int64 = 1): TDateTime;
*)
procedure _LapeDateTime_AddMilliseconds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.AddMilliseconds(PInteger(Params^[1])^);
end;

(*
TDateTime.YearsBetween
----------------------
> function TDateTime.YearsBetween(Other: TDateTime): Integer;
*)
procedure _LapeDateTime_YearsBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.YearsBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.MonthsBetween
-----------------------
> function TDateTime.MonthsBetween(Other: TDateTime): Integer;
*)
procedure _LapeDateTime_MonthsBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.MonthsBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.WeeksBetween
----------------------
> function TDateTime.WeeksBetween(Other: TDateTime): Integer;
*)
procedure _LapeDateTime_WeeksBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.WeeksBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.DaysBetween
---------------------
> function TDateTime.DaysBetween(Other: TDateTime): Integer;
*)
procedure _LapeDateTime_DaysBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.DaysBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.HoursBetween
----------------------
> function TDateTime.HoursBetween(Other: TDateTime): Int64;
*)
procedure _LapeDateTime_HoursBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PDateTime(Params^[0])^.HoursBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.MinutesBetween
------------------------
> function TDateTime.MinutesBetween(Other: TDateTime): Int64;
*)
procedure _LapeDateTime_MinutesBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PDateTime(Params^[0])^.MinutesBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.SecondsBetween
------------------------
> function TDateTime.SecondsBetween(Other: TDateTime): Int64;
*)
procedure _LapeDateTime_SecondsBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PDateTime(Params^[0])^.SecondsBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.MilliSecondsBetween
-----------------------------
> function TDateTime.MilliSecondsBetween(Other: TDateTime): Int64;
*)
procedure _LapeDateTime_MilliSecondsBetween(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PDateTime(Params^[0])^.MilliSecondsBetween(PDateTime(Params^[1])^);
end;

(*
TDateTime.Date
--------------
> property TDateTime.Date: TDateTime
> property TDateTime.Date(NewValue: TDateTime)

Read or write just the date part.
*)
procedure _LapeDateTime_Date_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.Date;
end;

procedure _LapeDateTime_Date_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Date := PDateTime(Params^[1])^;
end;

(*
TDateTime.Time
--------------
> property TDateTime.Time: TDateTime
> property TDateTime.Time(NewValue: TDateTime)
*)
procedure _LapeDateTime_Time_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateTime(Params^[0])^.Time;
end;

procedure _LapeDateTime_Time_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Time := PDateTime(Params^[1])^;
end;

(*
TDateTime.Year
--------------
> property TDateTime.Year: Integer
> property TDateTime.Year(NewValue: Integer)
*)
procedure _LapeDateTime_Year_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Year;
end;

procedure _LapeDateTime_Year_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Year := PInteger(Params^[1])^;
end;

(*
TDateTime.Month
--------------
> property TDateTime.Month: Integer
> property TDateTime.Month(NewValue: Integer)
*)
procedure _LapeDateTime_Month_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Month;
end;

procedure _LapeDateTime_Month_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Month := PInteger(Params^[1])^;
end;

(*
TDateTime.Day
--------------
> property TDateTime.Day: Integer
> property TDateTime.Day(NewValue: Integer)
*)
procedure _LapeDateTime_Day_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Day;
end;

procedure _LapeDateTime_Day_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Day := PInteger(Params^[1])^;
end;

(*
TDateTime.Hour
--------------
> property TDateTime.Hour: Integer
> property TDateTime.Hour(NewValue: Integer)
*)
procedure _LapeDateTime_Hour_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Hour;
end;

procedure _LapeDateTime_Hour_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Hour := PInteger(Params^[1])^;
end;

(*
TDateTime.Minute
----------------
> property TDateTime.Minute: Integer
> property TDateTime.Minute(NewValue: Integer)
*)
procedure _LapeDateTime_Minute_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Minute;
end;

procedure _LapeDateTime_Minute_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Minute := PInteger(Params^[1])^;
end;

(*
TDateTime.Second
----------------
> property TDateTime.Second: Integer
> property TDateTime.Second(NewValue: Integer)
*)
procedure _LapeDateTime_Second_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Second;
end;

procedure _LapeDateTime_Second_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Second := PInteger(Params^[1])^;
end;

(*
TDateTime.Millisecond
---------------------
> property TDateTime.Millisecond: Integer
> property TDateTime.Millisecond(NewValue: Integer)
*)
procedure _LapeDateTime_Millisecond_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDateTime(Params^[0])^.Millisecond;
end;

procedure _LapeDateTime_Millisecond_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Params^[0])^.Millisecond := PInteger(Params^[1])^;
end;

(*
Now
---
> function Now: TDateTime;
*)
procedure _LapeNow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.Now();
end;

(*
NowUTC
------
> function NowUTC: TDateTime;
*)
procedure _LapeNowUTC(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TDateTime.NowUTC();
end;

(*
UnixTime
--------
> function UnixTime: Int64;
*)
procedure _LapeUnixTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := DateTimeToUnix(Now(), False);
end;

(*
LocalTimeOffset
---------------
> function LocalTimeOffset: Integer;

Returns the local timezone offset in minutes. This is the difference between UTC time and local time.
*)
procedure _LapeLocalTimeOffset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := GetLocalTimeOffset();
end;


(*
PreciseSleep
------------
> procedure PreciseSleep(Milliseconds: UInt32);
*)
procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
end;

(*
MillisecondsToTime
------------------
> function MillisecondsToTime(Time: UInt64; out Days, Hours, Mins, Secs: Integer): Integer;
*)
procedure _LapeMillisecondsToTime1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := MillisecondsToTime(PUInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
MillisecondsToTime
------------------
> function MillisecondsToTime(Time: UInt64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer;
*)
procedure _LapeMillisecondsToTime2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := MillisecondsToTime(PUInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

(*
PerformanceTimer
----------------
> function PerformanceTimer: Double;
*)
procedure _LapePerformanceTimer(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := SimbaNativeInterface.HighResolutionTime();
end;

(*
FormatMilliseconds
------------------
> function FormatMilliseconds(Time: Double; Format: String): String;
*)
procedure _LapeFormatMilliseconds1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PString(Params^[1])^);
end;

(*
FormatMilliseconds
------------------
> function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String;
*)
procedure _LapeFormatMilliseconds2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure ImportDateTime(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Date & Time';

    addGlobalFunc('function TDateTime.Create(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer): TDateTime; static; overload', @_LapeDateTime_Create1);
    addGlobalFunc('function TDateTime.Create(AYear, AMonth, ADay: Integer): TDateTime; static; overload', @_LapeDateTime_Create2);
    addGlobalFunc('function TDateTime.Create(AHour, AMin, ASecond, AMillisecond: Integer): TDateTime; static; overload', @_LapeDateTime_Create3);
    addGlobalFunc('function TDateTime.CreateFromUnix(UnixTime: Int64): TDateTime; static;', @_LapeDateTime_CreateFromUnix);
    addGlobalFunc('function TDateTime.CreateFromString(Value: String): TDateTime; static; overload;', @_LapeDateTime_CreateFromString1);
    addGlobalFunc('function TDateTime.CreateFromString(Fmt, Value: String): TDateTime; static; overload;', @_LapeDateTime_CreateFromString2);

    addGlobalFunc('function TDateTime.ToUnix(IsUTC: Boolean = True): Int64', @_LapeDateTime_ToUnix);
    addGlobalFunc('function TDateTime.ToString(Fmt: String): String; overload', @_LapeDateTime_ToString1);
    addGlobalFunc('function TDateTime.ToString: String; overload', @_LapeDateTime_ToString2);

    addGlobalFunc('function TDateTime.AddYears(Amount: Integer = 1): TDateTime;', @_LapeDateTime_AddYears);
    addGlobalFunc('function TDateTime.AddMonths(Amount: Integer = 1): TDateTime;', @_LapeDateTime_AddMonths);
    addGlobalFunc('function TDateTime.AddDays(Amount: Integer = 1): TDateTime;', @_LapeDateTime_AddDays);
    addGlobalFunc('function TDateTime.AddHours(Amount: Int64 = 1): TDateTime;', @_LapeDateTime_AddHours);
    addGlobalFunc('function TDateTime.AddMinutes(Amount: Int64 = 1): TDateTime;', @_LapeDateTime_AddMinutes);
    addGlobalFunc('function TDateTime.AddSeconds(Amount: Int64 = 1): TDateTime;', @_LapeDateTime_AddSeconds);
    addGlobalFunc('function TDateTime.AddMilliseconds(Amount: Int64 = 1): TDateTime;', @_LapeDateTime_AddMilliseconds);

    addGlobalFunc('function TDateTime.YearsBetween(Other: TDateTime): Integer;', @_LapeDateTime_YearsBetween);
    addGlobalFunc('function TDateTime.MonthsBetween(Other: TDateTime): Integer;', @_LapeDateTime_MonthsBetween);
    addGlobalFunc('function TDateTime.WeeksBetween(Other: TDateTime): Integer;', @_LapeDateTime_WeeksBetween);
    addGlobalFunc('function TDateTime.DaysBetween(Other: TDateTime): Integer;', @_LapeDateTime_DaysBetween);
    addGlobalFunc('function TDateTime.HoursBetween(Other: TDateTime): Int64;', @_LapeDateTime_HoursBetween);
    addGlobalFunc('function TDateTime.MinutesBetween(Other: TDateTime): Int64;', @_LapeDateTime_MinutesBetween);
    addGlobalFunc('function TDateTime.SecondsBetween(Other: TDateTime): Int64;', @_LapeDateTime_SecondsBetween);
    addGlobalFunc('function TDateTime.MilliSecondsBetween(Other: TDateTime): Int64;', @_LapeDateTime_MilliSecondsBetween);

    addProperty('TDateTime', 'Date', 'TDateTime', @_LapeDateTime_Date_Read, @_LapeDateTime_Date_Write);
    addProperty('TDateTime', 'Time', 'TDateTime', @_LapeDateTime_Time_Read, @_LapeDateTime_Time_Write);
    addProperty('TDateTime', 'Year', 'Integer', @_LapeDateTime_Year_Read, @_LapeDateTime_Year_Write);
    addProperty('TDateTime', 'Month', 'Integer', @_LapeDateTime_Month_Read, @_LapeDateTime_Month_Write);
    addProperty('TDateTime', 'Day', 'Integer', @_LapeDateTime_Day_Read, @_LapeDateTime_Day_Write);
    addProperty('TDateTime', 'Hour', 'Integer', @_LapeDateTime_Hour_Read, @_LapeDateTime_Hour_Write);
    addProperty('TDateTime', 'Minute', 'Integer', @_LapeDateTime_Minute_Read, @_LapeDateTime_Minute_Write);
    addProperty('TDateTime', 'Second', 'Integer', @_LapeDateTime_Second_Read, @_LapeDateTime_Second_Write);
    addProperty('TDateTime', 'Millisecond', 'Integer', @_LapeDateTime_Millisecond_Read, @_LapeDateTime_Millisecond_Write);

    addGlobalFunc('function Now: TDateTime;', @_LapeNow);
    addGlobalFunc('function NowUTC: TDateTime;', @_LapeNowUTC);

    addGlobalFunc('function UnixTime: Int64;', @_LapeUnixTime);
    addGlobalFunc('function LocalTimeOffset: Integer', @_LapeLocalTimeOffset);

    addGlobalFunc('procedure PreciseSleep(Milliseconds: UInt32);', @_LapePreciseSleep);
    addGlobalFunc('function PerformanceTimer: Double;', @_LapePerformanceTimer);
    addGlobalFunc('function MillisecondsToTime(Time: UInt64; out Days, Hours, Mins, Secs: Integer): Integer; overload', @_LapeMillisecondsToTime1);
    addGlobalFunc('function MillisecondsToTime(Time: UInt64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer; overload', @_LapeMillisecondsToTime2);
    addGlobalFunc('function FormatMilliseconds(Time: Double; Format: String): String; overload;', @_LapeFormatMilliseconds1);
    addGlobalFunc('function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;', @_LapeFormatMilliseconds2);

    addDelayedCode([
      'type',
      '  TStopwatch = record',
      '    Name: String;',
      '    StartTime: Double;',
      '    StopTime: Double;',
      '  end;',
      '',
      'procedure TStopwatch.Start(AName: String = "");',
      'begin',
      '  Self.Name := AName;',
      '  Self.StartTime := PerformanceTimer();',
      'end;',
      '',
      'procedure TStopwatch.Stop;',
      'begin',
      '  Self.StopTime := PerformanceTimer();',
      'end;',
      '',
      'function TStopwatch.Elapsed: Double;',
      'begin',
      '  if (Self.StopTime > 0) then',
      '    Result := Self.StopTime - Self.StartTime',
      '  else',
      '    Result := PerformanceTimer() - Self.StartTime;',
      'end;',
      '',
      'function TStopwatch.ElapsedFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Elapsed(), Format);',
      'end;',
      '',
      'procedure TStopwatch.Reset;',
      'begin',
      '  Self.StopTime := Self.StartTime := 0;',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

end.


