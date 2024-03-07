unit simba.import_timing;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportTiming(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.datetime, simba.nativeinterface;

(*
Timing
======
Timing
*)

(*
Sleep
-----
> procedure Sleep(MilliSeconds: UInt32);
*)

(*
PreciseSleep
------------
> procedure PreciseSleep(Milliseconds: UInt32);

Much more accurate sleep method, if you need millisecond accurate sleeps under -50ms use this.

Note::
  This is only useful on Windows since on Linux and MacOS the regular `Sleep` is accurate to the milliseconds.
*)
procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
end;

(*
MillisecondsToTime
------------------
> function MillisecondsToTime(Time: UInt64; out Days, Hours, Mins, Secs: Integer): Integer;

Converts time (in milliseconds) to days,hours,mins and seconds.
Any remaining milliseconds are returned in the Result.
*)
procedure _LapeMillisecondsToTime1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := MillisecondsToTime(PUInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
MillisecondsToTime
------------------
> function MillisecondsToTime(Time: UInt64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer;

Converts time (in milliseconds) to years,months,weeks,days,hours,mins and seconds.
Any remaining milliseconds are returned in the Result.
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

Formats milliseconds into a string. Formatting is defined by the `Format` string.

```
WriteLn FormatMilliseconds(GetTickCount(), 's\s\e\c, m\m\i\n');
WriteLn FormatMilliseconds(GetTickCount(), 'hh\h:mm\m:ss\s:uu\m\s');
WriteLn FormatMilliseconds(GetTickCount(), 'YY-MM-DD h:m:s:u');
```
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

(*
GetTimeRunning
--------------
> function GetTimeRunning: UInt64;

Returns the current script runtime in milliseconds.
*)

(*
GetTickCount
------------
> function GetTickCount: UInt64;

Returns the number of milliseconds that have elapsed since the system was started.
However the more important use case of this function is for measuring time:

```
  T := GetTickCount();
  Sleep(1000);
  WriteLn('Should be around ~1000 :: ', GetTickCount()-T);
```
Resolution is typically in the range of 10 milliseconds to 16 milliseconds.

If you need a more accurate timer use `PerformanceTimer`
*)

procedure ImportTiming(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Timing';

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
      '    StartTime: UInt64;',
      '    StopTime: UInt64;',
      '  end;',
      '',
      'procedure TStopwatch.Start(AName: String = "");',
      'begin',
      '  Self.Name := AName;',
      '  Self.StartTime := GetTickCount();',
      'end;',
      '',
      'procedure TStopwatch.Stop;',
      'begin',
      '  Self.StopTime := GetTickCount();',
      'end;',
      '',
      'function TStopwatch.Elapsed: UInt64;',
      'begin',
      '  if (Self.StopTime > 0) then',
      '    Result := Self.StopTime - Self.StartTime',
      '  else',
      '    Result := GetTickCount() - Self.StartTime;',
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

    addDelayedCode([
      'type',
      '  TPerformanceStopwatch = record',
      '    Name: String;',
      '    StartTime: Double;',
      '    StopTime: Double;',
      '  end;',
      '',
      'procedure TPerformanceStopwatch.Start(AName: String = "");',
      'begin',
      '  Self.Name := AName;',
      '  Self.StartTime := PerformanceTimer();',
      'end;',
      '',
      'procedure TPerformanceStopwatch.Stop;',
      'begin',
      '  Self.StopTime := PerformanceTimer();',
      'end;',
      '',
      'function TPerformanceStopwatch.Elapsed: Double;',
      'begin',
      '  if (Self.StopTime > 0) then',
      '    Result := Self.StopTime - Self.StartTime',
      '  else',
      '    Result := PerformanceTimer() - Self.StartTime;',
      'end;',
      '',
      'function TPerformanceStopwatch.ElapsedFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Elapsed(), Format);',
      'end;',
      '',
      'procedure TPerformanceStopwatch.Reset;',
      'begin',
      '  Self.StopTime := Self.StartTime := 0;',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

end.

