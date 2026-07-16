unit simba.import_timing;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportTiming(Script: TSimbaScript);

implementation

uses
  DateUtils,
  lptypes,
  simba.datetime, simba.nativeinterface;

(*
Timing
======
Timing in milliseconds

```
var
  t: Int64;
begin
  t := Time();
  for 1 to 10 do
    Sleep(100);
  WriteLn('Took: ', Time()-t, ' ms');

  // 1 hour, 2.5 sec in milliseconds
  t := 60*60000+2500;
  WriteLn FormatMilliseconds(t, 'hh:mm:ss:uu');
end;
```
*)

(*
Time
----
```
function Time: Int64;
```
Return a timestamp for measuring time.<br>
```{note}
The timestamp returned is unix time in milliseconds. Divide by 1000 to get seconds.
```
*)
procedure _LapeTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := SimbaNativeInterface.UnixTime();
end;

(*
PerformanceTime
---------------
```
function PerformanceTime: Double;
```
Return a high resolution timestamp. Use this for accurately measuring very short time intervals.
*)
procedure _LapePerformanceTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := SimbaNativeInterface.HighResolutionTime();
end;

(*
Sleep
-----
```
procedure Sleep(Milliseconds: UInt32);
```
*)

(*
PreciseSleep
------------
```
procedure PreciseSleep(Milliseconds: Double);
```
High resolution sleep, like `PerformanceTime` use this when needing to sleep very short time intervals accurately.
*)
procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PreciseSleep(PDouble(Params^[0])^);
end;

(*
MillisecondsToTime
------------------
```
function MillisecondsToTime(Time: Int64; out Days, Hours, Mins, Secs: Integer): Integer;
```
*)
procedure _LapeMillisecondsToTime1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := MillisecondsToTime(PInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
MillisecondsToTime
------------------
```
function MillisecondsToTime(Time: Int64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer;
```
*)
procedure _LapeMillisecondsToTime2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := MillisecondsToTime(PInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

(*
FormatMilliseconds
------------------
```
function FormatMilliseconds(Time: Int64; Format: String): String;
```

```
Y = years
M = months
W = weeks
D = days

h = hours
m = minutes
s = seconds
u = milliseconds
```

```{note}
Doubling an argument will add padding. Example `hh` will produce `01`
`\` will escape the next character
```
*)
procedure _LapeFormatMilliseconds1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PInt64(Params^[0])^, PString(Params^[1])^);
end;

(*
FormatMilliseconds
------------------
```
function FormatMilliseconds(Time: Int64; TimeSymbols: Boolean = False): String;
```
*)
procedure _LapeFormatMilliseconds2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PInt64(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
TStopWatch.StartTime
--------------------
```
property TStopwatch.StartTime: Int64
```
Returns the time this stopwatch was started at.
*)

(*
TStopWatch.IsPaused
-------------------
```
property TStopWatch.IsPaused: Boolean;
```
*)

(*
TStopWatch.Start
----------------
```
procedure TStopWatch.Start;
```
*)

(*
TStopWatch.Resume
-----------------
```
procedure TStopWatch.Resume;
```
*)

(*
TStopWatch.Pause
----------------
```
procedure TStopWatch.Pause;
```
*)

(*
TStopWatch.Reset
----------------
```
procedure TStopWatch.Reset;
```
*)

(*
TStopWatch.Elapsed
------------------
```
property TStopwatch.Elapsed: Int64
```
Returns the milliseconds passed since the stopwatch was started.
*)

(*
TStopWatch.ElapsedFmt
---------------------
```
function TStopwatch.ElapsedFmt(Format: String = 'u'): String;
```
*)

(*
TCountDown.StartTime
--------------------
```
property TCountDown.StartTime: Int64;
```
Returns the time the countdown was started at.
*)

(*
TCountDown.Start
----------------
```
procedure TCountDown.Start(Milliseconds: Int64);
```
*)

(*
TCountDown.Extend
-----------------
```
procedure TCountDown.Extend(Milliseconds: Int64);
```
*)

(*
TCountDown.Remaining
--------------------
```
property TCountDown.Remaining: Int64;
```
*)

(*
TCountDown.RemainingFmt
-----------------------
```
function TCountDown.RemainingFmt(Format: String = 'u'): String;
```
*)

(*
TCountDown.IsFinished
---------------------
```
property TCountDown.IsFinished: Boolean;
```
*)

(*
TCountDown.IsPaused
-------------------
```
property TCountDown.IsPaused: Boolean;
```
*)

(*
TCountDown.Pause
----------------
```
procedure TCountDown.Pause;
```
*)

(*
TCountDown.Resume
-----------------
```
procedure TCountDown.Resume;
```
*)

(*
TCountDown.Restart
------------------
```
procedure TCountDown.Restart(Randomness: Integer = 0);
```
*)

procedure ImportTiming(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Timing';

    addGlobalFunc('function Time: Int64;', @_LapeTime);

    addGlobalFunc('procedure PreciseSleep(Milliseconds: Double);', @_LapePreciseSleep);
    addGlobalFunc('function PerformanceTime: Double;', @_LapePerformanceTime);
    addGlobalFunc('function MillisecondsToTime(Time: Int64; out Days, Hours, Mins, Secs: Integer): Integer; overload', @_LapeMillisecondsToTime1);
    addGlobalFunc('function MillisecondsToTime(Time: Int64; out Years, Months, Weeks, Days, Hours, Mins, Secs: Integer): Integer; overload', @_LapeMillisecondsToTime2);
    addGlobalFunc('function FormatMilliseconds(Time: Int64; Format: String): String; overload;', @_LapeFormatMilliseconds1);
    addGlobalFunc('function FormatMilliseconds(Time: Int64; TimeSymbols: Boolean = False): String; overload;', @_LapeFormatMilliseconds2);

    addDelayedCode([
      'type',
      '  TStopwatch = record',
      '    {%CODETOOLS OFF}',
      '    FStartTime: Int64;',      // time at last Start()
      '    FPauseTime: Int64;',      // time at last Pause()
      '    FTotalPauseTime: Int64;', // total time spent paused, increased in Start()
      '    {%CODETOOLS ON}',
      '  end;',
      '',
      'property TStopWatch.StartTime: Int64;',
      'begin',
      '  Result := Self.FStartTime;',
      'end;',
      '',
      'property TStopWatch.IsPaused: Boolean;',
      'begin',
      '  Result := Self.FPauseTime > 0;',
      'end;',
      '',
      'procedure TStopWatch.Start;',
      'begin',
      '  if Self.FPauseTime > 0 then',
      '  begin',
      '    Self.FTotalPauseTime += Time() - Self.FPauseTime;',
      '    Self.FPauseTime := 0;',
      '  end else',
      '    Self.FStartTime := Time();',
      'end;',
      '',
      'procedure TStopWatch.Resume;',
      'begin',
      '  Self.Start();',
      'end;',
      '',
      'procedure TStopWatch.Pause;',
      'begin',
      '  Self.FPauseTime := Time();',
      'end;',
      '',
      'procedure TStopWatch.Reset;',
      'begin',
      '  Self.FStartTime := Time();',
      '  Self.FPauseTime := 0;',
      '  Self.FTotalPauseTime := 0;',
      'end;',
      '',
      'property TStopwatch.Elapsed: Int64;',
      'begin',
      '  if (Self.FPauseTime > 0) then',
      '    Result := (Self.FPauseTime - Self.FStartTime) - Self.FTotalPauseTime',
      '  else',
      '    Result := (Time() - Self.FStartTime) - Self.FTotalPauseTime;',
      'end;',
      '',
      'function TStopwatch.ElapsedFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Elapsed, Format);',
      'end;'],
      DumpSection);

    addDelayedCode([
      'type',
      '  TCountDown = record',
      '    {%CODETOOLS OFF}',
      '    FStartTime: Int64;',    // time at last Start()
      '    FLength: Int64;',       // length of countdown at start()
      '    FFinishTime: Int64;',   // time when completed
      '    FPauseTime: Int64;',    // time when paused
      '    {%CODETOOLS ON}',
      '  end;',
      '',
      'property TCountDown.StartTime: Int64;',
      'begin',
      '  Result := Self.FStartTime;',
      'end;',
      '',
      'procedure TCountDown.Start(Milliseconds: Int64);',
      'begin',
      '  Self.FLength := Milliseconds;',
      '  Self.FStartTime := Time();',
      '  Self.FFinishTime := Self.FStartTime + Milliseconds;',
      '  Self.FPauseTime := 0;',
      'end;',
      '',
      'procedure TCountDown.Extend(Milliseconds: Int64);',
      'begin',
      '  Self.FFinishTime += Milliseconds;',
      'end;',
      '',
      'property TCountDown.Remaining: Int64;',
      'begin',
      '  if (Self.FPauseTime > 0) then',
      '    Result := Max(Self.FFinishTime - Self.FPauseTime, 0)',
      '  else',
      '    Result := Max(Self.FFinishTime - Time(), 0);',
      'end;',
      '',
      'function TCountDown.RemainingFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Remaining, Format);',
      'end;',
      '',
      'property TCountDown.IsFinished: Boolean;',
      'begin',
      '  Result := (Self.FFinishTime > 0) and (Self.FPauseTime = 0) and (Self.Remaining = 0);',
      'end;',
      '',
      'property TCountDown.IsPaused: Boolean;',
      'begin',
      '  Result := Self.FPauseTime > 0;',
      'end;',
      '',
      'procedure TCountDown.Pause;',
      'begin',
      '  if not Self.IsPaused then',
      '    Self.FPauseTime := Time();',
      'end;',
      '',
      'procedure TCountDown.Resume;',
      'begin',
      '  if not Self.IsPaused then Exit;',
      '  Self.FFinishTime := Self.FFinishTime + (Time() - Self.FPauseTime);',
      '  Self.FPauseTime := 0;',
      'end;',
      '',
      'procedure TCountDown.Restart(Randomness: Integer = 0);',
      'begin',
      '  Self.FFinishTime := Time() + (Self.FLength + Random(-Randomness, Randomness));',
      '  Self.FPauseTime := 0;',
      'end;'],
      DumpSection);

    DumpSection := '';
  end;
end;

end.

