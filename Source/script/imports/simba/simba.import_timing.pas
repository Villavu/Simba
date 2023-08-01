unit simba.import_timing;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportTiming(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.datetime, simba.nativeinterface;

procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
end;

procedure _LapeWait(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapeConvertTime(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ConvertTime(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeConvertTime64(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ConvertTime64(PUInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapePerformanceTimer(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := SimbaNativeInterface.HighResolutionTime();
end;

procedure _LapeFormatMilliseconds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFormatMillisecondsEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure ImportTiming(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Timing';

    addGlobalFunc('procedure PreciseSleep(MilliSeconds: UInt32);', @_LapePreciseSleep);
    addGlobalFunc('procedure Wait(Milliseconds: UInt32)', @_LapeWait);

    addGlobalFunc('procedure ConvertTime(Time: Integer; var h, m, s: Integer)', @_LapeConvertTime);
    addGlobalFunc('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Integer)', @_LapeConvertTime64);
    addGlobalFunc('function PerformanceTimer: Double;', @_LapePerformanceTimer);
    addGlobalFunc('function FormatMilliseconds(Time: Double; Format: String): String; overload;', @_LapeFormatMilliseconds);
    addGlobalFunc('function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;', @_LapeFormatMillisecondsEx);

    addDelayedCode([
      'const',
      '  ScriptStartTime: UInt64 := GetTickCount();',
      '',
      'function GetTimeRunning: UInt64;',
      'begin',
      '  Result := GetTickCount() - ScriptStartTime;',
      'end;'
    ], 'Timing');

    addDelayedCode([
      'type',
      '  TSimbaTimer = record',
      '    Name: String;',
      '    StartTime: UInt64;',
      '    StopTime: UInt64;',
      '  end;',
      '',
      'procedure TSimbaTimer.Start(AName: String = "");',
      'begin',
      '  Self.Name := AName;',
      '  Self.StartTime := GetTickCount();',
      'end;',
      '',
      'procedure TSimbaTimer.Stop;',
      'begin',
      '  Self.StopTime := GetTickCount();',
      'end;',
      '',
      'function TSimbaTimer.Elapsed: UInt64;',
      'begin',
      '  if (Self.StopTime > 0) then',
      '    Result := Self.StopTime - Self.StartTime',
      '  else',
      '    Result := GetTickCount() - Self.StartTime;',
      'end;',
      '',
      'function TSimbaTimer.ElapsedFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Elapsed(), Format);',
      'end;',
      '',
      'procedure TSimbaTimer.Reset;',
      'begin',
      '  Self.StopTime := Self.StartTime := 0;',
      'end;'
    ],'Timers');

    addDelayedCode([
      'type',
      '  TSimbaPerformanceTimer = record',
      '    Name: String;',
      '    StartTime: Double;',
      '    StopTime: Double;',
      '  end;',
      '',
      'procedure TSimbaPerformanceTimer.Start(AName: String = "");',
      'begin',
      '  Self.Name := AName;',
      '  Self.StartTime := PerformanceTimer();',
      'end;',
      '',
      'procedure TSimbaPerformanceTimer.Stop;',
      'begin',
      '  Self.StopTime := PerformanceTimer();',
      'end;',
      '',
      'function TSimbaPerformanceTimer.Elapsed: Double;',
      'begin',
      '  if (Self.StopTime > 0) then',
      '    Result := Self.StopTime - Self.StartTime',
      '  else',
      '    Result := PerformanceTimer() - Self.StartTime;',
      'end;',
      '',
      'function TSimbaPerformanceTimer.ElapsedFmt(Format: String = "u"): String;',
      'begin',
      '  Result := FormatMilliseconds(Self.Elapsed(), Format);',
      'end;',
      '',
      'procedure TSimbaPerformanceTimer.Reset;',
      'begin',
      '  Self.StopTime := Self.StartTime := 0;',
      'end;'
    ],'Timers');

    ImportingSection := '';
  end;
end;

end.

