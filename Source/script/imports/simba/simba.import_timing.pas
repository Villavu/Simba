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

    addGlobalFunc('procedure PreciseSleep(Milliseconds: UInt32);', @_LapePreciseSleep);
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
    ]);

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

