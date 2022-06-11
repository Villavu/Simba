unit simba.import_timing;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.datetime, simba.nativeinterface, simba.scriptthread;

procedure _LapePreciseSleep(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
end;

procedure _LapeSleep(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapeWait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapeGetTickCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := GetTickCount64();
end;

procedure _LapeConvertTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ConvertTime(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeConvertTime64(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ConvertTime64(PUInt64(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure _LapeGetTimeRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := Round(HighResolutionTime() - SimbaScriptThread.Script.RunningTime);
end;

procedure _LapePerformanceTimer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := SimbaNativeInterface.HighResolutionTime();
end;

procedure _LapeFormatMilliseconds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFormatMillisecondsEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := FormatMilliseconds(PDouble(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure ImportTiming(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Timing');

    addGlobalFunc('function GetTickCount: UInt64;', @_LapeGetTickCount);
    addGlobalFunc('procedure Sleep(MilliSeconds: UInt32);', @_LapeSleep);
    addGlobalFunc('procedure PreciseSleep(MilliSeconds: UInt32);', @_LapePreciseSleep);
    addGlobalFunc('procedure Wait(Milliseconds: UInt32)', @_LapeWait);

    addGlobalFunc('procedure ConvertTime(Time: Integer; var h, m, s: Integer)', @_LapeConvertTime);
    addGlobalFunc('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Integer)', @_LapeConvertTime64);
    addGlobalFunc('function GetTimeRunning: UInt64', @_LapeGetTimeRunning);
    addGlobalFunc('function PerformanceTimer: Double;', @_LapePerformanceTimer);
    addGlobalFunc('function FormatMilliseconds(Time: Double; Format: String): String; overload;', @_LapeFormatMilliseconds);
    addGlobalFunc('function FormatMilliseconds(Time: Double; TimeSymbols: Boolean = False): String; overload;', @_LapeFormatMillisecondsEx);

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
      'function TSimbaTimer.Elapsed: Integer;',
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

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTiming);

end.

