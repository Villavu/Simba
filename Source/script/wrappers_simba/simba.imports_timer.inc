pushSection('Timing');

addGlobalFunc('function GetTickCount: UInt64;', @_LapeGetTickCount);
addGlobalFunc('procedure Sleep(MilliSeconds: UInt32);', @_LapeSleep);
addGlobalFunc('procedure PreciseSleep(MilliSeconds: UInt32);', @_LapePreciseSleep);
addGlobalFunc('procedure Wait(Milliseconds: UInt32)', @_LapeWait);

addGlobalFunc('procedure ConvertTime(Time: Integer; var h, m, s: Integer)', @_LapeConvertTime);
addGlobalFunc('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Integer)', @_LapeConvertTime64);
addGlobalFunc('function GetTimeRunning: UInt64', @_LapeGetTimeRunning);
addGlobalFunc('function PerformanceTimer: Double;', @_LapePerformanceTimer);
addGlobalFunc('function FormatMilliseconds(Milliseconds: Int64; Format: String): String; overload;', @_LapeFormatMillisecondsI);
addGlobalFunc('function FormatMilliseconds(Milliseconds: Double; Format: String): String; overload;', @_LapeFormatMillisecondsF);

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
  'function TSimbaTimer.ElapsedStr(Format: String = "ms"): String;',
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
  'function TSimbaPerformanceTimer.ElapsedStr(Format: String = "ms"): String;',
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

