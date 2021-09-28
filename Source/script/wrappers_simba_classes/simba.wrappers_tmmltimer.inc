procedure _LapeMMLTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^ := TMMLTimer.Create;
end;

procedure _LapeMMLTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Free();
end;

procedure _LapeMMLTimer_On(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.On;
end;

procedure _LapeMMLTimer_Off(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Off;
end;

procedure _LapeMMLTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLTimer(Params^[0])^.Enabled;
end;

procedure _LapeMMLTimer_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeMMLTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMMLTimer(Params^[0])^.Interval;
end;

procedure _LapeMMLTimer_Interval_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Interval := PInteger(Params^[1])^;
end;

procedure _LapeMMLTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PMMLTimer(Params^[0])^.OnTimer;
end;

procedure _LapeMMLTimer_OnTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

