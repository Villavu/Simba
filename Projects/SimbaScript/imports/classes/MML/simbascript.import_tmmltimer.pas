unit simbascript.import_tmmltimer;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Register_TMMLTimer(Compiler: TScriptCompiler);

implementation

uses
  simba.mmltimer;

type
  PMMLTimer = ^TMMLTimer;
  PThreadPriority = ^TThreadPriority;
  PNotifyEvent = ^TNotifyEvent;

procedure TMMLTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^ := TMMLTimer.Create;
end;

procedure TMMLTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Free();
end;

procedure TMMLTimer_On(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.On;
end;

procedure TMMLTimer_Off(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Off;
end;

procedure TMMLTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLTimer(Params^[0])^.Enabled;
end;

procedure TMMLTimer_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure TMMLTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMMLTimer(Params^[0])^.Interval;
end;

procedure TMMLTimer_Interval_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.Interval := PInteger(Params^[1])^;
end;

procedure TMMLTimer_ThreadPriority_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PThreadPriority(Result)^ := PMMLTimer(Params^[0])^.ThreadPriority;
end;

procedure TMMLTimer_ThreadPriority_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.ThreadPriority := PThreadPriority(Params^[1])^;
end;

procedure TMMLTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PMMLTimer(Params^[0])^.OnTimer;
end;

procedure TMMLTimer_OnTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

procedure Register_TMMLTimer(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    addClass('TMMLTimer');

    addGlobalType('(tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,tpTimeCritical)', 'TThreadPriority');

    addClassVar('TMMLTimer', 'Enabled', 'Boolean', @TMMLTimer_Enabled_Read, @TMMLTimer_Enabled_Write);
    addClassVar('TMMLTimer', 'Interval', 'Integer', @TMMLTimer_Interval_Read, @TMMLTimer_Interval_Write);
    addClassVar('TMMLTimer', 'ThreadPriority', 'TThreadPriority', @TMMLTimer_ThreadPriority_Read, @TMMLTimer_ThreadPriority_Write);
    addClassVar('TMMLTimer', 'OnTimer', 'TNotifyEvent', @TMMLTimer_OnTimer_Read, @TMMLTimer_OnTimer_Write);
    addGlobalFunc('procedure TMMLTimer.On(); constref;', @TMMLTimer_On);
    addGlobalFunc('procedure TMMLTimer.Off(); constref;', @TMMLTimer_Off);
    addGlobalFunc('procedure TMMLTimer.Init();', @TMMLTimer_Init);
    addGlobalFunc('procedure TMMLTimer.Free(); constref;', @TMMLTimer_Free);
  end;
end;

initialization
  RegisterScriptImport(@Register_TMMLTimer);

end.

