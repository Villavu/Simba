unit simba.import_simba;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.scriptthread, simba.mufasatypes, simba.settings;

procedure _LapeClearDebug(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaDebugLn(ESimbaDebugLn.CLEAR, '');
end;

procedure _LapeDisguise(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('Disguise requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.Disguse(PString(Params^[0])^);
end;

procedure _LapeStatus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('Status requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.Status(PString(Params^[0])^);
end;

procedure _LapeGetSimbaPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('GetSimbaPID requires Simba communication');

  PPtrUInt(Result)^ := SimbaScriptThread.Script.SimbaCommunication.GetSimbaPID();
end;

procedure _LapeGetSimbaTargetPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('GetSimbaTargetPID requires Simba communication');

  PPtrUInt(Result)^ := SimbaScriptThread.Script.SimbaCommunication.GetSimbaTargetPID();
end;

procedure _LapeGetSimbaTargetWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('GetSimbaTargetWindow requires Simba communication');

  PPtrUInt(Result)^ := SimbaScriptThread.Script.SimbaCommunication.GetSimbaTargetWindow();
end;

procedure _LapeGetSimpleSetting(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SimbaSettings.GetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeSetSimpleSetting(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaSettings.SetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure ImportSimba(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Simba';

    addGlobalFunc('procedure ClearDebug', @_LapeClearDebug);

    addGlobalFunc('procedure SetSimbaStatus(Status: String)', @_LapeStatus);
    addGlobalFunc('procedure SetSimbaTitle(Title: String)', @_LapeDisguise);
    addGlobalFunc('function GetSimbaPID: PtrUInt', @_LapeGetSimbaPID);
    addGlobalFunc('function GetSimbaTargetPID: PtrUInt', @_LapeGetSimbaTargetPID);
    addGlobalFunc('function GetSimbaTargetWindow: TWindowHandle', @_LapeGetSimbaTargetWindow);

    addGlobalFunc('function SetSimbaSetting(Name: String; DefValue: String = ""): String', @_LapeGetSimpleSetting);
    addGlobalFunc('procedure GetSimbaSetting(Name, Value: String);', @_LapeSetSimpleSetting);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSimba);

end.

