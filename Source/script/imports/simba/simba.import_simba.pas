unit simba.import_simba;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSimba(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.settings;

procedure ClearScriptOutput(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaDebugLn([EDebugLn.CLEAR], '');
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

    addGlobalFunc('procedure ClearScriptOutput', @ClearScriptOutput);

    addGlobalFunc(
      'procedure SetSimbaStatus(S: String);', [
      'begin',
      '  _SimbaScript.SetSimbaStatus(S);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure SetSimbaTitle(S: String);', [
      'begin',
      '  _SimbaScript.SetSimbaTitle(S);',
      'end;'
    ]);

    addGlobalFunc(
      'function GetSimbaPID: TProcessID;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaPID();',
      'end;'
    ]);
    addGlobalFunc(
      'function GetSimbaTargetPID: TProcessID;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaTargetPID();',
      'end;'
    ]);
    addGlobalFunc(
      'function GetSimbaTargetWindow: TWindowHandle;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaTargetWindow();',
      'end;'
    ]);

    addGlobalFunc('function SetSimbaSetting(Name: String; DefValue: String = ""): String', @_LapeGetSimpleSetting);
    addGlobalFunc('procedure GetSimbaSetting(Name, Value: String);', @_LapeSetSimpleSetting);

    ImportingSection := '';
  end;
end;

end.

