unit script_import_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, lpcompiler, lptypes, mufasatypes, simba.settings;

procedure Lape_SetSettingValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaSettings.Value[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure Lape_GetSettingValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaSettings.Value[PString(Params^[1])^];
end;

procedure Lape_GetSettingValueDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaSettings.Value[PString(Params^[1])^];
  if PString(Result)^ = '' then
    PString(Result)^ := PString(Params^[2])^;
end;

procedure Lape_ListSettings(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Params^[2])^ := SimbaSettings.Keys[PString(Params^[1])^];
end;

procedure Lape_DeleteSetting(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaSettings.Value[PString(Params^[1])^] := '';
end;

procedure Lape_SaveSettings(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaSettings.Save();
end;

procedure Lape_Import_Settings(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function GetSettingValue(Path: String): String;', @Lape_GetSettingValue, Data);
    addGlobalMethod('function GetSettingValueDef(Path, DefaultValue: String): String;', @Lape_GetSettingValueDef, Data);
    addGlobalMethod('procedure SetSettingValue(Path, Value: String);', @Lape_SetSettingValue, Data);
    addGlobalMethod('procedure ListSettings(Path: String; var Settings: TStringArray);', @Lape_ListSettings, Data);
    addGlobalMethod('procedure DeleteSetting(Path: String);', @Lape_DeleteSetting, Data);
    addGlobalMethod('procedure SaveSettings;', @Lape_SaveSettings, Data);
  end;
end;

initialization
  ScriptImports.Add('Settings', @Lape_Import_Settings);

end.

