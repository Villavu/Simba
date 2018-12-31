unit script_import_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes;

procedure Lape_SetSettingValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.SetKeyValue(Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_KeyIsSetting(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.IsKey(PString(Params^[1])^);
end;

procedure Lape_KeyIsDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.IsDirectory(PString(Params^[1])^);
end;

procedure Lape_GetSettingValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TMMLScriptThread(Params^[0]).Settings.GetKeyValue(PString(Params^[1])^);
end;

procedure Lape_GetSettingValueDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TMMLScriptThread(Params^[0]).Settings.GetKeyValueDef(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_ListSettings(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.ListKeys(PString(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure Lape_DeleteSetting(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.DeleteKey(PString(Params^[1])^);
end;

procedure Lape_DeleteSubSettings(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Settings.DeleteSubKeys(PString(Params^[1])^);
end;

procedure Lape_Import_Settings(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function SetSettingValue(Key, Value: String): boolean', @Lape_SetSettingValue, Data);
    addGlobalMethod('function KeyIsSetting(Key: String): Boolean', @Lape_KeyIsSetting, Data);
    addGlobalMethod('function KeyIsDirectory(Key: String): Boolean', @Lape_KeyIsDirectory, Data);
    addGlobalMethod('function GetSettingValue(Key: String): String', @Lape_GetSettingValue, Data);
    addGlobalMethod('function GetSettingValueDef(Key, DefaultValue: String): String', @Lape_GetSettingValueDef, Data);
    addGlobalMethod('function ListSettings(Key: String; var Settings: TStringArray): Boolean', @Lape_ListSettings, Data);
    addGlobalMethod('function DeleteSetting(Key: String): Boolean', @Lape_DeleteSetting, Data);
    addGlobalMethod('function DeleteSubSettings(Key: String): Boolean', @Lape_DeleteSubSettings, Data);
  end;
end;

initialization
  ScriptImports.Add('Settings', @Lape_Import_Settings);

end.

