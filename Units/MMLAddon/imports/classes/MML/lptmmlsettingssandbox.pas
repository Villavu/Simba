unit lpTMMLSettingsSandbox;
//Depends: TMMLSettingsSandbox, TObject, TMMLSettings, String, string, TStringArray

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, script_imports;

procedure Register_TMMLSettingsSandbox(Compiler: TLapeCompiler);

implementation

uses
  settings, settingssandbox, lplclsystem;

type
  PMMLSettings = ^TMMLSettings;
  PMMLSettingsSandbox = ^TMMLSettingsSandbox;

//constructor Create(sett: TMMLSettings);
procedure TMMLSettingsSandbox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLSettingsSandbox(Params^[0])^ := TMMLSettingsSandbox.Create(PMMLSettings(Params^[1])^);
end;

//function IsKey(const KeyName: String): Boolean;
procedure TMMLSettingsSandbox_IsKey(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.IsKey(PlpString(Params^[1])^);
end;

//function IsDirectory(const KeyName: String): Boolean;
procedure TMMLSettingsSandbox_IsDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.IsDirectory(PlpString(Params^[1])^);
end;

//function SetKeyValue(const Keyname, Value : string) : boolean;
procedure TMMLSettingsSandbox_SetKeyValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.SetKeyValue(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function GetKeyValue(const KeyName: String): String;
procedure TMMLSettingsSandbox_GetKeyValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMMLSettingsSandbox(Params^[0])^.GetKeyValue(PlpString(Params^[1])^);
end;

//function GetKeyValueDef(const KeyName, defVal: String): String;
procedure TMMLSettingsSandbox_GetKeyValueDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMMLSettingsSandbox(Params^[0])^.GetKeyValueDef(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function ListKeys(const KeyName: String; out Keys : TStringArray): boolean;
procedure TMMLSettingsSandbox_ListKeys(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.ListKeys(PlpString(Params^[1])^, PStringArray(Params^[2])^);
end;

//function DeleteKey(const KeyName: String): Boolean;
procedure TMMLSettingsSandbox_DeleteKey(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.DeleteKey(PlpString(Params^[1])^);
end;

//function DeleteSubKeys(const KeyName: String): Boolean;
procedure TMMLSettingsSandbox_DeleteSubKeys(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMMLSettingsSandbox(Params^[0])^.DeleteSubKeys(PlpString(Params^[1])^);
end;

//Read: property prefix : string read GetPrefix write SetPrefix;
procedure TMMLSettingsSandbox_prefix_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMMLSettingsSandbox(Params^[0])^.prefix;
end;

//Write: property prefix : string read GetPrefix write SetPrefix;
procedure TMMLSettingsSandbox_prefix_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLSettingsSandbox(Params^[0])^.prefix := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TMMLSettingsSandbox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMMLSettingsSandbox(Params^[0])^.Free();
end;

procedure Register_TMMLSettingsSandbox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMMLSettingsSandbox', 'TObject');

    addGlobalFunc('function TMMLSettingsSandbox.IsKey(const KeyName: String): Boolean; constref;', @TMMLSettingsSandbox_IsKey);
    addGlobalFunc('function TMMLSettingsSandbox.IsDirectory(const KeyName: String): Boolean; constref;', @TMMLSettingsSandbox_IsDirectory);
    addGlobalFunc('function TMMLSettingsSandbox.SetKeyValue(const Keyname, Value : string): boolean; constref;', @TMMLSettingsSandbox_SetKeyValue);
    addGlobalFunc('function TMMLSettingsSandbox.GetKeyValue(const KeyName: String): String; constref;', @TMMLSettingsSandbox_GetKeyValue);
    addGlobalFunc('function TMMLSettingsSandbox.GetKeyValueDef(const KeyName, defVal: String): String; constref;', @TMMLSettingsSandbox_GetKeyValueDef);
    addGlobalFunc('function TMMLSettingsSandbox.ListKeys(const KeyName: String; out Keys : TStringArray): boolean; constref;', @TMMLSettingsSandbox_ListKeys);
    addGlobalFunc('function TMMLSettingsSandbox.DeleteKey(const KeyName: String): Boolean; constref;', @TMMLSettingsSandbox_DeleteKey);
    addGlobalFunc('function TMMLSettingsSandbox.DeleteSubKeys(const KeyName: String): Boolean; constref;', @TMMLSettingsSandbox_DeleteSubKeys);
    addClassVar('TMMLSettingsSandbox', 'prefix', 'string', @TMMLSettingsSandbox_prefix_Read, @TMMLSettingsSandbox_prefix_Write);
  end;
end;

end.

