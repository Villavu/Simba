Unit Rutis_INI_Settings;

Interface

Uses
  SysUtils, IniFiles, registry, Rutis_Engine;

Var
  Rutis_INI  : TCustomIniFile;

Procedure RINISettings_Open(AppPath, ScriptPath : String);
Function RINISettings_OpenReg : Boolean;
Procedure RINISettings_OpenIni(FileName : String);
Procedure RINISettings_ApplyMainSettings(Engine : TRutisEngine);

Implementation

Procedure RINISettings_Open(AppPath, ScriptPath : String);
Begin
  If FileExists(ScriptPath + '\RutisSettings.ini') Then
  Begin
    RINISettings_OpenIni(ScriptPath + '\RutisSettings.ini');
    exit;
  End;

  If RINISettings_OpenReg Then exit;

  SetCurrentDir(AppPath);
  If FileExists('.\Rutis_Settings.ini') Then
  Begin
    RINISettings_OpenIni('.\Rutis_Settings.ini');
    exit;
  End;

  If FileExists('..\Rutis_Settings.ini') Then
  Begin
    RINISettings_OpenIni('..\Rutis_Settings.ini');
    exit;
  End;

  RINISettings_OpenReg;
End;

Function RINISettings_OpenReg : Boolean;
Begin
  Rutis_INI.Free;
  Rutis_INI := TRegistryIniFile.Create('Software\RUTIS');
  Result    := Rutis_INI.SectionExists('');
End;

Procedure RINISettings_OpenIni(FileName : String);
Begin
  Rutis_INI.Free;
  Rutis_INI := TIniFile.Create(FileName);
End;

Procedure RINISettings_ApplyMainSettings(Engine : TRutisEngine);
Var
  i    : Integer;
  Str  : String;
Begin
  If Rutis_INI = nil Then
    Rutis_INI := TRegistryIniFile.Create('Software\RUTIS');

  //Libary Paths
  SetLength(Engine.UnitFileManager.Paths, 0);
  i := 0;
  While True Do
  Begin
    Str := Rutis_INI.ReadString('Compiler\Libary', 'Path' + IntToStr(i), '');
    If Str = '' Then break;
    Engine.UnitFileManager.AddPath(Str);
    Inc(i);
  End;

  //Compiler Settings
  Engine.Compiler.optArrangeFields := Rutis_INI.ReadBool('Compiler', 'ArrangeRecordFields', True);
  Engine.Compiler.optArrangeSize   := Rutis_INI.ReadInteger('Compiler', 'RecordArrangeSize', 8);

  Engine.OptProcessTimerCount := Rutis_INI.ReadInteger('VM', 'ProcessTickCount', 100000);

  //SAVE SETTINGS
  Rutis_INI.WriteBool('Compiler', 'ArrangeRecordFields', Engine.Compiler.optArrangeFields);
  Rutis_INI.WriteInteger('Compiler', 'RecordArrangeSize', Engine.Compiler.optArrangeSize);

  Rutis_INI.WriteInteger('VM', 'ProcessTickCount', Engine.OptProcessTimerCount);
End;

Initialization
Finalization
  Rutis_INI.Free;
End.

