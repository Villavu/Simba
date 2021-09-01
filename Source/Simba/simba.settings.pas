unit simba.settings;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils, inifiles, lazmethodlist, variants;

const
  SETTINGS_VERSION = 1; // Update if settings become incompatible!

type
  TSimbaSettings = class;
  TSimbaSetting = class
  protected
    FSettings: TSimbaSettings;
    FSection: String;
    FName: String;
    FValue: Variant;
    FDefaultValue: Variant;

    procedure SetValue(AValue: Variant);
  public
    constructor Create(ASettings: TSimbaSettings; ASection, AName: String; DefaultValue: Variant);

    property DefaultValue: Variant read FDefaultValue;
    property Value: Variant read FValue write SetValue;
  end;

  TSimbaSetting_Boolean      = class(TSimbaSetting);
  TSimbaSetting_String       = class(TSimbaSetting);
  TSimbaSetting_BinaryString = class(TSimbaSetting);
  TSimbaSetting_Integer      = class(TSimbaSetting);

  TSimbaSettingChangedEvent = procedure(Setting: TSimbaSetting) of object;

  TSimbaSettings = class
  protected
    FFileName: String;
    FChangeEventList: TMethodList;
    FSettingsArray: array of TSimbaSetting;
  public
    Settings: record
      Version: TSimbaSetting;
    end;

    GUI: record
      ConsoleVisible: TSimbaSetting;
      TrayIconVisible: TSimbaSetting;
      Layout: TSimbaSetting;
      LockLayout: TSimbaSetting;
      Notes: TSimbaSetting;
      RecentFiles: TSimbaSetting;
      CustomFontSize: TSimbaSetting;
      ToolbarSize: TSimbaSetting;
      ColorPickerHistory: TSimbaSetting;
    end;

    Editor: record
      DefaultScript: TSimbaSetting;
      CustomColors: TSimbaSetting;
      FontSize: TSimbaSetting;
      FontName: TSimbaSetting;
      RightMargin: TSimbaSetting;
      RightMarginVisible: TSimbaSetting;
      AntiAliased: TSimbaSetting;
      AllowCaretPastEOL: TSimbaSetting;
      IgnoreCodeToolsIDEDirective: TSimbaSetting;
      AutomaticallyOpenAutoCompletion: TSimbaSetting;
      AutomaticallyShowParameterHints: TSimbaSetting;
    end;

    Environment: record
      FirstLaunch: TSimbaSetting;
      OpenSSLOnLaunch: TSimbaSetting;
      LibCryptoHash: TSimbaSetting;
      LibSSLHash: TSimbaSetting;
    end;

    procedure RegisterChangeHandler(Event: TSimbaSettingChangedEvent);
    procedure UnRegisterChangeHandler(Event: TSimbaSettingChangedEvent);

    procedure Changed(Setting: TSimbaSetting);

    procedure Validate;
    procedure Load;
    procedure Save;

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

var
  SimbaSettings: TSimbaSettings;

implementation

uses
  LazLoggerBase,
  simba.stringutil, simba.files;

procedure TSimbaSetting.SetValue(AValue: Variant);
type
  TVarIsFunction = function(const V: Variant): Boolean;

  procedure AssertType(CheckType: TVarIsFunction);
  begin
    if not CheckType(AValue) then
      raise Exception.Create('Invalid setting value for ' + FName + ' Expected ' + ClassName);
  end;

begin
  if (Self is TSimbaSetting_String) or (Self is TSimbaSetting_BinaryString) then
    AssertType(@VarIsStr);
  if (Self is TSimbaSetting_Boolean) then
    AssertType(@VarIsBool);
  if (Self is TSimbaSetting_Integer) then
    AssertType(@VarIsOrdinal);

  if (AValue = FValue) then
    Exit;

  FValue := AValue;
  FSettings.Changed(Self);
end;

constructor TSimbaSetting.Create(ASettings: TSimbaSettings; ASection, AName: String; DefaultValue: Variant);
begin
  if (Self.ClassType = TSimbaSetting) then
    raise Exception.Create('Abstract setting!');

  FSettings := ASettings;
  FSettings.FSettingsArray += [Self];

  FSection := ASection;
  FName := AName;
  FDefaultValue := DefaultValue;
  FValue := FDefaultValue;
end;

procedure TSimbaSettings.Changed(Setting: TSimbaSetting);
var
  i: Integer;
begin
  if (FChangeEventList.Count = 0) then
    Exit;

  DebugLn('Setting changed: ', Setting.FName);

  i := FChangeEventList.Count;
  while FChangeEventList.NextDownIndex(i) do
    TSimbaSettingChangedEvent(FChangeEventList.Items[i])(Setting);
end;

procedure TSimbaSettings.RegisterChangeHandler(Event: TSimbaSettingChangedEvent);
begin
  FChangeEventList.Add(TMethod(Event));
end;

procedure TSimbaSettings.UnRegisterChangeHandler(Event: TSimbaSettingChangedEvent);
begin
  FChangeEventList.Remove(TMethod(Event));
end;

procedure TSimbaSettings.Validate;
var
  INI: TMemIniFile;
begin
  INI := TMemIniFile.Create(FFileName, [ifoWriteStringBoolean]);
  if INI.ReadInteger('Settings', 'Version', 0) <> SETTINGS_VERSION then
    INI.Clear();
  INI.Free();
end;

procedure TSimbaSettings.Load;
var
  INI: TIniFile;
  Setting: TSimbaSetting;
begin
  Validate();

  INI := TIniFile.Create(FFileName, [ifoWriteStringBoolean]);
  INI.SetBoolStringValues(True, ['True']);
  INI.SetBoolStringValues(False, ['False']);

  for Setting in FSettingsArray do
  begin
    if not INI.ValueExists(Setting.FSection, Setting.FName) then
      Continue;

    if (Setting is TSimbaSetting_Boolean) then
      Setting.Value := INI.ReadBool(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_Integer) then
      Setting.Value := INI.ReadInteger(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_String) then
      Setting.Value := INI.ReadString(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_BinaryString) then
    begin
      Setting.FValue := INI.ReadString(Setting.FSection, Setting.FName, Setting.Value);
      if (Setting.FValue <> '') then
        Setting.FValue := Base64Decode(Setting.Value);
    end;
  end;

  INI.Free();
end;

procedure TSimbaSettings.Save;
var
  INI: TIniFile;
  Setting: TSimbaSetting;
begin
  INI := TIniFile.Create(FFileName, [ifoWriteStringBoolean]);
  INI.CacheUpdates := True;
  INI.SetBoolStringValues(True, ['True']);
  INI.SetBoolStringValues(False, ['False']);

  Environment.FirstLaunch.Value := False;

  for Setting in FSettingsArray do
  begin
    if (Setting is TSimbaSetting_Boolean) then
      INI.WriteBool(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_Integer) then
      INI.WriteInteger(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_String) then
      INI.WriteString(Setting.FSection, Setting.FName, Setting.Value);

    if (Setting is TSimbaSetting_BinaryString) then
      INI.WriteString(Setting.FSection, Setting.FName, Base64Encode(Setting.Value));
  end;

  try
    INI.UpdateFile();
  except
  end;

  INI.Free();
end;

constructor TSimbaSettings.Create(FileName: String);
begin
  inherited Create();

  FFileName := ExpandFileName(FileName);
  FChangeEventList := TMethodList.Create();

  Settings.Version := TSimbaSetting_Integer.Create(Self, 'Config', 'Version', SETTINGS_VERSION);

  // Environment
  Environment.FirstLaunch := TSimbaSetting_Boolean.Create(Self, 'Environment', 'FirstLaunch', True);
  Environment.OpenSSLOnLaunch := TSimbaSetting_Boolean.Create(Self, 'Environment', 'OpenSSLOnLaunch', True);
  Environment.LibCryptoHash := TSimbaSetting_String.Create(Self, 'Environment', 'LibCryptoHash', '');
  Environment.LibSSLHash := TSimbaSetting_String.Create(Self, 'Environment', 'LibSSLHash', '');

  // GUI
  GUI.ConsoleVisible := TSimbaSetting_Boolean.Create(Self, 'GUI', 'ConsoleVisible', True);
  GUI.TrayIconVisible := TSimbaSetting_Boolean.Create(Self, 'GUI', 'TrayIconVisible', True);

  GUI.LockLayout := TSimbaSetting_Boolean.Create(Self, 'GUI', 'LockLayout', False);
  GUI.Layout := TSimbaSetting_BinaryString.Create(Self, 'GUI', 'Layout', '');

  GUI.Notes := TSimbaSetting_BinaryString.Create(Self, 'GUI', 'Notes', '');
  GUI.RecentFiles := TSimbaSetting_BinaryString.Create(Self, 'GUI', 'RecentFiles', '');

  GUI.CustomFontSize := TSimbaSetting_Integer.Create(Self, 'GUI', 'CustomFontSize', {$IFDEF WINDOWS}11{$ELSE}0{$ENDIF});
  GUI.ToolbarSize := TSimbaSetting_Integer.Create(Self, 'GUI', 'ToolbarSize', 24);

  GUI.ColorPickerHistory := TSimbaSetting_BinaryString.Create(Self, 'GUI', 'ColorPickerHistory', '');

  // Editor
  Editor.DefaultScript := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DefaultScript', 'program new;' + LineEnding + 'begin' + LineEnding + 'end.');

  Editor.CustomColors := TSimbaSetting_String.Create(Self, 'Editor', 'CustomColors', '');

  Editor.FontSize := TSimbaSetting_Integer.Create(Self, 'Editor', 'FontSize', {$IFDEF WINDOWS}13{$ELSE}0{$ENDIF});
  Editor.FontName := TSimbaSetting_String.Create(Self, 'Editor', 'FontName', {$IFDEF WINDOWS}'Consolas'{$ELSE}''{$ENDIF});

  Editor.AntiAliased := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AntiAliased', True);

  Editor.IgnoreCodeToolsIDEDirective := TSimbaSetting_Boolean.Create(Self, 'Editor', 'IgnoreCodeToolsIDEDirective', False);

  Editor.AllowCaretPastEOL := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AllowCaretPastEOL', True);

  Editor.AutomaticallyOpenAutoCompletion := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyOpenAutoCompletion', True);
  Editor.AutomaticallyShowParameterHints := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyShowParameterHints', True);

  Editor.RightMargin := TSimbaSetting_Integer.Create(Self, 'Editor', 'RightMargin', 80);
  Editor.RightMarginVisible := TSimbaSetting_Boolean.Create(Self, 'Editor', 'RightMarginVisible', False);

  Load();
end;

destructor TSimbaSettings.Destroy;
var
  Setting: TSimbaSetting;
begin
  if (FChangeEventList <> nil) then
    FreeAndNil(FChangeEventList);

  for Setting in FSettingsArray do
    if (Setting <> nil) then
      Setting.Free();
  FSettingsArray := nil;

  inherited Destroy();
end;

initialization
  DebugLn('simba.settings :: Initialization');

  SimbaSettings := TSimbaSettings.Create(GetDataPath() + 'settings.ini');

finalization
  DebugLn('simba.settings :: Finalization');

  SimbaSettings.Save();
  SimbaSettings.Free();
  SimbaSettings := nil;

end.

