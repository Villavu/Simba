{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settings;

{$i simba.inc}

interface

uses
  classes, sysutils, inifiles, lazmethodlist, variants, fgl;

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

    function GetName: String;
    procedure SetValue(AValue: Variant);
  public
    constructor Create(ASettings: TSimbaSettings; ASection, AName: String; DefaultValue: Variant);

    property DefaultValue: Variant read FDefaultValue;
    property Value: Variant read FValue write SetValue;
    property Name: String read GetName;
  end;

  TSimbaSetting_Boolean      = class(TSimbaSetting);
  TSimbaSetting_String       = class(TSimbaSetting);
  TSimbaSetting_BinaryString = class(TSimbaSetting);
  TSimbaSetting_Integer      = class(TSimbaSetting);

  TSimbaSettingList = specialize TFPGObjectList<TSimbaSetting>;
  TSimbaSettingChangedEvent = procedure(Setting: TSimbaSetting) of object;

  TSimbaSettings = class
  protected
    FFileName: String;
    FChangeEventList: TMethodList;
    FList: TSimbaSettingList;
    FFirstLaunch: Boolean;

    procedure Load;
    procedure Save;
  public
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
      MacOSKeystrokes: TSimbaSetting;

      OutputFontName: TSimbaSetting;
      OutputFontSize: TSimbaSetting;
      OutputFontAntiAliased: TSimbaSetting;
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
      AutomaticallyCompleteBegin: TSimbaSetting;
      AutomaticallyCompleteParentheses: TSimbaSetting;
      AutomaticallyCompleteIndex: TSimbaSetting;
    end;

    Environment: record
      OpenSSLOnLaunch: TSimbaSetting;
      LibCryptoHash: TSimbaSetting;
      LibSSLHash: TSimbaSetting;
    end;

    property FirstLaunch: Boolean read FFirstLaunch;

    procedure RegisterChangeHandler(Event: TSimbaSettingChangedEvent);
    procedure UnRegisterChangeHandler(Event: TSimbaSettingChangedEvent);

    procedure Changed(Setting: TSimbaSetting);

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

var
  SimbaSettings: TSimbaSettings;

implementation

uses
  lazloggerbase, synedit,
  simba.stringutil, simba.files;

function TSimbaSetting.GetName: String;
begin
  Result := FSection + '.' + FName;
end;

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
  FSettings.FList.Add(Self);

  FSection := ASection;
  FName := AName;
  FDefaultValue := DefaultValue;
  FValue := FDefaultValue;
end;

procedure TSimbaSettings.Changed(Setting: TSimbaSetting);
var
  i: Integer;
begin
  if (FChangeEventList = nil) or (FChangeEventList.Count = 0) then
    Exit;

  DebugLn('Setting changed: ', Setting.Name);

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

procedure TSimbaSettings.Load;
var
  INI: TIniFile;
  Setting: TSimbaSetting;
begin
  if FFirstLaunch then
    Exit;

  INI := TIniFile.Create(FFileName, [ifoWriteStringBoolean]);
  try
    INI.SetBoolStringValues(True, ['True']);
    INI.SetBoolStringValues(False, ['False']);

    if (INI.ReadInteger('Settings', 'Version', 0) <> SETTINGS_VERSION) then
    begin
      RenameFile(FFileName, FFileName + '.old');
      Exit;
    end;

    for Setting in FList do
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
  finally
    INI.Free();
  end;
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
  INI.WriteInteger('Settings', 'Version', SETTINGS_VERSION);

  for Setting in FList do
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
  FFirstLaunch := not FileExists(FFileName);
  FList := TSimbaSettingList.Create();
  FChangeEventList := TMethodList.Create();

  // Environment
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
  GUI.CustomFontSize := TSimbaSetting_Integer.Create(Self, 'GUI', 'CustomFontSize', 0);
  GUI.ToolbarSize := TSimbaSetting_Integer.Create(Self, 'GUI', 'ToolbarSize', 24);
  GUI.ColorPickerHistory := TSimbaSetting_BinaryString.Create(Self, 'GUI', 'ColorPickerHistory', '');
  GUI.MacOSKeystrokes := TSimbaSetting_Boolean.Create(Self, 'GUI', 'MacOSKeystrokes', {$IFDEF DARWIN}True{$ELSE}False{$ENDIF});

  GUI.OutputFontSize := TSimbaSetting_Integer.Create(Self, 'GUI', 'OutputFontSize', SynDefaultFontSize + 1);
  GUI.OutputFontName := TSimbaSetting_String.Create(Self, 'GUI', 'OutputFontName', {$IFDEF WINDOWS}'Consolas'{$ELSE}''{$ENDIF});
  GUI.OutputFontAntiAliased := TSimbaSetting_Boolean.Create(Self, 'GUI', 'OutputFontAntiAliased', True);

  // Editor
  Editor.DefaultScript := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DefaultScript', 'program new;' + LineEnding + 'begin' + LineEnding + 'end.');
  Editor.CustomColors := TSimbaSetting_String.Create(Self, 'Editor', 'CustomColors', '');
  Editor.FontSize := TSimbaSetting_Integer.Create(Self, 'Editor', 'FontSize', SynDefaultFontSize + 1);
  Editor.FontName := TSimbaSetting_String.Create(Self, 'Editor', 'FontName', {$IFDEF WINDOWS}'Consolas'{$ELSE}''{$ENDIF});
  Editor.AntiAliased := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AntiAliased', True);
  Editor.IgnoreCodeToolsIDEDirective := TSimbaSetting_Boolean.Create(Self, 'Editor', 'IgnoreCodeToolsIDEDirective', False);
  Editor.AllowCaretPastEOL := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AllowCaretPastEOL', True);
  Editor.AutomaticallyOpenAutoCompletion := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyOpenAutoCompletion', True);
  Editor.AutomaticallyShowParameterHints := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyShowParameterHints', True);
  Editor.RightMargin := TSimbaSetting_Integer.Create(Self, 'Editor', 'RightMargin', 80);
  Editor.RightMarginVisible := TSimbaSetting_Boolean.Create(Self, 'Editor', 'RightMarginVisible', False);

  Editor.AutomaticallyCompleteBegin := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteBegin', True);
  Editor.AutomaticallyCompleteParentheses := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteParentheses', False);
  Editor.AutomaticallyCompleteIndex := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteIndex', False);

  Load();
end;

destructor TSimbaSettings.Destroy;
begin
  Save();

  if (FChangeEventList <> nil) then
    FreeAndNil(FChangeEventList);
  if (FList <> nil) then
    FreeAndNil(FList);

  inherited Destroy();
end;

initialization
  SimbaSettings := TSimbaSettings.Create(GetDataPath() + 'settings.ini');

finalization
  if (SimbaSettings <> nil) then
    FreeAndNil(SimbaSettings);

end.

