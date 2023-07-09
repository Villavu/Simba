{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settings;

{$i simba.inc}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  classes, sysutils, inifiles, lazmethodlist, variants, fgl, controls, graphics;

const
  SETTINGS_VERSION = 1; // Increase if settings become incompatible

type
  TSimbaSettings = class;
  TSimbaSetting = class
  protected
    FSettings: TSimbaSettings;
    FSection: String;
    FName: String;
    FValue: Variant;
    FDefaultValue: Variant;
    FChangeEventList: TMethodList;

    procedure CheckValue(AValue: Variant); virtual; abstract;
    procedure ReadValue(INI: TINIFile); virtual; abstract;
    procedure WriteValue(INI: TINIFile); virtual; abstract;

    function GetName: String;
    procedure SetValue(AValue: Variant);
  public
    constructor Create(ASettings: TSimbaSettings; ASection, AName: String; DefaultValue: Variant);
    destructor Destroy; override;

    procedure SetDefault;
    function IsDefault: Boolean;

    property DefaultValue: Variant read FDefaultValue;
    property Value: Variant read FValue write SetValue;
    property Name: String read GetName;
  end;

  TSimbaSetting_Boolean = class(TSimbaSetting)
  protected
    procedure CheckValue(AValue: Variant); override;
    procedure ReadValue(INI: TINIFile); override;
    procedure WriteValue(INI: TINIFile); override;
  end;

  TSimbaSetting_String = class(TSimbaSetting)
  protected
    procedure CheckValue(AValue: Variant); override;
    procedure ReadValue(INI: TINIFile); override;
    procedure WriteValue(INI: TINIFile); override;
  end;

  TSimbaSetting_BinaryString = class(TSimbaSetting)
  protected
    procedure CheckValue(AValue: Variant); override;
    procedure ReadValue(INI: TINIFile); override;
    procedure WriteValue(INI: TINIFile); override;
  end;

  TSimbaSetting_Integer = class(TSimbaSetting)
  protected
    procedure CheckValue(AValue: Variant); override;
    procedure ReadValue(INI: TINIFile); override;
    procedure WriteValue(INI: TINIFile); override;
  end;

  TSimbaSettingChangedEvent = procedure(Setting: TSimbaSetting) of object;

  TSimbaSettings = class
  protected
  type
    TSettingList = specialize TFPGObjectList<TSimbaSetting>;
  protected
    FChangeEventList: TMethodList;
    FList: TSettingList;
    FFirstLaunch: Boolean;
  public
    General: record
      ConsoleVisible: TSimbaSetting;
      TrayIconVisible: TSimbaSetting;
      Layout: TSimbaSetting;
      LockLayout: TSimbaSetting;
      Notes: TSimbaSetting;
      RecentFiles: TSimbaSetting;
      CustomFontSize: TSimbaSetting;
      ToolbarSize: TSimbaSetting;
      ToolbarPosition: TSimbaSetting;
      ToolBarSpacing: TSimbaSetting;
      ColorPickerHistory: TSimbaSetting;

      OpenSSLCryptoHash: TSimbaSetting;
      OpenSSLHash: TSimbaSetting;

      ScrollBarSize: TSimbaSetting; // in 96 DPI
      ScrollBarArrowSize: TSimbaSetting;
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

      AutoCompleteWidth: TSimbaSetting;
      AutoCompleteLines: TSimbaSetting;

      DocumentationComment: TSimbaSetting;

      FindPanelVisible: TSimbaSetting;
    end;

    OutputBox: record
      FontName: TSimbaSetting;
      FontSize: TSimbaSetting;
      FontAntiAliased: TSimbaSetting;

      ClearOnCompile: TSimbaSetting;
    end;

    ScriptBackup: record
      Enabled: TSimbaSetting;
      Interval: TSimbaSetting;
    end;

    property FirstLaunch: Boolean read FFirstLaunch;

    class function GetINIFile: TINIFile;
    class procedure SetSimpleSetting(AName, Value: String);
    class function GetSimpleSetting(AName: String; DefValue: String = ''): String;

    procedure Load;
    procedure Save;

    procedure RegisterChangeHandler(Event: TSimbaSettingChangedEvent); overload; deprecated;
    procedure UnRegisterChangeHandler(Event: TSimbaSettingChangedEvent); overload;

    procedure RegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent; CallEventInitially: Boolean = False); overload;
    procedure UnRegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent); overload;

    procedure Changed(Setting: TSimbaSetting);

    constructor Create;
    destructor Destroy; override;
  end;

function GetSimbaSettings: TSimbaSettings;
function GetSimbaSettingsFileName: String;

property SimbaSettings: TSimbaSettings read GetSimbaSettings;
property SimbaSettingFileName: String read GetSimbaSettingsFileName;

implementation

uses
  Forms, SynEdit,
  simba.mufasatypes, simba.encoding, simba.env, simba.editor_docgenerator,
  simba.ide_initialization, simba.theme, simba.fonthelpers;

var
  SimbaSettingsInstance: TSimbaSettings = nil;

function GetSimbaSettings: TSimbaSettings;
begin
  if (SimbaSettingsInstance = nil) then
    raise Exception.Create('Simba settings have not been created');

  Result := SimbaSettingsInstance;
end;

function GetSimbaSettingsFileName: String;
begin
  Result := GetDataPath() + 'settings.ini';
end;

procedure TSimbaSetting_Integer.CheckValue(AValue: Variant);
begin
  if not VarIsOrdinal(AValue) then
    raise Exception.Create('TSimbaSetting_BinaryString: Value is not a String')
end;

procedure TSimbaSetting_Integer.ReadValue(INI: TINIFile);
begin
  Value := INI.ReadInt64(FSection, FName, Value);
end;

procedure TSimbaSetting_Integer.WriteValue(INI: TINIFile);
begin
  INI.WriteInt64(FSection, FName, Value);
end;

procedure TSimbaSetting_BinaryString.CheckValue(AValue: Variant);
begin
  if not VarIsStr(AValue) then
    raise Exception.Create('TSimbaSetting_BinaryString: Value is not a String')
end;

procedure TSimbaSetting_BinaryString.ReadValue(INI: TINIFile);
begin
  Value := Base64Decode(INI.ReadString(FSection, FName, Value));
end;

procedure TSimbaSetting_BinaryString.WriteValue(INI: TINIFile);
begin
  INI.WriteString(FSection, FName, Base64Encode(Value));
end;

procedure TSimbaSetting_String.CheckValue(AValue: Variant);
begin
  if not VarIsStr(AValue) then
    raise Exception.Create('TSimbaSetting_String: Value is not a String');
end;

procedure TSimbaSetting_String.ReadValue(INI: TINIFile);
begin
  Value := INI.ReadString(FSection, FName, Value);
end;

procedure TSimbaSetting_String.WriteValue(INI: TINIFile);
begin
  INI.WriteString(FSection, FName, Value);
end;

procedure TSimbaSetting_Boolean.CheckValue(AValue: Variant);
begin
  if not VarIsBool(AValue) then
    raise Exception.Create('TSimbaSetting_Boolean: Value is not a Boolean');
end;

procedure TSimbaSetting_Boolean.ReadValue(INI: TINIFile);
begin
  Value := INI.ReadBool(FSection, FName, Value);
end;

procedure TSimbaSetting_Boolean.WriteValue(INI: TINIFile);
begin
  INI.WriteBool(FSection, FName, Value);
end;

function TSimbaSetting.GetName: String;
begin
  Result := FSection + '.' + FName;
end;

procedure TSimbaSetting.SetValue(AValue: Variant);
begin
  CheckValue(AValue);
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

  FChangeEventList := TMethodList.Create();
end;

destructor TSimbaSetting.Destroy;
begin
  FreeAndNil(FChangeEventList);

  inherited Destroy();
end;

procedure TSimbaSetting.SetDefault;
begin
  Value := FDefaultValue;
end;

function TSimbaSetting.IsDefault: Boolean;
begin
  Result := Value = FDefaultValue;
end;

procedure TSimbaSettings.Changed(Setting: TSimbaSetting);
var
  i: Integer;
begin
  if (FChangeEventList = nil) or (FChangeEventList.Count = 0) then
    Exit;

  i := Setting.FChangeEventList.Count;
  while Setting.FChangeEventList.NextDownIndex(i) do
    TSimbaSettingChangedEvent(Setting.FChangeEventList.Items[i])(Setting);

  DebugLn('[TSimbaSettings.Changed] Setting changed: ' + Setting.Name);

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

type
  TManagedSimbaSetting = class(TComponent)
  public
    Setting: TSimbaSetting;
    Event: TSimbaSettingChangedEvent;

    constructor Create(AOwner: TComponent; ASetting: TSimbaSetting; AEvent: TSimbaSettingChangedEvent); reintroduce;
    destructor Destroy; override;
  end;

constructor TManagedSimbaSetting.Create(AOwner: TComponent; ASetting: TSimbaSetting; AEvent: TSimbaSettingChangedEvent);
begin
  inherited Create(AOwner);

  Setting := ASetting;
  Event := AEvent;
end;

destructor TManagedSimbaSetting.Destroy;
begin
  SimbaSettings.UnRegisterChangeHandler(Owner, Setting, Event);

  inherited Destroy;
end;

procedure TSimbaSettings.RegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent; CallEventInitially: Boolean);
begin
  TManagedSimbaSetting.Create(Owner, Setting, Event);

  Setting.FChangeEventList.Add(TMethod(Event));

  if CallEventInitially then
    Event(Setting);
end;

procedure TSimbaSettings.UnRegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent);
begin
  Setting.FChangeEventList.Remove(TMethod(Event));
end;

procedure TSimbaSettings.Load;
var
  INI: TIniFile;
  Setting: TSimbaSetting;
begin
  INI := GetINIFile();
  try
    if (INI.ReadInteger('Settings', 'Version', 0) <> SETTINGS_VERSION) then
    begin
      DeleteFile(INI.FileName + '.old');
      RenameFile(INI.FileName, INI.FileName + '.old');

      Exit;
    end;

    if FileExists(SimbaSettingFileName) then
    begin
      FFirstLaunch := False;

      for Setting in FList do
      begin
        if not INI.ValueExists(Setting.FSection, Setting.FName) then
          Continue;

        Setting.ReadValue(INI);
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
  INI := GetINIFile();
  INI.WriteInteger('Settings', 'Version', SETTINGS_VERSION);

  for Setting in FList do
    Setting.WriteValue(INI);

  try
    INI.UpdateFile();
  except
  end;

  INI.Free();
end;

class function TSimbaSettings.GetINIFile: TINIFile;
begin
  Result := TIniFile.Create(SimbaSettingFileName, [ifoWriteStringBoolean]);
  Result.CacheUpdates := True;
  Result.SetBoolStringValues(True, ['True']);
  Result.SetBoolStringValues(False, ['False']);
end;

class procedure TSimbaSettings.SetSimpleSetting(AName, Value: String);
begin
  try
    with GetINIFile() do
    try
      WriteString('Other', AName, Value);
    finally
      Free();
    end;
  except
  end;
end;

class function TSimbaSettings.GetSimpleSetting(AName: String; DefValue: String): String;
begin
  Result := '';

  try
    with GetINIFile() do
    try
      Result := ReadString('Other', AName, DefValue);
    finally
      Free();
    end;
  except
  end;
end;

constructor TSimbaSettings.Create;
begin
  inherited Create();

  if Screen.Fonts.IndexOf('Cascadia Code Light') > -1 then
    SynDefaultFontName := 'Cascadia Code Light';

  FFirstLaunch := True;
  FList := TSettingList.Create();
  FChangeEventList := TMethodList.Create();

  // General
  General.ConsoleVisible     := TSimbaSetting_Boolean.Create(Self, 'General', 'ConsoleVisible', True);
  General.TrayIconVisible    := TSimbaSetting_Boolean.Create(Self, 'General', 'TrayIconVisible', True);
  General.LockLayout         := TSimbaSetting_Boolean.Create(Self, 'General', 'LockLayout', False);
  General.Layout             := TSimbaSetting_BinaryString.Create(Self, 'General', 'Layout', '');
  General.Notes              := TSimbaSetting_BinaryString.Create(Self, 'General', 'Notes', '');
  General.RecentFiles        := TSimbaSetting_BinaryString.Create(Self, 'General', 'RecentFiles', '');
  General.CustomFontSize     := TSimbaSetting_Integer.Create(Self, 'General', 'CustomFontSize', GetDefaultFontSize());
  General.ToolbarSize        := TSimbaSetting_Integer.Create(Self, 'General', 'ToolbarSize', 24);
  General.ToolbarPosition    := TSimbaSetting_String.Create(Self, 'General', 'ToolbarPosition', 'Top');
  General.ToolbarSpacing     := TSimbaSetting_Integer.Create(Self, 'General', 'ToolbarSpacing', 0);
  General.ColorPickerHistory := TSimbaSetting_BinaryString.Create(Self, 'General', 'ColorPickerHistory', '');

  General.OpenSSLCryptoHash      := TSimbaSetting_String.Create(Self, 'General', 'OpenSSLCryptoHash', '');
  General.OpenSSLHash            := TSimbaSetting_String.Create(Self, 'General', 'OpenSSLHash', '');

  General.ScrollBarSize      := TSimbaSetting_Integer.Create(Self, 'General', 'ScrollBarSize', SimbaTheme.ScrollBarSize);
  General.ScrollBarArrowSize := TSimbaSetting_Integer.Create(Self, 'General', 'ScrollBarArrowSize', SimbaTheme.ScrollBarArrowSize);

  // Editor
  Editor.DefaultScript                   := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DefaultScript', 'program new;' + LineEnding + 'begin' + LineEnding + 'end.');
  Editor.CustomColors                    := TSimbaSetting_String.Create(Self, 'Editor', 'CustomColors', '');
  Editor.FontSize                        := TSimbaSetting_Integer.Create(Self, 'Editor', 'FontSize', SynDefaultFontSize);
  Editor.FontName                        := TSimbaSetting_String.Create(Self, 'Editor', 'FontName', SynDefaultFontName);
  Editor.AntiAliased                     := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AntiAliased', True);
  Editor.IgnoreCodeToolsIDEDirective     := TSimbaSetting_Boolean.Create(Self, 'Editor', 'IgnoreCodeToolsIDEDirective', False);
  Editor.AllowCaretPastEOL               := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AllowCaretPastEOL', True);
  Editor.AutomaticallyOpenAutoCompletion := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyOpenAutoCompletion', True);
  Editor.AutomaticallyShowParameterHints := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyShowParameterHints', True);
  Editor.RightMargin                     := TSimbaSetting_Integer.Create(Self, 'Editor', 'RightMargin', 80);
  Editor.RightMarginVisible              := TSimbaSetting_Boolean.Create(Self, 'Editor', 'RightMarginVisible', False);
  Editor.DocumentationComment            := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DocumentationComment', DEFAULT_DOCUMENTATION_COMMENT);
  Editor.FindPanelVisible                := TSimbaSetting_Boolean.Create(Self, 'Editor', 'FindPanelVisible', False);

  Editor.AutomaticallyCompleteBegin       := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteBegin', True);
  Editor.AutomaticallyCompleteParentheses := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteParentheses', False);
  Editor.AutomaticallyCompleteIndex       := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteIndex', False);

  Editor.AutoCompleteWidth := TSimbaSetting_Integer.Create(Self, 'Editor', 'AutoCompleteWidth', 400);
  Editor.AutoCompleteLines := TSimbaSetting_Integer.Create(Self, 'Editor', 'AutoCompleteLines', 8);

  OutputBox.FontSize        := TSimbaSetting_Integer.Create(Self, 'OutputBox', 'FontSize', SynDefaultFontSize);
  OutputBox.FontName        := TSimbaSetting_String.Create(Self,  'OutputBox', 'FontName', SynDefaultFontName);
  OutputBox.FontAntiAliased := TSimbaSetting_Boolean.Create(Self, 'OutputBox', 'FontAntiAliased', True);
  OutputBox.ClearOnCompile  := TSimbaSetting_Boolean.Create(Self, 'OutputBox', 'OutputClearOnCompile', False);

  ScriptBackup.Enabled  := TSimbaSetting_Boolean.Create(Self, 'ScriptBackup', 'Enabled', True);
  ScriptBackup.Interval := TSimbaSetting_Integer.Create(Self, 'ScriptBackup', 'Interval', 5);
end;

destructor TSimbaSettings.Destroy;
begin
  if (FChangeEventList <> nil) then
    FreeAndNil(FChangeEventList);
  if (FList <> nil) then
    FreeAndNil(FList);

  inherited Destroy();
end;

procedure CreateSimbaSettings;
begin
  SimbaSettingsInstance := TSimbaSettings.Create();
  SimbaSettingsInstance.Load();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnBeforeCreate(@CreateSimbaSettings, 'SimbaSettings');

finalization
  if (SimbaSettingsInstance <> nil) then
    FreeAndNil(SimbaSettingsInstance);

end.

