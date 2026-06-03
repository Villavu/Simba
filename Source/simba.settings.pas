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
  Classes, SysUtils, IniFiles, LazMethodList, Variants, Controls,
  simba.containers;

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

    procedure Changed;
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
    TSettingList = specialize TSimbaObjectList<TSimbaSetting>;
  protected
    FList: TSettingList;
    FFirstLaunch: Boolean;
  public
    General: record
      TrayIconVisible: TSimbaSetting;
      Layout: TSimbaSetting;
      LockLayout: TSimbaSetting;
      Notes: TSimbaSetting;
      RecentFiles: TSimbaSetting;
      ToolbarSize: TSimbaSetting;
      ToolbarPosition: TSimbaSetting;
      ToolBarSpacing: TSimbaSetting;
      ColorPickerHistory: TSimbaSetting;

      OpenSSLCryptoHash: TSimbaSetting;
      OpenSSLHash: TSimbaSetting;

      ScrollBarSize: TSimbaSetting; // in 96 DPI
      ScrollBarArrowSize: TSimbaSetting;

      FindInFilesWidth: TSimbaSetting;
      FindInFilesHeight: TSimbaSetting;
      FindInFilesSearch: TSimbaSetting;
      FindInFilesLocation: TSimbaSetting;
      FindInFilesSubDirs: TSimbaSetting;
      FindInFilesWholeWords: TSimbaSetting;
      FindInFilesCaseSens: TSimbaSetting;

      FileBrowserMasks: TSimbaSetting;
    end;

    Editor: record
      DefaultScriptType: TSimbaSetting;
      DefaultScript: TSimbaSetting;
      DefaultScriptFile: TSimbaSetting;
      CustomColors: TSimbaSetting;
      CustomTokenAttris: TSimbaSetting;
      Keystrokes: TSimbaSetting;
      FontSize: TSimbaSetting;
      FontName: TSimbaSetting;
      RightMargin: TSimbaSetting;
      RightMarginVisible: TSimbaSetting;
      AntiAliased: TSimbaSetting;
      AllowCaretPastEOL: TSimbaSetting;

      AutomaticallyCompleteBegin: TSimbaSetting;
      AutomaticallyCompleteParentheses: TSimbaSetting;
      AutomaticallyCompleteIndex: TSimbaSetting;

      AutoCompleteWidth: TSimbaSetting;
      AutoCompleteLines: TSimbaSetting;

      DocumentationComment: TSimbaSetting;

      FindPanelVisible: TSimbaSetting;
    end;

    CodeTools: record
      IgnoreIDEDirective: TSimbaSetting;

      CompletionAddKeywords: TSimbaSetting;
      CompletionOpenAutomatically: TSimbaSetting;
      ParamHintOpenAutomatically: TSimbaSetting;
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

    FunctionList: record
      ShowMouseoverHint: TSimbaSetting;
      HiddenSimbaSections: TSimbaSetting;
      CustomOrder: TSimbaSetting;
    end;

    Compiler: record
      ShowHints: TSimbaSetting;
    end;

    property FirstLaunch: Boolean read FFirstLaunch;

    class function GetINIFile: TINIFile;
    class procedure RemoveSimpleSetting(Section, Name: String);
    class procedure SetSimpleSetting(Section: String; Name, Value: String);
    class function GetSimpleSetting(Section: String; Name: String; DefValue: String = ''): String;

    procedure Load;
    procedure Save;

    (* Add or remove a callback when the setting is changed. When the component is destroyed the callback will be removed.
     * CallEventInitially will invoke the callback instantly regardless of setting value.
     *)
    procedure RegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent; CallEventInitially: Boolean = False); overload;
    procedure UnRegisterChangeHandler(Owner: TComponent; Setting: TSimbaSetting; Event: TSimbaSettingChangedEvent); overload;

    constructor Create;
    destructor Destroy; override;
  end;

function GetSimbaSettings: TSimbaSettings;
function GetSimbaSettingsFileName: String;

property SimbaSettings: TSimbaSettings read GetSimbaSettings;
property SimbaSettingFileName: String read GetSimbaSettingsFileName;

implementation

uses
  Forms, SynEdit, LCLType,
  simba.base, simba.encoding, simba.env, simba.ide_editor_docgenerator,
  simba.initializations, simba.component_theme, simba.misc;

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
  Result := SimbaEnv.DataPath + 'settings.ini';
end;

procedure TSimbaSetting_Integer.CheckValue(AValue: Variant);
begin
  if not VarIsOrdinal(AValue) then
    raise Exception.Create('TSimbaSetting_BinaryString: Value is not a String')
end;

procedure TSimbaSetting_Integer.ReadValue(INI: TINIFile);
begin
  FValue := INI.ReadInt64(FSection, FName, Value);
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
  FValue := BaseDecode(EBaseEncoding.b64, INI.ReadString(FSection, FName, Value));
end;

procedure TSimbaSetting_BinaryString.WriteValue(INI: TINIFile);
begin
  INI.WriteString(FSection, FName, BaseEncode(EBaseEncoding.b64, Value));
end;

procedure TSimbaSetting_String.CheckValue(AValue: Variant);
begin
  if not VarIsStr(AValue) then
    raise Exception.Create('TSimbaSetting_String: Value is not a String');
end;

procedure TSimbaSetting_String.ReadValue(INI: TINIFile);
begin
  FValue := INI.ReadString(FSection, FName, Value);
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
  FValue := INI.ReadBool(FSection, FName, Value);
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

  if (AValue <> FValue) then
  begin
    FValue := AValue;
    Changed();
  end;
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

procedure TSimbaSetting.Changed;
var
  I: Integer;
begin
  I := FChangeEventList.Count;
  while FChangeEventList.NextDownIndex(I) do
    TSimbaSettingChangedEvent(FChangeEventList.Items[I])(Self);
end;

procedure TSimbaSetting.SetDefault;
begin
  Value := FDefaultValue;
end;

function TSimbaSetting.IsDefault: Boolean;
begin
  Result := Value = FDefaultValue;
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
    if FileExists(SimbaSettingFileName) then
    begin
      FFirstLaunch := False;

      for Setting in FList.ToArray() do
        if INI.ValueExists(Setting.FSection, Setting.FName) then
          Setting.ReadValue(INI);
    end;
  except
    on E: Exception do
      DebugLn('TSimbaSettings.Load: %s', [E.Message]);
  end;
  INI.Free();
end;

procedure TSimbaSettings.Save;
var
  INI: TIniFile;
  Setting: TSimbaSetting;
begin
  INI := GetINIFile();
  for Setting in FList.ToArray() do
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

class procedure TSimbaSettings.RemoveSimpleSetting(Section, Name: String);
begin
  try
    with GetINIFile() do
    try
      if (Name = '') then
        EraseSection(Section)
      else
        DeleteKey(Section, Name);
    finally
      Free();
    end;
  except
  end;
end;

class procedure TSimbaSettings.SetSimpleSetting(Section: String; Name, Value: String);
begin
  try
    with GetINIFile() do
    try
      WriteString(Section, Name, Value);
    finally
      Free();
    end;
  except
  end;
end;

class function TSimbaSettings.GetSimpleSetting(Section: String; Name: String; DefValue: String): String;
begin
  Result := '';

  try
    with GetINIFile() do
    try
      Result := ReadString(Section, Name, DefValue);
    finally
      Free();
    end;
  except
  end;
end;

constructor TSimbaSettings.Create;
begin
  inherited Create();

  if Screen.Fonts.IndexOf('Consolas') > -1 then
    SynDefaultFontName := 'Consolas';
  SynDefaultFontSize := 12;

  FFirstLaunch := True;
  FList := TSettingList.Create(True);

  // General
  General.TrayIconVisible    := TSimbaSetting_Boolean.Create(Self, 'General', 'TrayIconVisible', True);
  General.LockLayout         := TSimbaSetting_Boolean.Create(Self, 'General', 'LockLayout', False);
  General.Layout             := TSimbaSetting_BinaryString.Create(Self, 'General', 'Layout', '');
  General.Notes              := TSimbaSetting_BinaryString.Create(Self, 'General', 'Notes', '');
  General.RecentFiles        := TSimbaSetting_BinaryString.Create(Self, 'General', 'RecentFiles', '');
  General.ToolbarSize        := TSimbaSetting_Integer.Create(Self, 'General', 'ToolbarSize', 24);
  General.ToolbarPosition    := TSimbaSetting_String.Create(Self, 'General', 'ToolbarPosition', 'Top');
  General.ToolbarSpacing     := TSimbaSetting_Integer.Create(Self, 'General', 'ToolbarSpacing', 2);
  General.ColorPickerHistory := TSimbaSetting_BinaryString.Create(Self, 'General', 'ColorPickerHistory', '');

  General.OpenSSLCryptoHash  := TSimbaSetting_String.Create(Self, 'General', 'OpenSSLCryptoHash', '');
  General.OpenSSLHash        := TSimbaSetting_String.Create(Self, 'General', 'OpenSSLHash', '');

  General.ScrollBarSize      := TSimbaSetting_Integer.Create(Self, 'General', 'ScrollBarSize', SimbaComponentTheme.ScrollBarSize);
  General.ScrollBarArrowSize := TSimbaSetting_Integer.Create(Self, 'General', 'ScrollBarArrowSize', SimbaComponentTheme.ScrollBarArrowSize);

  General.FindInFilesWidth      := TSimbaSetting_Integer.Create(Self, 'General', 'FindInFilesWidth', 700);
  General.FindInFilesHeight     := TSimbaSetting_Integer.Create(Self, 'General', 'FindInFilesHeight', 400);
  General.FindInFilesSearch     := TSimbaSetting_BinaryString.Create(Self, 'General', 'FindInFilesSearch', '');
  General.FindInFilesLocation   := TSimbaSetting_BinaryString.Create(Self, 'General', 'FindInFilesLocation', '');
  General.FindInFilesSubDirs    := TSimbaSetting_Boolean.Create(Self, 'General', 'FindInFilesSubDirs', False);
  General.FindInFilesWholeWords := TSimbaSetting_Boolean.Create(Self, 'General', 'FindInFilesWholeWords', False);
  General.FindInFilesCaseSens   := TSimbaSetting_Boolean.Create(Self, 'General', 'FindInFilesCaseSens', False);

  General.FileBrowserMasks := TSimbaSetting_Boolean.Create(Self, 'General', 'FileBrowserMasks', False);

  // Editor
  Editor.DefaultScriptType := TSimbaSetting_Integer.Create(Self, 'Editor', 'DefaultScriptType', 1);
  Editor.DefaultScript     := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DefaultScript', 'begin' + LineEnding + '  WriteLn('+#39+'Hello World'+#39+');' + LineEnding + 'end.');
  Editor.DefaultScriptFile := TSimbaSetting_String.Create(Self, 'Editor', 'DefaultScriptFile', '');
  Editor.CustomColors      := TSimbaSetting_String.Create(Self, 'Editor', 'CustomColors', '');
  Editor.CustomTokenAttris := TSimbaSetting_String.Create(Self, 'Editor', 'CustomTokenAttris', '');
  Editor.Keystrokes        := TSimbaSetting_String.Create(Self, 'Editor', 'Keystrokes', '');
  Editor.FontSize          := TSimbaSetting_Integer.Create(Self, 'Editor', 'FontSize', SynDefaultFontSize);
  Editor.FontName          := TSimbaSetting_String.Create(Self, 'Editor', 'FontName', SynDefaultFontName);
  Editor.AntiAliased       := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AntiAliased', True);
  Editor.AllowCaretPastEOL := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AllowCaretPastEOL', True);

  Editor.RightMargin          := TSimbaSetting_Integer.Create(Self, 'Editor', 'RightMargin', 80);
  Editor.RightMarginVisible   := TSimbaSetting_Boolean.Create(Self, 'Editor', 'RightMarginVisible', False);
  Editor.DocumentationComment := TSimbaSetting_BinaryString.Create(Self, 'Editor', 'DocumentationComment', DEFAULT_DOCUMENTATION_COMMENT);
  Editor.FindPanelVisible     := TSimbaSetting_Boolean.Create(Self, 'Editor', 'FindPanelVisible', False);

  Editor.AutomaticallyCompleteBegin       := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteBegin', True);
  Editor.AutomaticallyCompleteParentheses := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteParentheses', False);
  Editor.AutomaticallyCompleteIndex       := TSimbaSetting_Boolean.Create(Self, 'Editor', 'AutomaticallyCompleteIndex', False);

  Editor.AutoCompleteWidth := TSimbaSetting_Integer.Create(Self, 'Editor', 'AutoCompleteWidth', 400);
  Editor.AutoCompleteLines := TSimbaSetting_Integer.Create(Self, 'Editor', 'AutoCompleteLines', 8);

  // Codetools
  CodeTools.IgnoreIDEDirective          := TSimbaSetting_Boolean.Create(Self, 'CodeTools', 'IgnoreIDEDirective', False);

  CodeTools.CompletionAddKeywords       := TSimbaSetting_Boolean.Create(Self, 'CodeTools', 'CompletionAddKeywords', True);
  CodeTools.CompletionOpenAutomatically := TSimbaSetting_Boolean.Create(Self, 'CodeTools', 'CompletionOpenAutomatically', True);
  CodeTools.ParamHintOpenAutomatically  := TSimbaSetting_Boolean.Create(Self, 'CodeTools', 'ParamHintOpenAutomatically', True);

  // Output
  OutputBox.FontSize        := TSimbaSetting_Integer.Create(Self, 'OutputBox', 'FontSize', Editor.FontSize.DefaultValue);
  OutputBox.FontName        := TSimbaSetting_String.Create(Self,  'OutputBox', 'FontName', Editor.FontName.DefaultValue);
  OutputBox.FontAntiAliased := TSimbaSetting_Boolean.Create(Self, 'OutputBox', 'FontAntiAliased', Editor.AntiAliased.DefaultValue);
  OutputBox.ClearOnCompile  := TSimbaSetting_Boolean.Create(Self, 'OutputBox', 'OutputClearOnCompile', False);

  // Script Backup
  ScriptBackup.Enabled  := TSimbaSetting_Boolean.Create(Self, 'ScriptBackup', 'Enabled', True);
  ScriptBackup.Interval := TSimbaSetting_Integer.Create(Self, 'ScriptBackup', 'Interval', 3);

  // Function List
  FunctionList.ShowMouseoverHint := TSimbaSetting_Boolean.Create(Self, 'FunctionList', 'ShowMouseoverHint', True);
  FunctionList.HiddenSimbaSections := TSimbaSetting_BinaryString.Create(Self, 'FunctionList', 'HiddenSimbaSections', '');
  FunctionList.CustomOrder := TSimbaSetting_BinaryString.Create(Self, 'FunctionList', 'CustomOrder', '');

  // Compiler
  Compiler.ShowHints := TSimbaSetting_Boolean.Create(Self, 'Compiler', 'ShowHints', False);
end;

destructor TSimbaSettings.Destroy;
begin
  if (FList <> nil) then
    FreeAndNil(FList);

  inherited Destroy();
end;

procedure DoCreate;
begin
  SimbaSettingsInstance := TSimbaSettings.Create();
  SimbaSettingsInstance.Load();
end;

procedure DoDestroy;
begin
  if (SimbaProcessType = ESimbaProcessType.IDE) then
    SimbaSettingsInstance.Save();

  FreeAndNil(SimbaSettingsInstance);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.CREATE, @DoCreate, 'SimbaSettings', 10); // Priority 10 = init first
  SimbaInitialization_Add(ESimbaInit.DESTROY, @DoDestroy, 'SimbaSettings', -10); // Priority -10 = finalize last

end.

