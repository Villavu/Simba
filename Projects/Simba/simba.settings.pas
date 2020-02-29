unit simba.settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics,
  simba.setting;

type
  TSimbaSettings = class
  protected
    FINI: TINIFile;
    FSettings: TSimbaSettingsList;

    function addIntegerSetting(Section, Name: String): TSimbaSetting_Int64;
    function addStringSetting(Section, Name: String): TSimbaSetting_String;
    function addCompressedStringSetting(Section, Name: String): TSimbaSetting_CompressedString;
    function addBooleanSetting(Section, Name: String): TSimbaSetting_Boolean;
    function addDirectorySetting(Section, Name: String): TSimbaSetting_Directory;
    function addFileSetting(Section, Name: String): TSimbaSetting_File;
  public
    GUI: record
      ConsoleVisible: TSimbaSetting_Boolean;
      TrayIconVisible: TSimbaSetting_Boolean;
      Layout: TSimbaSetting_CompressedString;
      LayoutLocked: TSimbaSetting_Boolean;
      Notes: TSimbaSetting_CompressedString;
      RecentFiles: TSimbaSetting_String;
      CustomFontSize: TSimbaSetting_Int64;
      CustomToolbarSize: TSimbaSetting_Int64;
    end;

    Editor: record
      DefaultScriptPath: TSimbaSetting_File;
      ColorsPath: TSimbaSetting_String;
      FontHeight: TSimbaSetting_Int64;
      FontName: TSimbaSetting_String;
      RightMargin: TSimbaSetting_Int64;
      RightMarginVisible: TSimbaSetting_Boolean;
      DividerVisible: TSimbaSetting_Boolean;
      AntiAliasing: TSimbaSetting_Boolean;
      AllowCaretPastEOL: TSimbaSetting_Boolean;
      IgnoreCodeToolsIDEDirective: TSimbaSetting_Boolean;
      AutomaticallyOpenAutoCompletion: TSimbaSetting_Boolean;
      AutomaticallyShowParameterHints: TSimbaSetting_Boolean;
    end;

    Environment: record
      DataPath: TSimbaSetting_Directory;
      FontPath: TSimbaSetting_Directory;
      IncludePath: TSimbaSetting_Directory;
      PluginPath: TSimbaSetting_Directory;
      ScriptPath: TSimbaSetting_Directory;
      ScriptExecutablePath: TSimbaSetting_String;
      PackagePath: TSimbaSetting_Directory;
    end;

    Resources: record
      InitializeOpenSSL: TSimbaSetting_Boolean;
      ExtractOpenSSL: TSimbaSetting_Boolean;
      ExtractSimbaScript: TSimbaSetting_Boolean;
    end;

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaSettings: TSimbaSettings;

implementation

uses
  forms;

function TSimbaSettings.addIntegerSetting(Section, Name: String): TSimbaSetting_Int64;
begin
  Result := TSimbaSetting_Int64.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

function TSimbaSettings.addStringSetting(Section, Name: String): TSimbaSetting_String;
begin
  Result := TSimbaSetting_String.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

function TSimbaSettings.addCompressedStringSetting(Section, Name: String): TSimbaSetting_CompressedString;
begin
  Result := TSimbaSetting_CompressedString.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

function TSimbaSettings.addBooleanSetting(Section, Name: String): TSimbaSetting_Boolean;
begin
  Result := TSimbaSetting_Boolean.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

function TSimbaSettings.addDirectorySetting(Section, Name: String): TSimbaSetting_Directory;
begin
  Result := TSimbaSetting_Directory.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

function TSimbaSettings.addFileSetting(Section, Name: String): TSimbaSetting_File;
begin
  Result := TSimbaSetting_File.Create(FINI, Section, Name);

  FSettings.Add(Result);
end;

constructor TSimbaSettings.Create;
begin
  FSettings := TSimbaSettingsList.Create(True);

  FINI := TINIFile.Create(Application.Location + 'Data' + DirectorySeparator + 'settings.ini');
  FINI.CacheUpdates := True;

  // Environment
  Environment.DataPath := addDirectorySetting('Environment', 'DataPath');
  Environment.DataPath.DefaultValue := Application.Location + 'Data' + DirectorySeparator;

  Environment.IncludePath := addDirectorySetting('Environment', 'IncludePath');
  Environment.IncludePath.DefaultValue := Application.Location + 'Includes' + DirectorySeparator;

  Environment.PluginPath := addDirectorySetting('Environment', 'PluginPath');
  Environment.PluginPath.DefaultValue := Application.Location + 'Plugins' + DirectorySeparator;

  Environment.FontPath := addDirectorySetting('Environment', 'FontPath');
  Environment.FontPath.DefaultValue := Application.Location + 'Fonts' + DirectorySeparator;

  Environment.ScriptPath := addDirectorySetting('Environment', 'ScriptPath');
  Environment.ScriptPath.DefaultValue := Application.Location + 'Scripts' + DirectorySeparator;

  Environment.ScriptExecutablePath := addFileSetting('Environment', 'ScriptExecutablePath');
  Environment.ScriptExecutablePath.DefaultValue := Application.Location + 'SimbaScript' {$IFDEF WINDOWS} + '.exe' {$ENDIF};

  Environment.PackagePath := addDirectorySetting('Environment', 'PackagePath');
  Environment.PackagePath.DefaultValue := Application.Location + 'Data' + DirectorySeparator + 'packages' + DirectorySeparator;

  // GUI
  GUI.ConsoleVisible := addBooleanSetting('GUI', 'ConsoleVisible');
  GUI.ConsoleVisible.DefaultValue := True;
  GUI.TrayIconVisible := addBooleanSetting('GUI', 'TrayIconVisible');
  GUI.TrayIconVisible.DefaultValue := True;
  GUI.Layout := addCompressedStringSetting('GUI', 'Layout');
  GUI.LayoutLocked := addBooleanSetting('GUI', 'LayoutLocked');
  GUI.Notes := addCompressedStringSetting('GUI', 'Notes');
  GUI.RecentFiles := addStringSetting('GUI', 'RecentFiles');
  GUI.CustomFontSize := addIntegerSetting('GUI', 'CustomFontSize');
  GUI.CustomFontSize.DefaultValue := 0;
  GUI.CustomToolbarSize := addIntegerSetting('GUI', 'CustomToolbarSize');
  GUI.CustomToolbarSize.DefaultValue := 0;

  // Editor
  Editor.ColorsPath := addStringSetting('Editor', 'ColorsPath');
  Editor.DefaultScriptPath := addFileSetting('Editor', 'DefaultScriptPath');
  Editor.DefaultScriptPath.DefaultValue := Environment.DataPath.Value + 'default.simba';
  Editor.FontHeight := addIntegerSetting('Editor', 'FontHeight');
  Editor.FontHeight.DefaultValue := 18;
  Editor.FontName := addStringSetting('Editor', 'FontName');
  Editor.AntiAliasing := addBooleanSetting('Editor', 'AntiAliasing');
  Editor.AntiAliasing.DefaultValue := True;
  Editor.IgnoreCodeToolsIDEDirective := addBooleanSetting('Editor', 'IgnoreCodeToolsIDEDirective');
  Editor.IgnoreCodeToolsIDEDirective.DefaultValue := False;
  Editor.AllowCaretPastEOL := addBooleanSetting('Editor', 'AllowCaretPastEOL');
  Editor.AllowCaretPastEOL.DefaultValue := True;
  Editor.AutomaticallyOpenAutoCompletion := addBooleanSetting('Editor', 'AutomaticallyOpenAutoCompletion');
  Editor.AutomaticallyOpenAutoCompletion.DefaultValue := True;
  Editor.AutomaticallyShowParameterHints := addBooleanSetting('Editor', 'AutomaticallyShowParameterHints');
  Editor.AutomaticallyShowParameterHints.DefaultValue := True;
  Editor.RightMargin := addIntegerSetting('Editor', 'Right Margin');
  Editor.RightMargin.DefaultValue := 80;
  Editor.RightMarginVisible := addBooleanSetting('Editor', 'HideRightMargin');
  Editor.RightMarginVisible.DefaultValue := True;
  Editor.DividerVisible := addBooleanSetting('Editor', 'DividerVisible');
  Editor.DividerVisible.DefaultValue := True;

  Resources.ExtractOpenSSL := addBooleanSetting('Resources', 'ExtractOpenSSL');
  Resources.ExtractOpenSSL.DefaultValue := True;
  Resources.ExtractSimbaScript := addBooleanSetting('Resources', 'ExtractSimbaScript');
  Resources.ExtractSimbaScript.DefaultValue := True;
  Resources.InitializeOpenSSL := addBooleanSetting('Resources', 'InitializeOpenSSL');
  Resources.InitializeOpenSSL.DefaultValue := True;

  try
    FINI.UpdateFile();
  except
  end;
end;

destructor TSimbaSettings.Destroy;
begin
  FSettings.Free();

  FINI.UpdateFile();
  FINI.Free();
end;

initialization
  SimbaSettings := TSimbaSettings.Create();

finalization
  SimbaSettings.Free();
  SimbaSettings := nil;

end.

