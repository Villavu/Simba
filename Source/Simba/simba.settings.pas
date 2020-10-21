unit simba.settings;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, inifiles, graphics,
  simba.setting;

type
  TSimbaSettings = class
  protected
    FManager: TSimbaSettingManager;
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
      FontSize: TSimbaSetting_Int64;
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
      FirstLaunch: TSimbaSetting_Boolean;
      DataPath: TSimbaSetting_Directory;
      FontPath: TSimbaSetting_Directory;
      IncludePath: TSimbaSetting_Directory;
      PluginPath: TSimbaSetting_Directory;
      ScriptPath: TSimbaSetting_Directory;
      PackagePath: TSimbaSetting_Directory;
      OpenSSLPath: TSimbaSetting_Directory;
      OpenSSLOnLaunch: TSimbaSetting_Boolean;
    end;

    procedure Save;

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaSettings: TSimbaSettings;

implementation

uses
  forms;

procedure TSimbaSettings.Save;
begin
  try
    FManager.INIFile.UpdateFile();
  except
  end;
end;

constructor TSimbaSettings.Create;
begin
  FManager := TSimbaSettingManager.Create(Application.Location + 'Data' + DirectorySeparator + 'settings.ini');

  // Environment
  Environment.FirstLaunch := TSimbaSetting_Boolean.Create(FManager, 'Environment', 'FirstLaunch');
  Environment.FirstLaunch.DefaultValue := True;

  Environment.DataPath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'DataPath');
  Environment.DataPath.DefaultValue := Application.Location + 'Data' + DirectorySeparator;

  Environment.IncludePath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'IncludePath');
  Environment.IncludePath.DefaultValue := Application.Location + 'Includes' + DirectorySeparator;

  Environment.PluginPath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'PluginPath');
  Environment.PluginPath.DefaultValue := Application.Location + 'Plugins' + DirectorySeparator;

  Environment.FontPath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'FontPath');
  Environment.FontPath.DefaultValue := Application.Location + 'Fonts' + DirectorySeparator;

  Environment.ScriptPath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'ScriptPath');
  Environment.ScriptPath.DefaultValue := Application.Location + 'Scripts' + DirectorySeparator;

  Environment.PackagePath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'PackagePath');
  Environment.PackagePath.DefaultValue := Application.Location + 'Data' + DirectorySeparator + 'packages' + DirectorySeparator;

  Environment.OpenSSLOnLaunch := TSimbaSetting_Boolean.Create(FManager, 'Environment', 'OpenSSLOnLaunch');
  Environment.OpenSSLOnLaunch.DefaultValue := True;

  Environment.OpenSSLPath := TSimbaSetting_Directory.Create(FManager, 'Environment', 'OpenSSLPath');
  {$IFDEF WINDOWS}
  Environment.OpenSSLPath.Value := Application.Location + 'Data' + DirectorySeparator + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + DirectorySeparator;
  {$ELSE}
  Environment.OpenSSLPath.Value := Application.Location;
  {$ENDIF}

  // GUI
  GUI.ConsoleVisible := TSimbaSetting_Boolean.Create(FManager, 'GUI', 'ConsoleVisible');
  GUI.ConsoleVisible.DefaultValue := True;

  GUI.TrayIconVisible := TSimbaSetting_Boolean.Create(FManager, 'GUI', 'TrayIconVisible');
  GUI.TrayIconVisible.DefaultValue := True;

  GUI.Layout := TSimbaSetting_CompressedString.Create(FManager, 'GUI', 'Layout');
  GUI.LayoutLocked := TSimbaSetting_Boolean.Create(FManager, 'GUI', 'LayoutLocked');
  GUI.Notes := TSimbaSetting_CompressedString.Create(FManager, 'GUI', 'Notes');
  GUI.RecentFiles := TSimbaSetting_String.Create(FManager, 'GUI', 'RecentFiles');

  GUI.CustomFontSize := TSimbaSetting_Int64.Create(FManager, 'GUI', 'CustomFontSize');
  GUI.CustomFontSize.DefaultValue := 0;

  GUI.CustomToolbarSize := TSimbaSetting_Int64.Create(FManager, 'GUI', 'CustomToolbarSize');
  GUI.CustomToolbarSize.DefaultValue := 0;

  // Editor
  Editor.ColorsPath := TSimbaSetting_String.Create(FManager, 'Editor', 'ColorsPath');

  Editor.DefaultScriptPath := TSimbaSetting_File.Create(FManager, 'Editor', 'DefaultScriptPath');
  Editor.DefaultScriptPath.DefaultValue := Environment.DataPath.Value + 'default.simba';

  Editor.FontSize := TSimbaSetting_Int64.Create(FManager, 'Editor', 'FontSize');
  Editor.FontSize.DefaultValue := 15;
  Editor.FontName := TSimbaSetting_String.Create(FManager, 'Editor', 'FontName');

  Editor.AntiAliasing := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'AntiAliasing');
  Editor.AntiAliasing.DefaultValue := True;

  Editor.IgnoreCodeToolsIDEDirective := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'IgnoreCodeToolsIDEDirective');
  Editor.IgnoreCodeToolsIDEDirective.DefaultValue := False;

  Editor.AllowCaretPastEOL := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'AllowCaretPastEOL');
  Editor.AllowCaretPastEOL.DefaultValue := True;

  Editor.AutomaticallyOpenAutoCompletion := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'AutomaticallyOpenAutoCompletion');
  Editor.AutomaticallyOpenAutoCompletion.DefaultValue := True;

  Editor.AutomaticallyShowParameterHints := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'AutomaticallyShowParameterHints');
  Editor.AutomaticallyShowParameterHints.DefaultValue := True;

  Editor.RightMargin := TSimbaSetting_Int64.Create(FManager, 'Editor', 'Right Margin');
  Editor.RightMargin.DefaultValue := 80;

  Editor.RightMarginVisible := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'HideRightMargin');
  Editor.RightMarginVisible.DefaultValue := True;

  Editor.DividerVisible := TSimbaSetting_Boolean.Create(FManager, 'Editor', 'DividerVisible');
  Editor.DividerVisible.DefaultValue := True;

  Save(); // Update file with default values
end;

destructor TSimbaSettings.Destroy;
begin
  FManager.Free();
end;

initialization
  SimbaSettings := TSimbaSettings.Create();

finalization
  SimbaSettings.Free();

end.

