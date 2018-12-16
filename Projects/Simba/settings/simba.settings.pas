unit simba.settings;

(* JSON setting system that can be easily expanded to support any types by
   extending from the generic TSimbaSetting.

   Use class fields for static keys & use SimbaSettings.Value for dynamic keys.
 *)

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, jsonconf, fpjson;

type
  generic TSimbaSetting<_T> = class
  public
  type
    TSettingChangeEvent = procedure(Value: _T) of object;
  protected
    FConfig: TJSONConfig;
    FPath: String;
    FOnChange: TSettingChangeEvent;
    FDefaultValue: _T;

    procedure SetValue(AValue: _T); virtual; abstract;
    function GetValue: _T; virtual; abstract;
  public
    property DefaultValue: _T read FDefaultValue;
    property Value: _T read GetValue write SetValue;
    property OnChange: TSettingChangeEvent read FOnChange write FOnChange;

    function Exists: Boolean;
    function Delete: Boolean;

    constructor Create(Config: TJSONConfig; Path: String);
    constructor Create(Config: TJSONConfig; Path: String; ADefaultValue: _T);
  end;

  TSimbaSetting_Boolean = class(specialize TSimbaSetting<Boolean>)
  protected
    procedure SetValue(AValue: _T); override;
    function GetValue: _T; override;
  end;

  TSimbaSetting_Integer = class(specialize TSimbaSetting<Int64>)
  protected
    procedure SetValue(AValue: _T); override;
    function GetValue: _T; override;
  end;

  TSimbaSetting_String = class(specialize TSimbaSetting<WideString>)
  protected
    procedure SetValue(AValue: _T); override;
    function GetValue: _T; override;
  end;

  TSimbaSetting_StringArray = class(specialize TSimbaSetting<TStringArray>)
  protected
    procedure SetValue(AValue: _T); override;
    function GetValue: _T; override;
  end;

  TSimbaSettings = class
  protected
    function GetKeys(Path: String): TStringArray;
    function GetValue(Path: String): WideString;

    procedure SetValue(Path: String; AValue: WideString);
  public
    Config: TJSONConfig;

    General: record
      SaveScriptOnCompile: TSimbaSetting_Boolean;
      AutomaticallyCheckForSimbaUpdates: TSimbaSetting_Boolean;
      ShowSimbaNews: TSimbaSetting_Boolean;
      UpdateURL: TSimbaSetting_String;
      VersionURL: TSimbaSetting_String;
    end;

    ColorPicker: record
      AddColorToClipBoard: TSimbaSetting_Boolean;
      AddCoordinateToClipBoard: TSimbaSetting_Boolean;
      AddColorAndCoordinateToClipboard: TSimbaSetting_Boolean;
    end;

    Form: record
      Left: TSimbaSetting_Integer;
      Top: TSimbaSetting_Integer;
      Width: TSimbaSetting_Integer;
      Height: TSimbaSetting_Integer;
      Maximized: TSimbaSetting_Boolean;

      ConsoleVisible: TSimbaSetting_Boolean;
      TrayIconVisible: TSimbaSetting_Boolean;
      FunctionListVisible: TSimbaSetting_Boolean;
      FileBrowserVisible: TSimbaSetting_Boolean;
      NotesVisible: TSimbaSetting_Boolean;
      NotesContent: TSimbaSetting_String;

      RecentFiles: TSimbaSetting_StringArray;
    end;

    Environment: record
      PluginPath: TSimbaSetting_String;
      FontPath: TSimbaSetting_String;
      IncludePath: TSimbaSetting_String;
      ScriptPath: TSimbaSetting_String;
    end;

    Editor: record
      DefaultScriptPath: TSimbaSetting_String;
      HighlighterPath: TSimbaSetting_String;

      FontName: TSimbaSetting_String;
      FontSize: TSimbaSetting_Integer;

      CaretPastEOL: TSimbaSetting_Boolean;
      ShowSpecialCharacters: TSimbaSetting_Boolean;
    end;

    CodeTools: record
      AutomaticallyShowParameterHints: TSimbaSetting_Boolean;
      AutomaticallyShowAutoComplete: TSimbaSetting_Boolean;
    end;

    // Basic read/write
    property Value[Path: String]: WideString read GetValue write SetValue;
    property Keys[Path: String]: TStringArray read GetKeys;

    procedure Save;

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

var
  SimbaSettings: TSimbaSettings;

implementation

uses
  forms,
  mufasabase;

type
  TJSONConfig_Helper = class helper for TJSONConfig
  public
    function Exists(Path: String): Boolean;
  end;

function TJSONConfig_Helper.Exists(Path: String): Boolean;
begin
  Result := (FindElement(Path, False) <> nil);
end;

constructor TSimbaSetting.Create(Config: TJSONConfig; Path: String);
begin
  FConfig := Config;
  FPath := Path;
end;

constructor TSimbaSetting.Create(Config: TJSONConfig; Path: String; ADefaultValue: _T);
begin
  FConfig := Config;
  FPath := Path;
  FDefaultValue := ADefaultValue;

  if (not Self.Exists()) then
    Self.Value := FDefaultValue;
end;

function TSimbaSetting.Exists: Boolean;
begin
  Result := FConfig.Exists(FPath);
end;

function TSimbaSetting.Delete: Boolean;
begin
  Result := Self.Exists();
  if Result then
    FConfig.DeletePath(FPath);
end;

procedure TSimbaSetting_Boolean.SetValue(AValue: _T);
begin
  FConfig.SetValue(FPath, AValue);

  if (FOnChange <> nil) then
    FOnChange(AValue);
end;

function TSimbaSetting_Boolean.GetValue: _T;
begin
  Result := FConfig.GetValue(FPath, False);
end;

function TSimbaSetting_Integer.GetValue: _T;
begin
  Result := FConfig.GetValue(FPath, 0);
end;

procedure TSimbaSetting_Integer.SetValue(AValue: _T);
begin
  FConfig.SetValue(FPath, AValue);

  if (FOnChange <> nil) then
    FOnChange(AValue);
end;

function TSimbaSetting_String.GetValue: _T;
begin
  Result := FConfig.GetValue(FPath, '');
end;

procedure TSimbaSetting_String.SetValue(AValue: _T);
begin
  FConfig.SetValue(FPath, AValue);

  if (FOnChange <> nil) then
    FOnChange(AValue);
end;

procedure TSimbaSetting_StringArray.SetValue(AValue: _T);
var
  List: TStringList;
begin
  List := TStringList.Create();

  try
    List.AddStrings(AValue);

    FConfig.SetValue(FPath, List);
  finally
    List.Free();
  end;
end;

function TSimbaSetting_StringArray.GetValue: _T;
var
  List: TStringList;
  i: Int32;
begin
  List := TStringList.Create();

  try
    FConfig.GetValue(FPath, List, '');

    for i := 0 to List.Count -1 do
    begin
      SetLength(Result, Length(Result) + 1);

      Result[High(Result)] := List[i];
    end;
  finally
    List.Free();
  end;
end;

function TSimbaSettings.GetValue(Path: String): WideString;
begin
  Result := Config.GetValue(Path, '');
end;

function TSimbaSettings.GetKeys(Path: String): TStringArray;
var
  List: TStringList;
  i: Int32;
begin
  List := TStringList.Create();

  try
    Config.EnumSubKeys(Path, List);
    Config.EnumValues(Path, List);

    for i := 0 to List.Count - 1 do
    begin
      SetLength(Result, Length(Result) + 1);

      Result[High(Result)] := List[i];
    end;
  finally
    List.Free();
  end;
end;

procedure TSimbaSettings.SetValue(Path: String; AValue: WideString);
begin
  if (AValue = '') and Config.Exists(Path) then // delete
    Config.DeletePath(Path)
  else
    Config.SetValue(Path, AValue);
end;

procedure TSimbaSettings.Save;
begin
  Config.Flush();
end;

constructor TSimbaSettings.Create(FileName: String);
begin
  Config := TJSONConfig.Create(nil);
  Config.Formatted := True;

  try
    Config.FileName := FileName;
  except
    on e: Exception do
    begin
      WriteLn('Settings.json is corrupted, writing a new one.');

      // Write a backup
      if FileExists(FileName + '.corrupted') then
        DeleteFile(FileName + '.corrupted');
      RenameFile(FileName, FileName + '.corrupted');
    end;
  end;

  General.SaveScriptOnCompile := TSimbaSetting_Boolean.Create(Config, 'General/SaveScriptOnCompile', False);
  General.AutomaticallyCheckForSimbaUpdates := TSimbaSetting_Boolean.Create(Config, 'General/AutomaticallyCheckForSimbaUpdates', True);
  General.ShowSimbaNews := TSimbaSetting_Boolean.Create(Config, 'General/ShowSimbaNews', True);
  General.UpdateURL := TSimbaSetting_String.Create(Config, 'General/UpdateURL', SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF});
  General.VersionURL := TSimbaSetting_String.Create(Config, 'General/VersionURL', SimbaURL + 'Version');

  ColorPicker.AddColorToClipBoard := TSimbaSetting_Boolean.Create(Config, 'ColorPicker/AddColorToClipBoard', False);
  ColorPicker.AddCoordinateToClipBoard := TSimbaSetting_Boolean.Create(Config, 'ColorPicker/AddCoordinateToClipBoard', False);
  ColorPicker.AddColorAndCoordinateToClipboard := TSimbaSetting_Boolean.Create(Config, 'ColorPicker/AddColorAndCoordinateToClipboard', True);

  Form.Left := TSimbaSetting_Integer.Create(Config, 'Form/Left');
  Form.Top := TSimbaSetting_Integer.Create(Config, 'Form/Top');
  Form.Width := TSimbaSetting_Integer.Create(Config, 'Form/Width');
  Form.Height := TSimbaSetting_Integer.Create(Config, 'Form/Height');
  Form.Maximized := TSimbaSetting_Boolean.Create(Config, 'Form/Maximized', False);
  Form.ConsoleVisible := TSimbaSetting_Boolean.Create(Config, 'Form/ConsoleVisible', True);
  Form.TrayIconVisible := TSimbaSetting_Boolean.Create(Config, 'Form/TrayIconVisible', True);
  Form.FileBrowserVisible := TSimbaSetting_Boolean.Create(Config, 'Form/FileBrowserVisible', True);
  Form.FunctionListVisible := TSimbaSetting_Boolean.Create(Config, 'Form/FunctionListVisible', True);
  Form.NotesVisible := TSimbaSetting_Boolean.Create(Config, 'Form/NotesVisible', False);
  Form.NotesContent := TSimbaSetting_String.Create(Config, 'Form/NotesContent', '');
  Form.RecentFiles := TSimbaSetting_StringArray.Create(Config, 'Form/RecentFiles');

  Environment.PluginPath := TSimbaSetting_String.Create(Config, 'Environment/PluginPath', IncludeTrailingPathDelimiter(Application.Location + 'Plugins'));
  Environment.FontPath := TSimbaSetting_String.Create(Config, 'Environment/FontPath', IncludeTrailingPathDelimiter(Application.Location + 'Fonts'));
  Environment.IncludePath := TSimbaSetting_String.Create(Config, 'Environment/IncludePath', IncludeTrailingPathDelimiter(Application.Location + 'Includes'));
  Environment.ScriptPath := TSimbaSetting_String.Create(Config, 'Environment/ScriptPath', IncludeTrailingPathDelimiter(Application.Location + 'Scripts'));

  Editor.DefaultScriptPath := TSimbaSetting_String.Create(Config, 'Editor/DefaultScriptPath', IncludeTrailingPathDelimiter(Application.Location + 'AppData') + 'default.simba');
  Editor.HighlighterPath := TSimbaSetting_String.Create(Config, 'Editor/HighlighterPath', IncludeTrailingPathDelimiter(Application.Location + 'AppData') + 'highlighter.json');
  Editor.FontName := TSimbaSetting_String.Create(Config, 'Editor/FontName');
  Editor.FontSize := TSimbaSetting_Integer.Create(Config, 'Editor/FontSize');
  Editor.CaretPastEOL := TSimbaSetting_Boolean.Create(Config, 'Editor/CaretPastEOL', True);
  Editor.ShowSpecialCharacters := TSimbaSetting_Boolean.Create(Config, 'Editor/ShowSpecialCharacters', False);

  CodeTools.AutomaticallyShowAutoComplete := TSimbaSetting_Boolean.Create(Config, 'CodeTools/AutomaticallyShowAutoComplete', True);
  CodeTools.AutomaticallyShowParameterHints := TSimbaSetting_Boolean.Create(Config, 'CodeTools/AutomaticallyShowParameterHints', True);
end;

destructor TSimbaSettings.Destroy;
begin
  Save();

  General.SaveScriptOnCompile.Free();
  General.AutomaticallyCheckForSimbaUpdates.Free();
  General.ShowSimbaNews.Free();
  General.UpdateURL.Free();
  General.VersionURL.Free();

  ColorPicker.AddColorToClipBoard.Free();
  ColorPicker.AddCoordinateToClipBoard.Free();
  ColorPicker.AddColorAndCoordinateToClipboard.Free();

  Form.Left.Free();
  Form.Top.Free();
  Form.Width.Free();
  Form.Height.Free();
  Form.Maximized.Free();
  Form.ConsoleVisible.Free();
  Form.TrayIconVisible.Free();
  Form.FileBrowserVisible.Free();
  Form.FunctionListVisible.Free();
  Form.NotesVisible.Free();
  Form.NotesContent.Free();
  Form.RecentFiles.Free();

  Environment.PluginPath.Free();
  Environment.FontPath.Free();
  Environment.IncludePath.Free();
  Environment.ScriptPath.Free();

  Editor.DefaultScriptPath.Free();
  Editor.HighlighterPath.Free();
  Editor.FontName.Free();
  Editor.FontSize.Free();
  Editor.CaretPastEOL.Free();
  Editor.ShowSpecialCharacters.Free();

  CodeTools.AutomaticallyShowAutoComplete.Free();
  CodeTools.AutomaticallyShowParameterHints.Free();

  Config.Free();
end;

finalization
  if (SimbaSettings <> nil) then
    SimbaSettings.Free();

end.

