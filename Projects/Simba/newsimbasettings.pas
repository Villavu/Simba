unit newsimbasettings;

{$mode objfpc}{$H+}

{$I Simba.inc}

interface

{
  TODO:
       -  Add value constraints to each setting. Integer would have: min, max.
       String would have: regex to match?
       -  Use onChange return value?
       -  Figure out how to support some dynamic extensions. (Perhaps just make
           those functions use the MMLSettings API? they are already, kinda)
}

uses
  Classes, SysUtils,
  settings, // TMMLSettings
  comctrls // TTreeView
  ;

type
    TOnChangeSettings = function (obj: TObject): Boolean of object;
    TOnDefaultSetting = procedure (obj: TObject);

    TSetting = class(TObject)
    public
      procedure Save(MMLSettings: TMMLSettings); virtual; abstract;
      procedure Load(MMLSettings: TMMLSettings); virtual; abstract;

      destructor Destroy; virtual; abstract;
    end;

    TSettingsArray = array of TSetting;

    TValueSetting = class(TSetting)
    private
      FPath: string;
      FValueSet: boolean;
      FonChange: TOnChangeSettings;
      FonDefault: TOnDefaultSetting;
    public
      constructor Create(APath: String);
      destructor Destroy; override;

      property Path: string read FPath;
      property ValueSet: boolean read FValueSet;
      property onChange: TOnChangeSettings read FonChange write FonChange;
      property onDefault: TOnDefaultSetting read FonDefault write FonDefault;
    end;

    TIntegerSetting = class(TValueSetting)
    private
      FValue: Integer;
      function GetValue: Integer;
      procedure SetValue(val: Integer);
    public
      constructor Create(APath: String; val: Integer); overload;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetDefValue(val: Integer): Integer;

      property Value: Integer read GetValue write SetValue;
    end;

    TStringSetting = class(TValueSetting)
    private
      FValue: string;
      function GetValue: string;
      procedure SetValue(val: string);
    public
      constructor Create(APath: string; val: string); overload;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetDefValue(val: string): string; virtual;

      property Value: string read GetValue write SetValue;
    end;

    TBooleanSetting = class(TValueSetting)
    private
      FValue: boolean;
      function GetValue: Boolean;
      procedure SetValue(val: Boolean);
    public
      constructor Create(APath: String; val: Boolean); overload;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetDefValue(val: Boolean): Boolean;

      property Value: Boolean read GetValue write SetValue;
    end;

    TFileSetting = class(TStringSetting)
    private
      function GetValue: string;
      procedure SetValue(val: string);
    end;

    TPathSetting = class(TFileSetting)
    private
      procedure SetValue(val: string);
    end;

    TSection = class(TSetting)
    public
      Nodes: TSettingsArray;

      constructor Create();
      destructor Destroy; override;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;
      function AddChild(Child: TSetting): TSetting;
    end;

    TIncludesSection = class(TSection)
      Path: TPathSetting;
    end;

    TFontsSection = class(TSection)
      Path: TPathSetting;
      LoadOnStartUp: TBooleanSetting;
      Version: TIntegerSetting;
      VersionLink: TStringSetting;
      UpdateLink: TStringSetting;
    end;

    TExtensionsSection = class(TSection)
      Path: TPathSetting;
      FileExtension: TStringSetting;
    end;

    TScriptsSection = class(TSection)
      Path: TPathSetting;
    end;

    TFunctionListSection = class(TSection)
      ShowOnStart: TBooleanSetting;
    end;

    TTraySection = class(TSection)
      AlwaysVisible: TBooleanSetting;
    end;

    TInterpreterSection = class(TSection)
      _Type: TIntegerSetting;
      AllowSysCalls: TBooleanSetting;
    end;

    TSourceEditorSection = class(TSection)
      DefScriptPath: TFileSetting;
      LazColors: TBooleanSetting;
    end;

    TNewsSection = class(TSection)
      URL: TStringSetting;
    end;

    TPluginsSection = class(TSection)
      Path: TPathSetting;
    end;

    TTabsSection = class(TSection)
      OpenNextOnClose: TBooleanSetting;
      OpenScriptInNewTab: TBooleanSetting;
      CheckBeforeOpen: TBooleanSetting;
    end;

    TGeneralSection = class(TSection)
      MaxRecentFiles: TIntegerSetting;
    end;

    TUpdaterSection = class(TSection)
      CheckForUpdates: TBooleanSetting;
      AutomaticallyUpdate: TBooleanSetting;
      RemoteVersionLink: TStringSetting;
      RemoteLink: TStringSetting;
      CheckEveryXMinutes: TIntegerSetting;
    end;

    TColourPickerSection = class(TSection)
      AddToHistoryOnPick: TBooleanSetting;
      ShowHistoryOnPick: TBooleanSetting;
    end;

    TCodeHintsSection = class(TSection)
      ShowAutomatically: TBooleanSetting;
    end;

    TCodeCompletionSection = class(TSection)
      ShowAutomatically: TBooleanSetting;
    end;

    TScriptManagerSection = class(TSection)
      ServerURL: TStringSetting;
      StoragePath: TPathSetting;
      FileName: TStringSetting;
      FirstRun: TBooleanSetting;
    end;

    TMainForm = class(TSection)
      Position: TStringSetting;
      NormalSize: TStringSetting;
      State: TStringSetting;
      FunctionListShown: TBooleanSetting;
      ConsoleVisible: TBooleanSetting;
    end;

    TLastConfig = class(TSection)
      MainForm: TMainForm;
    end;

    TNotesSetion = class(TSection)
      Content: TStringSetting;
      Visible: TBooleanSetting;
    end;

    TSimbaSettings = class(TSection)
    public
      Includes: TIncludesSection;
      Fonts: TFontsSection;
      Extensions: TExtensionsSection;
      Scripts: TScriptsSection;
      FunctionList: TFunctionListSection;
      Tray: TTraySection;
      Interpreter: TInterpreterSection;
      SourceEditor: TSourceEditorSection;
      News: TNewsSection;
      Plugins: TPluginsSection;
      Tab: TTabsSection;
      General: TGeneralSection;
      Updater: TUpdaterSection;
      ColourPicker: TColourPickerSection;
      CodeHints: TCodeHintsSection;
      CodeCompletion: TCodeCompletionSection;
      Notes: TNotesSetion;

      ScriptManager: TScriptManagerSection;

      LastConfig: TLastConfig;

      MMLSettings: TMMLSettings;
      Oops: Boolean;

      constructor Create;

      procedure Save(SettingsFileName: String); overload;
    end;

    var
      SimbaSettings: TSimbaSettings;
      SimbaSettingsTreeView: TTreeView;
      SimbaSettingsFile: String;

    procedure CreateSimbaSettings(SettingsFileName: String);
    procedure ReloadSimbaSettings(SettingsFileName: String);
    procedure FreeSimbaSettings(Save: Boolean; SettingsFileName: String);

implementation

uses
   mufasabase,
   mufasatypes,
   fileutil,
   simbaunit; // mDebugLn

const
{$I settings_const.inc}

procedure CreateSimbaSettings(SettingsFileName: String);

  function FixSettingsFile: Boolean;
  begin
    result := true;
    mDebugLn('Could not load settings.xml!');
    if renamefileUTF8(SettingsFileName, 'settings.bak') then
    begin
      mDebugLn('Moved ' + SettingsFileName + ' to settings.bak');
    end else
    begin
      mDebugLn('Could not move ' + SettingsFileName + ' to settings.bak');
      if not deletefileUTF8(SettingsFileName) then
      begin
        mDebugLn('Couldnt delete the file either.');
        exit(false);
      end;
    end;

    SimbaSettingsTreeView.Items.Clear;
    SimbaSettings.MMLSettings.SaveToXML(SettingsFileName);
  end;

begin
  SimbaSettingsTreeView := TTreeView.Create(nil);

  SimbaSettings := TSimbaSettings.Create();
  SimbaSettings.MMLSettings := TMMLSettings.Create(SimbaSettingsTreeView.Items);

  SimbaSettings.Oops := False;

  if not FileExists(SettingsFileName) then
  begin
    SimbaSettingsTreeView.Items.Clear;
    SimbaSettings.MMLSettings.SaveToXML(SettingsFileName);
  end;
  SimbaSettingsTreeView.Items.Clear;

  if not SimbaSettings.MMLSettings.LoadFromXML(SettingsFileName) then
  begin
    if not FixSettingsFile() then
    begin
      mDebugLn('Could not create, move or delete ' + SimbaSettingsFile);
      mDebugLn('***************** Giving up... ********************');
      SimbaSettings.Oops := True;
    end;
  end;

  //Writeln('Loading from: ' + SettingsFileName);

  SimbaSettings.MMLSettings.LoadFromXML(SettingsFileName);

  SimbaSettings.Load(SimbaSettings.MMLSettings);
end;

procedure ReloadSimbaSettings(SettingsFileName: String);
begin
  SimbaSettingsTreeView.Items.Clear;
  if not SimbaSettings.MMLSettings.LoadFromXML(SettingsFileName) then
    mDebugLn('Failed to reload settings?');
  SimbaSettings.Load(SimbaSettings.MMLSettings);
end;

procedure FreeSimbaSettings(Save: Boolean; SettingsFileName: String);
begin
  if Save then
    SimbaSettings.Save(SettingsFileName);

  SimbaSettings.MMLSettings.Free;
  SimbaSettings.Free;

  SimbaSettingsTreeView.Free;
end;

{ Values }

constructor TValueSetting.Create(APath: String);
begin
  FPath := APath;
  FonChange := nil;
  FonDefault := nil;
end;

destructor TValueSetting.Destroy;
begin
end;

constructor TIntegerSetting.Create(APath: String; val: Integer);
begin
  Create(Path);
  Value := val;
end;

function TIntegerSetting.GetValue: Integer;
begin
  if (not (FValueSet)) then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TIntegerSetting.GetDefValue(val: Integer): Integer;
begin
  if (not (FValueSet)) then
    Value := val;

  Exit(FValue);
end;

procedure TIntegerSetting.SetValue(val: Integer);
begin
  FValueSet := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TIntegerSetting.Load(MMLSettings: TMMLSettings);
begin
  try
    if MMLSettings.KeyExists(FPath) then
      Value := StrToInt(MMLSettings.GetKeyValue(FPath))
    else
      if assigned(onDefault) then
        onDefault(Self);
  except
    On E : EConvertError do
    begin
      FValueSet := False;
      Writeln ('Invalid number encountered');
    end;
  end;
end;

procedure TIntegerSetting.Save(MMLSettings: TMMLSettings);
begin
  if (not (FValueSet)) then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    Exit;
  end;

  if MMLSettings.KeyExists(FPath) then
    MMLSettings.SetKeyValue(FPath, IntToStr(value))
  else
  begin
    if MMLSettings.CreateKey(FPath, True) then
      MMLSettings.SetKeyValue(FPath, IntToStr(value))
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

constructor TStringSetting.Create(APath: String; val: String);
begin
  Create(Path);
  Value := val;
end;

function TStringSetting.GetValue: String;
begin
  //writeln('Get Value: ' + APath);
  if (not (FValueSet)) then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TStringSetting.GetDefValue(val: String): String;
begin
  if (not (FValueSet)) then
    Value := val;

  Result := Value;
end;

procedure TStringSetting.SetValue(val: String);
begin
  FValueSet := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TStringSetting.Save(MMLSettings: TMMLSettings);
begin
  if (not (FValueSet)) then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    Exit;
  end;

  if MMLSettings.KeyExists(FPath) then
    MMLSettings.SetKeyValue(FPath, Value)
  else
  begin
    if MMLSettings.CreateKey(FPath, True) then
      MMLSettings.SetKeyValue(FPath, Value)
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

procedure TStringSetting.Load(MMLSettings: TMMLSettings);
begin
  if MMLSettings.KeyExists(FPath) then
    Value := MMLSettings.GetKeyValue(FPath)
  else
    if Assigned(onDefault) then
      onDefault(Self);
end;

constructor TBooleanSetting.Create(APath: String; val: Boolean);
begin
  Create(Path);
  Value := val;
end;

function TBooleanSetting.GetValue: Boolean;
begin
  //writeln('Get Value: ' + APath);
  if (not (FValueSet)) then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TBooleanSetting.GetDefValue(val: Boolean): Boolean;
begin
  if (not (FValueSet)) then
    Value := val;

  Exit(FValue);
end;

procedure TBooleanSetting.SetValue(val: Boolean);
begin
  //writeln('Setting ' + APath + ' to ' + BoolToStr(val));
  FValueSet := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TBooleanSetting.Save(MMLSettings: TMMLSettings);
begin
  if (not (FValueSet)) then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    Exit;
  end;

  if MMLSettings.KeyExists(FPath) then
    MMLSettings.SetKeyValue(FPath, BoolToStr(Value, True))
  else
  begin
    if MMLSettings.CreateKey(FPath, True) then
      MMLSettings.SetKeyValue(FPath, BoolToStr(Value, True))
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

procedure TBooleanSetting.Load(MMLSettings: TMMLSettings);
begin
  try
    if MMLSettings.KeyExists(FPath) then
    begin
      Value := StrToBool(MMLSettings.GetKeyValue(FPath));
      //writeln('Loaded: ' + BoolToStr(value) + ' for ' + APath);
    end else
      if assigned(onDefault) then
        onDefault(Self);
  except
    On E : EConvertError do
    begin
      FValueSet := False;
      Writeln ('Invalid boolean encountered');
    end;
  end;
end;

function IsAbsolute(const filename: string): boolean;
begin
  {$IFDEF WINDOWS}
    Result := False;
    if (Length(filename) > 2) then
      Result := (filename[2] = ':');
  {$ELSE}
    Result := (filename[1] = DS);
  {$ENDIF}
end;

function TFileSetting.GetValue: string;
begin
  if (FValueSet) then
  begin
    Result := FValue;
    if (not (IsAbsolute(Result))) then
      Result := AppPath + Result;
  end else
    raise Exception.Create('Value is not set yet.');
end;

procedure TFileSetting.SetValue(val: string);
begin
  {$IFNDEF NOTPORTABLE}
  if (IsAbsolute(val)) then
    val := ExtractRelativepath(AppPath, val);
  {$ENDIF}

  FValue := val;
  FValueSet := True;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TPathSetting.SetValue(val: string);
begin
  {$IFNDEF NOTPORTABLE}
  if (IsAbsolute(val)) then
    val := ExtractRelativepath(AppPath, val);
  {$ENDIF}

  FValue := IncludeLeadingPathDelimiter(val);
  FValueSet := True;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ }
constructor TSection.Create;
begin
  SetLength(Nodes, 0);
end;

destructor TSection.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(Nodes) - 1 do
    nodes[i].Free;
end;

function TSection.AddChild(Child: TSetting): TSetting;
begin
  SetLength(Nodes, Length(Nodes) + 1);
  Nodes[Length(Nodes) - 1] := Child;
  Result := Child;
end;

procedure TSection.Save(MMLSettings: TMMLSettings);

var
  i: Integer;
begin
  for i := 0 to Length(Nodes) - 1 do
    nodes[i].Save(MMLSettings)
end;

procedure TSection.Load(MMLSettings: TMMLSettings);

var
  i: Integer;

begin
  for i := 0 to Length(Nodes) - 1 do
    nodes[i].Load(MMLSettings)
end;

procedure GetIncludePath(obj: TObject);
begin
  TPathSetting(obj).Value := DataPath + 'Includes' + DS;
end;

procedure GetPluginPath(obj: TObject);
begin
  TPathSetting(obj).Value := DataPath + 'Plugins' + DS;
end;

{$IFDEF USE_EXTENSIONS}
procedure GetExtPath(obj: TObject);
begin
  TPathSetting(obj).Value := DataPath + 'Extensions' + DS;
end;
{$ENDIF}

procedure GetScriptPath(obj: TObject);
begin
  TPathSetting(obj).Value := DocPath + 'Scripts' + DS;
end;

procedure GetFontPath(obj: TObject);
begin
  TPathSetting(obj).Value := DataPath + 'Fonts' + DS;
end;

procedure GetDefScriptPath(obj: TObject);
begin
  TFileSetting(obj).Value := DataPath + 'default.simba';
end;

procedure GetUpdaterGetCheckForUpdates(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetUpdaterGetAutomaticallyUpdate(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetUpdaterCheckEveryXminutes(obj: TObject); begin TIntegerSetting(obj).Value := 30; end;
procedure GetUpdaterLink(obj: TObject); begin TStringSetting(obj).Value := SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF};; end;
procedure GetUpdaterVersionLink(obj: TObject); begin TStringSetting(obj).Value := SimbaURL + 'Version'; end;

procedure GetInterpreterType(obj: TObject); begin TIntegerSetting(obj).Value := 0; end; // 0 is PS
procedure GetInterpreterAllowSysCalls(obj: TObject); begin TBooleanSetting(obj).Value := False; end;

procedure GetFontsLoadOnStartUp(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetFontsVersion(obj: TObject); begin TIntegerSetting(obj).Value := -1; end;
procedure GetFontsVersionLink(obj: TObject); begin TStringSetting(obj).Value := FontURL + 'Version'; end;
procedure GetFontsUpdateLink(obj: TObject); begin TStringSetting(obj).Value := FontURL + 'Fonts.tar.bz2'; end;


procedure GetTabOpenNextOnClose(obj: TObject); begin TBooleanSetting(obj).Value := False; end;
procedure GetOpenScriptInNewTab(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetTabCheckBeforeOpen(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetGeneralMaxRecentFiles(obj: TObject); begin TIntegerSetting(obj).Value := 10; end;

procedure GetColourPickerShowHistoryonPick(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetColourPickerAddToHistoryOnPick(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetFunctionListShowOnStart(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetCodeHintsShowAutomatically(obj: TObject); begin TBooleanSetting(obj).Value := True; end;
procedure GetCodeCompletionShowAutomatically(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetScriptManagerURL(obj: TObject); begin TStringSetting(obj).Value := 'http://127.0.0.1/'; end;
procedure GetScriptManagerPath(obj: TObject); begin TPathSetting(obj).Value := SimbaSettings.Scripts.Path.Value+'ScriptStorage.xml'; end;
procedure GetScriptManagerFile(obj: TObject); begin TStringSetting(obj).Value := 'ScriptManager.xml'; end;
procedure GetScriptManagerFirstRun(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetSourceEditorLazColors(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetExtensionsFileExtension(obj: TObject); begin TStringSetting(obj).Value := 'sex'; end;


procedure GetMainFormNormalSize(obj: TObject); begin TStringSetting(obj).Value := '739:555'; end;
procedure GetMainFormPosition(obj: TObject); begin TStringSetting(obj).Value := ''; end;
procedure GetMainFormState(obj: TObject); begin TStringSetting(obj).Value := 'normal'; end;

procedure GetMainFormConsoleVisible(obj: TObject); begin TBooleanSetting(obj).Value := False; end;

procedure GetTrayAlwaysVisible(obj: TObject); begin TBooleanSetting(obj).Value := True; end;

procedure GetSimbaNewsURL(obj: TObject); begin TStringSetting(obj).Value := 'http://simba.villavu.com/bin/news'; end;

procedure GetNotesContent(obj: TObject); begin TStringSetting(obj).Value := 'FQAAAHic88svSS1WCE5NLsnMz1PQVXAqSvRWBABR+Abt'; end;
procedure GetNotesVisible(obj: TObject); begin TBooleanSetting(obj).Value := False; end;

constructor TSimbaSettings.Create;
begin
  inherited;

  Includes := AddChild(TIncludesSection.Create()) as TIncludesSection;
  Includes.Path := Includes.AddChild(TPathSetting.Create(ssIncludesPath)) as TPathSetting;
  Includes.Path.onDefault := @GetIncludePath;

  Fonts := AddChild(TFontsSection.Create()) as TFontsSection;
  Fonts.Path := Fonts.AddChild(TPathSetting.Create(ssFontsPath)) as TPathSetting;
  Fonts.Path.onDefault := @GetFontPath;

  Fonts.LoadOnStartUp := Fonts.AddChild(TBooleanSetting.Create(ssLoadFontsOnStart)) as TBooleanSetting;
  Fonts.LoadOnStartUp.onDefault := @GetFontsLoadOnStartUp;
  Fonts.Version := Fonts.AddChild(TIntegerSetting.Create(ssFontsVersion)) as TIntegerSetting;
  Fonts.Version.onDefault := @GetFontsVersion;
  Fonts.VersionLink := Fonts.AddChild(TStringSetting.Create(ssFontsVersionLink)) as TStringSetting;
  Fonts.VersionLink.onDefault := @GetFontsVersionLink;
  Fonts.UpdateLink := Fonts.AddChild(TStringSetting.Create(ssFontsLink)) as TStringSetting;
  Fonts.UpdateLink.onDefault := @GetFontsUpdateLink;

  {$IFDEF USE_EXTENSIONS}
  Extensions := AddChild(TExtensionsSection.Create()) as TExtensionsSection;
  Extensions.Path := Extensions.AddChild(TPathSetting.Create(ssExtensionsPath)) as TPathSetting;
  {$IFDEF USE_EXTENSIONS}Extensions.Path.onDefault := @GetExtPath;{$ENDIF}
  Extensions.FileExtension := Extensions.AddChild(TStringSetting.Create(ssExtensionsFileExtension)) as TStringSetting;
  Extensions.FileExtension.onDefault := @GetExtensionsFileExtension;
  {$ENDIF}

  Scripts := AddChild(TScriptsSection.Create()) as TScriptsSection;
  Scripts.Path := Scripts.AddChild(TPathSetting.Create(ssScriptsPath)) as TPathSetting;
  Scripts.Path.onDefault := @GetScriptPath;

  FunctionList := AddChild(TFunctionListSection.Create()) as TFunctionListSection;
  FunctionList.ShowOnStart := FunctionList.AddChild(TBooleanSetting.Create(ssFunctionListShowOnStart)) as TBooleanSetting;
  FunctionList.ShowOnStart.onDefault := @GetFunctionListShowOnStart;

  Tray := AddChild(TTraySection.Create()) as TTraySection;
  Tray.AlwaysVisible := Tray.AddChild(TBooleanSetting.Create(ssTrayAlwaysVisible)) as TBooleanSetting;
  Tray.AlwaysVisible.onDefault := @GetTrayAlwaysVisible;

  Interpreter := AddChild(TInterpreterSection.Create()) as TInterpreterSection;
  Interpreter._Type := Interpreter.AddChild(TIntegerSetting.Create(ssInterpreterType)) as TIntegerSetting;
  Interpreter._Type.onDefault := @GetInterpreterType;
  Interpreter.AllowSysCalls := Interpreter.AddChild(TBooleanSetting.Create(ssInterpreterAllowSysCalls)) as TBooleanSetting;
  Interpreter.AllowSysCalls.onDefault := @GetInterpreterAllowSysCalls;

  SourceEditor := AddChild(TSourceEditorSection.Create()) as TSourceEditorSection;
  SourceEditor.DefScriptPath := SourceEditor.AddChild(TFileSetting.Create(ssSourceEditorDefScriptPath)) as TFileSetting;
  SourceEditor.DefScriptPath.onDefault := @GetDefScriptPath;
  SourceEditor.LazColors := SourceEditor.AddChild(TBooleanSetting.Create(ssSourceEditorLazColors)) as TBooleanSetting;
  SourceEditor.LazColors.onDefault := @GetSourceEditorLazColors;

  News := AddChild(TNewsSection.Create()) as TNewsSection;
  News.URL := News.AddChild(TStringSetting.Create(ssNewsLink)) as TStringSetting;
  News.URL.onDefault := @GetSimbaNewsURL;

  Plugins := AddChild(TPluginsSection.Create()) as TPluginsSection;
  Plugins.Path := Plugins.AddChild(TPathSetting.Create(ssPluginsPath)) as TPathSetting;
  Plugins.Path.onDefault := @GetPluginPath;

  Tab := AddChild(TTabsSection.Create()) as TTabsSection;
  Tab.OpenNextOnClose := Tab.AddChild(TBooleanSetting.Create(ssTabsOpenNextOnClose)) as TBooleanSetting;
  Tab.OpenNextOnClose.onDefault := @GetTabOpenNextOnClose;
  Tab.OpenScriptInNewTab := Tab.AddChild(TBooleanSetting.Create(ssTabsOpenScriptInNewTab)) as TBooleanSetting;
  Tab.OpenScriptInNewTab.onDefault := @GetOpenScriptInNewTab;
  Tab.CheckBeforeOpen := Tab.AddChild(TBooleanSetting.Create(ssTabsCheckBeforeOpen)) as TBooleanSetting;
  Tab.CheckBeforeOpen.onDefault := @GetTabCheckBeforeOpen;

  General := AddChild(TGeneralSection.Create()) as TGeneralSection;
  General.MaxRecentFiles := General.AddChild(TIntegerSetting.Create(ssMaxRecentFiles)) as TIntegerSetting;
  General.MaxRecentFiles.onDefault := @GetGeneralMaxRecentFiles;

  Updater := AddChild(TUpdaterSection.Create()) as TUpdaterSection;
  Updater.CheckForUpdates := Updater.AddChild(TBooleanSetting.Create(ssCheckUpdate)) as TBooleanSetting;
  Updater.CheckForUpdates.onDefault := @GetUpdaterGetCheckForUpdates;
  Updater.AutomaticallyUpdate := Updater.AddChild(TBooleanSetting.Create(ssAutomaticallyUpdate)) as TBooleanSetting;
  Updater.AutomaticallyUpdate.onDefault := @GetUpdaterGetAutomaticallyUpdate;
  Updater.RemoteVersionLink := Updater.AddChild(TStringSetting.Create(ssUpdaterVersionLink)) as TStringSetting;
  Updater.RemoteversionLink.onDefault := @GetUpdaterVersionLink;
  Updater.RemoteLink := Updater.AddChild(TStringSetting.Create(ssUpdaterLink)) as TStringSetting;
  Updater.RemoteLink.onDefault := @GetUpdaterLink;
  Updater.CheckEveryXMinutes := Updater.AddChild(TIntegerSetting.Create(ssCheckUpdateMinutes)) as TIntegerSetting;
  Updater.CheckEveryXMinutes.onDefault := @GetUpdaterCheckEveryXminutes;

  ColourPicker := AddChild(TColourPickerSection.Create()) as TColourPickerSection;
  ColourPicker.AddToHistoryOnPick := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerAddToHistoryOnPick)) as TBooleanSetting;
  ColourPicker.AddToHistoryOnPick.onDefault := @GetColourPickerAddToHistoryOnPick;
  ColourPicker.ShowHistoryOnPick := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerShowHistoryOnPick)) as TBooleanSetting;
  ColourPicker.ShowHistoryOnPick.onDefault := @GetColourPickerShowHistoryonPick;

  CodeHints := AddChild(TCodeHintsSection.Create()) as TCodeHintsSection;
  CodeHints.ShowAutomatically := CodeHints.AddChild(TBooleanSetting.Create(ssCodeHintsShowAutomatically)) as TBooleanSetting;
  CodeHints.ShowAutomatically.onDefault := @GetCodeHintsShowAutomatically;

  CodeCompletion := AddChild(TCodeCompletionSection.Create()) as TCodeCompletionSection;
  CodeCompletion.ShowAutomatically := CodeCompletion.AddChild(TBooleanSetting.Create(ssCodeCompletionShowAutomatically)) as TBooleanSetting;
  CodeCompletion.ShowAutomatically.onDefault := @GetCodeCompletionShowAutomatically;

  Notes := AddChild(TNotesSetion.Create()) as TNotesSetion;
  Notes.Content := Notes.AddChild(TStringSetting.Create(ssNotesContent)) as TStringSetting;
  Notes.Content.onDefault := @GetNotesContent;
  Notes.Visible := Notes.AddChild(TBooleanSetting.Create(ssNotesVisible)) as TBooleanSetting;
  Notes.Visible.onDefault := @GetNotesVisible;

  ScriptManager := AddChild(TScriptManagerSection.Create()) as TScriptManagerSection;
  ScriptManager.ServerURL := ScriptManager.AddChild(TStringSetting.Create(ssSMURL)) as TStringSetting;
  ScriptManager.ServerURL.onDefault := @GetScriptManagerURL;
  ScriptManager.StoragePath := ScriptManager.AddChild(TPathSetting.Create(ssSMPath)) as TPathSetting;
  ScriptManager.StoragePath.onDefault := @GetScriptManagerPath;
  ScriptManager.FileName := ScriptManager.AddChild(TStringSetting.Create(ssSMFile)) as TStringSetting;
  ScriptManager.FileName.onDefault := @GetScriptManagerFile;
  ScriptManager.FirstRun := ScriptManager.AddChild(TBooleanSetting.Create(ssFRun)) as TBooleanSetting;
  ScriptManager.FirstRun.onDefault := @GetScriptManagerFirstRun;

  LastConfig := AddChild(TLastConfig.Create()) as TLastConfig;
  LastConfig.MainForm := LastConfig.AddChild(TMainForm.Create()) as TMainForm;
  LastConfig.MainForm.Position := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormPosition)) as TStringSetting;
  LastConfig.MainForm.Position.onDefault := @GetMainFormPosition;
  LastConfig.MainForm.NormalSize := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormNormalSize)) as TStringSetting;
  LastConfig.MainForm.NormalSize.onDefault := @GetMainFormNormalSize;
  LastConfig.MainForm.State := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormState)) as TStringSetting;
  LastConfig.MainForm.State.onDefault := @GetMainFormState;
  LastConfig.MainForm.FunctionListShown := LastConfig.MainForm.AddChild(TBooleanSetting.Create(ssFunctionListShown)) as TBooleanSetting;
  LastConfig.MainForm.FunctionListShown.onDefault := @GetFunctionListShowOnStart;
  LastConfig.MainForm.ConsoleVisible := LastConfig.MainForm.AddChild(TBooleanSetting.Create(ssFunctionListShown)) as TBooleanSetting;
  LastConfig.MainForm.ConsoleVisible.onDefault := @GetMainFormConsoleVisible;

end;

procedure TSimbaSettings.Save(SettingsFileName: String);
begin
  Save(MMLSettings);
  MMLSettings.SaveToXML(SettingsFileName);
end;


end.

