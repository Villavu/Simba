unit newsimbasettings;

{$mode objfpc}{$H+}

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

    TSetting = class(TObject)
      procedure Save(MMLSettings: TMMLSettings); virtual; abstract;
      procedure Load(MMLSettings: TMMLSettings); virtual; abstract;

      destructor Destroy; virtual; abstract;
    end;

    TSettingsArray = Array of TSetting;

    TValueSetting = class(TSetting)
      constructor Create(Path: String);
      destructor Destroy; override;

    public
      APath: String;
      set_value: Boolean;

      onChange: TOnChangeSettings;
    end;

    TIntegerSetting = class(TValueSetting)
      constructor Create(Path: String); overload;
      constructor Create(Path: String; val: Integer); overload;
      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetValue: Integer;
      procedure SetValue(val: Integer);
      function GetDefValue(val: Integer): Integer;
    public
      property Value: Integer read GetValue write SetValue;
    private
      FValue: Integer;
    end;

    TStringSetting = class(TValueSetting)
      constructor Create(Path: String); overload;
      constructor Create(Path: String; val: String); overload;
      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetValue: String;
      procedure SetValue(val: String);
      function GetDefValue(val: String): String;
    public
      property Value: String read GetValue write SetValue;
    private
      FValue: String;
    end;

    TBooleanSetting = class(TValueSetting)
      constructor Create(Path: String); overload;
      constructor Create(Path: String; val: Boolean); overload;
      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetValue: Boolean;
      procedure SetValue(val: Boolean);
      function GetDefValue(val: Boolean): Boolean;
    public
      property Value: Boolean read GetValue write SetValue;
    private
      FValue: Boolean;
    end;

    TSection = class(TSetting)
      constructor Create();
      destructor Destroy; override;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;
      function AddChild(Child: TSetting): TSetting;

      public
        Nodes: TSettingsArray;
    end;

    TIncludesSection = class(TSection)
      Path: TStringSetting;
    end;

    TFontsSection = class(TSection)
      Path: TStringSetting;
      LoadOnStartUp: TBooleanSetting;
      Version: TIntegerSetting;
      VersionLink: TStringSetting;
      UpdateLink: TStringSetting;
    end;

    TExtensionsSection = class(TSection)
      Path: TStringSetting;
      FileExtension: TStringSetting;
    end;

    TScriptsSection = class(TSection)
      Path: TStringSetting;
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
      DefScriptPath: TStringSetting;
      LazColors: TBooleanSetting;
    end;

    TNewsSection = class(TSection)
      URL: TStringSetting;
    end;

    TPluginsSection = class(TSection)
      Path: TStringSetting;
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

    TSimbaSettings = class(TSection)
      constructor Create;

      procedure Save(SettingsFileName: String); overload;

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

      LastConfig: TLastConfig;

      MMLSettings: TMMLSettings;
      Oops: Boolean;
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
   fileutil; // mDebugLn

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

constructor TSimbaSettings.Create;
begin
  inherited;

  Includes := AddChild(TIncludesSection.Create()) as TIncludesSection;
  Includes.Path := Includes.AddChild(TStringSetting.Create(ssIncludesPath)) as TStringSetting;

  Fonts := AddChild(TFontsSection.Create()) as TFontsSection;
  Fonts.Path := Fonts.AddChild(TStringSetting.Create(ssFontsPath)) as TStringSetting;
  Fonts.LoadOnStartUp := Fonts.AddChild(TBooleanSetting.Create(ssLoadFontsOnStart)) as TBooleanSetting;
  Fonts.Version := Fonts.AddChild(TIntegerSetting.Create(ssFontsVersion)) as TIntegerSetting;
  Fonts.VersionLink := Fonts.AddChild(TStringSetting.Create(ssFontsVersionLink)) as TStringSetting;
  Fonts.UpdateLink := Fonts.AddChild(TStringSetting.Create(ssFontsLink)) as TStringSetting;

  Extensions := AddChild(TExtensionsSection.Create()) as TExtensionsSection;
  Extensions.Path := Extensions.AddChild(TStringSetting.Create(ssExtensionsPath)) as TStringSetting;
  Extensions.FileExtension := Extensions.AddChild(TStringSetting.Create(ssExtensionsFileExtension)) as TStringSetting;

  Scripts := AddChild(TScriptsSection.Create()) as TScriptsSection;
  Scripts.Path := Scripts.AddChild(TStringSetting.Create(ssScriptsPath)) as TStringSetting;

  FunctionList := AddChild(TFunctionListSection.Create()) as TFunctionListSection;
  FunctionList.ShowOnStart := FunctionList.AddChild(TBooleanSetting.Create(ssFunctionListShowOnStart)) as TBooleanSetting;

  Tray := AddChild(TTraySection.Create()) as TTraySection;
  Tray.AlwaysVisible := Tray.AddChild(TBooleanSetting.Create(ssTrayAlwaysVisible)) as TBooleanSetting;

  Interpreter := AddChild(TInterpreterSection.Create()) as TInterpreterSection;
  Interpreter._Type := Interpreter.AddChild(TIntegerSetting.Create(ssInterpreterType)) as TIntegerSetting;
  Interpreter.AllowSysCalls := Interpreter.AddChild(TBooleanSetting.Create(ssInterpreterAllowSysCalls)) as TBooleanSetting;

  SourceEditor := AddChild(TSourceEditorSection.Create()) as TSourceEditorSection;
  SourceEditor.DefScriptPath := SourceEditor.AddChild(TStringSetting.Create(ssSourceEditorDefScriptPath)) as TStringSetting;
  SourceEditor.LazColors := SourceEditor.AddChild(TBooleanSetting.Create(ssSourceEditorLazColors)) as TBooleanSetting;

  News := AddChild(TNewsSection.Create()) as TNewsSection;
  News.URL := News.AddChild(TStringSetting.Create(ssNewsLink)) as TStringSetting;

  Plugins := AddChild(TPluginsSection.Create()) as TPluginsSection;
  Plugins.Path := Plugins.AddChild(TStringSetting.Create(ssPluginsPath)) as TStringSetting;

  Tab := AddChild(TTabsSection.Create()) as TTabsSection;
  Tab.OpenNextOnClose := Tab.AddChild(TBooleanSetting.Create(ssTabsOpenNextOnClose)) as TBooleanSetting;
  Tab.OpenScriptInNewTab := Tab.AddChild(TBooleanSetting.Create(ssTabsOpenScriptInNewTab)) as TBooleanSetting;
  Tab.OpenScriptInNewTab := Tab.AddChild(TBooleanSetting.Create(ssTabsOpenScriptInNewTab)) as TBooleanSetting;
  Tab.CheckBeforeOpen := Tab.AddChild(TBooleanSetting.Create(ssTabsCheckBeforeOpen)) as TBooleanSetting;

  General := AddChild(TGeneralSection.Create()) as TGeneralSection;
  General.MaxRecentFiles := General.AddChild(TIntegerSetting.Create(ssMaxRecentFiles)) as TIntegerSetting;

  Updater := AddChild(TUpdaterSection.Create()) as TUpdaterSection;
  Updater.CheckForUpdates := Updater.AddChild(TBooleanSetting.Create(ssCheckUpdate)) as TBooleanSetting;
  Updater.RemoteVersionLink := Updater.AddChild(TStringSetting.Create(ssUpdaterVersionLink)) as TStringSetting;
  Updater.RemoteLink := Updater.AddChild(TStringSetting.Create(ssUpdaterLink)) as TStringSetting;
  Updater.CheckEveryXMinutes := Updater.AddChild(TIntegerSetting.Create(ssCheckUpdateMinutes)) as TIntegerSetting;

  ColourPicker := AddChild(TColourPickerSection.Create()) as TColourPickerSection;
  ColourPicker.AddToHistoryOnPick := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerAddToHistoryOnPick)) as TBooleanSetting;
  ColourPicker.ShowHistoryOnPick := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerShowHistoryOnPick)) as TBooleanSetting;

  CodeHints := AddChild(TCodeHintsSection.Create()) as TCodeHintsSection;
  CodeHints.ShowAutomatically := CodeHints.AddChild(TBooleanSetting.Create(ssCodeHintsShowAutomatically)) as TBooleanSetting;

  CodeCompletion := AddChild(TCodeCompletionSection.Create()) as TCodeCompletionSection;
  CodeCompletion.ShowAutomatically := CodeCompletion.AddChild(TBooleanSetting.Create(ssCodeCompletionShowAutomatically)) as TBooleanSetting;

  LastConfig := AddChild(TLastConfig.Create()) as TLastConfig;
  LastConfig.MainForm := LastConfig.AddChild(TMainForm.Create()) as TMainForm;
  LastConfig.MainForm.Position := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormPosition)) as TStringSetting;
  LastConfig.MainForm.NormalSize := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormNormalSize)) as TStringSetting;
  LastConfig.MainForm.State := LastConfig.MainForm.AddChild(TStringSetting.Create(ssMainFormState)) as TStringSetting;
  LastConfig.MainForm.FunctionListShown := LastConfig.MainForm.AddChild(TBooleanSetting.Create(ssFunctionListShown)) as TBooleanSetting;
  LastConfig.MainForm.ConsoleVisible := LastConfig.MainForm.AddChild(TBooleanSetting.Create(ssFunctionListShown)) as TBooleanSetting;

end;

procedure TSimbaSettings.Save(SettingsFileName: String);
begin
  Save(MMLSettings);
  MMLSettings.SaveToXML(SettingsFileName);
end;

{ Values }

constructor TValueSetting.Create(Path: String);
begin
  APath := Path;
  set_value := False;
end;

destructor TValueSetting.Destroy;
begin
end;

constructor TIntegerSetting.Create(Path: String);
begin
  inherited Create(Path);
end;

constructor TIntegerSetting.Create(Path: String; val: Integer);
begin
  Create(Path);
  value := val;
end;

function TIntegerSetting.GetValue: Integer;
begin
  //writeln('Get Value: ' + APath);
  if not set_value then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TIntegerSetting.GetDefValue(val: Integer): Integer;
begin
  if set_value then
    Exit(FValue)
  else
    Value := val;
  Exit(FValue);
end;

procedure TIntegerSetting.SetValue(val: Integer);
begin
  set_value := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TIntegerSetting.Load(MMLSettings: TMMLSettings);
begin
  try
    if MMLSettings.KeyExists(APath) then
    begin
      value := StrToInt(MMLSettings.GetKeyValue(APath));
      //writeln('Loaded: ' + IntToStr(value) + ' for ' + APath);
    end
  except
    On E : EConvertError do
    begin
      set_value := False;
      Writeln ('Invalid number encountered');
    end;
  end;
end;

procedure TIntegerSetting.Save(MMLSettings: TMMLSettings);
begin
  if not set_value then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    exit;
  end;

  if MMLSettings.KeyExists(APath) then
    MMLSettings.SetKeyValue(APath, IntToStr(value))
  else
  begin
    if MMLSettings.CreateKey(APath, True) then
      MMLSettings.SetKeyValue(APath, IntToStr(value))
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

constructor TStringSetting.Create(Path: String);
begin
  inherited Create(Path);
end;

constructor TStringSetting.Create(Path: String; val: String);
begin
  Create(Path);
  value := val;
end;

function TStringSetting.GetValue: String;
begin
  //writeln('Get Value: ' + APath);
  if not set_value then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TStringSetting.GetDefValue(val: String): String;
begin
  if set_value then
    Exit(FValue)
  else
    Value := val;
  Exit(FValue);
end;

procedure TStringSetting.SetValue(val: String);
begin
  set_value := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TStringSetting.Save(MMLSettings: TMMLSettings);
begin
  if not set_value then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    exit;
  end;

  if MMLSettings.KeyExists(APath) then
    MMLSettings.SetKeyValue(APath, value)
  else
  begin
    if MMLSettings.CreateKey(APath, True) then
      MMLSettings.SetKeyValue(APath, value)
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

procedure TStringSetting.Load(MMLSettings: TMMLSettings);
begin
  if MMLSettings.KeyExists(APath) then
  begin
    value := MMLSettings.GetKeyValue(APath);
    //writeln('Loaded: ' + value + ' for ' + APath);
  end
end;

constructor TBooleanSetting.Create(Path: String);
begin
  inherited Create(Path);
end;

constructor TBooleanSetting.Create(Path: String; val: Boolean);
begin
  Create(Path);
  value := val;
end;

function TBooleanSetting.GetValue: Boolean;
begin
  //writeln('Get Value: ' + APath);
  if not set_value then
    raise Exception.Create('Value is not set yet.')
  else
    Result := FValue;
end;

function TBooleanSetting.GetDefValue(val: Boolean): Boolean;
begin
  if set_value then
    Exit(FValue)
  else
    Value := val;
  Exit(FValue);
end;

procedure TBooleanSetting.SetValue(val: Boolean);
begin
  //writeln('Setting ' + APath + ' to ' + BoolToStr(val));
  set_value := True;
  FValue := val;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TBooleanSetting.Save(MMLSettings: TMMLSettings);
begin
  if not set_value then
  begin
    //writeln(APath + ': Not setting anything, set_value = False');
    exit;
  end;

  if MMLSettings.KeyExists(APath) then
    MMLSettings.SetKeyValue(APath, BoolToStr(value, True))
  else
  begin
    if MMLSettings.CreateKey(APath, True) then
      MMLSettings.SetKeyValue(APath, BoolToStr(value, True))
    {else
      writeln('Could not create key: ' + APath);}
  end;
end;

procedure TBooleanSetting.Load(MMLSettings: TMMLSettings);
begin
  try
    if MMLSettings.KeyExists(APath) then
    begin
      value := StrToBool(MMLSettings.GetKeyValue(APath));
      //writeln('Loaded: ' + BoolToStr(value) + ' for ' + APath);
    end
  except
    On E : EConvertError do
    begin
      set_value := False;
      Writeln ('Invalid boolean encountered');
    end;
  end;
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

end.

