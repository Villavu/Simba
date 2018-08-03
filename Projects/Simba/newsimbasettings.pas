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
  comctrls, // TTreeView
  Graphics;

type
    TSetting = class(TObject)
    public
      procedure Save(MMLSettings: TMMLSettings); virtual; abstract;
      procedure Load(MMLSettings: TMMLSettings); virtual; abstract;
    end;

    TOnChangeSettings = procedure (obj: TSetting) of object;
    TOnDefaultSetting = procedure (obj: TSetting);

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
      function GetValue: string; virtual;
      procedure SetValue(val: string); virtual;
    public
      constructor Create(APath: string; val: string); overload;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      function GetDefValue(val: string): string;

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
      function GetValue: string; override;
      procedure SetValue(val: string); override;
    public
      property Value: string read GetValue write SetValue;
    end;

    TPathSetting = class(TFileSetting)
    private
      procedure SetValue(val: string); override;
    end;

    TFontSetting = class(TSetting)
    private
      FPath: string;
      FFont: TFont;
      FOnChange: TOnChangeSettings;
      function getValue(): TFont;
      procedure setValue(Font: TFont);
    public
      Name: TStringSetting;
      Size: TIntegerSetting;
      Color: TIntegerSetting;
      Style: TStringSetting;
      CharSet: TIntegerSetting;
      Quality: TIntegerSetting;
      Pitch: TIntegerSetting;

      constructor Create(APath: string); virtual;
      destructor Destroy; override;

      procedure Save(MMLSettings: TMMLSettings); override;
      procedure Load(MMLSettings: TMMLSettings); override;

      property Path: string read FPath;
      property OnChange: TOnChangeSettings write FOnChange;
      property Value: TFont read getValue write setValue;
    end;

    TSection = class(TSetting)
    public
      Nodes: TSettingsArray;

      constructor Create(); virtual;
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

    TCodeInsightSection = class(TSection)
      FunctionList: TFunctionListSection;
      ShowHidden: TBooleanSetting;
    end;

    TTraySection = class(TSection)
      AlwaysVisible: TBooleanSetting;
    end;

    TSourceEditorSection = class(TSection)
      DefScriptPath: TFileSetting;
      LazColors: TBooleanSetting;
      CaretPastEOL: TBooleanSetting;
      Font: TFontSetting;
      HighlighterPath: TFileSetting;                            
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
      AddCoordinateToClipBoard: TBooleanSetting;
      AddColourToClipBoard: TBooleanSetting;
    end;

    TCodeHintsSection = class(TSection)
      ShowAutomatically: TBooleanSetting;
    end;

    TShowBalloonHints = class(TSection)
      Show: TBooleanSetting;
    end;

    TCodeCompletionSection = class(TSection)
      ShowAutomatically: TBooleanSetting;
    end;

    TScriptManagerSection = class(TSection)
      ServerURL: TStringSetting;
      StoragePath: TFileSetting;
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

    TNotesSection = class(TSection)
      Content: TStringSetting;
      Visible: TBooleanSetting;
    end;

    TFileBrowserSection = class(TSection)
      Visible: TBooleanSetting;
    end;

    TMiscSection = class(TSection)
      RestartScriptIfStarted: TBooleanSetting;
      WarnIfRunning: TBooleanSetting;
      WarnIfModified: TBooleanSetting;
      SaveScriptOnCompile: TBooleanSetting;
    end;

    TSimbaSettings = class(TSection)
    public
      Includes: TIncludesSection;
      Fonts: TFontsSection;
      Scripts: TScriptsSection;
      CodeInsight: TCodeInsightSection;
      Tray: TTraySection;
      SourceEditor: TSourceEditorSection;
      News: TNewsSection;
      Plugins: TPluginsSection;
      Tab: TTabsSection;
      General: TGeneralSection;
      Updater: TUpdaterSection;
      ColourPicker: TColourPickerSection;
      CodeHints: TCodeHintsSection;
      CodeCompletion: TCodeCompletionSection;
      Notes: TNotesSection;
      FileBrowser: TFileBrowserSection;
      Misc: TMiscSection;
      ShowBalloonHints: TShowBalloonHints;

      ScriptManager: TScriptManagerSection;

      LastConfig: TLastConfig;

      MMLSettings: TMMLSettings;
      Oops: Boolean;

      constructor Create; override;

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
    if RenameFile(SettingsFileName, 'settings.bak') then
    begin
      mDebugLn('Moved ' + SettingsFileName + ' to settings.bak');
    end else
    begin
      mDebugLn('Could not move ' + SettingsFileName + ' to settings.bak');
      if not DeleteFile(SettingsFileName) then
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
  if (not (Assigned(SimbaSettings))) then
    Exit;

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

  FValue := IncludeTrailingPathDelimiter(val);
  FValueSet := True;
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TFontSetting.getValue(): TFont;
  function styleFromStr(Style: string): TFontStyles;
  var
    I: LongInt;
  begin
    Result := [];
    for I := 1 to Length(Style) do
      case StrToInt(Style[I]) of
        0: Include(Result, fsBold);
        1: Include(Result, fsItalic);
        2: Include(Result, fsStrikeOut);
        3: Include(Result, fsUnderline);
      end;
  end;
begin
  FFont.Name := Name.GetDefValue('Courier New');
  FFont.Size := Size.GetDefValue(10);
  FFont.Color := Color.GetDefValue(clDefault);
  FFont.Style := styleFromStr(Style.GetDefValue(''));
  FFont.CharSet := CharSet.GetDefValue(0);
  FFont.Quality := TFontQuality(Quality.GetDefValue(Ord(fqProof)));
  FFont.Pitch := TFontPitch(Pitch.GetDefValue(Ord(fpFixed)));

  Result := FFont;
end;

procedure TFontSetting.setValue(Font: TFont);
  function styleToStr(Style: TFontStyles): string;
  begin
    Result := '';
    if (fsBold in Style) then Result += '0';
    if (fsItalic in Style) then Result += '1';
    if (fsStrikeOut in Style) then Result += '2';
    if (fsUnderline in Style) then Result += '3';
  end;
begin
  FFont.Assign(Font);

  Name.Value := Font.Name;
  Size.Value := Font.Size;
  Color.Value := Font.Color;
  Style.Value := styleToStr(Font.Style);
  CharSet.Value := Font.CharSet;
  Quality.Value := Ord(Font.Quality);
  Pitch.Value := Ord(Font.Pitch);

  if (Assigned(FOnChange)) then
    FOnChange(Self);
end;

constructor TFontSetting.Create(APath: string);
begin
  FPath := APath;
  FFont := TFont.Create();

  Name := TStringSetting.Create(APath + 'Name');
  Size := TIntegerSetting.Create(APath + 'Size');
  Color := TIntegerSetting.Create(APath + 'Color');
  Style := TStringSetting.Create(APath + 'Styles');
  CharSet := TIntegerSetting.Create(APath + 'CharSet');
  Quality := TIntegerSetting.Create(APath + 'Quality');
  Pitch := TIntegerSetting.Create(APath + 'Pitch');
end;

destructor TFontSetting.Destroy();
begin
  Name.Free;
  Size.Free;
  Color.Free;
  Style.Free;
  CharSet.Free;
  Quality.Free;
  Pitch.Free;

  FreeAndNil(FFont);
end;

procedure TFontSetting.Save(MMLSettings: TMMLSettings);
begin
  Name.Save(MMLSettings);
  Size.Save(MMLSettings);
  Color.Save(MMLSettings);
  Style.Save(MMLSettings);
  CharSet.Save(MMLSettings);
  Quality.Save(MMLSettings);
  Pitch.Save(MMLSettings);
end;

procedure TFontSetting.Load(MMLSettings: TMMLSettings);
begin
  Name.Load(MMLSettings);
  Size.Load(MMLSettings);
  Color.Load(MMLSettings);
  Style.Load(MMLSettings);
  CharSet.Load(MMLSettings);
  Quality.Load(MMLSettings);
  Pitch.Load(MMLSettings);
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

procedure GetIncludePath(obj: TSetting);
begin
  TPathSetting(obj).Value := DataPath + 'Includes' + DS;
end;

procedure GetPluginPath(obj: TSetting);
begin
  TPathSetting(obj).Value := DataPath + 'Plugins' + DS;
end;

procedure GetScriptPath(obj: TSetting);
begin
  TPathSetting(obj).Value := DocPath + 'Scripts' + DS;
end;

procedure GetFontPath(obj: TSetting);
begin
  TPathSetting(obj).Value := DataPath + 'Fonts' + DS;
end;

procedure GetDefScriptPath(obj: TSetting);
begin
  TFileSetting(obj).Value := DataPath + 'default.simba';
end;

procedure GetHighlighterFile(obj: TSetting);
begin
  TFileSetting(obj).Value := DataPath + 'highlighter.ini';
end;                                                                                                   
procedure GetUpdaterGetCheckForUpdates(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetUpdaterGetAutomaticallyUpdate(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetUpdaterCheckEveryXminutes(obj: TSetting); begin TIntegerSetting(obj).Value := 30; end;
procedure GetUpdaterLink(obj: TSetting); begin TStringSetting(obj).Value := SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF};; end;
procedure GetUpdaterVersionLink(obj: TSetting); begin TStringSetting(obj).Value := SimbaURL + 'Version'; end;

procedure GetInterpreterType(obj: TSetting); begin TIntegerSetting(obj).Value := 1; end; // 1 is Lape
procedure GetInterpreterAllowSysCalls(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;

procedure GetFontsLoadOnStartUp(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetFontsCheckForUpdates(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetFontsVersion(obj: TSetting); begin TIntegerSetting(obj).Value := -1; end;

procedure GetTabOpenNextOnClose(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;
procedure GetOpenScriptInNewTab(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetTabCheckBeforeOpen(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetGeneralMaxRecentFiles(obj: TSetting); begin TIntegerSetting(obj).Value := 10; end;

procedure GetColourPickerShowHistoryonPick(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetColourPickerAddToHistoryOnPick(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetColourPickerAddCoordinateToClipBoard(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;
procedure GetColourPickerAddColourToClipBoard(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;

procedure GetCodeInsightShowHidden(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;
procedure GetFunctionListShowOnStart(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetCodeHintsShowAutomatically(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetCodeCompletionShowAutomatically(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetShowBalloonHints(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetScriptManagerURL(obj: TSetting); begin TStringSetting(obj).Value := 'http://127.0.0.1/'; end;
procedure GetScriptManagerPath(obj: TSetting); begin TFileSetting(obj).Value := SimbaSettings.Scripts.Path.Value+'ScriptStorage.xml'; end;
procedure GetScriptManagerFile(obj: TSetting); begin TStringSetting(obj).Value := 'ScriptManager.xml'; end;
procedure GetScriptManagerFirstRun(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetSourceEditorLazColors(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetSourceEditorCaretPastEOL(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetMainFormNormalSize(obj: TSetting); begin TStringSetting(obj).Value := '739:555'; end;
procedure GetMainFormPosition(obj: TSetting); begin TStringSetting(obj).Value := ''; end;
procedure GetMainFormState(obj: TSetting); begin TStringSetting(obj).Value := 'normal'; end;

procedure GetMainFormConsoleVisible(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;

procedure GetTrayAlwaysVisible(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetSimbaNewsURL(obj: TSetting); begin TStringSetting(obj).Value := 'http://simba.villavu.com/bin/news'; end;

procedure GetNotesContent(obj: TSetting); begin TStringSetting(obj).Value := ''; end;
procedure GetNotesVisible(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;

procedure GetFileBrowserVisible(Obj: TSetting); begin TBooleanSetting(obj).Value := True; end;

procedure GetMiscRestartScriptIfStarted(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;
procedure GetMiscWarnIfRunning(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetMiscWarnIfModified(obj: TSetting); begin TBooleanSetting(obj).Value := True; end;
procedure GetSaveScriptOnCompile(obj: TSetting); begin TBooleanSetting(obj).Value := False; end;

constructor TSimbaSettings.Create;
begin
  inherited;

  ShowBalloonHints := AddChild(TShowBalloonHints.Create()) as TShowBalloonHints;
  ShowBalloonHints.Show := ShowBalloonHints.AddChild(TBooleanSetting.Create(ssShowBalloonHints)) as TBooleanSetting;
  ShowBalloonHints.Show.onDefault := @GetShowBalloonHints;

  Includes := AddChild(TIncludesSection.Create()) as TIncludesSection;
  Includes.Path := Includes.AddChild(TPathSetting.Create(ssIncludesPath)) as TPathSetting;
  Includes.Path.onDefault := @GetIncludePath;

  Fonts := AddChild(TFontsSection.Create()) as TFontsSection;
  Fonts.Path := Fonts.AddChild(TPathSetting.Create(ssFontsPath)) as TPathSetting;
  Fonts.Path.onDefault := @GetFontPath;

  Scripts := AddChild(TScriptsSection.Create()) as TScriptsSection;
  Scripts.Path := Scripts.AddChild(TPathSetting.Create(ssScriptsPath)) as TPathSetting;
  Scripts.Path.onDefault := @GetScriptPath;

  CodeInsight := AddChild(TCodeInsightSection.Create()) as TCodeInsightSection;
  with CodeInsight do
  begin
    ShowHidden := AddChild(TBooleanSetting.Create(ssCodeInsightShowHidden)) as TBooleanSetting;
    ShowHidden.onDefault := @GetCodeInsightShowHidden;

    FunctionList := CodeInsight.AddChild(TFunctionListSection.Create()) as TFunctionListSection;
    FunctionList.ShowOnStart := FunctionList.AddChild(TBooleanSetting.Create(ssFunctionListShowOnStart)) as TBooleanSetting;
    FunctionList.ShowOnStart.onDefault := @GetFunctionListShowOnStart;
  end;

  Tray := AddChild(TTraySection.Create()) as TTraySection;
  Tray.AlwaysVisible := Tray.AddChild(TBooleanSetting.Create(ssTrayAlwaysVisible)) as TBooleanSetting;
  Tray.AlwaysVisible.onDefault := @GetTrayAlwaysVisible;

  SourceEditor := AddChild(TSourceEditorSection.Create()) as TSourceEditorSection;
  SourceEditor.DefScriptPath := SourceEditor.AddChild(TFileSetting.Create(ssSourceEditorDefScriptPath)) as TFileSetting;
  SourceEditor.DefScriptPath.onDefault := @GetDefScriptPath;
  SourceEditor.LazColors := SourceEditor.AddChild(TBooleanSetting.Create(ssSourceEditorLazColors)) as TBooleanSetting;
  SourceEditor.LazColors.onDefault := @GetSourceEditorLazColors;
  SourceEditor.CaretPastEOL := SourceEditor.AddChild(TBooleanSetting.Create(ssSourceEditorCaretPastEOL)) as TBooleanSetting;
  SourceEditor.CaretPastEOL.onDefault := @GetSourceEditorCaretPastEOL;
  SourceEditor.Font := SourceEditor.AddChild(TFontSetting.Create(ssSourceEditorFont)) as TFontSetting;
  SourceEditor.HighlighterPath := SourceEditor.AddChild(TFileSetting.Create(ssSourceEditorHighlighterPath)) as TFileSetting;
  SourceEditor.HighlighterPath.onDefault := @GetHighlighterFile;                                                             

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
  ColourPicker.AddCoordinateToClipBoard := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerAddCoordinateToClipBoard)) as TBooleanSetting;
  ColourPicker.AddCoordinateToClipBoard.onDefault := @GetColourPickerAddCoordinateToClipBoard;
  ColourPicker.AddColourToClipBoard := ColourPicker.AddChild(TBooleanSetting.Create(ssColourPickerAddColourToClipBoard)) as TBooleanSetting;
  ColourPicker.AddColourToClipBoard.onDefault := @GetColourPickerAddColourToClipBoard;

  CodeHints := AddChild(TCodeHintsSection.Create()) as TCodeHintsSection;
  CodeHints.ShowAutomatically := CodeHints.AddChild(TBooleanSetting.Create(ssCodeHintsShowAutomatically)) as TBooleanSetting;
  CodeHints.ShowAutomatically.onDefault := @GetCodeHintsShowAutomatically;

  CodeCompletion := AddChild(TCodeCompletionSection.Create()) as TCodeCompletionSection;
  CodeCompletion.ShowAutomatically := CodeCompletion.AddChild(TBooleanSetting.Create(ssCodeCompletionShowAutomatically)) as TBooleanSetting;
  CodeCompletion.ShowAutomatically.onDefault := @GetCodeCompletionShowAutomatically;

  Notes := AddChild(TNotesSection.Create()) as TNotesSection;
  Notes.Content := Notes.AddChild(TStringSetting.Create(ssNotesContent)) as TStringSetting;
  Notes.Content.onDefault := @GetNotesContent;
  Notes.Visible := Notes.AddChild(TBooleanSetting.Create(ssNotesVisible)) as TBooleanSetting;
  Notes.Visible.onDefault := @GetNotesVisible;

  FileBrowser := AddChild(TFileBrowserSection.Create()) as TFileBrowserSection;
  FileBrowser.Visible := FileBrowser.AddChild(TBooleanSetting.Create(ssFileBrowserVisible)) as TBooleanSetting;
  FileBrowser.Visible.onDefault := @GetFileBrowserVisible;

  Misc := AddChild(TMiscSection.Create()) as TMiscSection;
  Misc.RestartScriptIfStarted := Misc.AddChild(TBooleanSetting.Create(ssMiscRestartScriptIfStarted)) as TBooleanSetting;
  Misc.RestartScriptIfStarted.onDefault := @GetMiscRestartScriptIfStarted;
  Misc.WarnIfRunning := Misc.AddChild(TBooleanSetting.Create(ssMiscWarnIfRunning)) as TBooleanSetting;
  Misc.WarnIfRunning.onDefault := @GetMiscWarnIfRunning;
  Misc.WarnIfModified := Misc.AddChild(TBooleanSetting.Create(ssMiscWarnIfModified)) as TBooleanSetting;
  Misc.WarnIfModified.onDefault := @GetMiscWarnIfModified;
  Misc.SaveScriptOnCompile := Misc.AddChild(TBooleanSetting.Create(ssSaveScriptOnCompile)) as TBooleanSetting;
  Misc.SaveScriptOnCompile.onDefault := @GetSaveScriptOnCompile;

  ScriptManager := AddChild(TScriptManagerSection.Create()) as TScriptManagerSection;
  ScriptManager.ServerURL := ScriptManager.AddChild(TStringSetting.Create(ssSMURL)) as TStringSetting;
  ScriptManager.ServerURL.onDefault := @GetScriptManagerURL;
  ScriptManager.StoragePath := ScriptManager.AddChild(TFileSetting.Create(ssSMPath)) as TFileSetting;
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
  LastConfig.MainForm.ConsoleVisible := LastConfig.MainForm.AddChild(TBooleanSetting.Create(ssConsoleVisible)) as TBooleanSetting;
  LastConfig.MainForm.ConsoleVisible.onDefault := @GetMainFormConsoleVisible;

end;

procedure TSimbaSettings.Save(SettingsFileName: String);
begin
  Save(MMLSettings);
  MMLSettings.SaveToXML(SettingsFileName);
end;


end.
