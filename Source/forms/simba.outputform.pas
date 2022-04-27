{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.outputform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs,
  extctrls, synedit, syneditmiscclasses, menus, syncobjs,
  simba.settings;

type
  TSimbaOutputForm = class(TForm)
    Editor: TSynEdit;
    EditorPopupMenu: TPopupMenu;
    MenuItemSeperator: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemDelete: TMenuItem;
    Timer: TTimer;

    procedure DoEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FLock: TCriticalSection;
    FStrings: TStringList;

    function IsMagic(const S: String): Boolean;
    function IsClearMagic(const S: String): Boolean;
    function IsErrorMessage(var S: String): Boolean;
    function IsSuccessMessage(var S: String): Boolean;
    function IsHintMessage(var S: String): Boolean;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);
  public
    procedure Add(const S: String); overload;
    procedure Add(const Strings: TStrings); overload;

    function AddRaw(const Data: String): String;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

procedure DebugLnClear;
procedure DebugLnSuccess(Msg: String); overload;
procedure DebugLnSuccess(Msg: String; Args: array of const); overload;
procedure DebugLnError(Msg: String); overload;
procedure DebugLnError(Msg: String; Args: array of const); overload;
procedure DebugLnHint(Msg: String); overload;
procedure DebugLnHint(Msg: String; Args: array of const); overload;

implementation

{$R *.lfm}

uses
  lazloggerbase,
  simba.scriptthread, simba.fonthelpers;

const
  OUTPUT_SPECIAL = #0#0;

  OUTPUT_CLEAR   = OUTPUT_SPECIAL + 'CLEAR';
  OUTPUT_ERROR   = OUTPUT_SPECIAL + 'ERROR';
  OUTPUT_SUCCESS = OUTPUT_SPECIAL + 'SUCCESS';
  OUTPUT_HINT    = OUTPUT_SPECIAL + 'HINT';

procedure DebugLnClear;
begin
  if (SimbaScriptThread <> nil) and (SimbaScriptThread.Script.SimbaCommunication <> nil) then
    DebugLn(OUTPUT_CLEAR);
end;

procedure DebugLnSuccess(Msg: String);
begin
  if (SimbaScriptThread <> nil) and (SimbaScriptThread.Script.SimbaCommunication <> nil) then
    DebugLn(OUTPUT_SUCCESS + Msg)
  else
    DebugLn(Msg);
end;

procedure DebugLnSuccess(Msg: String; Args: array of const);
begin
  DebugLnSuccess(Format(Msg, Args));
end;

procedure DebugLnError(Msg: String);
begin
  if (SimbaScriptThread <> nil) and (SimbaScriptThread.Script.SimbaCommunication <> nil) then
    DebugLn(OUTPUT_ERROR + Msg)
  else
    DebugLn(Msg);
end;

procedure DebugLnError(Msg: String; Args: array of const);
begin
  DebugLnError(Format(Msg, Args));
end;

procedure DebugLnHint(Msg: String);
begin
  if (SimbaScriptThread <> nil) and (SimbaScriptThread.Script.SimbaCommunication <> nil) then
    DebugLn(OUTPUT_HINT + Msg)
  else
    DebugLn(Msg);
end;

procedure DebugLnHint(Msg: String; Args: array of const);
begin
  DebugLnHint(Format(Msg, Args));
end;

procedure TSimbaOutputForm.Add(const S: String);
var
  Line: String;
begin
  DebugLn(S);

  FLock.Enter();

  try
    for Line in S.Split([LineEnding]) do
      FStrings.Add(Line);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputForm.Add(const Strings: TStrings);
var
  I: Int32;
begin
  FLock.Enter();

  try
    for I := 0 to Strings.Count - 1 do
      FStrings.Add(Strings[I]);
  finally
    FLock.Leave();
  end;
end;

function TSimbaOutputForm.AddRaw(const Data: String): String;
var
  I: Integer;
  Lines: TStringArray;
begin
  Lines := Data.Split([LineEnding], TStringSplitOptions.ExcludeLastEmpty);

  with SimbaOutputForm do
  begin
    FLock.Enter();

    try
      if Data.EndsWith(LineEnding) then
      begin
        for I := 0 to High(Lines) do
          FStrings.Add(Lines[I]);

        Result := '';
      end else
      begin
        // Pipe buffer is full! Carry over last line.
        for I := 0 to High(Lines) - 1 do
          FStrings.Add(Lines[I]);

        Result := Lines[High(Lines)];
      end;
    finally
      FLock.Leave();
    end;
  end;
end;

procedure TSimbaOutputForm.MenuItemCutClick(Sender: TObject);
begin
  Editor.CutToClipboard();
end;

procedure TSimbaOutputForm.MenuItemDeleteClick(Sender: TObject);
begin
  Editor.ClearSelection();
end;

procedure TSimbaOutputForm.MenuItemPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard();
end;

procedure TSimbaOutputForm.MenuItemSelectAllClick(Sender: TObject);
begin
  Editor.SelectAll();
end;

procedure TSimbaOutputForm.TimerExecute(Sender: TObject);
var
  Scroll: Boolean;
  I, StartIndex: Int32;
  Line: String;
begin
  FLock.Enter();

  try
    if (FStrings.Count > 0) then
    begin
      StartIndex := -1;
      for I := 0 to FStrings.Count - 1 do
        if IsMagic(FStrings[I]) and IsClearMagic(FStrings[I]) then
        begin
          StartIndex := I;

          Break;
        end;

      if (StartIndex > -1) then
        Editor.Clear();

      Editor.BeginUpdate(False);

      // auto scroll if already scrolled to bottom.
      Scroll := (Editor.Lines.Count < Editor.LinesInWindow) or ((Editor.Lines.Count + 1) = (Editor.TopLine + Editor.LinesInWindow));
      for I := StartIndex + 1 to FStrings.Count - 1 do
      begin
        Line := FStrings[I];

        if IsMagic(Line) then
        begin
          if IsErrorMessage(Line) then
            Editor.Lines.AddObject(Line, TObject(PtrUInt($0000A5)))
          else
          if IsSuccessMessage(Line) then
            Editor.Lines.AddObject(Line, TObject(PtrUInt($228B22)))
          else
          if IsHintMessage(Line) then
            Editor.Lines.AddObject(Line, TObject(PtrUInt($00BFFF)))
        end else
          Editor.Lines.Add(Line);
      end;

      if Scroll then
        Editor.TopLine := Editor.Lines.Count;

      Editor.EndUpdate();
      Editor.Invalidate();

      FStrings.Clear();
    end;
  finally
    FLock.Leave();
  end;
end;

function TSimbaOutputForm.IsMagic(const S: String): Boolean;
begin
  Result := (Length(S) >= 2) and (S[1] = #0) and (S[2] = #0);
end;

function TSimbaOutputForm.IsClearMagic(const S: String): Boolean;
begin
  Result := (S = OUTPUT_CLEAR);
end;

function TSimbaOutputForm.IsErrorMessage(var S: String): Boolean;
begin
  Result := S.StartsWith(OUTPUT_ERROR);
  if Result then
    S := Copy(S, Length(OUTPUT_ERROR) + 1);
end;

function TSimbaOutputForm.IsSuccessMessage(var S: String): Boolean;
begin
  Result := S.StartsWith(OUTPUT_SUCCESS);
  if Result then
    S := Copy(S, Length(OUTPUT_SUCCESS) + 1);
end;

function TSimbaOutputForm.IsHintMessage(var S: String): Boolean;
begin
  Result := S.StartsWith(OUTPUT_HINT);
  if Result then
    S := Copy(S, Length(OUTPUT_HINT) + 1);
end;

procedure TSimbaOutputForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.GUI.OutputFontSize) then
    Editor.Font.Size := Setting.Value;

  if (Setting = SimbaSettings.GUI.OutputFontName) and SimbaFontHelpers.IsFontFixed(Setting.Value) then
    Editor.Font.Name := Setting.Value;

  if (Setting = SimbaSettings.GUI.OutputFontAntiAliased) then
    if Setting.Value then
      Editor.Font.Quality := fqCleartypeNatural
    else
      Editor.Font.Quality := fqNonAntialiased;
end;

procedure TSimbaOutputForm.MenuItemCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard();
end;

procedure TSimbaOutputForm.DoEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
begin
  Special := Editor.Lines.Objects[Line - 1] <> nil;

  if Special then
  begin
    Markup.BackAlpha  := 128;
    Markup.Background := PtrUInt(Editor.Lines.Objects[Line-1]);
    Markup.Foreground := clNone;
  end;
end;

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  SimbaSettingChanged(SimbaSettings.GUI.OutputFontSize);
  SimbaSettingChanged(SimbaSettings.GUI.OutputFontName);
  SimbaSettingChanged(SimbaSettings.GUI.OutputFontAntiAliased);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
end;

destructor TSimbaOutputForm.Destroy;
begin
  if (FLock <> nil) then
    FreeAndNil(FLock);
  if (FStrings <> nil) then
    FreeAndNil(FStrings);

  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  inherited Destroy();
end;

end.

