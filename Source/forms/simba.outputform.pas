{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.outputform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics,
  extctrls, synedit, synedittypes, syneditmiscclasses, syneditmousecmds, menus, syncobjs,
  simba.settings;

type
  TSimbaOutputBox = class(TSynEdit)
  protected
    FMouseLink: String;

    procedure DoOpenLink(Data: PtrInt);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;
  end;

  TSimbaOutputForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemSeperator: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemDelete: TMenuItem;
    Timer: TTimer;

    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FOutputBox: TSimbaOutputBox;
    FLock: TCriticalSection;
    FStrings: TStringList;

    function IsMagic(const S: String): Boolean;
    function IsClearMagic(const S: String): Boolean;
    function IsErrorMessage(var S: String): Boolean;
    function IsSuccessMessage(var S: String): Boolean;
    function IsHintMessage(var S: String): Boolean;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);
  public
    procedure ScrollToBottom;
    procedure Clear;

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
  SynEditMarkupBracket, SynEditMarkupWordGroup,
  lazloggerbase, lclintf,
  simba.fonthelpers, simba.scripttabsform, simba.nativeinterface, simba.helpers_string;

const
  OUTPUT_SPECIAL = #0#0;

  OUTPUT_CLEAR   = OUTPUT_SPECIAL + 'CLEAR';
  OUTPUT_ERROR   = OUTPUT_SPECIAL + 'ERROR';
  OUTPUT_SUCCESS = OUTPUT_SPECIAL + 'SUCCESS';
  OUTPUT_HINT    = OUTPUT_SPECIAL + 'HINT';

procedure DebugLnClear;
begin
  if Application.HasOption('simbacommunication') then
    DebugLn(OUTPUT_CLEAR);
end;

procedure DebugLnSuccess(Msg: String);
begin
  if Application.HasOption('simbacommunication') then
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
  if Application.HasOption('simbacommunication') then
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
  if Application.HasOption('simbacommunication') then
    DebugLn(OUTPUT_HINT + Msg)
  else
    DebugLn(Msg);
end;

procedure DebugLnHint(Msg: String; Args: array of const);
begin
  DebugLnHint(Format(Msg, Args));
end;

procedure TSimbaOutputBox.DoOpenLink(Data: PtrInt);
begin
  if FMouseLink.StartsWith('http') then
    OpenURL(FMouseLink)
  else
  if FMouseLink.EndsWith('.simba') then
    SimbaScriptTabsForm.Open(FMouseLink)
  else
  if DirectoryExists(FMouseLink) then
    SimbaNativeInterface.OpenDirectory(FMouseLink)
  else
  if FileExists(FMouseLink) then
    SimbaNativeInterface.OpenFile(FMouseLink);
end;

procedure TSimbaOutputBox.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
begin
  Special := Lines.Objects[Line - 1] <> nil;

  if Special then
  begin
    AMarkup.BackAlpha  := 115;
    AMarkup.Background := PtrUInt(Lines.Objects[Line - 1]);
    AMarkup.Foreground := clNone;
  end;
end;

procedure TSimbaOutputBox.DoMouseLeave(Sender: TObject);
begin
  LastMouseCaret := TPoint.Create(-1, -1);
end;

procedure TSimbaOutputBox.DoMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := (FMouseLink <> '') and (FMouseLink.StartsWith('http') or FileExists(FMouseLink));
end;

procedure TSimbaOutputBox.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.QueueAsyncCall(@DoOpenLink, 0);
end;

constructor TSimbaOutputBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnMouseLeave        := @DoMouseLeave;
  OnMouseLink         := @DoMouseLink;
  OnClickLink         := @DoMouseLinkClick;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;

  BorderStyle := bsNone;
  Options := Options + [eoHideRightMargin];

  Gutter.ChangesPart().Visible := False;
  Gutter.LineNumberPart().Visible := False;
  Gutter.CodeFoldPart().Visible := False;
  Gutter.MarksPart().Visible := False;

  Gutter.SeparatorPart.AutoSize := False;
  Gutter.SeparatorPart.Width := Scale96ToScreen(5);
  Gutter.SeparatorPart.MarkupInfo.Background := Self.Color;
  Gutter.SeparatorPart.MarkupInfo.Foreground := Self.Color;

  MarkupByClass[TSynEditMarkupBracket].Enabled := False;
  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  MouseOptions := [emUseMouseActions];
  ResetMouseActions();
  with MouseTextActions.Add() do
    Command := emcMouseLink;

  MouseLinkColor.Style := [fsUnderline];
end;

procedure TSimbaOutputBox.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer);
var
  QuoteStart, QuoteEnd: Integer;
  Line: String;
begin
  inherited GetWordBoundsAtRowCol(XY, StartX, EndX);

  Line := TextView[XY.Y - 1];

  QuoteStart := Line.LastIndexOf('"', XY.X);
  QuoteEnd   := Line.IndexOf('"', XY.X);
  if (QuoteStart > 0) and (QuoteEnd > 0) then
  begin
    StartX := QuoteStart + 1;
    EndX   := QuoteEnd;

    FMouseLink := Copy(Line, StartX, EndX - StartX);
  end else
    FMouseLink := '';
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
  I: Integer;
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
  Lines := Data.Split(LineEnding);

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
  FOutputBox.CutToClipboard();
end;

procedure TSimbaOutputForm.MenuItemDeleteClick(Sender: TObject);
begin
  FOutputBox.ClearSelection();
end;

procedure TSimbaOutputForm.MenuItemPasteClick(Sender: TObject);
begin
  FOutputBox.PasteFromClipboard();
end;

procedure TSimbaOutputForm.MenuItemSelectAllClick(Sender: TObject);
begin
  FOutputBox.SelectAll();
end;

procedure TSimbaOutputForm.TimerExecute(Sender: TObject);
var
  Scroll: Boolean;
  I, StartIndex: Integer;
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
        FOutputBox.Clear();

      FOutputBox.BeginUpdate(False);

      // auto scroll if already scrolled to bottom.
      Scroll := (FOutputBox.Lines.Count < FOutputBox.LinesInWindow) or ((FOutputBox.Lines.Count + 1) = (FOutputBox.TopLine + FOutputBox.LinesInWindow));
      for I := StartIndex + 1 to FStrings.Count - 1 do
      begin
        Line := FStrings[I];

        if IsMagic(Line) then
        begin
          if IsErrorMessage(Line) then
            FOutputBox.Lines.AddObject(Line, TObject(PtrUInt($0000A5)))
          else
          if IsSuccessMessage(Line) then
            FOutputBox.Lines.AddObject(Line, TObject(PtrUInt($228B22)))
          else
          if IsHintMessage(Line) then
            FOutputBox.Lines.AddObject(Line, TObject(PtrUInt($00BFFF)))
        end else
          FOutputBox.Lines.Add(Line);
      end;

      if Scroll then
        FOutputBox.TopLine := FOutputBox.Lines.Count;

      FOutputBox.EndUpdate();
      FOutputBox.Invalidate();

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
  case Setting.Name of
    'General.OutputFontSize':
      begin
        FOutputBox.Font.Size := Setting.Value;
      end;

    'General.OutputFontName':
      begin
        if SimbaFontHelpers.IsFontFixed(Setting.Value) then
          FOutputBox.Font.Name := Setting.Value;
      end;

    'General.OutputFontAntiAliased':
      begin
        if Setting.Value then
          FOutputBox.Font.Quality := fqCleartypeNatural
        else
          FOutputBox.Font.Quality := fqNonAntialiased;
      end;
  end;
end;

procedure TSimbaOutputForm.ScrollToBottom;
begin
  FOutputBox.TopLine := FOutputBox.Lines.Count;
end;

procedure TSimbaOutputForm.Clear;
begin
  FLock.Enter();
  try
    FStrings.Add(OUTPUT_CLEAR);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputForm.MenuItemCopyClick(Sender: TObject);
begin
  FOutputBox.CopyToClipboard();
end;

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  FOutputBox := TSimbaOutputBox.Create(Self);
  FOutputBox.Parent := Self;
  FOutputBox.Align := alClient;
  FOutputBox.PopupMenu := ContextMenu;

  SimbaSettingChanged(SimbaSettings.General.OutputFontName);
  SimbaSettingChanged(SimbaSettings.General.OutputFontSize);
  SimbaSettingChanged(SimbaSettings.General.OutputFontAntiAliased);

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

