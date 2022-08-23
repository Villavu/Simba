{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.outputform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, comctrls, graphics, menus, extctrls, syncobjs,
  synedit, synedittypes, syneditmiscclasses, syneditmousecmds,
  simba.settings;

type
  TSimbaOutputBox = class(TSynEdit)
  protected
    FMouseLink: record
      Quote: record
        URL: String;
        FileName: String;
      end;
      DocPos: record
        Line, Col: Integer;
        FileName: String;
      end;
    end;

    procedure DoOpenLink(Data: PtrInt);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetAntialiasing: Boolean;
    procedure SetAntialiasing(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ScrollToBottom;
    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    property Antialiasing: Boolean read GetAntialiasing write SetAntialiasing;
  end;

  TSimbaOutputForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemCopyAll: TMenuItem;
    MenuItemCopyLine: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PageControl: TPageControl;
    TabSimba: TTabSheet;
    TabScript: TTabSheet;
    Timer: TTimer;

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyAllClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCopyLineClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FSimbaOutputBox: TSimbaOutputBox;
    FScriptOutputBox: TSimbaOutputBox;
    FLock: TCriticalSection;
    FStrings: TStringList;
    FClear: Boolean;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);
  public
    procedure Clear;

    procedure Add(const S: String); overload;
    procedure Add(const Strings: TStrings); overload;
    function AddRaw(const Data: String): String;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

{$R *.lfm}

uses
  SynEditMarkupBracket, SynEditMarkupWordGroup,
  lazloggerbase, lclintf,
  simba.dockinghelpers, simba.fonthelpers, simba.scripttabsform, simba.nativeinterface, simba.helpers_string, simba.mufasatypes;

function GetDebugLnType(var S: String): ESimbaDebugLn;
var
  I, NewLength: Integer;
begin
  Result := ESimbaDebugLn.NONE;

  if (Length(S) > 2) and (S[1] = #0) and (S[2] = #0) and TryStrToInt(S[3], I) then
  begin
    Result := ESimbaDebugLn(I);

    // Remove magic header without memory allocations.
    NewLength := Length(S) - 3;
    if (NewLength > 0) then
      Move(S[4], S[1], NewLength * SizeOf(Char));
    SetLength(S, NewLength);
  end;
end;

procedure SimbaDebugLn(const Msg: String);
begin
  SimbaOutputForm.FSimbaOutputBox.Lines.Add(Msg);
  SimbaOutputForm.FSimbaOutputBox.ScrollToBottom();
  SimbaOutputForm.PageControl.ActivePage := SimbaOutputForm.TabSimba;
end;

function TSimbaOutputBox.GetAntialiasing: Boolean;
begin
  Result := (Font.Quality = fqCleartypeNatural);
end;

procedure TSimbaOutputBox.SetAntialiasing(Value: Boolean);
begin
  case Value of
    True:  Font.Quality := fqCleartypeNatural;
    False: Font.Quality := fqNonAntialiased;
  end;
end;

procedure TSimbaOutputBox.DoOpenLink(Data: PtrInt);
begin
  if (FMouseLink.DocPos.FileName <> '') then
  begin
    if FileExists(FMouseLink.DocPos.FileName) then
      SimbaScriptTabsForm.Open(FMouseLink.DocPos.FileName);

    with SimbaScriptTabsForm.CurrentEditor, FMouseLink.DocPos do
    begin
      CaretX := Col;
      CaretY := Line;
      TopLine := TopLine - (LinesInWindow div 2);
      if CanSetFocus() then
        SetFocus();
    end;

    Exit;
  end;

  if (FMouseLink.Quote.FileName <> '') then
  begin
    if DirectoryExists(FMouseLink.Quote.FileName) then
      SimbaNativeInterface.OpenDirectory(FMouseLink.Quote.FileName)
    else
    if FileExists(FMouseLink.Quote.FileName) and FMouseLink.Quote.FileName.EndsWith('.simba') then
      SimbaScriptTabsForm.Open(FMouseLink.Quote.FileName)
    else
    if FileExists(FMouseLink.Quote.FileName) then
      SimbaNativeInterface.OpenFile(FMouseLink.Quote.FileName);

    Exit;
  end;

  if (FMouseLink.Quote.URL <> '') then
  begin
    OpenURL(FMouseLink.Quote.URL);

    Exit;
  end;
end;

procedure TSimbaOutputBox.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; AMarkup: TSynSelectedColor);
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

procedure TSimbaOutputBox.DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := ((FMouseLink.DocPos.FileName <> '') and (FMouseLink.DocPos.FileName = 'Untitled') or FileExists(FMouseLink.DocPos.FileName)) or
                    ((FMouseLink.Quote.URL <> '') or ((FMouseLink.Quote.FileName <> '') and FileExists(FMouseLink.Quote.FileName)));
end;

procedure TSimbaOutputBox.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.QueueAsyncCall(@DoOpenLink, 0);
end;

constructor TSimbaOutputBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnMouseLeave        := @DoMouseLeave;
  OnMouseLink         := @DoAllowMouseLink;
  OnClickLink         := @DoMouseLinkClick;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;

  BorderStyle := bsNone;
  Options := Options + [eoHideRightMargin];

  Gutter.Visible := False;
  MouseLinkColor.Style := [fsUnderline];

  MarkupByClass[TSynEditMarkupBracket].Enabled := False;
  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  MouseOptions := [emUseMouseActions];
  ResetMouseActions();
  with MouseTextActions.Add() do
    Command := emcMouseLink;
end;

procedure TSimbaOutputBox.ScrollToBottom;
begin
  TopLine := Lines.Count;
end;

procedure TSimbaOutputBox.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer);

  // at line 3, column 3 in file "C:\Users\OllyC\Desktop\Code\Simba\Scripts\testing.simba"
  function FindDocPos(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := 1;
    EndPos := Length(Line);

    Result := Line.Contains('at line') and Line.Contains('column') and Line.Contains('in file');
    if Result then
    begin
      FMouseLink.DocPos.FileName := Line.After('file').Between('"', '"');
      FMouseLink.DocPos.Line     := Line.Between('line', ',').ExtractInteger();
      FMouseLink.DocPos.Col      := Line.Between('column', 'in').ExtractInteger();
    end;
  end;

  // "www.google.com"
  // "directory/file.simba"
  function FindURLOrFile(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := Line.LastIndexOf('"', XY.X) + 1;
    EndPos   := Line.IndexOf('"', XY.X);

    Result := (StartPos > 1) and (EndPos > 0);
    if Result then
    begin
      Line := Copy(Line, StartPos, EndPos - StartPos);
      if Line.StartsWith('www.') or Line.StartsWith('http://') or Line.StartsWith('https://') then
        FMouseLink.Quote.URL := Line
      else
        FMouseLink.Quote.FileName := Line;
    end;
  end;

var
  Line: String;
  StartPos, EndPos: Integer;
begin
  inherited GetWordBoundsAtRowCol(XY, StartX, EndX);

  FMouseLink.Quote.URL       := '';
  FMouseLink.Quote.FileName  := '';
  FMouseLink.DocPos.FileName := '';

  Line := TextView[XY.Y - 1];
  if FindDocPos(Line, StartPos, EndPos) or FindURLOrFile(Line, StartPos, EndPos) then
  begin
    StartX := StartPos;
    EndX   := EndPos;
  end;
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

procedure TSimbaOutputForm.MenuItemSelectAllClick(Sender: TObject);
begin
  FScriptOutputBox.SelectAll();
end;

procedure TSimbaOutputForm.TimerExecute(Sender: TObject);

  function GetStartIndex: Integer;
  var
    I: Integer;
    Line: String;
  begin
    Result := -1;

    for I := FStrings.Count - 1 downto 0 do
    begin
      Line := FStrings[I];

      if (GetDebugLnType(Line) = ESimbaDebugLn.CLEAR) then
      begin
        FScriptOutputBox.Clear();

        Exit(I);
      end;
    end;
  end;

var
  I: Integer;
  Line: String;
  LineType: ESimbaDebugLn;
  NeedScroll, NeedShow: Boolean;
begin
  NeedShow := False;

  FLock.Enter();
  try
    if FClear then
    begin
      FScriptOutputBox.Clear();
      FStrings.Clear();

      FClear := False;
    end;

    if (FStrings.Count = 0) then
      Exit;

    FScriptOutputBox.BeginUpdate(False);

    // auto scroll if already scrolled to bottom.
    NeedScroll := (FScriptOutputBox.Lines.Count < FScriptOutputBox.LinesInWindow) or ((FScriptOutputBox.Lines.Count + 1) = (FScriptOutputBox.TopLine + FScriptOutputBox.LinesInWindow));
    for I := GetStartIndex() + 1 to FStrings.Count - 1 do
    begin
      Line := FStrings[I];
      LineType := GetDebugLnType(Line);

      case LineType of
        ESimbaDebugLn.NONE:   FScriptOutputBox.Lines.Add(Line);
        ESimbaDebugLn.YELLOW: FScriptOutputBox.Lines.AddObject(Line, TObject(PtrUInt($00BFFF)));
        ESimbaDebugLn.RED:    FScriptOutputBox.Lines.AddObject(Line, TObject(PtrUInt($0000A5)));
        ESimbaDebugLn.GREEN:  FScriptOutputBox.Lines.AddObject(Line, TObject(PtrUInt($228B22)));
      end;

      NeedShow := NeedShow or (LineType in [ESimbaDebugLn.YELLOW, ESimbaDebugLn.RED, ESimbaDebugLn.GREEN]);
    end;

    if NeedScroll or NeedShow then
      FScriptOutputBox.TopLine := FScriptOutputBox.Lines.Count;
    if NeedShow then
      PageControl.ActivePage := TabScript;

    FScriptOutputBox.EndUpdate();
    FStrings.Clear();
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  case Setting.Name of
    'General.OutputFontSize':
      begin
        FScriptOutputBox.Font.Size := Setting.Value;
        FSimbaOutputBox.Font.Size  := Setting.Value;
      end;

    'General.OutputFontName':
      if IsFontFixed(Setting.Value) then
      begin
        FScriptOutputBox.Font.Name := Setting.Value;
        FSimbaOutputBox.Font.Name  := Setting.Value;
      end;

    'General.OutputFontAntiAliased':
      begin
        FScriptOutputBox.Antialiasing := Setting.Value;
        FSimbaOutputBox.Antialiasing  := Setting.Value;
      end;
  end;
end;

procedure TSimbaOutputForm.Clear;
begin
  FClear := True;
end;

procedure TSimbaOutputForm.MenuItemCopyClick(Sender: TObject);
begin
  FScriptOutputBox.CopyToClipboard();
end;

procedure TSimbaOutputForm.MenuItemCopyLineClick(Sender: TObject);
var
  Line: Integer;
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
    begin
      Line := FScriptOutputBox.PixelsToRowColumn(ScreenToClient(ContextMenu.PopupPoint), []).Y;
      if (Line > 0) and (Line <= Lines.Count) then
        DoCopyToClipboard(Lines[Line - 1]);
    end;
end;

procedure TSimbaOutputForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaOutputForm.FormMouseLeave(Sender: TObject);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaOutputForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

procedure TSimbaOutputForm.MenuItemCopyAllClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      DoCopyToClipboard(Lines.Text);
end;

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DebugLnFunc := @SimbaDebugLn;
  PageControl.ActivePage := TabScript;

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  FSimbaOutputBox := TSimbaOutputBox.Create(Self);
  FSimbaOutputBox.Parent := TabSimba;
  FSimbaOutputBox.Align := alClient;
  FSimbaOutputBox.PopupMenu := ContextMenu;

  FScriptOutputBox := TSimbaOutputBox.Create(Self);
  FScriptOutputBox.Parent := TabScript;
  FScriptOutputBox.Align := alClient;
  FScriptOutputBox.PopupMenu := ContextMenu;

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

