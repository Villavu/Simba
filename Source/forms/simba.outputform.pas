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
  simba.settings, simba.mufasatypes;

type
  TSimbaOutputBox = class(TSynEdit)
  protected
    FLock: TCriticalSection;
    FBuffer: TStringList;

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

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure DoOpenLink(Data: PtrInt);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetTab: TTabSheet;
    function GetAntialiasing: Boolean;
    procedure SetAntialiasing(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    function Add(const S: String): String;
    procedure Empty;
    procedure Flush;

    property Tab: TTabSheet read GetTab;
    property Antialiasing: Boolean read GetAntialiasing write SetAntialiasing;
  end;

  TSimbaOutputTab = class(TTabSheet)
  protected
    FOutputBox: TSimbaOutputBox;
  public
    constructor Create(AOwner: TComponent); override;

    property OutputBox: TSimbaOutputBox read FOutputBox;
  end;

  TSimbaOutputForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemCustomize: TMenuItem;
    MenuItemCopyAll: TMenuItem;
    MenuItemCopyLine: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PageControl: TPageControl;
    Separator1: TMenuItem;
    FlushTimer: TTimer;

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyAllClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCopyLineClick(Sender: TObject);
    procedure MenuItemCustomizeClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure DoFlushTimerExecute(Sender: TObject);
  protected
    FSimbaOutputBox: TSimbaOutputBox;

    procedure DebugLn(const S: String);
  public
    property SimbaOutputBox: TSimbaOutputBox read FSimbaOutputBox;

    function AddSimbaOutput: TSimbaOutputBox;
    function AddScriptOutput(AOwner: TComponent; TabTitle: String): TSimbaOutputBox;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

{$R *.lfm}

uses
  SynEditMarkupBracket, SynEditMarkupWordGroup,
  simba.dockinghelpers, simba.fonthelpers, simba.scripttabsform,
  simba.nativeinterface, simba.settingsform, simba.main;

constructor TSimbaOutputTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOutputBox := TSimbaOutputBox.Create(Self);
  FOutputBox.Parent := Self;
  FOutputBox.Align := alClient;
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

function TSimbaOutputBox.GetTab: TTabSheet;
begin
  if (not (Parent is TTabSheet)) then
    raise Exception.Create('TSimbaOutputBox.GetTab: Parent is not a TTabSheet!');

  Result := TTabSheet(Parent);
end;

procedure TSimbaOutputBox.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if Setting.Equals(SimbaSettings.OutputBox.FontAntiAliased) then
    Antialiasing := Setting.Value
  else
  if Setting.Equals(SimbaSettings.OutputBox.Color) then
    Color := Setting.Value
  else
  if Setting.Equals(SimbaSettings.OutputBox.FontColor) then
    Font.Color := Setting.Value
  else
  if Setting.Equals(SimbaSettings.OutputBox.FontSize) then
    Font.Size := Setting.Value
  else
  if Setting.Equals(SimbaSettings.OutputBox.FontName) and IsFontFixed(Setting.Value) then
    Font.Name := Setting.Value;
end;

procedure TSimbaOutputBox.DoOpenLink(Data: PtrInt);
begin
  if (FMouseLink.DocPos.FileName <> '') then
  begin
    if FileExists(FMouseLink.DocPos.FileName) then
      SimbaScriptTabsForm.Open(FMouseLink.DocPos.FileName);

    with FMouseLink.DocPos do
      SimbaScriptTabsForm.CurrentEditor.FocusLine(Line, Col, $0000A5);

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
    SimbaNativeInterface.OpenURL(FMouseLink.Quote.URL);

    Exit;
  end;
end;

procedure TSimbaOutputBox.DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
var
  Flags: EDebugLnFlags;
begin
  Flags := EDebugLnFlags(Integer(PtrUInt(Lines.Objects[Line - 1])));

  if (([EDebugLn.YELLOW, EDebugLn.RED, EDebugLn.GREEN] * Flags) <> []) then
  begin
    if (EDebugLn.YELLOW in Flags) then AMarkup.Background := $00BFFF else
    if (EDebugLn.RED    in Flags) then AMarkup.Background := $0000A5 else
    if (EDebugLn.GREEN  in Flags) then AMarkup.Background := $228B22;

    AMarkup.BackAlpha  := 115;
    AMarkup.Foreground := clNone;

    Special := True;
  end else
    Special := False;
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
var
  Setting: TSimbaSetting;
begin
  inherited Create(AOwner);

  FBuffer := TStringList.Create();
  FLock := TCriticalSection.Create();

  OnMouseLeave        := @DoMouseLeave;
  OnMouseLink         := @DoAllowMouseLink;
  OnClickLink         := @DoMouseLinkClick;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;

  TabStop := False;
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

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
  for Setting in [SimbaSettings.OutputBox.Color, SimbaSettings.OutputBox.FontColor, SimbaSettings.OutputBox.FontSize, SimbaSettings.OutputBox.FontName, SimbaSettings.OutputBox.FontAntiAliased] do
    SimbaSettingChanged(Setting);
end;

destructor TSimbaOutputBox.Destroy;
begin
  SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  if (FLock <> nil) then
    FreeAndNil(FLock);
  if (FBuffer <> nil) then
    FreeAndNil(FBuffer);

  inherited Destroy();
end;

procedure TSimbaOutputBox.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer);

  // Line 3 in function "cpuuu" in file "Untitled"
  function FindStackTrace(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := 1;
    EndPos := Length(Line) + 1;

    Result := Line.Contains('Line') and Line.Contains('in') and Line.Contains('in file');
    if Result then
    begin
      FMouseLink.DocPos.FileName := Line.After('file').Between('"', '"');
      FMouseLink.DocPos.Line     := Line.Between('Line', 'in').ExtractInteger();
      FMouseLink.DocPos.Col      := 0;
    end;
  end;

  // at line 3, column 3 in file "C:\Users\OllyC\Desktop\Code\Simba\Scripts\testing.simba"
  function FindDocPos(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := 1;
    EndPos := Length(Line) + 1;

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
  if FindStackTrace(Line, StartPos, EndPos) or FindDocPos(Line, StartPos, EndPos) or FindURLOrFile(Line, StartPos, EndPos) then
  begin
    StartX := StartPos;
    EndX   := EndPos;
  end;
end;

function TSimbaOutputBox.Add(const S: String): String;
var
  Arr: TStringArray;
  I: Integer;
  Line: String;
  Flags: EDebugLnFlags;
begin
  Arr := S.Split(LineEnding);
  if (Length(Arr) = 0) then
    Result := ''
  else
  begin
    FLock.Enter();

    if S.EndsWith(LineEnding) then
    begin
      Result := '';

      for I := 0 to High(Arr) do
      begin
        Line  := Arr[I];
        Flags := FlagsFromString(Line);

        FBuffer.AddObject(Line, TObject(PtrUInt(Integer(Flags))));
      end;
    end else
    begin
      Result := Arr[High(Arr)];

      for I := 0 to High(Arr) - 1 do
      begin
        Line  := Arr[I];
        Flags := FlagsFromString(Line);

        FBuffer.AddObject(Line, TObject(PtrUInt(Integer(Flags))));
      end;
    end;

    FLock.Leave();
  end;
end;

procedure TSimbaOutputBox.Empty;
begin
  Add(FlagsToString([EDebugLn.CLEAR]) + LineEnding);
end;

procedure TSimbaOutputBox.Flush;
var
  I, StartIndex: Integer;
  NeedFocus, NeedScroll: Boolean;
  Flags: EDebugLnFlags;
begin
  FLock.Enter();

  try
    if (FBuffer.Count > 0) then
    begin
      NeedFocus := False;

      StartIndex := 0;
      for I := 0 to FBuffer.Count - 1 do
      begin
        Flags := EDebugLnFlags(Integer(PtrUInt(FBuffer.Objects[I])));
        if (EDebugLn.CLEAR in Flags) then
          StartIndex := I+1;
        if (EDebugLn.FOCUS in Flags) then
          NeedFocus := True;
      end;

      BeginUpdate(False);
      if (StartIndex > 0) then
        Clear();

      // auto scroll if already scrolled to bottom.
      NeedScroll := (Lines.Count < LinesInWindow) or ((Lines.Count + 1) = (TopLine + LinesInWindow));
      for I := StartIndex to FBuffer.Count - 1 do
        Lines.AddObject(FBuffer[I], FBuffer.Objects[I]);

      if NeedFocus or NeedScroll then
      begin
        if NeedFocus and (Parent is TTabSheet) then
          TTabSheet(Parent).PageControl.ActivePage := TTabSheet(Parent);
        TopLine := Lines.Count;
      end;
      EndUpdate();

      FBuffer.Clear();
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputForm.MenuItemSelectAllClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      SelectAll();
end;

procedure TSimbaOutputForm.DoFlushTimerExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
    TSimbaOutputTab(PageControl.Pages[I]).OutputBox.Flush();
end;

procedure TSimbaOutputForm.DebugLn(const S: String);
begin
  SimbaOutputBox.Add(S + LineEnding);
end;

function TSimbaOutputForm.AddSimbaOutput: TSimbaOutputBox;
var
  Tab: TSimbaOutputTab;
begin
  Tab := TSimbaOutputTab.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := 'Simba';
  Tab.ImageIndex := IMAGE_SIMBA;

  Result := Tab.OutputBox;
  Result.PopupMenu := ContextMenu;
end;

function TSimbaOutputForm.AddScriptOutput(AOwner: TComponent; TabTitle: String): TSimbaOutputBox;
var
  Tab: TSimbaOutputTab;
begin
  Tab := TSimbaOutputTab.Create(AOwner);
  Tab.PageControl := PageControl;
  Tab.Caption := TabTitle;

  Result := Tab.OutputBox;
  Result.PopupMenu := ContextMenu;
end;

procedure TSimbaOutputForm.MenuItemCopyClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      CopyToClipboard();
end;

procedure TSimbaOutputForm.MenuItemCopyLineClick(Sender: TObject);
var
  Line: Integer;
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
    begin
      Line := PixelsToRowColumn(ScreenToClient(ContextMenu.PopupPoint), []).Y;
      if (Line > 0) and (Line <= Lines.Count) then
        DoCopyToClipboard(Lines[Line - 1]);
    end;
end;

procedure TSimbaOutputForm.MenuItemCopyAllClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      DoCopyToClipboard(Lines.Text);
end;

procedure TSimbaOutputForm.MenuItemCustomizeClick(Sender: TObject);
begin
  SimbaSettingsForm.TreeView.Selected := SimbaSettingsForm.TreeView.Items.FindNodeWithText('Output Box');
  SimbaSettingsForm.ShowModal();
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

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSimbaOutputBox := AddSimbaOutput();

  DoSimbaDebugLn := @DebugLn;
end;


end.

