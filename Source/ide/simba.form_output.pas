{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_output;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, comctrls, graphics, menus, extctrls, syncobjs,
  synedit, syneditmiscclasses, syneditmousecmds,
  simba.settings, simba.base, simba.component_tabcontrol, simba.component_synedit,
  simba.vartype_string;

type
  TSimbaOutputBox = class(TSimbaMemo)
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

    procedure DoSettingChange_FontName(Setting: TSimbaSetting);
    procedure DoSettingChange_FontSize(Setting: TSimbaSetting);
    procedure DoSettingChange_FontAntiAliased(Setting: TSimbaSetting);

    procedure DoOpenLink(Data: PtrInt);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetTabTitle: String;
    function GetTabImageIndex: Integer;
    procedure SetTabTitle(Value: String);
    procedure SetTabImageIndex(Value: Integer);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    function Add(const S: String): String;
    procedure AddLine(Flags: EDebugLnFlags; const S: String);
    procedure Empty;
    procedure Flush;

    procedure MakeVisible;

    property TabTitle: String read GetTabTitle write SetTabTitle;
    property TabImageIndex: Integer read GetTabImageIndex write SetTabImageIndex;
  end;

  TSimbaOutputForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemCustomize: TMenuItem;
    MenuItemCopyAll: TMenuItem;
    MenuItemCopyLine: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    Separator1: TMenuItem;
    FlushTimer: TTimer;

    procedure ContextMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
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
    FTabControl: TSimbaTabControl;
    FSimbaOutputBox: TSimbaOutputBox;

    function CanAnchorDocking(X, Y: Integer): Boolean;

    procedure DoScriptTabChange(Sender: TObject);

    procedure DebugLn(const S: String);

    function GetActiveOutputBox: TSimbaOutputBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SimbaOutputBox: TSimbaOutputBox read FSimbaOutputBox;
    property ActiveOutputBox: TSimbaOutputBox read GetActiveOutputBox;

    function AddSimbaOutput: TSimbaOutputBox;
    function AddScriptOutput(TabTitle: String): TSimbaOutputBox;

    procedure RemoveTab(OutputBox: TSimbaOutputBox);
    procedure MoveTab(AFrom, ATo: Integer);
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

{$R *.lfm}

uses
  SynEditMarkupBracket, SynEditMarkupWordGroup,
  simba.ide_dockinghelpers, simba.misc,
  simba.form_main, simba.form_tabs,  simba.form_settings,
  simba.nativeinterface,
  simba.ide_tab, simba.ide_events, simba.ide_utils;

type
  TSimbaOutputTab = class(TSimbaTab)
  protected
    FOutputBox: TSimbaOutputBox;

    procedure DoTabScriptStateChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    property OutputBox: TSimbaOutputBox read FOutputBox;
  end;

procedure TSimbaOutputTab.DoTabScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) and (TSimbaScriptTab(Sender).OutputBox = FOutputBox) then
  begin
    case TSimbaScriptTab(Sender).ScriptState of
      ESimbaScriptState.STATE_RUNNING: ImageIndex := IMG_PLAY;
      ESimbaScriptState.STATE_PAUSED:  ImageIndex := IMG_PAUSE;
      else
        ImageIndex := IMG_STOP;
    end;
  end;
end;

constructor TSimbaOutputTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOutputBox := TSimbaOutputBox.Create(Self);
  FOutputBox.Parent := Self;
  FOutputBox.Align := alClient;

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_SCRIPTSTATE_CHANGE, @DoTabScriptStateChange);
end;

function TSimbaOutputBox.GetTabTitle: String;
begin
  if (Parent is TSimbaOutputTab) then
    Result := TSimbaOutputTab(Parent).Caption
  else
    Result := '';
end;

procedure TSimbaOutputBox.SetTabTitle(Value: String);
begin
  if (Parent is TSimbaOutputTab) then
    TSimbaOutputTab(Parent).Caption := Value;
end;

function TSimbaOutputBox.GetTabImageIndex: Integer;
begin
  if (Parent is TSimbaOutputTab) then
    Result := TSimbaOutputTab(Parent).ImageIndex
  else
    Result := -1;
end;

procedure TSimbaOutputBox.SetTabImageIndex(Value: Integer);
begin
  if (Parent is TSimbaOutputTab) then
    TSimbaOutputTab(Parent).ImageIndex := Value;
end;

procedure TSimbaOutputBox.DoSettingChange_FontName(Setting: TSimbaSetting);
begin
  if IsFontFixed(Setting.Value) then
    Font.Name := Setting.Value;
end;

procedure TSimbaOutputBox.DoSettingChange_FontSize(Setting: TSimbaSetting);
begin
  Font.Size := Setting.Value;
end;

procedure TSimbaOutputBox.DoSettingChange_FontAntiAliased(Setting: TSimbaSetting);
begin
  FontAntialising := Setting.Value;
end;

procedure TSimbaOutputBox.DoOpenLink(Data: PtrInt);
begin
  if (FMouseLink.DocPos.FileName <> '') then
  begin
    if FileExists(FMouseLink.DocPos.FileName) then
      SimbaTabsForm.Open(FMouseLink.DocPos.FileName);

    with FMouseLink.DocPos do
      SimbaTabsForm.CurrentEditor.FocusLine(Line, Col, $0000A5);

    Exit;
  end;

  if (FMouseLink.Quote.FileName <> '') then
  begin
    if DirectoryExists(FMouseLink.Quote.FileName) then
      SimbaNativeInterface.OpenDirectory(FMouseLink.Quote.FileName)
    else
    if FileExists(FMouseLink.Quote.FileName) and FMouseLink.Quote.FileName.EndsWith('.simba') then
      SimbaTabsForm.Open(FMouseLink.Quote.FileName)
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
begin
  inherited Create(AOwner, False);

  FBuffer := TStringList.Create();
  FLock := TCriticalSection.Create();

  OnMouseLeave        := @DoMouseLeave;
  OnMouseLink         := @DoAllowMouseLink;
  OnClickLink         := @DoMouseLinkClick;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;

  TabStop := False;

  MouseLinkColor.Style := [fsUnderline];

  MarkupByClass[TSynEditMarkupBracket].Enabled := False;
  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  MouseOptions := [emUseMouseActions];
  ResetMouseActions();
  with MouseTextActions.Add() do
    Command := emcMouseLink;

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.OutputBox.FontName, @DoSettingChange_FontName, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.OutputBox.FontSize, @DoSettingChange_FontSize, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.OutputBox.FontAntiAliased, @DoSettingChange_FontAntiAliased, True);
end;

destructor TSimbaOutputBox.Destroy;
begin
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
  Arr := S.Split(LineEnding, False);
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

procedure TSimbaOutputBox.AddLine(Flags: EDebugLnFlags; const S: String);
begin
  FLock.Enter();
  FBuffer.AddObject(S, TObject(PtrUInt(Integer(Flags))));
  FLock.Leave();
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
        if NeedFocus then
          MakeVisible();

        TopLine := Lines.Count;
      end;
      EndUpdate();
      Invalidate();

      FBuffer.Clear();
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputBox.MakeVisible;
begin
  if (Parent is TSimbaOutputTab) then
    TSimbaOutputTab(Parent).Show();
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
  for I := 0 to FTabControl.TabCount - 1 do
    TSimbaOutputTab(FTabControl.Tabs[I]).OutputBox.Flush();
end;

function TSimbaOutputForm.GetActiveOutputBox: TSimbaOutputBox;
begin
  if (FTabControl.ActiveTab is TSimbaOutputTab) then
    Result := TSimbaOutputTab(FTabControl.ActiveTab).OutputBox
  else
    Result := nil;
end;

procedure TSimbaOutputForm.DebugLn(const S: String);
begin
  SimbaOutputBox.Add(S + LineEnding);
end;

function TSimbaOutputForm.AddSimbaOutput: TSimbaOutputBox;
var
  Tab: TSimbaOutputTab;
begin
  Tab := FTabControl.AddTab('Simba') as TSimbaOutputTab;
  Tab.ImageIndex := IMG_SIMBA;

  Result := Tab.OutputBox;
  Result.PopupMenu := ContextMenu;
end;

function TSimbaOutputForm.AddScriptOutput(TabTitle: String): TSimbaOutputBox;
var
  Tab: TSimbaOutputTab;
begin
  Tab := FTabControl.AddTab(TabTitle) as TSimbaOutputTab;

  Result := Tab.OutputBox;
  Result.PopupMenu := ContextMenu;
end;

procedure TSimbaOutputForm.RemoveTab(OutputBox: TSimbaOutputBox);
begin
  if (OutputBox <> nil) and (OutputBox.Parent is TSimbaOutputTab) then
    FTabControl.DeleteTab(TSimbaOutputTab(OutputBox.Parent));
end;

procedure TSimbaOutputForm.MoveTab(AFrom, ATo: Integer);
begin
  FTabControl.MoveTab(AFrom + 1, ATo + 1); // + 1 because of Simba tab
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

function TSimbaOutputForm.CanAnchorDocking(X, Y: Integer): Boolean;
begin
  Result := FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging);
end;

procedure TSimbaOutputForm.DoScriptTabChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    TSimbaScriptTab(Sender).OutputBox.MakeVisible();
end;

procedure TSimbaOutputForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaOutputForm.ContextMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

procedure TSimbaOutputForm.FormMouseLeave(Sender: TObject);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaOutputForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTabControl := TSimbaTabControl.Create(Self, TSimbaOutputTab);
  FTabControl.Parent := Self;
  FTabControl.Align := alClient;
  FTabControl.CanAddTabOnDoubleClick := False;
  FTabControl.CanMoveTabs := False;
  FTabControl.ShowCloseButtons := False;

  FTabControl.OnMouseMove := @FormMouseMove;
  FTabControl.OnMouseDown := @FormMouseDown;
  FTabControl.OnMouseLeave := @FormMouseLeave;

  FSimbaOutputBox := AddSimbaOutput();

  OnDebugLn := @DebugLn;

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE, @DoScriptTabChange);
end;

destructor TSimbaOutputForm.Destroy;
begin
  OnDebugLn := nil;

  inherited Destroy();
end;

end.

