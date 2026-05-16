{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
// TODO:
// Context menu
// Settings
// DoRunCompileStopPause scripttabs. Maybe use event tho?
// TSimbaScriptTabRunner.ShowOutputBox
// Package updater
unit simba.form_output;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Menus, RegExpr,
  simba.base,
  simba.ide_events,
  simba.ide_tab,
  simba.ide_output_components,
  simba.component_tabcontrol;

type
  TSimbaOutputForm = class(TForm)
  private type
    TOutputTab = class(TSimbaTab)
    protected
      FScriptTabUID: Int64;
      FList: TOutputListComponentReal;
    public
      constructor Create(AOwner: TComponent); override;
    end;
  protected
    FTabControl: TSimbaTabControl;
    FSimbaTab: TOutputTab;

    FRegexError: TRegExpr;
    FRegexTrace: TRegExpr;

    function FindTab(ScriptTabUID: Int64): TOutputTab;

    procedure DoDebugRedirect(const S: String);
    procedure DoDebugLnRedirect(const S: String);
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

    function DoCheckLinkable(Sender: TObject; var Line: String; X: Integer; out X1, X2: Integer): Boolean;
    procedure DoLinkClick(Sender: TObject; Link: String);

    // This form has no docking header and docking is performed on empty space here
    procedure DoTabControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoTabControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function FindList(ScriptTabUID: Int64): TOutputListComponentReal;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

uses
  AnchorDocking,
  simba.initializations,
  simba.component_images,
  simba.ide_dockinghelpers,
  simba.ide_controller,
  simba.ide_codetools_base,
  simba.fs;

function TSimbaOutputForm.FindTab(ScriptTabUID: Int64): TOutputTab;
var
  I: Integer;
begin
  for I := 0 to FTabControl.TabCount - 1 do
    if (TOutputTab(FTabControl.Tabs[I]).FScriptTabUID = ScriptTabUID) then
      Exit(TOutputTab(FTabControl.Tabs[I]));
  Result := nil;
end;

procedure TSimbaOutputForm.DoDebugRedirect(const S: String);
begin
  FSimbaTab.FList.Add(S);
end;

procedure TSimbaOutputForm.DoDebugLnRedirect(const S: String);
begin
  FSimbaTab.FList.Add(S + LineEnding);
end;

procedure TSimbaOutputForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  // Flush all tabs (adding text to component from buffer)
  procedure DoFlush;
  var
    I: Integer;
  begin
    for I := 0 to FTabControl.TabCount - 1 do
      TOutputTab(FTabControl.Tabs[I]).FList.Flush();
  end;

  // show/hide ourselfs
  procedure DoViewOutput(Item: TMenuItem);
  begin
    DockMaster.Show(Self);
  end;

  // clear active
  procedure DoClearOutput;
  begin
    if (FTabControl.ActiveTab <> nil) then
      TOutputTab(FTabControl.ActiveTab).FList.Clear();
  end;

  // open output tab on tab switch
  procedure DoTabChange(Tab: TSimbaScriptTab);
  var
    OutputTab: TOutputTab;
  begin
    OutputTab := FindTab(Tab.UID);
    if (OutputTab <> nil) then
      OutputTab.Show();
  end;

  // Add a output tab for the script tab
  procedure DoTabAdd(Tab: TSimbaScriptTab);
  var
    NewTab: TOutputTab;
  begin
    NewTab := TOutputTab(FTabControl.AddTab(Tab.Caption));
    NewTab.FScriptTabUID := Tab.UID;
    NewTab.ImageIndex := SimbaImages.STOP;
    NewTab.FList.OnCheckLinkable := @DoCheckLinkable;
    NewTab.FList.OnLinkClick := @DoLinkClick;
  end;

  // remove the tab
  procedure DoTabClosed(Tab: TSimbaScriptTab);
  begin
    FTabControl.DeleteTab(FindTab(Tab.UID));
  end;

  // update tab icon
  procedure DoScriptStateChange(Tab: TSimbaScriptTab);
  var
    OutputTab: TOutputTab;
  begin
    OutputTab := FindTab(Tab.UID);
    if (OutputTab <> nil) then
    begin
      case Tab.RunningState of
        ESimbaScriptState.RUNNING: OutputTab.ImageIndex := SimbaImages.PLAY;
        ESimbaScriptState.PAUSED:  OutputTab.ImageIndex := SimbaImages.PAUSE;
        else
          OutputTab.ImageIndex := SimbaImages.STOP;
      end;
    end;
  end;

  // Update tab caption
  procedure DoTabCaption(Tab: TSimbaScriptTab);
  var
    OutputTab: TOutputTab;
  begin
    OutputTab := FindTab(Tab.UID);
    if (OutputTab <> nil) then
      OutputTab.Caption := Tab.Caption;
  end;

  // keep order inline with script tabs
  procedure DoTabMoved(Data: TSimbaEvents.TTabMoved);
  begin
    FTabControl.MoveTab(Data.FromIndex + 1, Data.ToIndex + 1);
  end;

begin
  case Event of
    ESimbaEvent.TIMER_750:              DoFlush();
    ESimbaEvent.ACTION_VIEW_OUTPUT:     DoViewOutput(TMenuItem(Data));
    ESimbaEvent.ACTION_CLEAROUTPUT:     DoClearOutput();
    ESimbaEvent.TAB_CHANGE:             DoTabChange(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_ADD:                DoTabAdd(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CLOSED:             DoTabClosed(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CAPTION:            DoTabCaption(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_MOVED:              DoTabMoved(TSimbaEvents.TTabMoved(Data^));
    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE: DoScriptStateChange(TSimbaScriptTab(Data));
  end;
end;

function TSimbaOutputForm.DoCheckLinkable(Sender: TObject; var Line: String; X: Integer; out X1, X2: Integer): Boolean;
var
  StartQuoteX, EndQuoteX: Integer;
  QuotedText: String;
begin
  Result := False;

  // DocPos that spans the entire line
  if Line.Contains('in file') and (FRegexTrace.Exec(Line) or FRegexError.Exec(Line)) then
  begin
    X1 := 1;
    X2 := Length(Line) + 1;

    Result := True;
  end
  else // check if quoted text at X
  begin
    StartQuoteX := X;
    while (StartQuoteX >= 1) and (Line[StartQuoteX] <> '"') do
      Dec(StartQuoteX);
    EndQuoteX := X;
    while (EndQuoteX <= Length(Line)) and (Line[EndQuoteX] <> '"') do
      Inc(EndQuoteX);

    // text is quoted somewhat...
    if (StartQuoteX > 0) and (EndQuoteX <= Length(Line)) and (StartQuoteX <> EndQuoteX) then
    begin
      QuotedText := Copy(Line, StartQuoteX + 1, (EndQuoteX - StartQuoteX) - 1);

      // either link, or file exists
      Result := QuotedText.StartsWith('http') or FileExists(QuotedText);
      if Result then
      begin
        X1 := StartQuoteX;
        X2 := EndQuoteX+1;
        Line := QuotedText;
      end;
    end;
  end;
end;

procedure TSimbaOutputForm.DoLinkClick(Sender: TObject; Link: String);
begin
  if Link.Contains('in file') then
  begin
    if FRegexError.Exec(Link) then
      SimbaController.OpenInTab(FRegexError.Match[3], StrToInt(FRegexError.Match[1]), StrToInt(FRegexError.Match[2]))
    else if FRegexError.Exec(Link) then
      SimbaController.OpenInTab(FRegexError.Match[3], 1, StrToInt(FRegexError.Match[1]));
  end
  else if Link.StartsWith('http://') or Link.StartsWith('https://') then
  begin
    SimbaController.OpenInBrowser(Link);
  end
  else if FileExists(Link) then
  begin
    if TSimbaFile.FileIsText(Link) then
      SimbaController.OpenInTab(Link)
    else
      SimbaController.OpenInExplorer(Link);
  end;
end;

procedure TSimbaOutputForm.DoTabControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

procedure TSimbaOutputForm.DoTabControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

constructor TSimbaOutputForm.Create;
begin
  inherited Create(nil);

  Name := 'SimbaOutputForm';
  Caption := 'Output';
  TabStop := False;

  // Line 13 in "main" in file "Untitled"
  // Line 11 in function "hmm" in file "Untitled"
  FRegexError := TRegExpr.Create('at line (\d+), column (\d+) in file "([^"]+)"');
  // Unknown declaration "hmm" at line 13, column 3 in file "Untitled"
  FRegexTrace := TRegExpr.Create('Line (\d+) in "([^"]+)" in file "([^"]+)"');

  FTabControl := TSimbaTabControl.Create(Self, TOutputTab);
  FTabControl.Parent := Self;
  FTabControl.Align := alClient;
  FTabControl.Images := SimbaImages;
  FTabControl.CanAddTabOnDoubleClick := False;
  FTabControl.CanMoveTabs := False;
  FTabControl.ShowCloseButtons := False;
  FTabControl.OnMouseMove := @DoTabControlMouseMove;
  FTabControl.OnMouseDown := @DoTabControlMouseDown;

  FSimbaTab := TOutputTab(FTabControl.AddTab());
  FSimbaTab.Caption := 'Simba';
  FSimbaTab.ImageIndex := SimbaImages.SIMBA;

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.TIMER_750,
    ESimbaEvent.ACTION_VIEW_OUTPUT,
    ESimbaEvent.ACTION_CLEAROUTPUT,
    ESimbaEvent.TAB_CHANGE,
    ESimbaEvent.TAB_ADD,
    ESimbaEvent.TAB_CLOSED,
    ESimbaEvent.TAB_CAPTION,
    ESimbaEvent.TAB_MOVED,
    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE
  ]);

  SetDebugRedirects(@DoDebugRedirect, @DoDebugLnRedirect);
end;

destructor TSimbaOutputForm.Destroy;
begin
  SetDebugRedirects(nil, nil);

  FreeAndNil(FRegexTrace);
  FreeAndNil(FRegexError);

  inherited Destroy();
end;

function TSimbaOutputForm.FindList(ScriptTabUID: Int64): TOutputListComponentReal;
var
  I: Integer;
begin
  for I := 0 to FTabControl.TabCount - 1 do
    if (TOutputTab(FTabControl.Tabs[I]).FScriptTabUID = ScriptTabUID) then
      Exit(TOutputTab(FTabControl.Tabs[I]).FList);
  Result := nil;
end;

constructor TSimbaOutputForm.TOutputTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FList := TOutputListComponentReal.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
end;

procedure DoCreate;
begin
  SimbaOutputForm := TSimbaOutputForm.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaOutputForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaOutputForm', 5);
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaOutputForm');

end.

