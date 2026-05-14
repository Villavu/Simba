{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
// TODO:
// Context menu
// Settings
// Link clicks
// SetCodetoolsMessageHandler(@DebugLn);
// OnDebugLn := @DebugLn;
// DoRunCompileStopPause scripttabs. Maybe use event tho?
// TSimbaScriptTabRunner.ShowOutputBox
// Package updater
unit simba.form_output;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Menus,
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

    function FindTab(ScriptTabUID: Int64): TOutputTab;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

    // This form has no docking header and docking is performed on empty space here
    procedure DoTabControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoTabControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create; reintroduce;

    function FindList(ScriptTabUID: Int64): TOutputListComponentReal;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

uses
  AnchorDocking,
  simba.initializations,
  simba.component_images,
  simba.ide_dockinghelpers;

function TSimbaOutputForm.FindTab(ScriptTabUID: Int64): TOutputTab;
var
  I: Integer;
begin
  for I := 0 to FTabControl.TabCount - 1 do
    if (TOutputTab(FTabControl.Tabs[I]).FScriptTabUID = ScriptTabUID) then
      Exit(TOutputTab(FTabControl.Tabs[I]));
  Result := nil;
end;

procedure TSimbaOutputForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

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

  procedure DoTabAdd(Tab: TSimbaScriptTab);
  var
    NewTab: TOutputTab;
  begin
    NewTab := TOutputTab(FTabControl.AddTab(Tab.Caption));
    NewTab.FScriptTabUID := Tab.UID;
    NewTab.ImageIndex := SimbaImages.STOP;
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

