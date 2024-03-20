{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_mainstatusbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls,
  simba.base, simba.component_statusbar;

type
  TSimbaMainStatusBar = class(TComponent)
  protected
    FStatusBar: TSimbaStatusBar;

    procedure DoUpdateScriptStatus(Sender: TObject);

    procedure DoMouseLoggerChange(Sender: TObject);
    procedure DoTabCaretMoved(Sender: TObject);
    procedure DoTabLoaded(Sender: TObject);
    procedure DoTabSearch(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoTabScriptStateChange(Sender: TObject);
    procedure DoFunctionListSelection(Sender: TObject);
  public
    constructor Create; reintroduce;
  end;

var
  SimbaMainStatusBar: TSimbaMainStatusBar;

implementation

uses
  simba.ide_initialization, simba.ide_events, simba.ide_mouselogger, simba.ide_tab,
  simba.ide_editor_findreplace, simba.form_tabs, simba.form_functionlist;

procedure TSimbaMainStatusBar.DoUpdateScriptStatus(Sender: TObject);
begin
  if Assigned(SimbaTabsForm) and Assigned(SimbaTabsForm.CurrentTab) then
    FStatusBar.PanelText[1] := SimbaTabsForm.CurrentTab.ScriptStateStr;
end;

procedure TSimbaMainStatusBar.DoMouseLoggerChange(Sender: TObject);
begin
  if (Sender is TSimbaMouseLogger) then
    with TSimbaMouseLogger(Sender) do
    begin
      FStatusBar.PanelText[0] := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
      if HotkeyPressed then
        DebugLn([EDebugLn.FOCUS], FStatusBar.PanelText[0]);
    end;
end;

procedure TSimbaMainStatusBar.DoTabCaretMoved(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    with TSimbaScriptTab(Sender) do
      FStatusBar.PanelText[2] := 'Line ' + IntToStr(Editor.CaretY) + ', Col ' + IntToStr(Editor.CaretX);
end;

procedure TSimbaMainStatusBar.DoTabLoaded(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    FStatusBar.PanelText[3] := TSimbaScriptTab(Sender).ScriptFileName;
end;

procedure TSimbaMainStatusBar.DoTabSearch(Sender: TObject);
begin
  if (Sender is TSimbaEditorFind) then
    FStatusBar.PanelText[3] := 'Find matches: ' + IntToStr(TSimbaEditorFind(Sender).Matches);
end;

procedure TSimbaMainStatusBar.DoTabChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    FStatusBar.PanelText[1] := TSimbaScriptTab(Sender).ScriptStateStr;
end;

procedure TSimbaMainStatusBar.DoTabScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) and TSimbaScriptTab(Sender).IsActiveTab() then
    FStatusBar.PanelText[1] := TSimbaScriptTab(Sender).ScriptStateStr;
end;

procedure TSimbaMainStatusBar.DoFunctionListSelection(Sender: TObject);
begin
  if (Sender is TSimbaFunctionListNode) then
    FStatusBar.PanelText[3] := TSimbaFunctionListNode(Sender).Hint;
end;

constructor TSimbaMainStatusBar.Create;
begin
  inherited Create(nil);

  with TIdleTimer.Create(Self) do
  begin
    AutoEnabled := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent := itaOnUserInput;
    Interval := 750;
    OnTimer := @DoUpdateScriptStatus;
  end;

  FStatusBar := TSimbaStatusBar.Create(Application.MainForm);
  FStatusBar.Parent := Application.MainForm;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := 4;
  FStatusBar.PanelTextMeasure[0] := '(10000, 10000)';
  FStatusBar.PanelTextMeasure[1] := '[000:000:000]';
  FStatusBar.PanelTextMeasure[2] := 'Line 1000, Col 1000';

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.MOUSELOGGER_CHANGE,      @DoMouseLoggerChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CARETMOVED,          @DoTabCaretMoved);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_LOADED,              @DoTabLoaded);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_SEARCH,              @DoTabSearch);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE,              @DoTabChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_SCRIPTSTATE_CHANGE,  @DoTabScriptStateChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.FUNCTIONLIST_SELECTION,  @DoFunctionListSelection);
end;

procedure CreateMainStatusBar;
begin
  SimbaMainStatusBar := TSimbaMainStatusBar.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@CreateMainStatusBar, 'Create Main StatusBar');

finalization
  if Assigned(SimbaMainStatusBar) then
    FreeAndNil(SimbaMainStatusBar);

end.

