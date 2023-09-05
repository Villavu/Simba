{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_mainstatusbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms,
  simba.mufasatypes, simba.component_statusbar, simba.scriptinstance;

type
  TSimbaMainStatusBar = class
  protected
    FStatusBar: TSimbaStatusBar;

    procedure SetScriptStatePanel(Instance: TSimbaScriptInstance);

    procedure DoMouseLoggerChange(Sender: TObject);
    procedure DoTabCaretMoved(Sender: TObject);
    procedure DoTabLoaded(Sender: TObject);
    procedure DoTabSearch(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoScriptStateChange(Sender: TObject);
    procedure DoScriptRunning(Sender: TObject);
  public
    constructor Create;

    procedure SetMainPanelText(Str: String);
  end;

var
  SimbaMainStatusBar: TSimbaMainStatusBar;

implementation

uses
  simba.ide_initialization, simba.ide_events, simba.mouselogger, simba.scripttab,
  simba.editor_findreplace, simba.datetime;

procedure TSimbaMainStatusBar.SetScriptStatePanel(Instance: TSimbaScriptInstance);
begin
  if (Instance <> nil) and (Instance.State = ESimbaScriptState.STATE_RUNNING) then
    FStatusBar.PanelText[1] := FormatMilliseconds(Instance.TimeRunning, 'hh:mm:ss')
  else
  if (Instance <> nil) and (Instance.State = ESimbaScriptState.STATE_PAUSED) then
    FStatusBar.PanelText[1] := 'Paused'
  else
    FStatusBar.PanelText[1] := 'Stopped'
end;

procedure TSimbaMainStatusBar.DoMouseLoggerChange(Sender: TObject);
begin
  if (Sender is TSimbaMouseLogger) then
    with TSimbaMouseLogger(Sender) do
    begin
      FStatusBar.PanelText[0] := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
      if HotkeyPressed then
        SimbaDebugLn([EDebugLn.FOCUS], FStatusBar.PanelText[0]);
    end;
end;

procedure TSimbaMainStatusBar.DoTabCaretMoved(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    with TSimbaScriptTab(Sender) do
      FStatusBar.PanelText[2] := 'Line ' + IntToStr(Editor.CaretX) + ', Col ' + IntToStr(Editor.CaretY);
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
    SetScriptStatePanel(TSimbaScriptTab(Sender).ScriptInstance);
end;

procedure TSimbaMainStatusBar.DoScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptInstance) and TSimbaScriptInstance(Sender).IsActiveTab() then
    SetScriptStatePanel(TSimbaScriptInstance(Sender));
end;

procedure TSimbaMainStatusBar.DoScriptRunning(Sender: TObject);
var
  ScriptInstance: TSimbaScriptInstance absolute Sender;
begin
  if (Sender is TSimbaScriptInstance) and ScriptInstance.IsActiveTab() and (ScriptInstance.State <> ESimbaScriptState.STATE_PAUSED) then
    FStatusBar.PanelText[1] := FormatMilliseconds(ScriptInstance.TimeRunning, 'hh:mm:ss');
end;

constructor TSimbaMainStatusBar.Create;
begin
  FStatusBar := TSimbaStatusBar.Create(Application.MainForm);
  FStatusBar.Parent := Application.MainForm;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := 4;
  FStatusBar.PanelTextMeasure[0] := '(10000, 10000)';
  FStatusBar.PanelTextMeasure[1] := '[000:000:000]';
  FStatusBar.PanelTextMeasure[2] := 'Line 1000, Col 1000';

  SimbaIDEEvents.RegisterMethodOnMouseLoggerChange(@DoMouseLoggerChange);
  SimbaIDEEvents.RegisterMethodOnEditorCaretMoved(@DoTabCaretMoved);
  SimbaIDEEvents.RegisterMethodOnEditorLoaded(@DoTabLoaded);
  SimbaIDEEvents.RegisterMethodOnEditorSearch(@DoTabSearch);
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoTabLoaded);
  SimbaIDEEvents.RegisterMethodOnScriptStateChange(@DoScriptStateChange);
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoTabChange);
  SimbaIDEEvents.RegisterMethodOnScriptRunning(@DoScriptRunning);
end;

procedure TSimbaMainStatusBar.SetMainPanelText(Str: String);
begin
  FStatusBar.PanelText[3] := Str;
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

