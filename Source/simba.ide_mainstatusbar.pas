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
  simba.mufasatypes, simba.component_statusbar;

type
  TSimbaMainStatusBar = class
  protected
    FStatusBar: TSimbaStatusBar;

    procedure DoMouseLoggerChange(Sender: TObject);
    procedure DoTabCaretMoved(Sender: TObject);
    procedure DoTabLoaded(Sender: TObject);
    procedure DoTabSearch(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoScriptStateChange(Sender: TObject);
    procedure DoFunctionListNodeSelection(Sender: TObject);
  public
    constructor Create;
  end;

var
  SimbaMainStatusBar: TSimbaMainStatusBar;

implementation

uses
  simba.ide_initialization, simba.ide_events, simba.mouselogger, simba.scripttab,
  simba.editor_findreplace, simba.functionlistform;

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

procedure TSimbaMainStatusBar.DoScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) and TSimbaScriptTab(Sender).IsActiveTab() then
    FStatusBar.PanelText[1] := TSimbaScriptTab(Sender).ScriptStateStr;
end;

procedure TSimbaMainStatusBar.DoFunctionListNodeSelection(Sender: TObject);
begin
  if (Sender is TSimbaFunctionListNode) then
    FStatusBar.PanelText[3] := TSimbaFunctionListNode(Sender).Hint;
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
  SimbaIDEEvents.RegisterOnFunctionListNodeSelection(@DoFunctionListNodeSelection);
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

