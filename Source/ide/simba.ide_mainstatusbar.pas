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
  simba.base,
  simba.component_statusbar,
  simba.ide_events;

type
  TSimbaMainStatusBar = class(TComponent)
  protected
    FStatusBar: TSimbaStatusBar;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  public
    constructor Create; reintroduce;
  end;

var
  SimbaMainStatusBar: TSimbaMainStatusBar;

implementation

uses
  simba.initializations,
  simba.ide_mouselogger,
  simba.ide_tab,
  simba.ide_editor_findreplace,
  simba.functionlist_page,
  simba.datetime;

procedure TSimbaMainStatusBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.MOUSELOGGER_CHANGE:
      begin
        with TSimbaMouseLogger(Data) do
        begin
          FStatusBar.PanelText[0] := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
          if HotkeyPressed then
            DebugLn([EDebugLn.FOCUS], FStatusBar.PanelText[0]);
        end;
      end;

    ESimbaEvent.TAB_CARETMOVED:
      begin
        with TSimbaScriptTab(Data) do
          FStatusBar.PanelText[2] := 'Line ' + IntToStr(Editor.CaretY) + ', Col ' + IntToStr(Editor.CaretX);
      end;

    ESimbaEvent.TAB_LOADED:
      begin
        FStatusBar.PanelText[3] := TSimbaScriptTab(Data).ScriptFileName;
      end;

    ESimbaEvent.TAB_SEARCH:
      begin
        FStatusBar.PanelText[3] := 'Find matches: ' + IntToStr(TSimbaEditorFind(Data).Matches);
      end;

    ESimbaEvent.TAB_CHANGE:
      begin
        case TSimbaScriptTab(Data).RunningState of
          ESimbaScriptState.RUNNING: FStatusBar.PanelText[1] := 'Running';
          ESimbaScriptState.PAUSED:  FStatusBar.PanelText[1] := 'Paused';
          else
            FStatusBar.PanelText[1] := 'Stopped';
        end;
      end;

    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE:
      begin
        if TSimbaScriptTab(Data).IsActiveTab then
          case TSimbaScriptTab(Data).RunningState of
            ESimbaScriptState.RUNNING: FStatusBar.PanelText[1] := 'Running';
            ESimbaScriptState.PAUSED:  FStatusBar.PanelText[1] := 'Paused';
            else
              FStatusBar.PanelText[1] := 'Stopped';
          end;
      end;

    ESimbaEvent.FUNCTIONLIST_SELECTION_CHANGE:
      begin
        FStatusBar.PanelText[3] := TSimbaFunctionListNode(Data).Hint;
      end;

    ESimbaEvent.SCRIPT_RUNNING:
      begin
        if TSimbaScriptTabRunner(Data).IsActiveTab then
          case TSimbaScriptTabRunner(Data).State of
            ESimbaScriptState.RUNNING: FStatusBar.PanelText[1] := FormatMilliseconds(TSimbaScriptTabRunner(Data).TimeRunning, 'hh:mm:ss');
            ESimbaScriptState.PAUSED:  FStatusBar.PanelText[1] := 'Paused';
          end;
      end;
  end;
end;

constructor TSimbaMainStatusBar.Create;
begin
  inherited Create(nil);

  FStatusBar := TSimbaStatusBar.Create(Application.MainForm);
  FStatusBar.Parent := Application.MainForm;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := 4;
  FStatusBar.PanelTextMeasure[0] := '(10000, 10000)';
  FStatusBar.PanelTextMeasure[1] := '[000:000:000]';
  FStatusBar.PanelTextMeasure[2] := 'Line 1000, Col 1000';

  SimbaEvents.Register(Self, @DoSimbaEvent);
end;

procedure DoCreate;
begin
  SimbaMainStatusBar := TSimbaMainStatusBar.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaMainStatusBar);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaMainStatusBar');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaMainStatusBar');

end.

