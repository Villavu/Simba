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
  simba.vartype_windowhandle,
  simba.ide_codetools_parser,
  simba.ide_controller,
  simba.ide_tab,
  simba.ide_editor_findreplace,
  simba.datetime;

procedure TSimbaMainStatusBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoUpdateMouse;
  begin
    try
      with SimbaController.WindowSelection.EnsureValid().GetRelativeCursorPos() do
        FStatusBar.PanelText[0] := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
    except
      FStatusBar.PanelText[0] := '(-1, -1)';
    end;
  end;

  procedure DoCaretMoved(Tab: TSimbaScriptTab);
  begin
    FStatusBar.PanelText[2] := 'Line ' + IntToStr(Tab.Editor.CaretY) + ', ' +
                               'Col ' + IntToStr(Tab.Editor.CaretX);
  end;

  procedure DoTabLoaded(Tab: TSimbaScriptTab);
  begin
    FStatusBar.PanelText[3] := Tab.ScriptFileName;
  end;

  procedure DoTabSearch(Find: TSimbaEditorFind);
  begin
    FStatusBar.PanelText[3] := 'Find matches: ' + IntToStr(Find.Matches);
  end;

  procedure DoFunctionListSelectionChange(Decl: TDeclaration);
  begin
    FStatusBar.PanelText[3] := Decl.Header;
  end;

  procedure DoUpdateRunningState(Tab: TSimbaScriptTab);
  begin
    case Tab.RunningState of
      ESimbaScriptState.RUNNING: FStatusBar.PanelText[1] := FormatMilliseconds(Tab.RunningTime, 'hh:mm:ss');
      ESimbaScriptState.PAUSED:  FStatusBar.PanelText[1] := 'Paused';
      else
        FStatusBar.PanelText[1] := 'Stopped';
    end;
  end;

begin
  case Event of
    ESimbaEvent.TIMER_750:                     DoUpdateMouse();
    ESimbaEvent.TAB_CARETMOVED:                DoCaretMoved(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_LOADED:                    DoTabLoaded(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_SEARCH:                    DoTabSearch(TSimbaEditorFind(Data));
    ESimbaEvent.TAB_CHANGE:                    DoUpdateRunningState(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_ACTIVE_750:                DoUpdateRunningState(TSimbaScriptTab(Data));
    ESimbaEvent.FUNCTIONLIST_SELECTION_CHANGE: DoFunctionListSelectionChange(TDeclaration(Data));
  end;
end;

constructor TSimbaMainStatusBar.Create;
begin
  inherited Create(nil);

  FStatusBar := TSimbaStatusBar.Create(Self);
  FStatusBar.Parent := Application.MainForm;
  FStatusBar.Align := alBottom;
  FStatusBar.PanelCount := 4;
  FStatusBar.PanelTextMeasure[0] := '(10000, 10000)';
  FStatusBar.PanelTextMeasure[1] := '[000:000:000]';
  FStatusBar.PanelTextMeasure[2] := 'Line 1000, Col 1000';

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.TIMER_750,
    ESimbaEvent.TAB_CARETMOVED,
    ESimbaEvent.TAB_LOADED,
    ESimbaEvent.TAB_SEARCH,
    ESimbaEvent.TAB_CHANGE,
    ESimbaEvent.TAB_ACTIVE_750,
    ESimbaEvent.FUNCTIONLIST_SELECTION_CHANGE
  ]);
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

