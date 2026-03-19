{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editorcarethistory;

{$i simba.inc}

interface

uses
  Classes, SysUtils, SynEdit, SynEditMouseCmds, LazSynEditMouseCmdsTypes,
  simba.base,
  simba.containers,
  simba.ide_tab,
  simba.ide_events;

type
  TSimbaEditorCaretHistory = class(TComponent)
  strict private
  type
    THistoryPoint = record
      Tab    : TSimbaScriptTab;
      Caret  : TPoint;
    end;
    THistoryList = specialize TSimbaList<THistoryPoint>;
  strict private
    FIndex    : Integer;       // 1-based “next slot”; 0 means empty
    FHistory  : THistoryList;
    FMaxDepth : Integer;
    FMoving   : Boolean;

    procedure PruneIfNeeded;
    procedure DumpState(const msg: String);

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  public
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;

    procedure PushFromEditor(Tab: TSimbaScriptTab);
    procedure Clear(Tab: TSimbaScriptTab);
    procedure GoBack;
    procedure GoForward;

    constructor Create; reintroduce;
    destructor  Destroy; override;
  end;

  TSimbaEditorPlugin_History = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    function DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
  end;

var
  SimbaEditorCaretHistory: TSimbaEditorCaretHistory;

implementation

uses
  Math,
  simba.form_tabs,
  simba.initializations,
  simba.ide_editor_mousecommands,
  simba.threading;

procedure TSimbaEditorCaretHistory.DumpState(const msg: String);
var
  i: Integer;
begin
  DebugLn('│  State @ ' + Msg +
          '  Index=' + IntToStr(FIndex) +
          '  Count=' + IntToStr(FHistory.Count));

  for i := 0 to FHistory.Count-1 do
  begin
    DebugLn(Format('│  %2d%s %s (%d,%d)',
                   [i+1,
                    IfThen(i+1 = FIndex, ' ►', ''),
                    FHistory[i].Tab.Caption,
                    FHistory[i].Caret.X,
                    FHistory[i].Caret.Y]));
  end;
end;

procedure TSimbaEditorCaretHistory.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    // remove the tab from history
    ESimbaEvent.TAB_CLOSED: Clear(TSimbaScriptTab(Data));
    // add to history
    ESimbaEvent.TAB_CARETMOVED: PushFromEditor(TSimbaScriptTab(Data));
  end;
end;

procedure TSimbaEditorCaretHistory.PruneIfNeeded;
begin
  if (FMaxDepth > 0) and (FHistory.Count > FMaxDepth) then
  begin
    while FHistory.Count > FMaxDepth do
      FHistory.Delete(0);
    FIndex := EnsureRange(FIndex, 0, FHistory.Count);

    {$IFDEF DEBUG}
    DumpState('Prune');
    {$ENDIF}
  end;
end;

procedure TSimbaEditorCaretHistory.PushFromEditor(Tab: TSimbaScriptTab);
var
  CaretXY: TPoint;
  pt: THistoryPoint;
begin
  if FMoving then
    Exit;

  {$IFDEF DEBUG}
  DebugLn('TSimbaEditorCaretHistory.PushFromEditor');
  {$ENDIF}
  CaretXY := Tab.Editor.CaretXY;
  if (CaretXY.X = 1) and (CaretXY.Y = 1) then
  begin
    {$IFDEF DEBUG}
    DebugLn('TSimbaEditorCaretHistory.PushFromEditor default caret pos');
    {$ENDIF}
    Exit;
  end;

  pt := Default(THistoryPoint);
  pt.Tab := Tab;
  pt.Caret := Tab.Editor.CaretXY;

  {$IFDEF DEBUG}
  DebugLn(Format('TSimbaEditorCaretHistory.PushFromEditor pt %d, %d', [pt.Caret.X, pt.Caret.Y]));
  {$ENDIF}

  { ignore tiny moves in the same file }
  if (FHistory.Count > 0) and (FIndex > 0) then
    with FHistory[FIndex-1] do
      if (Tab = Pt.Tab) and (Abs(Caret.Y - Pt.Caret.Y) < 15) then
      begin
        {$IFDEF DEBUG}
        DebugLn('TSimbaEditorCaretHistory.PushFromEditor short distance in same file');
        {$ENDIF}
        Exit;
      end;

  { if we were in the middle of the list … }
  if FIndex < FHistory.Count then
  begin
    with FHistory[FIndex] do
      { same file and same LINE counts as the same forward point }
      if (Tab = Pt.Tab) and (Caret.Y = Pt.Caret.Y) then
      begin
        Inc(FIndex);      // just move forward
        Exit;             // keep the rest of the forward stack
      end
      else begin
        { different location → start a new branch }
        while FHistory.Count > FIndex do
          FHistory.Delete(FHistory.Count - 1);
      end;
  end;

  FHistory.Add(Pt);
  Inc(FIndex);
  {$IFDEF DEBUG}
  DumpState('Push');
  {$ENDIF}
  PruneIfNeeded;
end;

procedure TSimbaEditorCaretHistory.Clear(Tab: TSimbaScriptTab);
var
  i: Integer;
begin
  for i := FHistory.Count-1 downto 0 do
    if FHistory[i].Tab = Tab then
    begin
      if i < FIndex then Dec(FIndex);
      FHistory.Delete(i);
    end;
  {$IFDEF DEBUG}
  DumpState('Clear');
  {$ENDIF}
end;

procedure TSimbaEditorCaretHistory.GoBack;
begin
  try
    {$IFDEF DEBUG}
    DebugLn('GoBack');
    {$ENDIF}
    if FIndex <= 1 then Exit;

    FMoving := True;

    repeat
      Dec(FIndex);
    until (FIndex = 0) or
          (FHistory[FIndex-1].Caret.Y <> FHistory[FIndex].Caret.Y) or
          (FHistory[FIndex-1].Tab     <> FHistory[FIndex].Tab);

    if FIndex = 0 then FIndex := 1;          // safety

    {$IFDEF DEBUG}
    DumpState('Back → '+IntToStr(FIndex));
    {$ENDIF}

    with FHistory[FIndex-1] do
    begin
      SimbaTabsForm.CurrentTab := Tab;
      SimbaTabsForm.CurrentTab.Editor.CaretXY := Caret;
      SimbaTabsForm.CurrentTab.Editor.TopLine := Caret.Y - (Tab.Editor.LinesInWindow div 2);
    end;
  finally
    FMoving := False;
  end;
end;

procedure TSimbaEditorCaretHistory.GoForward;
begin
  {$IFDEF DEBUG}
  DebugLn('GoForward');
  {$ENDIF}
  if FIndex >= FHistory.Count then Exit;

  repeat
    Inc(FIndex);
  until (FIndex >= FHistory.Count) or
        (FHistory[FIndex-1].Caret.Y <> FHistory[FIndex-2].Caret.Y) or
        (FHistory[FIndex-1].Tab     <> FHistory[FIndex-2].Tab);

  if FIndex > FHistory.Count then FIndex := FHistory.Count; // safety

  {$IFDEF DEBUG}
  DumpState('Forward → '+IntToStr(FIndex));
  {$ENDIF}

  with FHistory[FIndex-1] do
  begin
    SimbaTabsForm.CurrentTab := Tab;
    SimbaTabsForm.CurrentTab.Editor.CaretXY := Caret;
    SimbaTabsForm.CurrentTab.Editor.TopLine := Caret.Y - (Tab.Editor.LinesInWindow div 2);
  end;
end;

{ ───── lifecycle ───── }
constructor TSimbaEditorCaretHistory.Create;
begin
  inherited Create(nil);

  FHistory  := THistoryList.Create();
  FIndex    := 0;
  FMaxDepth := 128;
  FMoving   := False;

  SimbaEvents.Register(Self, @DoSimbaEvent);
end;

destructor TSimbaEditorCaretHistory.Destroy;
begin
  if (FHistory <> nil) then
    FreeAndNil(FHistory);

  inherited Destroy();
end;

procedure TSimbaEditorPlugin_History.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  if (Value <> nil) then
  begin
    Value.MouseActions.AddCommand(emcMouseButtonForward, False, LazSynEditMouseCmdsTypes.mbExtra2, ccSingle, cdDown, [], []);
    Value.MouseActions.AddCommand(emcMouseButtonBack,    False, LazSynEditMouseCmdsTypes.mbExtra1, ccSingle, cdDown, [], []);
    Value.RegisterMouseActionExecHandler(@DoMouseAction);
  end;
end;

function TSimbaEditorPlugin_History.DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := (AnAction.Command = emcMouseButtonForward) or (AnAction.Command = emcMouseButtonBack);

  if Result then
    if (AnAction.Command = emcMouseButtonForward) then
      QueueOnMainThread(@SimbaEditorCaretHistory.GoForward)
    else if (AnAction.Command = emcMouseButtonBack) then
      QueueOnMainThread(@SimbaEditorCaretHistory.GoBack);
end;

procedure DoCreate;
begin
  SimbaEditorCaretHistory := TSimbaEditorCaretHistory.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaEditorCaretHistory);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaEditorCaretHistory');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaEditorCaretHistory');

end.

