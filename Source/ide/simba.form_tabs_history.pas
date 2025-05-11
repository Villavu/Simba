unit simba.form_tabs_history;

{$i simba.inc}

interface
uses
  Classes, SysUtils, Math, LazLogger,
  simba.ide_editor, simba.ide_tab, simba.containers;

type
  TSimbaScriptTabHistory = class
  strict private
    type
      THistoryPoint = class
        Tab    : TSimbaScriptTab;
        Caret  : TPoint;
      end;
      THistoryList = specialize TSimbaObjectList<THistoryPoint>;
  strict private
    FIndex    : Integer;       // 1-based “next slot”; 0 means empty
    FHistory  : THistoryList;
    FMaxDepth : Integer;

    function  CreateHistoryPoint(Tab: TSimbaScriptTab; x, y: Integer): THistoryPoint;
    procedure PruneIfNeeded;
    procedure DumpState(const msg: String);
  public
    FSilent   : Boolean;
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;

    procedure PushFromEditor(Tab: TSimbaScriptTab; CaretXY: TPoint);
    procedure Clear(Tab: TSimbaScriptTab);
    procedure GoBack;
    procedure GoForward;

    constructor Create;
    destructor  Destroy; override;
  end;

var
  SimbaScriptTabHistory: TSimbaScriptTabHistory;

implementation
uses simba.base, simba.form_tabs;

{ ───── helpers ───── }
function TSimbaScriptTabHistory.CreateHistoryPoint(Tab: TSimbaScriptTab; x, y: Integer): THistoryPoint;
begin
  DebugLn('TSimbaScriptTabHistory.MakePoint');
  Result := THistoryPoint.Create;
  Result.Tab := Tab;
  Result.Caret := Point(x, y);
end;

procedure TSimbaScriptTabHistory.DumpState(const Msg: String);
var
  i      : Integer;
  capStr : String;
begin
  DebugLn('│  State @ ' + Msg +
          '  Index=' + IntToStr(FIndex) +
          '  Count=' + IntToStr(FHistory.Count));

  for i := 0 to FHistory.Count-1 do
  begin
    if Assigned(FHistory[i].Tab) then
      capStr := FHistory[i].Tab.Caption
    else
      capStr := '<nil-tab>';

    DebugLn(Format('│  %2d%s %s (%d,%d)',
                   [i+1,
                    IfThen(i+1 = FIndex, ' ►', ''),
                    capStr,
                    FHistory[i].Caret.X,
                    FHistory[i].Caret.Y]));
  end;
end;


procedure TSimbaScriptTabHistory.PruneIfNeeded;
begin
  if (FMaxDepth > 0) and (FHistory.Count > FMaxDepth) then
  begin
    DebugLn(['PruneIfNeeded – trimming to ', FMaxDepth]);
    while FHistory.Count > FMaxDepth do
      FHistory.Delete(0);
    FIndex := EnsureRange(FIndex, 0, FHistory.Count);
    DumpState('Prune');
  end;
end;

procedure TSimbaScriptTabHistory.PushFromEditor(Tab: TSimbaScriptTab; CaretXY: TPoint);
var
  Pt: THistoryPoint;
begin
  DebugLn('TSimbaScriptTabHistory.PushFromEditor');
  Pt := CreateHistoryPoint(Tab, CaretXY.X, CaretXY.Y);
  DebugLn(Format('TSimbaScriptTabHistory.PushFromEditor pt %d, %d', [pt.Caret.X, pt.Caret.Y]));

  { ignore tiny moves in the same file }
  if (FHistory.Count > 0) and (FIndex > 0) then
    with FHistory[FIndex-1] do
      if (Tab = Pt.Tab) and (Abs(Caret.Y - Pt.Caret.Y) < 15) then
        Exit;                     // nothing added, nothing deleted

  DebugLn('TSimbaScriptTabHistory.PushFromEditor deduped');

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
      else
      begin
        { different location → start a new branch }
        while FHistory.Count > FIndex do
          FHistory.Delete(FHistory.Count - 1);
      end;
  end;

  FHistory.Add(Pt);
  Inc(FIndex);
  DumpState('Push');
  PruneIfNeeded;
end;

procedure TSimbaScriptTabHistory.Clear(Tab: TSimbaScriptTab);
var
  i: Integer;
begin
  for i := FHistory.Count-1 downto 0 do
    if FHistory[i].Tab = Tab then
    begin
      if i < FIndex then Dec(FIndex);
      FHistory.Delete(i);
    end;
  DumpState('Clear');
end;

procedure TSimbaScriptTabHistory.GoBack;
begin
  try
    DebugLn('GoBack');
    if FIndex <= 1 then Exit;

    FSilent := True;

    repeat
      Dec(FIndex);
    until (FIndex = 0) or
          (FHistory[FIndex-1].Caret.Y <> FHistory[FIndex].Caret.Y) or
          (FHistory[FIndex-1].Tab     <> FHistory[FIndex].Tab);

    if FIndex = 0 then FIndex := 1;          // safety

    DumpState('Back → '+IntToStr(FIndex));

    with FHistory[FIndex-1] do
    begin
      Tab.Show;
      Tab.Editor.CaretXY := Caret;
      Tab.Editor.TopLine := Caret.Y - (Tab.Editor.LinesInWindow div 2);
      Tab.Editor.EnsureCursorPosVisible;
    end;
  finally
    FSilent := False;
  end;
end;

procedure TSimbaScriptTabHistory.GoForward;
begin
  DebugLn('GoForward');
  if FIndex >= FHistory.Count then Exit;

  repeat
    Inc(FIndex);
  until (FIndex >= FHistory.Count) or
        (FHistory[FIndex-1].Caret.Y <> FHistory[FIndex-2].Caret.Y) or
        (FHistory[FIndex-1].Tab     <> FHistory[FIndex-2].Tab);

  if FIndex > FHistory.Count then FIndex := FHistory.Count; // safety

  DumpState('Forward → '+IntToStr(FIndex));

  with FHistory[FIndex-1] do
  begin
    Tab.Show;
    Tab.Editor.CaretXY := Caret;
    Tab.Editor.TopLine := Caret.Y - (Tab.Editor.LinesInWindow div 2);
    Tab.Editor.EnsureCursorPosVisible;
  end;
end;

{ ───── lifecycle ───── }
constructor TSimbaScriptTabHistory.Create;
begin
  inherited Create;
  DebugLn('TSimbaScriptTabHistory.Create');
  FHistory  := THistoryList.Create(True);
  FIndex    := 0;
  FMaxDepth := 128;
  FSilent   := False;
end;

destructor TSimbaScriptTabHistory.Destroy;
begin
  DumpState('Destroy');
  FHistory.Free;
  inherited Destroy;
end;

initialization
  DebugLn('simba.form_tabs_history – initialization');
  SimbaScriptTabHistory := TSimbaScriptTabHistory.Create;

finalization
  DebugLn('simba.form_tabs_history – finalization');
  SimbaScriptTabHistory.Free;
  SimbaScriptTabHistory := nil;

end.

