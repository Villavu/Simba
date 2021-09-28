unit simba.scripttabhistory;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, generics.collections,
  simba.scripttab;

type
  TSimbaScriptTabHistory = class
  protected
  type
    THistoryPoint = class
      Tab: TSimbaScriptTab;
      CaretX, CaretY: Int32;
    end;
    THistoryList = specialize TObjectList<THistoryPoint>;
  protected
    FIndex: Int32;
    FHistory: THistoryList;
  public
    procedure Add(Tab: TSimbaScriptTab);
    procedure Clear(Tab: TSimbaScriptTab);

    procedure GoBack;
    procedure GoForward;

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaScriptTabHistory: TSimbaScriptTabHistory;

implementation

procedure TSimbaScriptTabHistory.Add(Tab: TSimbaScriptTab);
var
  Point: THistoryPoint;
begin
  if (Tab.Editor.CaretX = 1) and (Tab.Editor.CaretY = 1) then
    Exit;

  if (FIndex >= 0) and (FIndex < FHistory.Count) then
  begin
    // duplicate
    if (FHistory[FIndex].CaretX = Tab.Editor.CaretX) and (FHistory[FIndex].CaretY = Tab.Editor.CaretY) and
       (FHistory[FIndex].Tab = Tab) then
      Exit;

    // not enough movement
    if (FHistory[FIndex].Tab = Tab) and (Abs(Tab.Editor.CaretY - FHistory[FIndex].CaretY) < 20) then
      Exit;
  end;

  if (FHistory.Count > 50) then
  begin
    if (FIndex > (FHistory.Count div 2)) then
    begin
      FHistory.Delete(0);

      Dec(FIndex);
    end else
      FHistory.Delete(FHistory.Count - 1);
  end;

  Point := THistoryPoint.Create();
  Point.Tab := Tab;
  Point.CaretX := Tab.Editor.CaretX;
  Point.CaretY := Tab.Editor.CaretY;

  FHistory.Insert(FIndex, Point);

  Inc(FIndex);
end;

procedure TSimbaScriptTabHistory.Clear(Tab: TSimbaScriptTab);
var
  I: Int32;
begin
  for I := FHistory.Count - 1 downto 0 do
    if (FHistory[I].Tab = Tab) then
    begin
      if (I < FIndex) then
        Dec(FIndex);

      FHistory.Delete(I);
    end;
end;

procedure TSimbaScriptTabHistory.GoBack;
begin
  Dec(FIndex);
  if (FIndex < 0) then
    FIndex := 0;
  if (FIndex >= FHistory.Count) then
    FIndex := FHistory.Count - 1;

  while (FIndex > 0) and
        (FHistory[FIndex].Tab.Editor.CaretX = FHistory[FIndex].CaretX) and
        (FHistory[FIndex].Tab.Editor.CaretY = FHistory[FIndex].CaretY) do
    Dec(FIndex);

  if (FHistory.Count > 0) then
  begin
    FHistory[FIndex].Tab.Editor.CaretX  := FHistory[FIndex].CaretX;
    FHistory[FIndex].Tab.Editor.CaretY  := FHistory[FIndex].CaretY;
    FHistory[FIndex].Tab.Editor.TopLine := FHistory[FIndex].CaretY - (FHistory[FIndex].Tab.Editor.LinesInWindow div 2);
    FHistory[FIndex].Tab.Show();
  end;
end;

procedure TSimbaScriptTabHistory.GoForward;
begin
  Inc(FIndex);
  if (FIndex < 0) then
    FIndex := 0;
  if (FIndex >= FHistory.Count) then
    FIndex := FHistory.Count - 1;

  while (FIndex < FHistory.Count - 1) and
        (FHistory[FIndex].Tab.Editor.CaretX = FHistory[FIndex].CaretX) and
        (FHistory[FIndex].Tab.Editor.CaretY = FHistory[FIndex].CaretY) do
    Inc(FIndex);

  if (FHistory.Count > 0) then
  begin
    FHistory[FIndex].Tab.Editor.CaretX  := FHistory[FIndex].CaretX;
    FHistory[FIndex].Tab.Editor.CaretY  := FHistory[FIndex].CaretY;
    FHistory[FIndex].Tab.Editor.TopLine := FHistory[FIndex].CaretY - (FHistory[FIndex].Tab.Editor.LinesInWindow div 2);
    FHistory[FIndex].Tab.Show();
  end;
end;

constructor TSimbaScriptTabHistory.Create;
begin
  FHistory := THistoryList.Create();
end;

destructor TSimbaScriptTabHistory.Destroy;
begin
  if (FHistory <> nil) then
    FHistory.Free();

  inherited Destroy();
end;

initialization
  SimbaScriptTabHistory := TSimbaScriptTabHistory.Create();

finalization
  SimbaScriptTabHistory.Free();
  SimbaScriptTabHistory := nil;

end.

