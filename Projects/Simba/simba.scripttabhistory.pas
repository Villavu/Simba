unit simba.scripttabhistory;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.scripttab;

type
  TSimbaScriptTabHistory = class
  protected
    FIndex: Int32;
    FHistory: TStringList;
  public
    procedure Add(Tab: TSimbaScriptTab);
    procedure Clear(Tab: TSimbaScriptTab);

    procedure GoBack;
    procedure GoForward;

    constructor Create;
    destructor Destroy; override;
  end;

var
  ScriptTabHistory: TSimbaScriptTabHistory;

implementation

procedure TSimbaScriptTabHistory.Add(Tab: TSimbaScriptTab);
begin
  while FHistory.Count > FIndex + 1 do
    FHistory.Delete(FHistory.Count - 1);

  if FHistory.Count > 25 then
  begin
    FHistory.Delete(0);

    Dec(FIndex);
  end;

  FHistory.AddObject(Format('%d:%d', [Tab.Editor.CaretX, Tab.Editor.CaretY]), Tab);
  if (FHistory.Count > 1) then
    FIndex += 1;
end;

procedure TSimbaScriptTabHistory.Clear(Tab: TSimbaScriptTab);
var
  i: Int32;
begin
  while FHistory.IndexOfObject(Tab) > -1 do
  begin
    i := FHistory.IndexOfObject(Tab);
    if i < FIndex then
      Dec(FIndex);

    FHistory.Delete(i);
  end;
end;

procedure TSimbaScriptTabHistory.GoBack;
begin
  Dec(FIndex);

  if (FIndex < 0) then
    FIndex := 0;
  if (FIndex >= FHistory.Count) then
    FIndex := FHistory.Count - 1;

  if FHistory.Count > 0 then
    with FHistory.Objects[FIndex] as TSimbaScriptTab do
    begin
      MakeVisible();

      Editor.CaretX := StrToInt(FHistory[FIndex].Split(':')[0]);
      Editor.CaretY := StrToInt(FHistory[FIndex].Split(':')[1]);
      Editor.TopLine := Editor.CaretY - (Editor.LinesInWindow div 2);
    end;
end;

procedure TSimbaScriptTabHistory.GoForward;
begin
  Inc(FIndex);

  if (FIndex < 0) then
    FIndex := 0;
  if (FIndex >= FHistory.Count) then
    FIndex := FHistory.Count - 1;

  if FHistory.Count > 0 then
    with FHistory.Objects[FIndex] as TSimbaScriptTab do
    begin
      MakeVisible();

      Editor.CaretX := StrToInt(FHistory[FIndex].Split(':')[0]);
      Editor.CaretY := StrToInt(FHistory[FIndex].Split(':')[1]);
      Editor.TopLine := Editor.CaretY - (Editor.LinesInWindow div 2);
    end;
end;

constructor TSimbaScriptTabHistory.Create;
begin
  FHistory := TStringList.Create();
end;

destructor TSimbaScriptTabHistory.Destroy;
begin
  FHistory.Free();

  inherited Destroy();
end;

initialization
  ScriptTabHistory := TSimbaScriptTabHistory.Create();

finalization
  ScriptTabHistory.Free();

end.

