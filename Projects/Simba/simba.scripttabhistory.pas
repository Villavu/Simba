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
  // duplicate
  if (FIndex >= 0) and (FIndex < FHistory.Count) then
  begin
    if (FHistory[FIndex] = Format('%d:%d', [Tab.Editor.CaretX, Tab.Editor.CaretY])) and
       (FHistory.Objects[FIndex] = Tab) then
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

  FHistory.InsertObject(FIndex, Format('%d:%d', [Tab.Editor.CaretX, Tab.Editor.CaretY]), Tab);

  Inc(FIndex);
end;

procedure TSimbaScriptTabHistory.Clear(Tab: TSimbaScriptTab);
var
  I: Int32;
begin
  while FHistory.IndexOfObject(Tab) > -1 do
  begin
    I := FHistory.IndexOfObject(Tab);
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

  if (FHistory.Count > 0) then
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

  if (FHistory.Count > 0) then
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
  ScriptTabHistory := nil;

end.

