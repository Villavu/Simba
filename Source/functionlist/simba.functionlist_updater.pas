{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlist_updater;

{$i simba.inc}

interface

uses
  classes, sysutils, comctrls,
  simba.codeparser, simba.codeinsight, simba.functionlistform;

type
  TSimbaFunctionListUpdater = class(TThread)
  protected
    FParser: TCodeInsight;
    FChangeStamp: Int64;

    FFunctionList: TSimbaFunctionList;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListUpdater: TSimbaFunctionListUpdater;

implementation

uses
  simba.scripttabsform, simba.scripttab;

constructor TSimbaFunctionListUpdater.Create;
begin
  inherited Create(False, 512*512);
end;

destructor TSimbaFunctionListUpdater.Destroy;
begin
  Terminate();
  WaitFor();

  inherited Destroy();
end;

procedure TSimbaFunctionListUpdater.BeginUpdate;
var
  Tab: TSimbaScriptTab;
begin
  Tab := SimbaScriptTabsForm.CurrentTab;
  if (Tab = nil) then
    Exit;
  // No changes
  if (Tab.FunctionList = FFunctionList) and (Tab.Editor.ChangeStamp = FChangeStamp) then
    Exit;

  FFunctionList := Tab.FunctionList;
  FFunctionList.IncRef();
  FFunctionList.TreeView.BeginUpdate();
  FFunctionList.ExpandedState.CreateChildNodes(FFunctionList.ScriptNode);

  FChangeStamp := Tab.Editor.ChangeStamp;
  FParser      := Tab.GetParser();
end;

procedure TSimbaFunctionListUpdater.EndUpdate;
begin
  FFunctionList.ExpandedState.Apply(FFunctionList.TreeView);
  FFunctionList.ScriptNode.Expanded := True;
  FFunctionList.TreeView.EndUpdate();
  FFunctionList.DecRef();
end;

procedure TSimbaFunctionListUpdater.Execute;
var
  I: Integer;
begin
  while (not Terminated) do
  begin
    Synchronize(@BeginUpdate);

    if Assigned(FParser) then
    try
      FParser.Run();

      FFunctionList.ScriptNode.DeleteChildren();
      FFunctionList.AddDecls(FFunctionList.ScriptNode, FParser.Items);

      if (FParser.IncludesHash <> FFunctionList.IncludesHash) then
      begin
        FFunctionList.IncludesNode.DeleteChildren();
        FFunctionList.PluginsNode.DeleteChildren();
        for I := 0 to High(FParser.Includes) do
          FFunctionList.AddInclude(FParser.Includes[I]);

        FFunctionList.IncludesHash := FParser.IncludesHash;
      end;
    finally
      FParser.Free();
      FParser := nil;

      Synchronize(@EndUpdate);
    end;

    Sleep(850);
  end;
end;

initialization
  SimbaFunctionListUpdater := nil;

finalization
  if Assigned(SimbaFunctionListUpdater) then
    FreeAndNil(SimbaFunctionListUpdater);

end.

