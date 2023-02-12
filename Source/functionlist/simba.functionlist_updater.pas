{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlist_updater;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_insight, simba.functionlistform;

type
  TSimbaFunctionListUpdater = class(TThread)
  protected
    FNeedUpdate: Boolean;

    FCodeinsight: TCodeinsight;
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
  SimbaFunctionListUpdater: TSimbaFunctionListUpdater = nil;

implementation

uses
  simba.scripttabsform, simba.scripttab, simba.functionlist_simbasection, simba.ide_codetools_setup;

constructor TSimbaFunctionListUpdater.Create;
begin
  inherited Create(False, 512*512);

  FCodeinsight := TCodeinsight.Create();
  FCodeinsight.ScriptParser.NoErrorMessages := True;
end;

destructor TSimbaFunctionListUpdater.Destroy;
begin
  Terminate();
  WaitFor();

  if (FCodeinsight <> nil) then
    FreeAndNil(FCodeinsight);

  inherited Destroy();
end;

procedure TSimbaFunctionListUpdater.BeginUpdate;
var
  Tab: TSimbaScriptTab;
begin
  AssertMainThread('TSimbaFunctionListUpdater.BeginUpdate');

  if (SimbaScriptTabsForm <> nil) and (SimbaScriptTabsForm.CurrentTab <> nil) then
  begin
    Tab := SimbaScriptTabsForm.CurrentTab;

    FNeedUpdate := (Tab.FunctionList <> FFunctionList) or (Tab.Editor.ChangeStamp <> FChangeStamp); // Changes happened: Tab changed or editor modified
    if FNeedUpdate then
    begin
      FChangeStamp := Tab.Editor.ChangeStamp;

      FFunctionList := Tab.FunctionList;
      FFunctionList.IncRef();
      FFunctionList.TreeView.BeginUpdate();
      FFunctionList.ExpandedState.CreateChildNodes(FFunctionList.ScriptNode);

      FCodeinsight.SetScript(Tab.Script, Tab.ScriptFileName, -1, -1);
    end;
  end;
end;

procedure TSimbaFunctionListUpdater.EndUpdate;
begin
  AssertMainThread('TSimbaFunctionListUpdater.EndUpdate');

  FFunctionList.ExpandedState.Apply(FFunctionList.TreeView);
  FFunctionList.ScriptNode.Expanded := True;
  FFunctionList.TreeView.EndUpdate();
  FFunctionList.DecRef();

  FNeedUpdate := False;
end;

procedure TSimbaFunctionListUpdater.Execute;
var
  I: Integer;
begin
  while (not Terminated) and (not CodetoolsSetup) do
    Sleep(500);

  while (not Terminated) do
  begin
    Synchronize(@BeginUpdate);

    if FNeedUpdate then
    begin
      FCodeinsight.Run();

      FFunctionList.ScriptNode.DeleteChildren();
      FFunctionList.AddDecls(FFunctionList.ScriptNode, FCodeinsight.ScriptParser.Items);

      if (FCodeinsight.IncludesHash <> FFunctionList.IncludesHash) then
      begin
        FFunctionList.IncludesNode.DeleteChildren();
        FFunctionList.PluginsNode.DeleteChildren();
        for I := 0 to FCodeinsight.Includes.Count - 1 do
          FFunctionList.AddInclude(FCodeinsight.Includes[I]);

        FFunctionList.IncludesHash := FCodeinsight.IncludesHash;
      end;

      if (not SimbaFunctionList_SimbaSection.Added(FFunctionList)) and SimbaFunctionList_SimbaSection.Loaded then
        SimbaFunctionList_SimbaSection.Add(FFunctionList);

      Synchronize(@EndUpdate);
    end;

    Sleep(850);
  end;
end;

finalization
  if Assigned(SimbaFunctionListUpdater) then
    FreeAndNil(SimbaFunctionListUpdater);

end.

