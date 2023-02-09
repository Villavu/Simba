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
  simba.ide_codetools_parser, simba.ide_codetools_insight, simba.functionlistform;

type
  TSimbaFunctionListUpdater = class(TThread)
  protected
    FParser: TCodeinsight;
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
  simba.scripttabsform, simba.scripttab, simba.functionlist_simbasection, simba.ide_codetools_setup;

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
  if (SimbaScriptTabsForm = nil) or (SimbaScriptTabsForm.CurrentTab = nil) or (not CodetoolsSetup) then
    Exit;

  Tab := SimbaScriptTabsForm.CurrentTab;
  if (not SimbaFunctionList_SimbaSection.Added(Tab.FunctionList)) and SimbaFunctionList_SimbaSection.Loaded then
    SimbaFunctionList_SimbaSection.Add(Tab.FunctionList);

  if (Tab.FunctionList <> FFunctionList) or (Tab.Editor.ChangeStamp <> FChangeStamp) then // Changes happened: Tab changed or editor modified
  begin
    FFunctionList := Tab.FunctionList;
    FFunctionList.IncRef();
    FFunctionList.TreeView.BeginUpdate();
    FFunctionList.ExpandedState.CreateChildNodes(FFunctionList.ScriptNode);

    FChangeStamp := Tab.Editor.ChangeStamp;
    //FParser      := Tab.GetParser();
  end;
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
    {
    Synchronize(@BeginUpdate);

    if Assigned(FParser) then
    try
      //FParser.NoErrorMessages := True;
      FParser.Run();

      //FFunctionList.ScriptNode.DeleteChildren();
      //FFunctionList.AddDecls(FFunctionList.ScriptNode, FParser.Items);

      {
      if (FParser.IncludesHash <> FFunctionList.IncludesHash) then
      begin
        FFunctionList.IncludesNode.DeleteChildren();
        FFunctionList.PluginsNode.DeleteChildren();
        for I := 0 to High(FParser.Includes) do
          FFunctionList.AddInclude(FParser.Includes[I]);

        FFunctionList.IncludesHash := FParser.IncludesHash;
      end;
      }
    finally
      FParser.Free();
      FParser := nil;

      Synchronize(@EndUpdate);
    end;

    }
    Sleep(850);
  end;
end;

initialization
  SimbaFunctionListUpdater := nil;

finalization
  if Assigned(SimbaFunctionListUpdater) then
    FreeAndNil(SimbaFunctionListUpdater);

end.

