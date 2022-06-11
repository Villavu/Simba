{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ComCtrls, syncobjs,
  simba.codeparser, simba.codeinsight, simba.functionlistform;

type
  TSimbaFunctionListUpdater = class(TThread)
  protected
    FEvent: TSimpleEvent;
    FParser: TCodeInsight;
    FFunctionList: TSimbaFunctionList;
    FExpandedState: TTreeNodeExpandedState;
    FChangeStamp: Int64;
    FIncludesHash: String;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Execute; override;
  public
    constructor Create(FunctionList: TSimbaFunctionList); reintroduce;
    destructor Destroy; override;

    procedure Update(AParser: TCodeInsight; AChangeStamp: Int64);

    property ChangeStamp: Int64 read FChangeStamp write FChangeStamp;
  end;

implementation

uses
  LazLoggerBase, simba.functionlist_simbasection;

constructor TSimbaFunctionListUpdater.Create(FunctionList: TSimbaFunctionList);
begin
  inherited Create(False);

  FreeOnTerminate := False;

  FEvent := TSimpleEvent.Create();
  FEvent.ResetEvent();

  FFunctionList := FunctionList;
  FExpandedState := TTreeNodeExpandedState.Create(FFunctionList.TreeView);
end;

destructor TSimbaFunctionListUpdater.Destroy;
begin
  DebugLn('TSimbaFunctionListUpdater.Destroy');

  Terminate();
  Update(nil, 0);
  WaitFor();

  FEvent.Free();
  FExpandedState.Free();

  inherited Destroy();
end;

procedure TSimbaFunctionListUpdater.BeginUpdate;
begin
  FFunctionList.TreeView.BeginUpdate();

  FExpandedState.CreateChildNodes(FFunctionList.ScriptNode);
end;

procedure TSimbaFunctionListUpdater.EndUpdate;
begin
  FExpandedState.Apply(FFunctionList.TreeView);

  FFunctionList.ScriptNode.Expanded := True;
  FFunctionList.TreeView.EndUpdate();
end;

procedure TSimbaFunctionListUpdater.Execute;
var
  I: Integer;
begin
  while (not Terminated) do
  begin
    FEvent.WaitFor(INFINITE);

    if (FParser <> nil) then
    begin
      Synchronize(@BeginUpdate);

      try
        FParser.Run();

        FFunctionList.AddDeclarations(FParser.Items, FFunctionList.ScriptNode, True, False, True);

        if (FParser.IncludesHash <> FIncludesHash) then
        begin
          DebugLn('Update Includes Node');

          FFunctionList.IncludesNode.DeleteChildren();
          FFunctionList.PluginsNode.DeleteChildren();

          for I := 0 to High(FParser.Includes) do
            FFunctionList.AddInclude(FParser.Includes[I]);

          FIncludesHash := FParser.IncludesHash;
        end;
      finally
        FParser.Free();
        FParser := nil;

        Synchronize(@EndUpdate);
      end;
    end;

    FEvent.ResetEvent();
  end;
end;

procedure TSimbaFunctionListUpdater.Update(AParser: TCodeInsight; AChangeStamp: Int64);
begin
  FParser := AParser;
  FChangeStamp := AChangeStamp;
  FEvent.SetEvent();
end;

end.

