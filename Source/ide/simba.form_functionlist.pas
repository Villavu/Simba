{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_functionlist;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  simba.base,
  simba.containers,
  simba.component_notebook,
  simba.functionlist_page,
  simba.ide_events,
  simba.ide_tab;

type
  TSimbaFunctionListForm = class(TForm)
  protected type
    TFunctionListPageList = specialize TSimbaList<TSimbaFunctionListPage>;
  protected
    FUpdateThread: TThread;
    FIsIdle: Boolean;
    FNotebook: TSimbaNotebook;
    FPendingRemoves: TFunctionListPageList;

    function PageForTab(Tab: TSimbaScriptTab): TSimbaFunctionListPage;

    procedure DoUpdateThread;
    procedure DoIdleBegin(Sender: TObject);
    procedure DoIdleEnd(Sender: TObject);

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

uses
  AnchorDocking, Menus,
  simba.initializations,
  simba.ide_docking,
  simba.threading;

function TSimbaFunctionListForm.PageForTab(Tab: TSimbaScriptTab): TSimbaFunctionListPage;
var
  I: Integer;
begin
  for I := 0 to FNotebook.PageCount - 1 do
    if (TSimbaFunctionListPage(FNotebook.Page[I]).TabID = Tab.UID) then
      Exit(TSimbaFunctionListPage(FNotebook.Page[I]));

  Result := nil;
end;

procedure TSimbaFunctionListForm.DoUpdateThread;
begin
  try
    while not TThread.CurrentThread.CheckTerminated do
    begin
      if FIsIdle then
      begin
        if (FNotebook.ActivePage <> nil) then
          TSimbaFunctionListPage(FNotebook.ActivePage).Fill();

        while (FPendingRemoves.Count > 0) do
          RunInMainThread(@FPendingRemoves.Pop.Free);
      end;

      Sleep(350);
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaFunctionListForm.DoUpdateThread]: ' + E.Message);
  end;
end;

procedure TSimbaFunctionListForm.DoIdleBegin(Sender: TObject);
begin
  FIsIdle := True;
end;

procedure TSimbaFunctionListForm.DoIdleEnd(Sender: TObject);
begin
  FIsIdle := False;
end;

constructor TSimbaFunctionListForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name := 'SimbaFunctionListForm'; // important - docking requires control names
  Caption := 'Function List';

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_VIEW_FUNCTIONLIST,
    ESimbaEvent.CODETOOLS_SETUP,
    ESimbaEvent.TAB_CHANGE,
    ESimbaEvent.TAB_CLOSED,
    ESimbaEvent.TAB_ADD,
    ESimbaEvent.TAB_MODIFIED,
    ESimbaEvent.SPLITTER_DOUBLE_CLICK
  ]);

  with TIdleTimer.Create(Self) do
  begin
    AutoEnabled := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent := itaOnUserInput;
    Interval := 350;
    OnTimer := @DoIdleBegin;
    OnStopTimer := @DoIdleEnd;
  end;

  FNotebook := TSimbaNotebook.Create(Self, TSimbaFunctionListPage);
  FNotebook.Parent := Self;
  FNotebook.Align := alClient;

  FPendingRemoves := TFunctionListPageList.Create();
end;

destructor TSimbaFunctionListForm.Destroy;
begin
  if (FUpdateThread <> nil) then
  begin
    FUpdateThread.Terminate();
    FUpdateThread.WaitFor();
    FreeAndNil(FUpdateThread);
  end;

  while (FPendingRemoves.Count > 0) do
    FPendingRemoves.Pop.Free();
  FreeAndNil(FPendingRemoves);

  inherited Destroy();
end;

procedure TSimbaFunctionListForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewFunctionList(Item: TMenuItem);
  begin
    DockMaster.Show(Self);
  end;

  procedure DoCodetoolsSetup;
  begin
    FUpdateThread := RunInThread(@DoUpdateThread);
  end;

  procedure DoTabChange(Tab: TSimbaScriptTab);
  var
    Page: TSimbaFunctionListPage;
  begin
    Page := PageForTab(Tab);
    if (Page <> nil) then
      Page.NeedUpdate := True;
    FNotebook.ActivePage := Page;
  end;

  procedure DoTabClosed(Tab: TSimbaScriptTab);
  var
    Page: TSimbaFunctionListPage;
  begin
    Page := PageForTab(Tab);
    if (Page <> nil) then
      FPendingRemoves.Add(Page);
  end;

  procedure DoTabAdd(Tab: TSimbaScriptTab);
  var
    Page: TSimbaFunctionListPage;
  begin
    Page := TSimbaFunctionListPage(FNotebook.AddPage());
    Page.TabID := Tab.UID;
    Page.NeedUpdate := True;
  end;

  procedure DoTabModified(Tab: TSimbaScriptTab);
  var
    Page: TSimbaFunctionListPage;
  begin
    Page := PageForTab(Tab);
    if (Page <> nil) then
      Page.NeedUpdate := True;
  end;

  procedure DoSplitterDoubleClick;
  var
    Splitter: TAnchorDockSplitter;
  begin
    if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akRight, Splitter) and (Splitter = TObject(Data))) then
      Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() - Width) + TSimbaFunctionListPage(FNotebook.ActivePage).TreeView.MaxRight)
    else if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akLeft, Splitter) and (Splitter = TObject(Data))) then
      Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() + Width) - TSimbaFunctionListPage(FNotebook.ActivePage).TreeView.MaxRight);
  end;

begin
  case Event of
    ESimbaEvent.ACTION_VIEW_FUNCTIONLIST: DoViewFunctionList(TMenuItem(Data));
    ESimbaEvent.CODETOOLS_SETUP:          DoCodetoolsSetup();
    ESimbaEvent.TAB_CHANGE:               DoTabChange(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CLOSED:               DoTabClosed(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_ADD:                  DoTabAdd(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_MODIFIED:             DoTabModified(TSimbaScriptTab(Data));
    ESimbaEvent.SPLITTER_DOUBLE_CLICK:    DoSplitterDoubleClick();
  end;
end;

procedure DoCreate;
begin
  SimbaFunctionListForm := TSimbaFunctionListForm.Create(nil);
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaFunctionListForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaFunctionListForm', 5);
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaFunctionListForm');

end.
