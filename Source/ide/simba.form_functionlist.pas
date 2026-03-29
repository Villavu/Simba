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

{.$DEFINE DEBUG}

uses
  AnchorDocking,
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

  SimbaEvents.Register(Self, @DoSimbaEvent);

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
  FUpdateThread.Terminate();
  FUpdateThread.WaitFor();
  while (FPendingRemoves.Count > 0) do
    FPendingRemoves.Pop.Free();

  FreeAndNil(FUpdateThread);
  FreeAndNil(FPendingRemoves);

  inherited Destroy();
end;

procedure TSimbaFunctionListForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
var
  Page: TSimbaFunctionListPage;
  Splitter: TAnchorDockSplitter;
begin
  case Event of
    ESimbaEvent.CODETOOLS_SETUP:
      begin
        FUpdateThread := RunInThread(@DoUpdateThread);
      end;

    ESimbaEvent.TAB_CHANGE:
      begin
        Page := PageForTab(TSimbaScriptTab(Data));
        if (Page <> nil) then
          Page.NeedUpdate := True;
        FNotebook.ActivePage := Page;
      end;

    ESimbaEvent.TAB_CLOSED:
      begin
        Page := PageForTab(TSimbaScriptTab(Data));
        if (Page <> nil) then
          FPendingRemoves.Add(Page);
      end;

    ESimbaEvent.TAB_ADD:
      begin
        Page := TSimbaFunctionListPage(FNotebook.AddPage());
        Page.TabID := TSimbaScriptTab(Data).UID;
        Page.NeedUpdate := True;
      end;

    ESimbaEvent.TAB_MODIFIED:
      begin
        Page := PageForTab(TSimbaScriptTab(Data));
        if (Page <> nil) then
          Page.NeedUpdate := True;
      end;

    ESimbaEvent.SPLITTER_DOUBLE_CLICK:
      begin
        if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akRight, Splitter) and (Splitter = TObject(Data))) then
          Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() - Width) + TSimbaFunctionListPage(FNotebook.ActivePage).TreeView.MaxRight)
        else if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akLeft, Splitter) and (Splitter = TObject(Data))) then
          Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() + Width) - TSimbaFunctionListPage(FNotebook.ActivePage).TreeView.MaxRight);
      end;
  end;
end;

{$R *.lfm}

end.
