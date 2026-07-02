{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_docking;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, AnchorDocking,
  simba.base,
  simba.ide_events,
  simba.component_tabcontrol;

type
  TSimbaDockedTab = class(TSimbaTab)
  protected
    FSite: TAnchorDockHostSite;
    FForm: TCustomForm;
  end;

  TSimbaAnchorDockHeader = class(TAnchorDockHeader)
  protected
    procedure ParentFontChanged; override;
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    // force top alignment
    procedure SetAlign(Value: TAlign); override;
  public
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseLeave; override;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAnchorDockHostSite = class(TAnchorDockHostSite)
  protected
    FNeedDefaultPosition: Boolean;
    FNeedRestore: Boolean;

    procedure DoShow; override;
    procedure DoHide; override;
    function GetHeader: TSimbaAnchorDockHeader;
    procedure SetVisible(Value: Boolean); override;
    procedure SetParent(Value: TWinControl); override;
    function ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): Boolean; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    property Header: TSimbaAnchorDockHeader read GetHeader;
  end;

  // page docking is disabled BUT we add support for docking into simba output tabs
  TSimbaAnchorDockManager = class(TAnchorDockManager)
  protected
    function IsOutputSite: Boolean;
  public
    function GetDockEdge(ADockObject: TDragDockObject): Boolean; override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect); override;
  end;

  TSimbaAnchorDockSplitter = class(TAnchorDockSplitter)
  protected
    procedure Paint; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TSimbaDocking = class(TComponent)
  private
    FControlsNotFound: TStringArray;
    FDragTab: TSimbaDockedTab;
    FPendingUndock: TSimbaDockedTab;
    FLayoutBuilding: Boolean;
    FHiddenDuringBuild: TList;

    procedure DoFormVisibleChanged(Sender: TObject; Form: TCustomForm);
    procedure BeginLayoutBuild;
    procedure EndLayoutBuild(ReshowHidden: Boolean);

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoCreateControl(Sender: TObject; aName: String; var AControl: TControl; DoDisableAutoSizing: Boolean);
    procedure DoRestore(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoMainFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoSiteClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoDefaultDocking;
    procedure DoOutputTabMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOutputTabMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoOutputTabMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoAsyncUndock(Data: PtrInt);
    procedure DoTabDragEnded(Sender, Target: TObject; X, Y: Integer);
    function DetachTabSite(Tab: TSimbaDockedTab): TAnchorDockHostSite;
    procedure UndockTab(Tab: TSimbaDockedTab);

    procedure RestoreDockedTabs(const Names: TStringArray; const Indices: TIntegerArray);

    procedure MakeDockable(Form: TCustomForm);
    procedure MakeVisible(Form: TCustomForm; Visible: Boolean);
    function Load(Layout: String): Boolean;
    function Save: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure SetLocked(Locked: Boolean);
    procedure Show(Form: TCustomForm);
    procedure Reset;

    // Dock a window into the output form as a new tab (called from ExecuteDock).
    function DockFormToOutputTab(Form: TCustomForm; Site: TAnchorDockHostSite): Boolean;
  end;

var
  SimbaDocking: TSimbaDocking;

implementation

uses
  Types, Graphics, XMLPropStorage, LazConfigStorage,
  simba.ide_debugimage,
  simba.dialog,
  simba.component_theme,
  simba.component_images,
  simba.misc,
  simba.settings,
  simba.initializations,
  simba.threading,
  simba.form_main,
  simba.form_colorpickhistory,
  simba.form_findinfiles,
  simba.form_filebrowser,
  simba.form_notes,
  simba.form_functionlist,
  simba.form_backups,
  simba.form_scripttabs,
  simba.form_output;

const
  SimbaLayoutVersion = 1; // bump when incompatible layouts so default is loaded

type
  TControlAccess = class(TControl);

procedure TSimbaAnchorDockHeader.ParentFontChanged;
begin
  inherited ParentFontChanged();

  if Assigned(Parent) and Assigned(Parent.Font) then
  begin
    Font.Size := GetFontSize(Parent, 1);
    Font.Color := SimbaComponentTheme.ColorFont;
  end;
end;

procedure TSimbaAnchorDockHeader.Paint;
var
  Style: TTextStyle;
begin
  Style := Canvas.TextStyle;
  Style.Layout := tlCenter;
  Style.Alignment := taCenter;

  Canvas.TextRect(ClientRect, 0, 0, Self.Caption, Style);
end;

procedure TSimbaAnchorDockHeader.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self, 1);

    PreferredHeight := Canvas.TextHeight('Tay');
  finally
    Free();
  end;
end;

procedure TSimbaAnchorDockHeader.SetAlign(Value: TAlign);
begin
  inherited SetAlign(alTop);
end;

procedure TSimbaAnchorDockHeader.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaAnchorDockHeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaAnchorDockHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TSimbaAnchorDockHeader.MouseLeave;
begin
  inherited MouseLeave();
end;

constructor TSimbaAnchorDockHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CloseButton.Parent := nil;
  MinimizeButton.Parent := nil;

  Color := SimbaComponentTheme.ColorFrame;
end;

function TSimbaAnchorDockHostSite.GetHeader: TSimbaAnchorDockHeader;
begin
  Result := inherited Header as TSimbaAnchorDockHeader;
end;

procedure TSimbaAnchorDockHostSite.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  if (Parent <> nil) then
    ShowInTaskBar := stNever
  else
    ShowInTaskBar := stAlways;
end;

procedure TSimbaAnchorDockHostSite.SetParent(Value: TWinControl);
begin
  if (SimbaEvents <> nil) then
    if (Value = nil) then
      SimbaEvents.Post(ESimbaEvent.FORM_UNDOCK, Self)
    else
      SimbaEvents.Post(ESimbaEvent.FORM_DOCK, Self);

  if (Value <> nil) then
    ShowInTaskBar := stNever
  else
    ShowInTaskBar := stAlways;

  inherited SetParent(Value);
end;

constructor TSimbaAnchorDockHostSite.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);

  FNeedDefaultPosition := True;
end;

procedure TSimbaAnchorDockHostSite.DoShow;
begin
  inherited DoShow();

  VisibleChanged();
end;

procedure TSimbaAnchorDockHostSite.DoHide;
begin
  inherited DoHide();

  VisibleChanged();
end;

// Search dock host sites to find actual form
function ResolveDockedForm(Ctrl: TControl): TCustomForm;

  function SiteOneControl(Site: TAnchorDockHostSite): TControl;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Site.ControlCount - 1 do
      if (Site.Controls[I].Owner <> Site) then
        Exit(Site.Controls[I]);
  end;

var
  Next: TControl;
begin
  Result := nil;
  while (Ctrl is TAnchorDockHostSite) and (TAnchorDockHostSite(Ctrl).SiteType = adhstOneControl) do
  begin
    Next := SiteOneControl(TAnchorDockHostSite(Ctrl));
    if (Next = nil) or (Next = Ctrl) then
      Exit(nil);
    Ctrl := Next;
  end;
  if (Ctrl is TCustomForm) and (not (Ctrl is TAnchorDockHostSite)) then
    Result := TCustomForm(Ctrl);
end;

function TSimbaAnchorDockHostSite.ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): Boolean;
var
  Form: TCustomForm;
begin
  // drop into output form = add as a tab in its tab control.
  if (DockAlign = alClient) and (SimbaOutputForm <> nil) and (SimbaOutputForm.HostDockSite = Self) then
  begin
    if (NewControl is TAnchorDockHostSite) and (SimbaDocking <> nil) then
    begin
      Form := ResolveDockedForm(NewControl);
      if (Form <> nil) and (Form <> SimbaOutputForm) then
        Exit(SimbaDocking.DockFormToOutputTab(Form, TAnchorDockHostSite(NewControl)));
    end;

    // Anything we can't turn into a single tab is rejected
    Exit(False);
  end;

  Result := inherited ExecuteDock(NewControl, DropOnControl, DockAlign);
end;

function TSimbaAnchorDockManager.IsOutputSite: Boolean;
begin
  Result := (SimbaOutputForm <> nil) and (DockSite <> nil) and (SimbaOutputForm.HostDockSite = DockSite);
end;

function TSimbaAnchorDockManager.GetDockEdge(ADockObject: TDragDockObject): Boolean;
begin
  if IsOutputSite() and (DockSite.Pages = nil) and (SimbaOutputForm <> nil) then
  begin
    // Dropping onto the tab strip will add as new tab
    if PtInRect(SimbaOutputForm.TabControl.TabStripScreenRect, ADockObject.DragPos) then
    begin
      ADockObject.DropAlign := alClient;
      ADockObject.DropOnControl := nil;
      Exit(True);
    end;

    // else fallback to default docking
    Result := inherited GetDockEdge(ADockObject);
    ADockObject.DropOnControl := nil;
    Exit;
  end;

  Result := inherited GetDockEdge(ADockObject);
end;

procedure TSimbaAnchorDockManager.PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect);
begin
  // Preview only the tab bar "strip"
  if (DropAlign = alClient) and IsOutputSite() and (DockSite.Pages = nil) and (SimbaOutputForm <> nil) then
  begin
    DockRect := SimbaOutputForm.TabControl.TabStripScreenRect;
    Exit;
  end;

  inherited PositionDockRect(Client, DropCtl, DropAlign, DockRect);
end;

procedure TSimbaAnchorDockSplitter.Paint;
begin
  Canvas.Brush.Color := SimbaComponentTheme.ColorFrame;
  Canvas.FillRect(ClientRect);

  if MouseInClient then
  begin
    Canvas.Brush.Color := SimbaComponentTheme.ColorActive;
    Canvas.FillRect(3, 3, Width-3, Height-3);
  end;
end;

procedure TSimbaAnchorDockSplitter.DblClick;
begin
  inherited DblClick();

  SimbaEvents.Post(ESimbaEvent.SPLITTER_DOUBLE_CLICK, Self);
end;

procedure TSimbaAnchorDockSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssDouble in Shift) then
    Exit;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaAnchorDockSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssDouble in Shift) then
    Exit;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaDocking.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoResetLayout;
  begin
    QueueOnMainThread(@Reset);
  end;

  procedure DoLockLayout(MenuItem: TMenuItem);
  begin
    SimbaSettings.General.LockLayout.Value := MenuItem.Checked;

    DockMaster.ShowHeader := not MenuItem.Checked;
    DockMaster.AllowDragging := not MenuItem.Checked;
  end;

  procedure DoInit;
  begin
    Application.AddOnRestoreHandler(@DoRestore);
    Application.AddOnMinimizeHandler(@DoMinimize);
    SimbaMainForm.AddHandlerClose(@DoMainFormClose);

    try
      DockMaster.BeginUpdate();
      DockMaster.SplitterWidth := SimbaMainForm.Scale96ToScreen(6);
      DockMaster.HeaderClass := TSimbaAnchorDockHeader;
      DockMaster.SplitterClass := TSimbaAnchorDockSplitter;
      DockMaster.SiteClass := TSimbaAnchorDockHostSite;
      DockMaster.ManagerClass := TSimbaAnchorDockManager;
      DockMaster.HideHeaderCaptionFloatingControl := False;
      DockMaster.HeaderAlignTop := $FFFFFF;
      DockMaster.PageAreaInPercent := 0; // disabled but TSimbaAnchorDockManager handles it for docking to output form
      DockMaster.HeaderHint := 'Use the mouse to drag and dock this window';
      DockMaster.MakeDockPanel(SimbaMainForm.DockPanel, admrpChild);
      DockMaster.DragTreshold := 40;

      MakeDockable(SimbaScriptTabsForm);
      MakeDockable(SimbaOutputForm);
      MakeDockable(SimbaFileBrowserForm);
      MakeDockable(SimbaFunctionListForm);
      MakeDockable(SimbaNotesForm);
      MakeDockable(SimbaDebugImageForm);
      MakeDockable(SimbaDebugMatrixForm);
      MakeDockable(SimbaColorPickHistoryForm);
      MakeDockable(SimbaBackupsForm);
      MakeDockable(SimbaFindInFilesForm);

      SimbaOutputForm.TabControl.OnMouseDown := @DoOutputTabMouseDown;
      SimbaOutputForm.TabControl.OnMouseMove := @DoOutputTabMouseMove;
      SimbaOutputForm.TabControl.OnMouseUp   := @DoOutputTabMouseUp;

      if (Length(FControlsNotFound) = 0) and Load(SimbaSettings.General.Layout.Value) then
      begin
        DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
        DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

        SimbaMainForm.ShowOnTop();
      end else
        QueueOnMainThread(@DoDefaultDocking);

      SetLocked(SimbaSettings.General.LockLayout.Value);
    finally
      DockMaster.EndUpdate();
    end;
  end;

begin
  case Event of
    ESimbaEvent.ACTION_LOCK_LAYOUT: DoLockLayout(TMenuItem(Data));
    ESimbaEvent.ACTION_RESET_LAYOUT: DoResetLayout();
    ESimbaEvent.SIMBA_SETUP_COMPLETED: DoInit();
  end;
end;

procedure TSimbaDocking.DoCreateControl(Sender: TObject; aName: String; var AControl: TControl; DoDisableAutoSizing: Boolean);
begin
  FControlsNotFound := FControlsNotFound + [aName];
  AControl := nil;
end;

procedure TSimbaDocking.DoRestore(Sender: TObject);
var
  I: Integer;
  Site: TSimbaAnchorDockHostSite;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Site := TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite);
    if (Site is TSimbaAnchorDockHostSite) and Site.FNeedRestore then
    begin
      DockMaster.MakeVisible(Screen.CustomForms[I], False);
      Site.FNeedRestore := False;
    end;
  end;
end;

procedure TSimbaDocking.DoMinimize(Sender: TObject);
var
  I: Integer;
  Site: TSimbaAnchorDockHostSite;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Site := TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite);
    if (Site is TSimbaAnchorDockHostSite) and Site.Floating and Site.Showing then
    begin
      Site.FNeedRestore := True;
      Site.CloseSite();
    end;
  end;
end;

procedure TSimbaDocking.DoMainFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (CloseAction = caFree) and (TForm(Sender).WindowState <> wsMinimized) then
    SimbaSettings.General.Layout.Value := Save();
end;

function TSimbaDocking.DockFormToOutputTab(Form: TCustomForm; Site: TAnchorDockHostSite): Boolean;
var
  Tab: TSimbaDockedTab;
begin
  if (Form = nil) or (Site = nil) or (SimbaOutputForm = nil) then
    Exit(False);

  Tab := TSimbaDockedTab(SimbaOutputForm.TabControl.AddTab(Form.Caption, TSimbaDockedTab));
  Tab.ImageIndex := SimbaImages.WINDOW;
  Tab.FForm := Form;
  Tab.FSite := Site;
  if (Site is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(Site).Header.Visible := False;

  Site.Parent := Tab;
  Site.Align := alClient;
  Site.Visible := True;

  DragManager.RegisterDockSite(Site, False);

  Tab.Show();

  Result := True;
end;

function TSimbaDocking.DetachTabSite(Tab: TSimbaDockedTab): TAnchorDockHostSite;
begin
  Result := Tab.FSite;
  Tab.FSite := nil;
  Tab.FForm := nil;

  if (Result is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(Result).Header.Visible := True;

  if (Result <> nil) then
  begin
    DragManager.RegisterDockSite(Result, True);
    DockMaster.ManualFloat(Result);
  end;
end;

procedure TSimbaDocking.UndockTab(Tab: TSimbaDockedTab);
var
  Site: TAnchorDockHostSite;
begin
  if (Tab = nil) or (SimbaOutputForm = nil) then
    Exit;

  Site := DetachTabSite(Tab);
  SimbaOutputForm.TabControl.DeleteTab(Tab);

  if (Site <> nil) then
  begin
    TControlAccess(TObject(Site)).OnEndDock := @DoTabDragEnded;
    Site.Visible := False;
    DragManager.DragStart(Site, True, 0);
  end;
end;

procedure TSimbaDocking.DoTabDragEnded(Sender, Target: TObject; X, Y: Integer);
begin
  if (Sender is TControl) then
  begin
    TControlAccess(Sender).OnEndDock := nil;
    TControl(Sender).Visible := True;
  end;
end;

procedure TSimbaDocking.DoAsyncUndock(Data: PtrInt);
var
  Tab: TSimbaDockedTab;
begin
  Tab := FPendingUndock;
  FPendingUndock := nil;
  if (Tab <> nil) then
    UndockTab(Tab);
end;

procedure TSimbaDocking.DoOutputTabMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabControl: TSimbaTabControl;
  Host: TWinControl;
  Tab: TSimbaTab;
begin
  FDragTab := nil;
  if (SimbaOutputForm = nil) then
    Exit;

  TabControl := SimbaOutputForm.TabControl;
  Host := SimbaOutputForm.HostDockSite;

  if TabControl.InEmptySpace(X, Y) and (not TabControl.Dragging) and (Host is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(Host).Header.MouseDown(Button, Shift, X, Y)
  else if (Button = mbLeft) then
  begin
    Tab := TabControl.GetTabAt(X, Y);
    if (Tab is TSimbaDockedTab) then
      FDragTab := TSimbaDockedTab(Tab);
  end;
end;

procedure TSimbaDocking.DoOutputTabMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TabControl: TSimbaTabControl;
  Host: TWinControl;
  Target: TSimbaTab;
  FromIndex, ToIndex: Integer;
begin
  if (SimbaOutputForm = nil) then
    Exit;

  TabControl := SimbaOutputForm.TabControl;

  if (FDragTab <> nil) and (ssLeft in Shift) then
  begin
    // undock
    if (not TabControl.InTabStrip(X, Y)) then
    begin
      FPendingUndock := FDragTab;
      FDragTab := nil;
      Application.QueueAsyncCall(@DoAsyncUndock, 0);
    end else
    begin
      // rearrange
      Target := TabControl.GetTabAt(X, Y);
      if (Target <> nil) and (Target <> FDragTab) then
      begin
        FromIndex := TabControl.IndexOfTab(FDragTab);
        ToIndex := TabControl.IndexOfTab(Target);
        if (FromIndex >= 0) and (ToIndex >= 0) then
          TabControl.MoveTab(FromIndex, ToIndex);
      end;
    end;
    Exit;
  end;

  Host := SimbaOutputForm.HostDockSite;

  if TabControl.InEmptySpace(X, Y) and (not TabControl.Dragging) and (Host is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(Host).Header.MouseMove(Shift, X, Y);
end;

procedure TSimbaDocking.DoOutputTabMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragTab := nil;
end;

procedure TSimbaDocking.RestoreDockedTabs(const Names: TStringArray; const Indices: TIntegerArray);
var
  I, FromIndex, ToIndex: Integer;
  Form: TCustomForm;
  Site: TAnchorDockHostSite;
begin
  for I := 0 to High(Names) do
  begin
    if (Names[I] = '') then
      Continue;

    Form := Screen.FindForm(Names[I]);
    if (Form = nil) or (Form = SimbaOutputForm) then
      Continue;

    Site := DockMaster.GetAnchorSite(Form);
    if (Site = nil) or (not DockFormToOutputTab(Form, Site)) then
      Continue;

    // DockFormAsTab appends the tab; move it to its saved position.
    FromIndex := SimbaOutputForm.TabControl.TabCount - 1;
    ToIndex := Indices[I];
    if (ToIndex < 0) or (ToIndex > FromIndex) then
      ToIndex := FromIndex;
    if (ToIndex <> FromIndex) then
      SimbaOutputForm.TabControl.MoveTab(FromIndex, ToIndex);
  end;
end;

procedure TSimbaDocking.DoSiteClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Form: TCustomForm;
begin
  CloseAction := caHide;

  if (Sender is TCustomForm) then
  begin
    Form := TCustomForm(Sender);
    if (Form.HostDockSite is TSimbaAnchorDockHostSite) then
    begin
      with TSimbaAnchorDockHostSite(Form.HostDockSite) do
        CloseSite();
      CloseAction := caNone;
    end;
  end;
end;

procedure TSimbaDocking.DoFormVisibleChanged(Sender: TObject; Form: TCustomForm);
begin
  // While (re)building a layout try to ensure nothing is shown for a cleaner apperance
  if FLayoutBuilding and Form.Visible and (Form is TAnchorDockHostSite) and (Form.Parent = nil) then
  begin
    if (FHiddenDuringBuild.IndexOf(Form) < 0) then
      FHiddenDuringBuild.Add(Form);
    Form.Visible := False;
  end;
end;

procedure TSimbaDocking.BeginLayoutBuild;
begin
  FHiddenDuringBuild.Clear();
  FLayoutBuilding := True;
end;

procedure TSimbaDocking.EndLayoutBuild(ReshowHidden: Boolean);

  function StillExists(Form: TCustomForm): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Screen.CustomFormCount - 1 do
      if (Screen.CustomForms[I] = Form) then
        Exit(True);
  end;

var
  I: Integer;
  Form: TCustomForm;
begin
  FLayoutBuilding := False;

  // Show now that they are in their final position
  if ReshowHidden then
    for I := 0 to FHiddenDuringBuild.Count - 1 do
    begin
      Form := TCustomForm(FHiddenDuringBuild[I]);
      if StillExists(Form) then
        Form.Visible := True;
    end;

  FHiddenDuringBuild.Clear();
end;

{
  |-------------------------------------|
  |              |        |             |
  | functionlist | editor | filebrowser |
  |              |--------|             |
  |              | output |             |
  |-------------------------------------|
}
procedure TSimbaDocking.DoDefaultDocking;

  function Site(Form: TCustomForm): TSimbaAnchorDockHostSite;
  begin
    Result := TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(Form));
  end;

  procedure SetSplitter(Side: TAnchorKind; Position: Integer);
  var
    Splitter: TAnchorDockSplitter;
  begin
    if GetDockSplitter(Site(SimbaScriptTabsForm), Side, Splitter) then
      Splitter.SetSplitterPosition(Position);
  end;

var
  I: Integer;
begin
  BeginLayoutBuild();
  try
    SimbaMainForm.Hide();
    SimbaMainForm.WindowState := wsNormal;

    // Remove docked tabs
    for I := SimbaOutputForm.TabControl.TabCount - 1 downto 0 do
      if (SimbaOutputForm.TabControl[I] is TSimbaDockedTab) then
      begin
        DetachTabSite(TSimbaDockedTab(SimbaOutputForm.TabControl[I]));
        SimbaOutputForm.TabControl.DeleteTab(SimbaOutputForm.TabControl[I]);
      end;

    // Undock everything back to a (hidden) floating state so the layout is rebuilt from scratch.
    for I := 0 to Screen.CustomFormCount - 1 do
      if (Screen.CustomForms[I].HostDockSite is TCustomForm) and (Site(Screen.CustomForms[I]) <> nil) then
      begin
        Site(Screen.CustomForms[I]).Visible := False;
        DockMaster.ManualFloat(Screen.CustomForms[I]);
        Site(Screen.CustomForms[I]).Header.Visible := True;
      end;

    DockMaster.ManualDock(Site(SimbaScriptTabsForm),   SimbaMainForm.DockPanel, alClient);
    DockMaster.ManualDock(Site(SimbaOutputForm),       SimbaMainForm.DockPanel, alBottom);
    DockMaster.ManualDock(Site(SimbaFunctionListForm), SimbaMainForm.DockPanel, alLeft);
    DockMaster.ManualDock(Site(SimbaFileBrowserForm),  SimbaMainForm.DockPanel, alRight);

    DockMaster.MakeVisible(SimbaScriptTabsForm, False);
    DockMaster.MakeVisible(SimbaOutputForm, False);
    DockMaster.MakeVisible(SimbaFunctionListForm, False);
    DockMaster.MakeVisible(SimbaFileBrowserForm, False);

    DockMaster.ScaleOnResize := False;
    SimbaMainForm.Width := 1000;
    SimbaMainForm.Height := 800;
    SetSplitter(akLeft, 200);
    SetSplitter(akRight, 800);
    SetSplitter(akBottom, 450);
    DockMaster.ScaleOnResize := True;

    Site(SimbaScriptTabsForm).Header.Visible := False;
    Site(SimbaOutputForm).Header.Visible := False;
  finally
    EndLayoutBuild(False);
  end;

  SimbaMainForm.WindowState := wsMaximized;
  SimbaMainForm.ShowOnTop();
end;

procedure TSimbaDocking.MakeDockable(Form: TCustomForm);
begin
  Form.Name := Form.ClassName;

  DockMaster.MakeDockable(Form, False, False, True);
  if (Form.HostDockSite is TSimbaAnchorDockHostSite) then
    Form.AddHandlerClose(@DoSiteClose, True);
end;

procedure TSimbaDocking.MakeVisible(Form: TCustomForm; Visible: Boolean);
var
  Site: TSimbaAnchorDockHostSite;
  Center: TPoint;
begin
  if (Form.HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    Site := TSimbaAnchorDockHostSite(Form.HostDockSite);

    if Visible then
    begin
      if Site.FNeedDefaultPosition then
      begin
        Center := Application.MainForm.Monitor.WorkareaRect.CenterPoint;
        Site.BoundsRect := Rect(
            Center.X - (Site.Width div 2), Center.Y - (Site.Height div 2),
            Center.X + (Site.Width div 2), Center.Y + (Site.Height div 2)
          );
        Site.FNeedDefaultPosition := False;
      end;
      Site.EnsureVisible();
    end else
      Site.CloseSite();
  end;
end;

function TSimbaDocking.Load(Layout: String): Boolean;

  procedure LoadRestoredBounds(Config: TConfigStorage);
  var
    I: Integer;
    R: TRect;
    Form: TCustomForm;
  begin
    Config.AppendBasePath('Restores');

    for I := 1 to Config.GetValue('Count', 0) do
    begin
      Config.AppendBasePath('Item' + IntToStr(I) + '/');

      Form := Screen.FindForm(Config.GetValue('Names', ''));
      if (Form <> nil) and (Form.HostDockSite is TSimbaAnchorDockHostSite) then
      begin
        Config.AppendBasePath('Nodes/Bounds/');

        R.Left := Config.GetValue('Left', 0);
        R.Top := Config.GetValue('Top', 0);
        R.Width := Config.GetValue('Width', 0);
        R.Height := Config.GetValue('Height', 0);

        Config.UndoAppendBasePath();

        if not R.IsEmpty() then
        begin
          TSimbaAnchorDockHostSite(Form.HostDockSite).FNeedDefaultPosition := False;
          TSimbaAnchorDockHostSite(Form.HostDockSite).BoundsRect := R;
        end;
      end;

      Config.UndoAppendBasePath();
    end;

    Config.UndoAppendBasePath();
  end;

  procedure LoadRestoredTab(Config: TConfigStorage; out Names: TStringArray; out Indices: TIntegerArray);
  var
    I: Integer;
  begin
    Names := [];
    Indices := [];
    Config.AppendBasePath('SimbaOutputTabs/');
    for I := 1 to Config.GetValue('Count', 0) do
    begin
      Names += [Config.GetValue('Item' + IntToStr(I) + '/Name', '')];
      Indices += [Config.GetValue('Item' + IntToStr(I) + '/Index', -1)];
    end;
    Config.UndoAppendBasePath();
  end;

var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
  Names: TStringArray;
  Indices: TIntegerArray;
begin
  Result := False;

  if (Layout <> '') then
  begin
    Stream := TStringStream.Create(Layout);
    Config := TXMLConfigStorage.Create(Stream);

    BeginLayoutBuild();
    try
      try
        if (Config.GetValue('SimbaLayoutVersion', 0) = SimbaLayoutVersion) then
        begin
          LoadRestoredBounds(Config);
          Result := DockMaster.LoadLayoutFromConfig(Config, True);
          if Result then
          begin
            LoadRestoredTab(Config, Names, Indices);
            RestoreDockedTabs(Names, Indices);
          end;
        end;
      except
        // A corrupt or incompatible saved layout - fall back to the default layout.
        on E: Exception do
        begin
          DebugLn('Failed to restore docking layout: ' + E.Message);
          Result := False;
        end;
      end;
    finally
      EndLayoutBuild(True);
      Config.Free();
      Stream.Free();
    end;
  end;
end;

function TSimbaDocking.Save: String;

  procedure SaveOutputTabs(Config: TConfigStorage);
  var
    Names: TStringArray;
    Indices: TIntegerArray;
    I: Integer;
  begin
    Names := [];
    Indices := [];
    for I := 0 to SimbaOutputForm.TabControl.TabCount - 1 do
      if (SimbaOutputForm.TabControl[I] is TSimbaDockedTab) and (TSimbaDockedTab(SimbaOutputForm.TabControl[I]).FForm <> nil) then
      begin
        Indices += [I];
        Names += [TSimbaDockedTab(SimbaOutputForm.TabControl[I]).FForm.Name];
      end;

    Config.AppendBasePath('SimbaOutputTabs/');
    Config.SetDeleteValue('Count', Length(Names), 0);
    for I := 0 to High(Names) do
    begin
      Config.SetValue('Item' + IntToStr(I + 1) + '/Name', Names[I]);
      Config.SetValue('Item' + IntToStr(I + 1) + '/Index', Indices[I]);
    end;
    Config.UndoAppendBasePath();
  end;

var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
  I: Integer;
begin
  Result := '';

  Stream := TStringStream.Create();
  Config := TXMLConfigStorage.Create('', False);

  try
    DockMaster.RestoreLayouts.Clear();
    for I := 0 to Screen.CustomFormCount - 1 do
      if Screen.CustomForms[I].Showing and (Screen.CustomForms[I].HostDockSite is TSimbaAnchorDockHostSite) and TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite).Floating then
        DockMaster.RestoreLayouts.Add(DockMaster.CreateRestoreLayout(Screen.CustomForms[I].HostDockSite), True);
    DockMaster.SaveLayoutToConfig(Config);
    SaveOutputTabs(Config);
    Config.SetValue('SimbaLayoutVersion', SimbaLayoutVersion);

    Config.SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free();
    Config.Free();
  end;
end;

constructor TSimbaDocking.Create;
begin
  inherited Create(nil);

  FHiddenDuringBuild := TList.Create();

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_LOCK_LAYOUT,
    ESimbaEvent.ACTION_RESET_LAYOUT,
    ESimbaEvent.SIMBA_SETUP_COMPLETED
  ]);

  Screen.AddHandlerFormVisibleChanged(@DoFormVisibleChanged);
  DockMaster.OnCreateControl := @DoCreateControl;
end;

destructor TSimbaDocking.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  Screen.RemoveHandlerFormVisibleChanged(@DoFormVisibleChanged);
  FreeAndNil(FHiddenDuringBuild);

  inherited Destroy();
end;

procedure TSimbaDocking.SetLocked(Locked: Boolean);
begin
  DockMaster.ShowHeader := not Locked;
  DockMaster.AllowDragging := not Locked;
end;

procedure TSimbaDocking.Show(Form: TCustomForm);
begin
  MakeVisible(Form, True);
end;

procedure TSimbaDocking.Reset;
begin
  if (ShowQuestionDialog('Layout', 'Reset to default layout?', []) = ESimbaDialogButton.YES) then
  begin
    SimbaSettings.General.Layout.Value := '';
    SimbaSettings.General.LockLayout.Value := False;

    DoDefaultDocking();
  end;
end;

procedure DoCreate;
begin
  SimbaDocking := TSimbaDocking.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaDocking);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'SimbaDocking', 5);
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaDocking', -5);

end.
