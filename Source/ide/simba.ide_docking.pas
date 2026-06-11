{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_docking;

{$I simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, AnchorDocking,
  simba.base,
  simba.ide_events;

type
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
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    property Header: TSimbaAnchorDockHeader read GetHeader;
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

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoCreateControl(Sender: TObject; aName: String; var AControl: TControl; DoDisableAutoSizing: Boolean);
    procedure DoRestore(Sender: TObject);
    procedure DoMinimize(Sender: TObject);
    procedure DoMainFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoSiteClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoDefaultDocking;

    procedure MakeDockable(Form: TCustomForm);
    procedure MakeVisible(Form: TCustomForm; Visible: Boolean);
    function Load(Layout: String): Boolean;
    function Save: String;
  public
    constructor Create; reintroduce;

    procedure SetLocked(Locked: Boolean);
    procedure Show(Form: TCustomForm);
    procedure Reset;
  end;

var
  SimbaDocking: TSimbaDocking;

implementation

uses
  Menus, Graphics, XMLPropStorage, LazConfigStorage,
  simba.ide_debugimage,
  simba.dialog,
  simba.component_theme,
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
      DockMaster.HideHeaderCaptionFloatingControl := False;
      DockMaster.HeaderAlignTop := $FFFFFF;
      DockMaster.PageAreaInPercent := 0;
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

      if Load(SimbaSettings.General.Layout.Value) and (Length(FControlsNotFound) = 0) then
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

{
  |-------------------------------------|
  |              |        |             |
  | functionlist | editor | filebrowser |
  |              |--------|             |
  |              | output |             |
  |-------------------------------------|
}
procedure TSimbaDocking.DoDefaultDocking;
var
  Splitter: TAnchorDockSplitter;
  I: Integer;
begin
  // Reset everything
  SimbaMainForm.Hide();
  SimbaMainForm.WindowState := wsNormal;

  for I := 0 to Screen.CustomFormCount - 1 do
    if (Screen.CustomForms[I].HostDockSite is TCustomForm) then
    begin
      if (DockMaster.GetAnchorSite(Screen.CustomForms[I]) <> nil) then
        DockMaster.GetAnchorSite(Screen.CustomForms[I]).Visible := False;
      DockMaster.ManualFloat(Screen.CustomForms[I]);
      if (DockMaster.GetAnchorSite(Screen.CustomForms[I]) <> nil) then
        DockMaster.GetAnchorSite(Screen.CustomForms[I]).Header.Visible := True;
    end;

  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaScriptTabsForm), SimbaMainForm.DockPanel, alClient);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaOutputForm), SimbaMainForm.DockPanel, alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFunctionListForm), SimbaMainForm.DockPanel, alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFileBrowserForm), SimbaMainForm.DockPanel, alRight);

  DockMaster.MakeVisible(SimbaScriptTabsForm, False);
  DockMaster.MakeVisible(SimbaOutputForm, False);
  DockMaster.MakeVisible(SimbaFunctionListForm, False);
  DockMaster.MakeVisible(SimbaFileBrowserForm, False);
  DockMaster.ScaleOnResize := False;

  SimbaMainForm.Width := 1000;
  SimbaMainForm.Height := 800;

  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akLeft, Splitter) then
    Splitter.SetSplitterPosition(200);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akRight, Splitter) then
    Splitter.SetSplitterPosition(800);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akBottom, Splitter) then
    Splitter.SetSplitterPosition(450);

  DockMaster.ScaleOnResize := True;
  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
  DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

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

var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
begin
  Result := False;

  if (Layout <> '') then
  begin
    Stream := TStringStream.Create(Layout);
    Config := TXMLConfigStorage.Create(Stream);
    try
      LoadRestoredBounds(Config);

      Result := DockMaster.LoadLayoutFromConfig(Config, True);
    finally
      Config.Free();
      Stream.Free();
    end;
  end;
end;

function TSimbaDocking.Save: String;
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

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_LOCK_LAYOUT,
    ESimbaEvent.ACTION_RESET_LAYOUT,
    ESimbaEvent.SIMBA_SETUP_COMPLETED
  ]);

  DockMaster.OnCreateControl := @DoCreateControl;
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
