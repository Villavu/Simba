{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dockinghelpers;

{$I simba.inc}

interface

uses
  classes, sysutils, controls, menus, forms, graphics,
  anchordocking, anchordockpanel;

type
  TSimbaAnchorDockHeader = class(TAnchorDockHeader)
  protected
    procedure ParentFontChanged; override;
    procedure Paint; override;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure SetAlign(Value: TAlign); override;
  public
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseLeave;  override;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAnchorDockHostSite = class(TAnchorDockHostSite)
  protected
    FMenuItem: TMenuItem;
    FNeedDefaultPosition: Boolean;

    procedure DoMenuItemDestroyed(Sender: TObject);
    procedure DoMenuItemClicked(Sender: TObject);

    function GetHeader: TSimbaAnchorDockHeader;

    procedure SetMenuItem(Value: TMenuItem);
    procedure SetVisible(Value: Boolean); override;
    procedure SetParent(Value: TWinControl); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;

    function ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): Boolean; override;

    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property NeedDefaultPosition: Boolean read FNeedDefaultPosition write FNeedDefaultPosition;
    property Header: TSimbaAnchorDockHeader read GetHeader;
  end;

  TSimbaAnchorDockSplitter = class(TAnchorDockSplitter)
  protected
  const
    GRIPPER_SIZE = 75;
  protected
    procedure Paint; override;
  end;

  TAnchorDockMasterHelper = class helper for TAnchorDockMaster
  private
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure MakeDockable(Form: TCustomForm; MenuItem: TMenuItem);
    procedure ManualDockPanel(SrcSite: TAnchorDockHostSite; TargetPanel: TAnchorDockPanel; Align: TAlign; TargetControl: TControl = nil);

    function SaveLayout: String;
    function LoadLayout(Layout: String): Boolean;
  end;

implementation

uses
  anchordockstorage, xmlpropstorage, lazloggerbase, lazconfigstorage,
  simba.theme, simba.fonthelpers;

procedure TSimbaAnchorDockHeader.ParentFontChanged;
begin
  inherited ParentFontChanged();

  if Assigned(Parent) and Assigned(Parent.Font) then
  begin
    Font.Size := GetFontSize(Parent, 2);
    Font.Color := SimbaTheme.ColorFont;
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

  PopupMenu := nil;

  CloseButton.Parent := nil;
  //MinimizeButton.Parent := nil;

  ParentFont := True;
  Color := SimbaTheme.ColorFrame;
end;

function TSimbaAnchorDockHostSite.GetHeader: TSimbaAnchorDockHeader;
begin
  Result := inherited Header as TSimbaAnchorDockHeader;
end;

procedure TSimbaAnchorDockHostSite.DoMenuItemDestroyed(Sender: TObject);
begin
  FMenuItem := nil;
end;

procedure TSimbaAnchorDockHostSite.DoMenuItemClicked(Sender: TObject);
begin
  Visible := TMenuItem(Sender).Checked;

  if Visible then
  begin
    if FNeedDefaultPosition then
    begin
      with Application.MainForm.Monitor.WorkareaRect.CenterPoint do
        BoundsRect := TRect.Create(X - (Width div 2), Y - (Height div 2), X + (Width div 2), Y + (Height div 2));

      FNeedDefaultPosition := False;
    end;

    EnsureVisible();
  end;
end;

procedure TSimbaAnchorDockHostSite.SetMenuItem(Value: TMenuItem);
begin
  FMenuItem := Value;
  FMenuItem.Checked := False;
  FMenuItem.AddHandlerOnDestroy(@DoMenuItemDestroyed);
  FMenuItem.OnClick := @DoMenuItemClicked;
end;

procedure TSimbaAnchorDockHostSite.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  if (MenuItem <> nil) then
    MenuItem.Checked := Value;

  if (Parent <> nil) then
    ShowInTaskBar := stNever
  else
    ShowInTaskBar := stAlways;
end;

procedure TSimbaAnchorDockHostSite.SetParent(Value: TWinControl);
begin
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

function TSimbaAnchorDockHostSite.ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): Boolean;
begin
  Result := inherited;
end;

procedure TSimbaAnchorDockSplitter.Paint;
begin
  Canvas.Brush.Color := SimbaTheme.ColorFrame;
  Canvas.FillRect(ClientRect);
end;

procedure TAnchorDockMasterHelper.MakeDockable(Form: TCustomForm; MenuItem: TMenuItem);
begin
  inherited MakeDockable(Form, False, False, True);

  if (Form.HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    Form.AddHandlerClose(@OnFormClose, True);

    TSimbaAnchorDockHostSite(Form.HostDockSite).MenuItem := MenuItem;
  end;
end;

procedure TAnchorDockMasterHelper.ManualDockPanel(SrcSite: TAnchorDockHostSite; TargetPanel: TAnchorDockPanel; Align: TAlign; TargetControl: TControl);
var
  Site: TSimbaAnchorDockHostSite;
  aManager: TAnchorDockManager;
  DockObject: TDragDockObject;
begin
  {$IFDEF VerboseAnchorDocking}
  debugln(['TAnchorDockMaster.ManualDock SrcSite=',DbgSName(SrcSite),' TargetPanel=',DbgSName(TargetPanel),' Align=',dbgs(Align),' TargetControl=',DbgSName(TargetControl)]);
  {$ENDIF}
  if SrcSite.IsParentOf(TargetPanel) then
    raise Exception.Create('TAnchorDockMaster.ManualDock SrcSite.IsParentOf(TargetSite)');
  if TargetPanel.IsParentOf(SrcSite) then
    raise Exception.Create('TAnchorDockMaster.ManualDock TargetSite.IsParentOf(SrcSite)');


  aManager:=TAnchorDockManager(TargetPanel.DockManager);
  Site:=aManager.GetChildSite as TSimbaAnchorDockHostSite;
  if Site=nil then begin
    // dock as first site into AnchorDockPanel
    {$IFDEF VerboseAnchorDocking}
    debugln(['TAnchorDockMaster.ManualDock dock as first site into AnchorDockPanel: SrcSite=',DbgSName(SrcSite),' TargetPanel=',DbgSName(TargetPanel),' Align=',dbgs(Align)]);
    {$ENDIF}
    BeginUpdate;
    try
      DockObject := TDragDockObject.Create(SrcSite);
      try
        DockObject.DropAlign:=alClient;
        DockObject.DockRect:=SrcSite.BoundsRect;
        DockObject.Control.Dock(TargetPanel, SrcSite.BoundsRect);
        aManager.InsertControl(DockObject);
      finally
        DockObject.Free;
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;

  if AutoFreedIfControlIsRemoved(Site,SrcSite) then
    raise Exception.Create('TAnchorDockMaster.ManualDock TargetPanel depends on SrcSite');
  BeginUpdate;
  try
    Site.ExecuteDock(SrcSite,TargetControl,Align);
  finally
    EndUpdate;
  end;
end;

function TAnchorDockMasterHelper.SaveLayout: String;
var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
  I: Integer;
begin
  Result := '';

  Stream := TStringStream.Create();
  Config := TXMLConfigStorage.Create('', False);

  try
    RestoreLayouts.Clear();
    for I := 0 to Screen.CustomFormCount-1 do
      if (Screen.CustomForms[I].HostDockSite is TSimbaAnchorDockHostSite) then
        if TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite).Floating then
          RestoreLayouts.Add(CreateRestoreLayout(Screen.CustomForms[I].HostDockSite), True);

    SaveLayoutToConfig(Config);

    Config.SaveToStream(Stream);

    Result := Stream.DataString;
  finally
    Stream.Free();
    Config.Free();
  end;
end;

procedure TAnchorDockMasterHelper.OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
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

function TAnchorDockMasterHelper.LoadLayout(Layout: String): Boolean;

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

        if R.IsEmpty() then
          Continue;

        TSimbaAnchorDockHostSite(Form.HostDockSite).NeedDefaultPosition := False;
        TSimbaAnchorDockHostSite(Form.HostDockSite).BoundsRect := R;
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

  Stream := TStringStream.Create(Layout);
  Config := TXMLConfigStorage.Create(Stream);

  try
    LoadRestoredBounds(Config);

    Result := LoadLayoutFromConfig(Config, True);
  finally
    Config.Free();
    Stream.Free();
  end;
end;

end.
