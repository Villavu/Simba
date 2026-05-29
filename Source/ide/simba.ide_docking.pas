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
  simba.base;

const
  SIMBA_DOCKING_VERSION = 1; // update if saved layout will become invalid (think changing form names)

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

  TAnchorDockMasterHelper = class helper for TAnchorDockMaster
  private
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetVisible(Form: TCustomForm; Visible: Boolean);
  public
    procedure MakeDockable(Form: TCustomForm);

    procedure Minimized;
    procedure Restored;

    function SaveLayout: String;
    function LoadLayout(Layout: String): Boolean;

    procedure Show(Form: TCustomForm);
  end;

implementation

uses
  Graphics, XMLPropStorage, LazConfigStorage,
  simba.ide_events,
  simba.component_theme,
  simba.misc;

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

procedure TAnchorDockMasterHelper.MakeDockable(Form: TCustomForm);
begin
  inherited MakeDockable(Form, False, False, True);

  Form.Name := Form.ClassName;
  if (Form.HostDockSite is TSimbaAnchorDockHostSite) then
    Form.AddHandlerClose(@OnFormClose, True);
end;

procedure TAnchorDockMasterHelper.Minimized;
var
  I: Integer;
  Site: TSimbaAnchorDockHostSite;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Site := TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite);
    if Screen.CustomForms[I].Showing and (Site is TSimbaAnchorDockHostSite) and Site.Floating then
    begin
      Site.FNeedRestore := True;
      Site.CloseSite();
    end;
  end;
end;

procedure TAnchorDockMasterHelper.Restored;
var
  I: Integer;
  Site: TSimbaAnchorDockHostSite;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Site := TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite);
    if (Site is TSimbaAnchorDockHostSite) and Site.FNeedRestore then
    begin
      inherited MakeVisible(Screen.CustomForms[I], False);
      Site.FNeedRestore := False;
    end;
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
    for I := 0 to Screen.CustomFormCount - 1 do
      if Screen.CustomForms[I].Showing and (Screen.CustomForms[I].HostDockSite is TSimbaAnchorDockHostSite) and TSimbaAnchorDockHostSite(Screen.CustomForms[I].HostDockSite).Floating then
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

procedure TAnchorDockMasterHelper.SetVisible(Form: TCustomForm; Visible: Boolean);
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

procedure TAnchorDockMasterHelper.Show(Form: TCustomForm);
begin
  SetVisible(Form, True);
end;

end.
