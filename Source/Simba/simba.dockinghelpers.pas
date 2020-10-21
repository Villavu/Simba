unit simba.dockinghelpers;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, forms;

type
  SimbaDockingHelper = class
    class procedure Resize(Form: TCustomForm; Width, Height: Int32);
    class procedure EnsureVisible(Form: TCustomForm);
    class function SaveLayoutToString: String;
    class function LoadLayoutFromString(Layout: String): Boolean;
    class procedure Show(Form: TCustomForm);
    class procedure ShowOnTop(Form: TCustomForm);
    class procedure Hide(Form: TCustomForm);
    class procedure Center(Form: TCustomForm);
    class function IsVisible(Form: TCustomForm): Boolean;
  end;

implementation

uses
  anchordocking, anchordockstorage, xmlpropstorage;

type
  TCustomFormHelper = class helper for TCustomForm
    procedure MoveToDefaultPosition;
  end;

procedure TCustomFormHelper.MoveToDefaultPosition;
begin
  inherited MoveToDefaultPosition;
end;

class procedure SimbaDockingHelper.Resize(Form: TCustomForm; Width, Height: Int32);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
  begin
    Form := DockMaster.GetAnchorSite(Form);
    if TAnchorDockHostSite(Form).Header <> nil then
      Height := Height + TAnchorDockHostSite(Form).Header.Height;
  end;

  Form.Width := Width;
  Form.Height := Height;
end;

class procedure SimbaDockingHelper.EnsureVisible(Form: TCustomForm);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
    Form := DockMaster.GetAnchorSite(Form);

  if (not Form.Visible) or (Form.Left < Form.Monitor.Left) or (Form.Top < Form.Monitor.Top) then
  begin
    if (Form.Position <> poScreenCenter) then
      Form.Position := poScreenCenter;

    Form.MoveToDefaultPosition();
  end;

  if (Form.Left < Form.Monitor.Left) then
    Form.Left := 0;
  if (Form.Top < Form.Monitor.Top) then
    Form.Top := 0;
end;

class function SimbaDockingHelper.SaveLayoutToString: String;
var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create();
  Config := TXMLConfigStorage.Create('', False);

  DockMaster.SaveLayoutToConfig(Config);

  Config.SaveToStream(Stream);

  Result := Stream.DataString;

  Stream.Free();
  Config.Free();
end;

class function SimbaDockingHelper.LoadLayoutFromString(Layout: String): Boolean;
var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
begin
  Result := False;

  Stream := TStringStream.Create(Layout);
  Config := TXMLConfigStorage.Create(Stream);

  try
    Result := DockMaster.LoadLayoutFromConfig(Config, True);
  finally
    Config.Free();
    Stream.Free();
  end;
end;

class procedure SimbaDockingHelper.Show(Form: TCustomForm);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
    Form := DockMaster.GetAnchorSite(Form);

  if not Form.Visible then
    Form.ShowOnTop();
end;

class procedure SimbaDockingHelper.ShowOnTop(Form: TCustomForm);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
    Form := DockMaster.GetAnchorSite(Form);

  Form.ShowOnTop();
end;

class procedure SimbaDockingHelper.Hide(Form: TCustomForm);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
  begin
    Form := DockMaster.GetAnchorSite(Form);

    DockMaster.RestoreLayouts.Add(DockMaster.CreateRestoreLayout(Form), True);
    with Form as TAnchorDockHostSite do
      CloseSite();
  end else
    Form.Hide();
end;

class procedure SimbaDockingHelper.Center(Form: TCustomForm);
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
    Form := DockMaster.GetAnchorSite(Form);

  Form.Position := poScreenCenter;
  Form.MoveToDefaultPosition();
  Form.ShowOnTop();
end;

class function SimbaDockingHelper.IsVisible(Form: TCustomForm): Boolean;
begin
  if DockMaster.GetAnchorSite(Form) <> nil then
    Form := DockMaster.GetAnchorSite(Form);

  Result := Form.Visible;
end;

end.


