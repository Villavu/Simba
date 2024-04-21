{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_debugimage;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls,
  simba.component_imagebox;

type
  TSimbaDebugImageForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  protected
    FImageBox: TSimbaImageBox;
    FMaxWidth, FMaxHeight: Integer;

    procedure DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
  public
    procedure Close;

    procedure SetMaxSize(AWidth, AHeight: Integer);
    procedure SetSize(AWidth, AHeight: Integer; AForce: Boolean; AEnsureVisible: Boolean = True);

    property ImageBox: TSimbaImageBox read FImageBox;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

{$R *.lfm}

uses
  simba.base, simba.ide_dockinghelpers;

procedure TSimbaDebugImageForm.Close;
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  Form.Close();
end;

procedure TSimbaDebugImageForm.FormCreate(Sender: TObject);
begin
  FMaxWidth := 1500;
  FMaxHeight := 1000;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnImgDoubleClick := @DoImgDoubleClick;
end;

procedure TSimbaDebugImageForm.DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
begin
  DebugLn([EDebugLn.FOCUS], 'Debug Image Click: (%d, %d)', [X, Y]);
end;

procedure TSimbaDebugImageForm.SetSize(AWidth, AHeight: Integer; AForce: Boolean; AEnsureVisible: Boolean);
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  if (Form is TSimbaAnchorDockHostSite) and (TSimbaAnchorDockHostSite(Form).Header <> nil) then
  begin
    AHeight := AHeight + TSimbaAnchorDockHostSite(Form).Header.Height +
                         TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Top +
                         TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Bottom;
  end;

  AHeight := AHeight + FImageBox.StatusBar.Height;

  if AForce or (AWidth > Form.Width) then
    Form.Width := Min(AWidth, FMaxWidth);
  if AForce or (AHeight > Form.Height) then
    Form.Height := Min(AHeight, FMaxHeight);

  if AEnsureVisible then
    Form.EnsureVisible(True);
end;

procedure TSimbaDebugImageForm.SetMaxSize(AWidth, AHeight: Integer);
begin
  if (FMaxWidth = AWidth) and (FMaxHeight = AHeight) then
    Exit;

  if (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    FMaxWidth := AWidth;
    FMaxHeight := AHeight;

    if (HostDockSite.Width > FMaxWidth) then
      HostDockSite.Width := FMaxWidth;
    if (HostDockSite.Height > FMaxHeight) then
      HostDockSite.Height := FMaxHeight;
  end;
end;

end.

