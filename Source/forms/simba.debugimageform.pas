{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.debugimageform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls,
  simba.imagebox;

type
  TSimbaDebugImageForm = class(TForm)
  protected
    FMouseX, FMouseY: Integer;
    FImageBox: TSimbaImageBox;
    FMaxWidth, FMaxHeight: Integer;

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageDoubleClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Close;

    procedure SetMaxSize(AWidth, AHeight: Integer);
    procedure SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean = True);

    property ImageBox: TSimbaImageBox read FImageBox;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

{$R *.lfm}

uses
  lcltype, math,
  simba.dockinghelpers, simba.outputform;

procedure TSimbaDebugImageForm.Close;
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  Form.Close();
end;

procedure TSimbaDebugImageForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;

procedure TSimbaDebugImageForm.ImageDoubleClick(Sender: TObject);
begin
  SimbaOutputForm.Add('Debug Image Click: ' + IntToStr(FMouseX) + ', ' + IntToStr(FMouseY));
end;

procedure TSimbaDebugImageForm.SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean);
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

  if (AWidth > Form.Width) then
    Form.Width := Min(AWidth, FMaxWidth);
  if (AHeight > Form.Height) then
    Form.Height := Min(AHeight, FMaxHeight);

  if AEnsureVisible then
    Form.EnsureVisible(True);
end;

constructor TSimbaDebugImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMaxWidth := 1500;
  FMaxHeight := 1000;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnMouseMove := @ImageMouseMove;
  FImageBox.OnDblClick := @ImageDoubleClick;
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

