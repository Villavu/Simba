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
    procedure SetSize(AWidth, AHeight: Integer);

    property ImageBox: TSimbaImageBox read FImageBox;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

{$R *.lfm}

uses
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

procedure TSimbaDebugImageForm.SetSize(AWidth, AHeight: Integer);
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
    Form.Width := AWidth;
  if (AHeight > Form.Height) then
    Form.Height := AHeight;

  Form.EnsureVisible(True);

  SetMaxSize(FMaxWidth, FMaxHeight);
end;

constructor TSimbaDebugImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMaxWidth := 1500;
  FMaxHeight := 1500;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnMouseMove := @ImageMouseMove;
  FImageBox.OnDblClick := @ImageDoubleClick;
end;

procedure TSimbaDebugImageForm.SetMaxSize(AWidth, AHeight: Integer);
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  FMaxWidth := AWidth;
  FMaxHeight := AHeight;

  Form.Constraints.MinWidth  := 150;
  Form.Constraints.MinHeight := 150;

  Form.Constraints.MaxWidth  := FMaxWidth;
  Form.Constraints.MaxHeight := FMaxHeight;

  if (Form.Width > FMaxWidth) then
    Form.Width := FMaxWidth;
  if (Form.Height > FMaxHeight) then
    Form.Height := FMaxHeight;
end;

end.

