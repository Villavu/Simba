{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.debugimage;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls,
  simba.imagebox;

type
  TSimbaDebugImageForm = class(TForm)
  protected
    FMouseX, FMouseY: Int32;
    FImageBox: TSimbaImageBox;

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
    procedure ImageDoubleClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Display(AWidth, AHeight: Int32; AShow: Boolean);

    property ImageBox: TSimbaImageBox read FImageBox;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

{$R *.lfm}

uses
  simba.debugform, simba.dockinghelpers;

procedure TSimbaDebugImageForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
begin
  FMouseX := X;
  FMouseY := Y;
end;

procedure TSimbaDebugImageForm.ImageDoubleClick(Sender: TObject);
begin
  SimbaDebugForm.Add('Debug Image Click: ' + IntToStr(FMouseX) + ', ' + IntToStr(FMouseY));
end;

procedure TSimbaDebugImageForm.Display(AWidth, AHeight: Int32; AShow: Boolean);
var
  Form: TCustomForm;
begin
  if (AWidth < 250) then
    AWidth := 250;
  if (AHeight < 250) then
    AHeight := 250;

  if (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    Form := TSimbaAnchorDockHostSite(HostDockSite);
    if (TSimbaAnchorDockHostSite(Form).Header <> nil) then
    begin
      AHeight := AHeight + TSimbaAnchorDockHostSite(Form).Header.Height +
                           TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Top +
                           TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Bottom;
    end;
  end else
    Form := Self;

  Form.Width := AWidth;
  Form.Height := AHeight;
  if AShow then
    Form.EnsureVisible(True);
end;

constructor TSimbaDebugImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnMouseMove := @ImageMouseMove;
  FImageBox.OnDblClick := @ImageDoubleClick;
end;

end.

