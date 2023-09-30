{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_buttonpanel;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms,
  simba.component_button;

type
  TSimbaButtonPanel = class(TCustomControl)
  protected
    FButtonOk: TSimbaButton;
    FButtonCancel: TSimbaButton;

    procedure DoButtonCancelResize(Sender: TObject);

    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;

    property ButtonOk: TSimbaButton read FButtonOk;
    property ButtonCancel: TSimbaButton read FButtonCancel;
  end;

implementation

uses
  simba.theme;

type
  TButtonPanelButton = class(TSimbaButton)
  public
    procedure Click; override;
  end;

procedure TButtonPanelButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetTopFormSkipNonDocked(Self);

  if Assigned(Form) and (Parent is TSimbaButtonPanel) then
  begin
    if Self.Equals(TSimbaButtonPanel(Parent).FButtonOk) then
      Form.ModalResult := mrOK;
    if Self.Equals(TSimbaButtonPanel(Parent).FButtonCancel) then
      Form.ModalResult := mrCancel;

    Form.Close();
  end;

  inherited Click();
end;

procedure TSimbaButtonPanel.DoButtonCancelResize(Sender: TObject);
begin
  FButtonOk.Width := FButtonCancel.Width;
  FButtonOk.Height := FButtonCancel.Height;
end;

procedure TSimbaButtonPanel.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  Align := alBottom;
end;

constructor TSimbaButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FButtonOk := TButtonPanelButton.Create(Self);
  FButtonOk.Parent := Self;
  FButtonOk.Align := alRight;
  FButtonOk.Caption := 'Ok';
  FButtonOk.BorderSpacing.Around := 8;
  FButtonOk.XPadding := 10;
  FButtonOk.SetImage(ESimbaButtonImage.OK);

  FButtonCancel := TButtonPanelButton.Create(Self);
  FButtonCancel.Parent := Self;
  FButtonCancel.Align := alRight;
  FButtonCancel.Caption := 'Cancel';
  FButtonCancel.BorderSpacing.Around := 8;
  FButtonCancel.XPadding := 10;
  FButtonCancel.SetImage(ESimbaButtonImage.CLOSE);
  FButtonCancel.OnResize := @DoButtonCancelResize;

  Color := SimbaTheme.ColorFrame;

  AutoSize := True;
end;

end.

