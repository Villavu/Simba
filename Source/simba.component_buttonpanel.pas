{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_buttonpanel;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls,
  simba.component_button;

type
  TSimbaButtonPanel = class(TCustomControl)
  protected
    FButtonsContainer: TFlowPanel;
    FButtonOk: TSimbaButton;
    FButtonCancel: TSimbaButton;

    FCloseOnCancel: Boolean;
    FCloseOnOk: Boolean;

    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;

    function AddButton: TSimbaButton;

    property ButtonOk: TSimbaButton read FButtonOk;
    property ButtonCancel: TSimbaButton read FButtonCancel;

    property CloseOnCancel: Boolean read FCloseOnCancel write FCloseOnCancel;
    property CloseOnOk: Boolean read FCloseOnOk write FCloseOnOk;
  end;

implementation

uses
  simba.ide_theme;

type
  TButtonPanelButton = class(TSimbaButton)
  protected
    function GetIndex: Integer; virtual;
    procedure SetIndex(AValue: Integer); virtual;
  public
    ButtonPanel: TSimbaButtonPanel;

    procedure Click; override;
    property Index: Integer read GetIndex write SetIndex;
  end;

function TButtonPanelButton.GetIndex: Integer;
begin
  if (Parent is TFlowPanel) then
    Result := TFlowPanel(Parent).GetControlIndex(Self)
  else
    Result := -1;
end;

procedure TButtonPanelButton.SetIndex(AValue: Integer);
begin
  if (Parent is TFlowPanel) then
    TFlowPanel(Parent).SetControlIndex(Self, AValue);
end;

procedure TButtonPanelButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetTopFormSkipNonDocked(Self);

  if Assigned(Form) then
  begin
    if (Self = ButtonPanel.FButtonOk) then
    begin
      Form.ModalResult := mrOk;
      if ButtonPanel.CloseOnOk then
        Form.Close();
    end else
    if (Self = ButtonPanel.FButtonCancel) then
    begin
      Form.ModalResult := mrCancel;
      if ButtonPanel.CloseOnCancel then
        Form.Close();
    end;
  end;

  inherited Click();
end;

procedure TSimbaButtonPanel.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  Align := alBottom;
end;

constructor TSimbaButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCloseOnOk := True;
  FCloseOnCancel := True;

  FButtonsContainer := TFlowPanel.Create(Self);
  FButtonsContainer.Parent := Self;
  FButtonsContainer.AutoSize := True;
  FButtonsContainer.Align := alClient;
  FButtonsContainer.BorderStyle := bsNone;
  FButtonsContainer.BevelOuter := bvNone;
  FButtonsContainer.FlowStyle := fsRightLeftTopBottom;

  FButtonCancel := AddButton();
  FButtonCancel.Caption := 'Cancel';
  FButtonCancel.BorderSpacing.Around := 5;
  FButtonCancel.Image := ESimbaButtonImage.CLOSE;

  FButtonOk := AddButton();
  FButtonOk.Caption := 'Ok';
  FButtonOk.BorderSpacing.Around := 5;
  FButtonOk.Image := ESimbaButtonImage.OK;

  Color := SimbaTheme.ColorFrame;
  AutoSize := True;
end;

function TSimbaButtonPanel.AddButton: TSimbaButton;
begin
  Result := TButtonPanelButton.Create(Self);
  Result.Parent := FButtonsContainer;

  TButtonPanelButton(Result).ButtonPanel := Self;
end;

end.

