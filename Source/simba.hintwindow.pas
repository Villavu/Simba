{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.hintwindow;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, Forms;

type
  TSimbaHintWindow = class(THintWindowRendered)
  protected
    FLabel: TLabel;

    procedure Paint; override;

    procedure HandleNeedHide(Sender: TObject);
  public
    procedure ActivateHint(ARect: TRect; const AHint: String); override;  //Rename

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure TSimbaHintWindow.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);

  inherited Paint();
end;

// Dont use activate rendered
procedure TSimbaHintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  if Visible and (FLabel.Caption = AHint) then
    Exit;
  FLabel.Caption := AHint;

  ActivateWithBounds(ARect, AHint);
end;

procedure TSimbaHintWindow.HandleNeedHide(Sender: TObject);
begin
  Visible := False;
end;

constructor TSimbaHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (AOwner is TControl) then
    TControl(AOwner).AddHandlerOnVisibleChanged(@HandleNeedHide);

  AutoSize := True;

  Font.Color := clWindowText;

  FLabel := TLabel.Create(Self);
  FLabel.BorderSpacing.Top := 2;
  FLabel.BorderSpacing.Left := 5;
  FLabel.BorderSpacing.Right := 5;
  FLabel.BorderSpacing.Bottom := 2;
  FLabel.Parent := Self;
  FLabel.OptimalFill := True;
  FLabel.AutoSize := True;
  FLabel.Transparent := True;

  Application.AddOnDeactivateHandler(@HandleNeedHide);
end;

destructor TSimbaHintWindow.Destroy;
begin
  Application.RemoveOnDeactivateHandler(@HandleNeedHide);

  inherited Destroy();
end;

end.

