unit simba.hintwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, Forms;

type
  TSimbaHintWindow = class(THintWindowRendered)
  protected
    FApplicationActivated: Boolean;
    FLabel: TLabel;

    procedure SetVisible(Value: Boolean); override;

    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationDeactivate(Sender: TObject);
  public
    procedure Paint; override;

    procedure ActivateHint(ARect: TRect; const AHint: String); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure TSimbaHintWindow.ApplicationActivate(Sender: TObject);
begin
  FApplicationActivated := True;
end;

procedure TSimbaHintWindow.ApplicationDeactivate(Sender: TObject);
begin
  Visible := False;

  FApplicationActivated := False;
end;

procedure TSimbaHintWindow.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);

  inherited Paint();
end;

procedure TSimbaHintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  FLabel.Caption := AHint;
  HintRect := ARect;

  ActivateRendered();
end;

procedure TSimbaHintWindow.SetVisible(Value: Boolean);
begin
  if FApplicationActivated then
    inherited SetVisible(Value);
end;

constructor TSimbaHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;

  Font.Color := clWindowText;

  FLabel := TLabel.Create(Self);
  FLabel.BorderSpacing.Top := 2;
  FLabel.BorderSpacing.Left := 5;
  FLabel.BorderSpacing.Right := 5;
  FLabel.BorderSpacing.Bottom := 4;
  FLabel.Parent := Self;
  FLabel.OptimalFill := True;
  FLabel.AutoSize := True;
  FLabel.Transparent := True;

  FApplicationActivated := True;

  Application.AddOnActivateHandler(@ApplicationActivate);
  Application.AddOnDeactivateHandler(@ApplicationDeactivate);
end;

destructor TSimbaHintWindow.Destroy;
begin
  Application.RemoveOnActivateHandler(@ApplicationActivate);
  Application.RemoveOnDeactivateHandler(@ApplicationDeactivate);

  inherited Destroy();
end;

end.

