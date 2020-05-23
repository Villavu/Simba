unit simba.hintwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TSimbaHintWindow = class(THintWindow)
  protected
    FApplicationActivated: Boolean;

    procedure SetVisible(Value: Boolean); override;

    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationDeactivate(Sender: TObject);
  public
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

procedure TSimbaHintWindow.SetVisible(Value: Boolean);
begin
  if FApplicationActivated then
    inherited SetVisible(Value);
end;

constructor TSimbaHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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

