unit simba.splashform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type
  TSimbaSplashForm = class(TForm)
    VersionLabel: TLabel;
    StatusLabel: TLabel;
    SimbaImage: TImage;
    procedure FormPaint(Sender: TObject);
  public
    procedure Execute(Task: String; Method: TProcedureOfObject);

    constructor Create(AOwner: TComponent); override;
  end;

  TApplicationHelper = class helper for TApplication
  public
    procedure CreateForm(FormClass: TComponentClass; out Reference);
  end;

var
  SimbaSplashForm: TSimbaSplashForm;

implementation

uses
  simba.mufasabase;

procedure TSimbaSplashForm.FormPaint(Sender: TObject);
begin
  Canvas.Pen.Color := cl3DLight;
  Canvas.Frame(ClientRect);
end;

procedure TSimbaSplashForm.Execute(Task: String; Method: TProcedureOfObject);
begin
  WriteLn(Task);

  StatusLabel.Caption := Task;
  StatusLabel.Update();

  Method();
end;

constructor TSimbaSplashForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  VersionLabel.Caption := 'Simba Version: ' + IntToStr(SimbaVersion);

  // Remove ourself as the main form.
  // Application.CreateForm seems to only allow the first form to a mainform even if
  // it doesn't pass the requirements. :S
  Application.Notification(SimbaSplashForm, opRemove);
end;

procedure TApplicationHelper.CreateForm(FormClass: TComponentClass; out Reference);
var
  Task: String;
begin
  Task := FormClass.ClassName;
  if Task[1] = 'T' then
    Delete(Task, 1, 1);

  Task := 'Creating ' + Task;

  if (SimbaSplashForm <> nil) then
  begin
    SimbaSplashForm.StatusLabel.Caption := Task;
    SimbaSplashForm.Update();
  end;

  inherited CreateForm(FormClass, Reference);
end;

initialization
  {$I simba.splashform.lrs}

end.

