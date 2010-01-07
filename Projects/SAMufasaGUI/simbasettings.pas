unit simbasettings;

{$mode objfpc} {$M+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, settings;

const
   SimbaSettingsFile = 'settings.xml';

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    SettingsTreeView: TTreeView;
    Settings: TMMLSettings;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { private declarations }
  public
    procedure SaveCurrent;
    procedure Reload;
    { public declarations }
  end; 

var
  SettingsForm: TSettingsForm;

implementation

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);

begin
  Settings := TMMLSettings.Create(SettingsTreeView.Items);
  if not FileExists('settings.xml') then
  begin
    SettingsTreeView.Items.Clear;
    Settings.SaveToXML('settings.xml');
  end;

  SettingsTreeView.Items.Clear;
  Settings.LoadFromXML('settings.xml');
end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TSettingsForm.SaveCurrent;
begin
  Settings.SaveToXML('settings.xml');
end;

procedure TSettingsForm.Reload;
begin
  if not FileExists('settings.xml') then
  begin
    SettingsTreeView.Items.Clear;
    Settings.SaveToXML('settings.xml');
  end;

  SettingsTreeView.Items.Clear;
  Settings.LoadFromXML('settings.xml');
end;

initialization
  {$I simbasettings.lrs}

end.

