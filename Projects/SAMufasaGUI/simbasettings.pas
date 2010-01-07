unit simbasettings;

{$mode objfpc} {$M+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, settings;

const
   SimbaSettingsFile = 'settings.xml';

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    SettingsFormButtonCancel: TButton;
    SettingsFormButtonOK: TButton;
    SettingsTreeView: TTreeView;
    Settings: TMMLSettings;
    procedure SettingsFormButtonCancelClick(Sender: TObject);
    procedure SettingsFormButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SettingsTreeViewDblClick(Sender: TObject);
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

procedure TSettingsForm.SettingsFormButtonOKClick(Sender: TObject);
begin
  SettingsForm.Settings.SaveToXML(SimbaSettingsFile);
  SettingsForm.ModalResult:=mrOK;
end;

procedure TSettingsForm.SettingsFormButtonCancelClick(Sender: TObject);
begin
  if not FileExists(SimbaSettingsFile) then
  begin
    SettingsForm.SettingsTreeView.Items.Clear;
    SettingsForm.Settings.SaveToXML(SimbaSettingsFile);
    SettingsForm.SettingsTreeView.Items.Clear;
    SettingsForm.Settings.LoadFromXML(SimbaSettingsFile);
  end;
  SettingsForm.ModalResult:=mrOK;
end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TSettingsForm.SettingsTreeViewDblClick(Sender: TObject);
var
  p, pp: TPoint;
  N: TTreeNode;
  Path, NewVal: String;

begin
  p := Mouse.CursorPos;
  pp := TSettingsForm(Sender).ScreenToClient(p);
  N := SettingsTreeView.GetNodeAt(pp.x, pp.y);
  if N <> nil then
    if (N.Text = 'Value') and (N.Parent <> nil) then
    begin
      Path := Settings.GetNodePath(N.Parent);
      NewVal := InputBox('Change Setting', 'Change value for ' + N.Parent.Text,
                            Settings.GetKeyValue(Path));
      Settings.SetKeyValue(Path, NewVal);
    end;
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

