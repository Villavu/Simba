unit simbasettings;

{$mode objfpc} {$M+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,MufasaBase, Graphics, Dialogs,
  ComCtrls, StdCtrls, settings;

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
  SimbaSettingsFile : string;

implementation

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  FirstNode : TTreeNode;
begin
  Settings := TMMLSettings.Create(SettingsTreeView.Items);
  if not FileExists(SimbaSettingsFile) then
  begin
    SettingsTreeView.Items.Clear;
    Settings.SaveToXML(SimbaSettingsFile);
  end;
  SettingsTreeView.Items.Clear;
  Settings.LoadFromXML(SimbaSettingsFile);
  FirstNode := SettingsTreeView.Items.GetFirstNode;
  if FirstNode <> nil then
    if FirstNode.Text = 'Settings' then
      FirstNode.Expand(false);
end;

procedure TSettingsForm.SettingsFormButtonOKClick(Sender: TObject);
begin
  Self.Settings.SaveToXML(SimbaSettingsFile);
  Self.ModalResult:=mrOK;
end;

procedure TSettingsForm.SettingsFormButtonCancelClick(Sender: TObject);
begin
  if not FileExists(SimbaSettingsFile) then
  begin
    Self.SettingsTreeView.Items.Clear;
    Self.Settings.SaveToXML(SimbaSettingsFile);
    Self.SettingsTreeView.Items.Clear;
    Self.Settings.LoadFromXML(SimbaSettingsFile);
  end;
  Self.ModalResult:=mrOK;
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
    if assigned(N.Data) then
    begin
      Path := Settings.GetNodePath(N.Parent);
      NewVal := InputBox('Change Setting', 'Change value for ' + TSettingData(N.Data).Val,
                            Settings.GetKeyValue(Path));
      mDebugLn('NewVal: ' + NewVal);
      Settings.SetKeyValue(Path, NewVal);
      N.Text := NewVal;
    end;
end;

procedure TSettingsForm.SaveCurrent;
begin
  Settings.SaveToXML(SimbaSettingsFile);
end;

procedure TSettingsForm.Reload;
begin
  if not FileExists(SimbaSettingsFile) then
  begin
    SettingsTreeView.Items.Clear;
    Settings.SaveToXML(SimbaSettingsFile);
  end;

  SettingsTreeView.Items.Clear;
  Settings.LoadFromXML(SimbaSettingsFile);
end;

initialization
  {$I simbasettings.lrs}

end.

