{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Settings form for Simba
}
unit Simbasettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,MufasaBase, Graphics, Dialogs,
  ComCtrls, StdCtrls, Menus, settings;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    DeleteButton: TButton;
    PopupCreate: TMenuItem;
    PopupRename: TMenuItem;
    PopupDelete: TMenuItem;
    SettingsPopup: TPopupMenu;
    SettingsFormButtonCancel: TButton;
    SettingsFormButtonOK: TButton;
    SettingsTreeView: TTreeView;
    Settings: TMMLSettings;
    procedure DeleteSelected(Sender: TObject);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnKeyPress(Sender: TObject; var Key: char);
    procedure PopupCreateKey(Sender: TObject);
    procedure PopupDeleteClick(Sender: TObject);
    procedure PopupRenameClick(Sender: TObject);
    procedure SettingsFormButtonCancelClick(Sender: TObject);
    procedure SettingsFormButtonOKClick(Sender: TObject);
    procedure DeleteANode(N: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SettingsTreeViewDblClick(Sender: TObject);
    function FixSettingsFile: Boolean;
    { private declarations }
  public
    procedure SaveCurrent;
    procedure Reload;
    { public declarations }
  public
    Oops: Boolean;
  end; 

var
  SettingsForm: TSettingsForm;
  SimbaSettingsFile : string;

implementation

uses LCLtype;

{ TSettingsForm }

function TSettingsForm.FixSettingsFile: Boolean;
begin
  result := true;
  mDebugLn('Could not load settings.xml!');
  if renamefileUTF8('settings.xml', 'settings.bak') then
  begin
    mDebugLn('Moved settings.xml to settings.bak');
  end else
  begin
    mDebugLn('Could not move settings.xml to settings.bak');
    if not deletefileUTF8('settings.xml') then
    begin
      mDebugLn('Couldnt delete the file either.');
      exit(false);
    end;
  end;

  SettingsTreeView.Items.Clear;
  Settings.SaveToXML(SimbaSettingsFile);
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  FirstNode : TTreeNode;

begin
  Oops := False;
  Settings := TMMLSettings.Create(SettingsTreeView.Items);
  if not FileExists(SimbaSettingsFile) then
  begin
    SettingsTreeView.Items.Clear;
    Settings.SaveToXML(SimbaSettingsFile);
  end;
  SettingsTreeView.Items.Clear;

  if not Settings.LoadFromXML(SimbaSettingsFile) then
  begin
    if not FixSettingsFile() then
    begin
      mDebugLn('Could not create, move or delete settings.xml.');
      mDebugLn('***************** Giving up... ********************');
      Oops := True;
    end;
  end;

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

procedure TSettingsForm.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  N: TTreeNode;
begin
  if Button = mbRight then
  begin
    N := SettingsTreeView.GetNodeAt(X, Y);
    if N = nil then
      exit;
    SettingsTreeView.Selected := N;
    SettingsPopup.PopUp();
  end;
end;

procedure TSettingsForm.OnKeyPress(Sender: TObject; var Key: char);
var
  N: TTreeNode;
begin
  if (Ord(Key) = VK_DELETE) or (Ord(Key) = VK_BACK) then
  begin
    N := SettingsTreeView.Selected;
    if N = nil then
      exit;
    DeleteANode(N);
  end;
end;

procedure TSettingsForm.PopupCreateKey(Sender: TObject);
var
  KeyName, P: String;
  N, NN: TTreeNode;
begin
  N := SettingsTreeView.Selected;
  if N = nil then
    exit;

  if N.Data <> nil then
    exit;

  if N.GetFirstChild <> nil then
    if N.GetFirstChild.Data <> nil then
      exit;

  KeyName := InputBox('Create new Key', 'Please enter the key name', '');

  if KeyName = '' then
    exit;
  P := Settings.GetNodePath(N);

  if Settings.CreateKey(P + '/' + KeyName) then
    mDebugLn('Added key ' + KeyName);
end;


procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TSettingsForm.PopupDeleteClick(Sender: TObject);
begin
  DeleteSelected(Sender);
end;

procedure TSettingsForm.PopupRenameClick(Sender: TObject);
var
  N: TTreeNode;
  MBox, Path: String;
begin
  N := SettingsTreeView.Selected;
  if N = nil then
    exit;

  Path := Settings.GetNodePath(N);

  MBox := InputBox('Rename', 'Please fill in the new name', '');
  if MBox = '' then
    exit;

  if Settings.RenameKey(Path, MBox) then
    N.Text := MBox;
end;

procedure TSettingsForm.DeleteSelected(Sender: TObject);
var
  N: TTreeNode;
begin
  N := SettingsTreeView.Selected;
  if N = nil then
    exit;
  DeleteANode(N);
end;

procedure TSettingsForm.DeleteANode(N: TTreeNode);
var
  Path: String;
begin
  if N = nil then
    exit;
  if MessageDlg('Delete a setting', 'Are you sure you want to delete this setting?', mtWarning, [mbYes, mbNo], 0) = mrNo then
    exit;

  Path := Settings.GetNodePath(N);
  Settings.DeleteKey(Path);
  N.DeleteChildren;
  N.Delete;
end;

procedure TSettingsForm.SettingsTreeViewDblClick(Sender: TObject);
var
  p, pp: TPoint;
  N: TTreeNode;
  Path, NewVal: String;

begin
  p := Mouse.CursorPos;

  pp := TTreeView(Sender).ScreenToClient(p);
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
  {$R *.lfm}

end.

