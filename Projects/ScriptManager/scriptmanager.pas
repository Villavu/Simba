{
	This file is part of the Simba Project
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    Simba is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Simba is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Script Manager for the Simba project.
}

unit scriptmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus, settings, MufasaTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    ListView1: TListView;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ScriptPopup: TPopupMenu;
    TreeView1: TTreeView;
    procedure ClickItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TSimbaScript = class(TObject)

  public
    Name, Version, Author, Description: String;
    Tags, Files: TStringArray;

  private

  public
    constructor Create;
    destructor Delete;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }
procedure fill(s: TMMLSettings);
var
  i:integer;
  ss: TSimbaScript;
  LI: TListItem;
  strarr: TStringArray;
begin
  if not s.ListKeys('Scripts/ScriptList', strarr) then
    writeln('ListKeys returned false');
  writeln('strarr length: ' + inttostr(length(strarr)));
  for i := 0 to high(strarr) do
  begin
    writeln(s.GetKeyValue('Scripts/ScriptList/Script/Name'));
    ss := TSimbaScript.Create();
    ss.Name := s.GetKeyValue('Scripts/ScriptList/Script/Name');
    ss.Author:=   s.GetKeyValue('Scripts/ScriptList/Script/Author');
    ss.Description:= s.GetKeyValue('Scripts/ScriptList/Script/Description');
    LI := Form1.ListView1.Items.Add;
    LI.Caption := ss.Name;
    LI.Data := ss;
    LI.ImageIndex:= 0;

    s.DeleteKey('Scripts/ScriptList/Script');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: TMMLSettings;
begin
  s := TMMLSettings.Create(TreeView1.Items);
  s.LoadFromXML('/scratch/gittest/list.xml');
  fill(s);
  s.Free();
end;

procedure TForm1.ClickItem(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  item: TListItem;
begin
  item := Form1.ListView1.GetItemAt(x, y);
  if item = nil then
    exit;
  if item.data = nil then
    exit;

  { Any selection causes the description to change }
  form1.Memo1.Lines.Clear();
  form1.Memo1.Lines.Append(TSimbaScript(item.data).Description);

  if Button = mbLeft then
  begin

  end else if Button = mbRight then
  begin
    { Popup Actions }
    Form1.ScriptPopup.Items[0].Caption:= 'Install ' +  TSimbaScript(item.data).Name;
    Form1.ScriptPopup.PopUp();
  end;
  //form1.Memo1.Text := TSimbaScript(item.data).Description;
end;

{ TSimbaScript }

constructor TSimbaScript.Create;
begin
  inherited;

  {stuff here}
end;

destructor TSimbaScript.Delete;
begin

  {stuff here}

  inherited;
end;

end.

