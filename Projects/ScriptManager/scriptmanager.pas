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
  {$IFDEF UNIX}cthreads,cmem,{$ENDIF} Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus, settings, updater,strutils, MufasaTypes,
  dom;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SearchIn: TComboBox;
    Edit1: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ScriptPopup: TPopupMenu;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure ClickItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TSimbaScript }

  TSimbaScript = class(TObject)
  private
    procedure LoadFromNode( Script : TDOMNode);
  public
    Name, Version, Author, Description: String;
    Tags, Files: TStringList;
    procedure Dbg;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLSimbaScript }

  TLSimbaScript = class(TSimbaScript) //Installed Script (Local Simba Script)
  public
    procedure LoadFromFile(const filename : string);
    procedure SaveToFile(const FileName : string);
    procedure Save(const MainDir : string); //MainDir = maindir of ScriptManager
  end;

  { TScriptManager }

  TScriptManager = class (TObject)
  private
    FScripts : TList; //Array of the online scripts
    FLScripts: TList; //Array of the local scripts
    FVersion : String;
    FUpdating : boolean;
    function GetLScriptCount: integer;
    function GetScriptCount: integer;
  public
    MainDir : string;
    procedure Update; //Gets the new online scripts
    procedure LUpdate; //Loads the local scripts, uses MainDir
    procedure LSave; //Saves the local scripts, uses MainDir
    property LScriptCount : integer read GetLScriptCount; //LScript = Local Script = Installed Script
    property ScriptCount : integer read GetScriptCount; //Online script
    property Version : string read FVersion;
    constructor Create;
    destructor Destroy; override;
  end;



var
  Form1: TForm1; 

implementation

uses
  XMLRead,XMLWrite;
{$R *.lfm}

{ TForm1 }
procedure fill(s: TMMLSettings);
var
  i:integer;
  ss: TSimbaScript;
  LI: TListItem;
  strarr: TStringArray;
  b: TButton;

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
{  s := TMMLSettings.Create(TreeView1.Items);
  s.LoadFromXML('/scratch/gittest/list.xml');
  fill(s);
  s.Free();}
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

procedure TForm1.Button1Click(Sender: TObject);
var
  Mngr : TScriptManager;
begin
  Mngr := TScriptManager.Create;
  Mngr.Update;
  Mngr.free;
end;

{ TSimbaScript }

procedure TSimbaScript.LoadFromNode(Script: TDOMNode);
  function NodeContents(ItemStr : string; node : TDOMNode) : string;
  var
    tmpNode : TDOMNode;
  begin
    result := '';
    if node = nil then
      exit;
    tmpNode := node.FindNode(itemstr);
    if tmpNode <> nil then
      result := Trim(tmpNode.TextContent);
  end;
  function NodeSubContents(ItemStr : string; node : TDOMNode) : TStringList;
  var
    tmpNode : TDOMNode;
  begin
    Result := TStringList.Create;
    if node = nil then
      exit;
    tmpNode := node.FindNode(itemstr);
    if tmpNode <> nil then
    begin
      tmpNode := tmpNode.FirstChild;
      while tmpNode <> nil do
      begin
        Result.add(trim(tmpNode.TextContent));
        tmpNode := tmpNode.NextSibling;
      end;
    end;
  end;
begin
  Author:= NodeContents('Author',script);
  Name := NodeContents('Name',script);
  Version := NodeContents('Version',script);
  Description:= NodeContents('Description',script);
  Tags := NodeSubContents('Tags',script);
  Files := NodeSubContents('Files',script);
end;

procedure TSimbaScript.Dbg;
var
  i : integer;
begin
  Writeln(Name);
  Writeln('  Author: ' + Author);
  Writeln('  Version: ' + Version);
  Writeln('  Description: ' + Description);
  Writeln('  Tags:');
  for i := 0 to Tags.Count - 1 do
    Writeln('    ' + Tags[i]);
  Writeln('  Files:');
  for i := 0 to Files.Count - 1 do
    Writeln('    ' + Files[i]);
end;

constructor TSimbaScript.Create;
begin
  inherited;

  {stuff here}
end;

destructor TSimbaScript.Destroy;
begin
  if Files <> nil then
    FreeAndNil(Files);
  if Tags <> nil then
    FreeAndNil(Tags);
  {stuff here}

  inherited;
end;

{ TScriptManager }

function TScriptManager.GetLScriptCount: integer;
begin
  result := FLScripts.Count;
end;

function TScriptManager.GetScriptCount: integer;
begin
  result := FScripts.Count;
end;

procedure TScriptManager.Update;
var
  XMLFile : string;
  Stream  : TStringStream;
  XMLDoc  : TXMLDocument;
  Node,Script : TDOMNode;
  Subs : TStringList;
  Down : TDownloadThread;
  SimbaScript : TLSimbaScript;
begin
  if FUpdating then
    exit;
  FUpdating := True;
  Down := TDownloadThread.Create('http://old.villavu.com/sm',@XMLFile);
  down.Execute;
  while down.Done = false do
  begin
    Application.ProcessMessages;
    Sleep(25);
  end;
  Stream := TStringStream.Create(XMLFile);
  ReadXMLFile(XMLDoc,Stream);
  Stream.Free;
  Node := XMLDoc.FirstChild.FindNode('Version');
  if node <> nil then
    FVersion:= Node.TextContent;
  Node := XMLDoc.FirstChild.FindNode('ScriptList');
  if node <> nil then
  begin
    script := Node.FirstChild;
    while Script <> nil do
    begin
      SimbaScript := TLSimbaScript.Create;
      SimbaScript.LoadFromNode(Script);
      FLScripts.Add(SimbaScript);
      SimbaScript.Dbg;
      Script := Script.NextSibling;
    end;
  end;
  SimbaScript.SaveToFile('c:\testme.xml');
  XMLDoc.Free;
  FUpdating := false;
end;

procedure TScriptManager.LUpdate;
begin
  if DirectoryExists(MainDir) = false then
    exit;
  if FileExists( IncludeTrailingPathDelimiter(maindir) + 'General' + DirectorySeparator+
     'scripts.xml') then
  begin

  end;
end;

procedure TScriptManager.LSave;
var
  XMLDoc : TXMLDocument;
procedure AddTextElement(root : TDOMNode; Element : string; Text : string);
var
  node : TDOMNode;
begin
  Node := XMLDoc.createElement(Element);
  root.AppendChild(node);
  node.TextContent:= Text;
end;
var
  Node : TDOMNode;
  i : integer;
begin
  if DirectoryExists(MainDir) = false then
    exit;
  XMLDoc := TXMLDocument.Create;
  Node := XMLDoc.CreateElement('Scripts');
  XMLDoc.AppendChild(node);
  for i := 0 to FLScripts.Count - 1 do
    AddTextElement(node,'Script', TLSimbaScript(FLScripts[i]).Name);
  WriteXMLFile(XMLDoc,IncludeTrailingPathDelimiter(maindir) + 'General' + DirectorySeparator+
     'scripts.xml');
  XMLDoc.Free;
end;

constructor TScriptManager.Create;
begin
  inherited;
  FLScripts := TList.Create;
  FScripts := TList.Create;
  FVersion := '';
  FUpdating:= False;
end;

destructor TScriptManager.Destroy;
begin
  while FScripts.Count > 0 do
  begin
    TSimbaScript(FScripts[0]).Free;
    FScripts.Delete(0);
  end;
  while FLScripts.Count > 0 do
  begin
    TLSimbaScript(FLScripts[0]).Free;
    FLScripts.Delete(0);
  end;
  inherited Destroy;
end;

{ TLSimbaScript }

procedure TLSimbaScript.LoadFromFile(const filename: string);
var
  XMLDoc : TXMLDocument;
begin
  ReadXMLFile(XMLDoc,filename);
  Self.LoadFromNode(XMLDoc.FirstChild);
end;

procedure TLSimbaScript.SaveToFile(const FileName: string);

var
  XMLDoc : TXMLDocument;
procedure AddTextElement(root : TDOMNode; Element : string; Text : string);
var
  node : TDOMNode;
begin
  Node := XMLDoc.createElement(Element);
  root.AppendChild(node);
  node.TextContent:= Text;
end;
var
  Node,SubNode : TDOMNode;
  i : integer;
begin
  XMLDoc := TXMLDocument.Create;
  Node := XMLDoc.CreateElement('Script');
  XMLDoc.AppendChild(Node);
  AddTextElement(node,'Name',Name);
  AddTextElement(node,'Author',Author);
  AddTextElement(node,'Version',Version);
  AddTextElement(node,'Description',description);
  SubNode := XMLDoc.CreateElement('Tags');
  Node.AppendChild(SubNode);
  for i := 0 to Tags.Count - 1 do
    AddTextElement(SubNode,'Tag',Tags[i]);
  SubNode := XMLDoc.CreateElement('Files');
  Node.AppendChild(SubNode);
  for i := 0 to Files.Count - 1 do
    AddTextElement(SubNode,'File',Files[i]);
  WriteXMLFile(XMLDoc,FileName);
  XMLDoc.Free;
end;

procedure TLSimbaScript.Save(const MainDir: string);
begin

end;

end.

