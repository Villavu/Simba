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

{
Raymond`, 11-9-2010 20:26:27:
ScriptManager/General/Settings.xml (Lijst van alle geinstaleerde scripts)
ScriptManager/General/<Scriptname>/Info.xml (Informatie over het script)
ScriptManager/<ScriptName>/<Files> (Alle files die het script nodig heeft)
}
unit scriptmanager;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}cthreads,cmem,{$ENDIF} Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus, settings, updater,strutils, MufasaTypes,
  dom, mmisc;

type

  { TSimbaScript }

  TSimbaScript = class(TObject)
  private
    function IsInstalled: boolean;
    procedure LoadFromNode( Script : TDOMNode);
  public
    Name, Version, Author, Description, URL: String;
    Tags, Files: TStringList;
    LocalScript : TSimbaScript;
    property Installed : boolean read IsInstalled;
    procedure Dbg;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLSimbaScript }

  TLSimbaScript = class(TSimbaScript) //Installed Script (Local Simba Script)
  public
    AutoCheckUpdates : boolean;
    OnlineScript : TSimbaScript;
    procedure LoadFromFile(const filename : string);
    function LoadFromName(const ScriptName,maindir : string) : boolean; //Maindir = maindir of ScriptManager
    procedure SaveToFile(const FileName : string);
    procedure Save(const MainDir : string); //MainDir = maindir of ScriptManager
    constructor create;
  end;

  { TScriptManager }

  TScriptManager = class (TObject)
  private
    FMaindir: string;
    FScripts : TList; //Array of the online scripts
    FLScripts: TList; //Array of the local scripts
    FVersion : String;
    FUpdating : boolean;
    function GetLScriptCount: integer;
    function GetMainDir: string;
    function GetScript(index : integer): TSimbaScript;
    function GetScriptCount: integer;
    procedure MatchLocalOnline;
  public
    function FindScriptByName(name : string) : Integer;
    function FindLScriptByName(name : string) : Integer;
    property MainDir : string read GetMainDir write FMaindir;
    property SimbaScript[index : integer] : TSimbaScript read GetScript;
    procedure Update; //Gets the online scripts
    procedure LUpdate; //Loads the local scripts, uses MainDir
    function NewVersion(Script : integer) : boolean; //Checks for updates for Script
    procedure InstallNewScript(Script : integer); //Installs Script (Online -> Local)
    function UpdateScript(Script : integer; ignoreupdating : boolean = false) : boolean; //Updates all the info/files of local script
    procedure LSave; //Saves the local scripts, uses MainDir
    property LScriptCount : integer read GetLScriptCount; //LScript = Local Script = Installed Script
    property ScriptCount : integer read GetScriptCount; //Online script
    property Version : string read FVersion;
    constructor Create;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ListView1: TListView;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ClickItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    Mng : TScriptManager;
  public
    { public declarations }
  end;




var
  Form1: TForm1; 

implementation

uses
  XMLRead,XMLWrite;
{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Mng := TScriptManager.Create;
  ListView1.Columns.Add.Width:= ClientWidth;
end;

procedure TForm1.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Script : TSimbaScript;
begin
  if Item.Data <> nil then
  begin
    Memo1.Clear;
    Script := TSimbaScript(Item.data);
    Memo1.Lines.add('Name: ' + Script.Name);
    Memo1.lines.add('Author: ' + Script.Author);
    Memo1.Lines.add('Version: ' + Script.Version);
    Memo1.Lines.add('Installed: '+ BoolToStr(Script.Installed,true));
    Memo1.Lines.add('Description: ' + Script.Description);
  end;
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
{  form1.Memo1.Lines.Clear();
  form1.Memo1.Lines.Append(TSimbaScript(item.data).Description);

  if Button = mbLeft then
  begin

  end else if Button = mbRight then
  begin
    { Popup Actions }
    Form1.ScriptPopup.Items[0].Caption:= 'Install ' +  TSimbaScript(item.data).Name;
    Form1.ScriptPopup.PopUp();
  end;
  //form1.Memo1.Text := TSimbaScript(item.data).Description; }
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i : integer;
  Item : TListItem;
begin
  Mng.Update;
  ListView1.Items.Clear;
  for i := 0 to Mng.ScriptCount - 1 do
  begin
    Item := ListView1.Items.Add;
    Item.Data:= Mng.SimbaScript[i];
    Item.Caption:= Mng.SimbaScript[i].Name;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Script : TSimbaScript;
begin
  if (ListView1.Selected <> nil) and (ListView1.Selected.Data <> nil) then
  begin
    Script := TSimbaScript(ListView1.Selected.Data);
    Mng.InstallNewScript(mng.FindScriptByName(Script.Name));
  end;
end;

{ TSimbaScript }

function TSimbaScript.IsInstalled: boolean;
begin
  Result := (LocalScript <> nil);
end;

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
  URL := 'http://old.villavu.com/sm/scripts/'+Name+'.tar.bz2';
end;

procedure TSimbaScript.Dbg;
var
  i : integer;
begin
  Writeln(Name);
  Writeln('  Author: ' + Author);
  Writeln('  Version: ' + Version);
  Writeln('  Description: ' + Description);
  Writeln('  Installed: '+ BoolToStr(Installed,true));
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

function TScriptManager.FindScriptByName(name: string): Integer;
var
  I : integer;
begin
  for i := FScripts.Count - 1 downto 0 do
    if TSimbaScript(FScripts[i]).Name = Name then
      exit(i);
  result := -1;
end;

function TScriptManager.FindLScriptByName(name: string): Integer;
var
  I : integer;
begin
  for i := FScripts.Count - 1 downto 0 do
    if TLSimbaScript(FLScripts[i]).Name = Name then
      exit(i);
  result := -1;
end;

function TScriptManager.GetLScriptCount: integer;
begin
  result := FLScripts.Count;
end;

function TScriptManager.GetMainDir: string;
begin
  result := IncludeTrailingPathDelimiter(FMainDir);
end;

function TScriptManager.GetScript(index : integer): TSimbaScript;
begin
  result := TSimbaScript(FScripts[index]);
end;

function TScriptManager.GetScriptCount: integer;
begin
  result := FScripts.Count;
end;

procedure TScriptManager.MatchLocalOnline;
var
  Scrpt : TLSimbaScript;
  I,II : integer;
begin
  for ii := 0 to LScriptCount - 1 do
  begin
    Scrpt := TLSimbaScript(FLScripts[ii]);
    if Scrpt.OnlineScript = nil then
      for i := 0 to ScriptCount-1 do
        if TSimbaScript(FScripts[i]).Name = Scrpt.Name then
        begin
          Scrpt.OnlineScript := TSimbaScript(FScripts[i]);
          Break;
        end;
    Scrpt.OnlineScript.LocalScript := Scrpt;
  end;
end;

procedure TScriptManager.Update;
var
  XMLFile : string;
  Stream  : TStringStream;
  XMLDoc  : TXMLDocument;
  Node,Script : TDOMNode;
  Subs : TStringList;
  Down : TDownloadThread;
  SScript : TSimbaScript;
  I : integer;
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
      SScript := TSimbaScript.Create;
      SScript.LoadFromNode(Script);
      SScript.Dbg;
      i := FindScriptByName(SScript.Name);
      if (i = -1) then
        FScripts.Add(SScript)
      else
      begin
        SScript.free;
        TSimbaScript(FScripts[i]).LoadFromNode(Script);
      end;
      Script := Script.NextSibling;
    end;
  end;
  XMLDoc.Free;
  FUpdating := false;
  MatchLocalOnline;
end;

procedure TScriptManager.LUpdate;
var
  XMLDoc  : TXMLDocument;
  Node,Script : TDOMNode;
  Subs : TStringList;
  Down : TDownloadThread;
  SScript : TLSimbaScript;
  I : integer;
begin
  if DirectoryExists(MainDir) = false then
    exit;
  if FileExists(maindir + 'General' + DirectorySeparator+  'scripts.xml') then
  begin
    ReadXMLFile(XMLDoc,maindir + 'General' + DirectorySeparator+ 'scripts.xml');
    Node := XMLDoc.FirstChild.FindNode('Scripts');
    if node <> nil then
    begin
      script := Node.FirstChild;
      while Script <> nil do
      begin
        SScript := TLSimbaScript.Create;
        SScript.LoadFromName(Node.TextContent,maindir);
        i := FindLScriptByName(SScript.Name);
        SScript.Dbg;
        if (i = -1) then
          FLScripts.Add(SScript)
        else
        begin
          SScript.free;
          TLSimbaScript(FLScripts[i]).LoadFromName(Node.TextContent,maindir);
        end;
        Script := Script.NextSibling;
      end;
    end;
    XMLDoc.Free;
  end;
  MatchLocalOnline;
end;

function TScriptManager.NewVersion(Script: integer): boolean;
begin
  MatchLocalOnline;
  with TLSimbaScript(FLScripts[Script]) do
    result := OnlineScript.Version <> Version;
end;

procedure TScriptManager.InstallNewScript(Script: integer);
var
  Scrpt : TSimbaScript;
  LScrpt: TLSimbaScript;
  Dir : string;
begin
  if FUpdating then
    exit;
  FUpdating := true;
  Scrpt := TSimbaScript(FScripts[Script]);
  LScrpt := TLSimbaScript.create;
  FLScripts.Add(LScrpt);
  LScrpt.Name:= Scrpt.Name;
  LScrpt.OnlineScript := Scrpt;
  Dir := MainDir + LScrpt.Name + DirectorySeparator;
  if DirectoryExists(dir) then
    Writeln('Directory already exists, yet continue?');
  if not CreateDir(Dir) then
    Writeln('Failed to create dir..');
  UpdateScript(FLScripts.Count - 1,true);
end;

function TScriptManager.UpdateScript(Script: integer; ignoreupdating : boolean = false) : boolean;
var
  LScrpt : TLSimbaScript;
  Scrpt : TSimbaScript;
  DownloadThread : TDownloadDecompressThread;
begin
  Result := true;
  if not NewVersion(Script) then
    Exit;
  if FUpdating and not ignoreupdating then
    exit;
  FUpdating := true;
  LScrpt := TLSimbaScript(FLScripts[Script]);
  Scrpt := LScrpt.OnlineScript;
  with LScrpt do
  begin
    Version:= Scrpt.Version;
    Name:= Scrpt.Name;
    Author := Scrpt.Author;
    Description:= Scrpt.Version;
    Tags.Assign(Scrpt.Tags);
    Files.Assign(Scrpt.Files);
    URL := 'http://old.villavu.com/sm/scripts/'+name+ '.tar.bz2';
  end;
  LScrpt.Save(MainDir);      //Saves the setting file, now we only need to update the files
  DownloadThread := TDownloadDecompressThread.Create(LScrpt.URL,MainDir + LScrpt.Name + DS,true);
  DownloadThread.execute;
  while DownloadThread.Done = false do
  begin
    Application.ProcessMessages;
    Sleep(25);
  end;
  Result := DownloadThread.Succeeded;
  DownloadThread.Free;
  LSave; //Update the scripts XML file
  FUPdating := false;
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
  WriteXMLFile(XMLDoc,maindir + 'General' + DirectorySeparator+  'scripts.xml');
  XMLDoc.Free;
end;

constructor TScriptManager.Create;
begin
  inherited;
  FLScripts := TList.Create;
  FScripts := TList.Create;
  FVersion := '';
  FUpdating:= False;
  FMainDir:= ExtractFileDir(Application.ExeName);
  CreateDir(MainDir + 'General');
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
  XMLDoc.Free;
end;

function TLSimbaScript.LoadFromName(const ScriptName,MainDir : string) : boolean;
begin
  Result := false;
  if FileExists(MainDir + 'General' + DirectorySeparator + ScriptName +
                'info.xml') then
  begin
    Result := true;
    LoadFromFile(MainDir + 'General' + DirectorySeparator + ScriptName +
                'info.xml');

  end;
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
  AddTextElement(node,'AutoCheckUpdates', BoolToStr(AutoCheckUpdates,true));
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
  if not DirectoryExists(MainDir + 'General' + DirectorySeparator + Name) then
    CreateDir(MainDir + 'General' + DirectorySeparator + Name);
  SaveToFile(MainDir + 'General' + DirectorySeparator + Name + DirectorySeparator +
             'info.xml');
end;

constructor TLSimbaScript.create;
begin
  inherited;
  Tags := TStringList.Create; //Might leak, but careface
  Files := TStringList.create; //Same ^
  AutoCheckUpdates:= true;
end;

end.
