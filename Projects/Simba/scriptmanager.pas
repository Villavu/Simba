unit ScriptManager;

{$mode objfpc}

interface

uses
  Classes,  LResources, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, Menus, Buttons, StdCtrls, Grids,

  {$IFDEF UNIX}cthreads,cmem,{$ENDIF} ExtCtrls, ActnList, settings,
  updater, strutils, MufasaTypes, dom, mmisc;


Type

{ TSimbaScript }

  TSimbaScript = class(TObject)
  private
    function IsInstalled: boolean;
    procedure LoadFromNode( Script : TDOMNode);
  public
    Name, Version, Author, Description, URL: String;
    Tags, Files: TStringList;
    LocalScript : TSimbaScript;
//    property Installed : boolean read IsInstalled;
    Installed : boolean;
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
    FRScripts : TList; //Array of the online scripts
    FLScripts : TList; //Array of the local scripts
    FRVersion : String;
    FUpdating : boolean;
    function GetLScriptCount: integer;
    function GetMainDir: string;
    function GetScript(index : integer): TSimbaScript;
    function GetRScriptCount: integer;
    function GetLScriptByName(name: string): TLSimbaScript;
    function isNewerVersion(ver1, ver2: string): boolean;
    procedure AppendRemoteDB(url: string);
  public
    function FindRScriptByName(name : string) : Integer;
    function FindLScriptByName(name : string) : Integer;
    property MainDir : string read GetMainDir write FMaindir;
    property SimbaScript[index : integer] : TSimbaScript read GetScript;
    procedure RUpdate; //Gets the online scripts
    procedure LUpdate; //Loads the local scripts, uses MainDir
    function NewVersion(Script : integer) : boolean; //Checks for updates for Script
    procedure InstallNewRScript(Script : integer); //Installs Script (Online -> Local)
    function UpdateLScript(Script : integer; ignoreupdating : boolean = false) : boolean; //Updates all the info/files of local script
    procedure LSave; //Saves the local scripts, uses MainDir
    property LScriptCount : integer read GetLScriptCount; //LScript = Local Script = Installed Script
    property RScriptCount : integer read GetRScriptCount; //Online script
    property Version : string read FRVersion;
    constructor Create;
    destructor Destroy; override;
  end;


  { TScriptManagerForm }

  TScriptManagerForm = class(TForm)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    ImageList1: TImageList;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Mng : TScriptManager;
  public
    { public declarations }
  end; 

var
  ScriptManagerForm: TScriptManagerForm;

implementation

  uses
    XMLRead,XMLWrite;


  {$R *.lfm}

  { TScriptManagerForm }

procedure TScriptManagerForm.FormCreate(Sender: TObject);
begin
  Mng := TScriptManager.Create;
end;

procedure TScriptManagerForm.FormShow(Sender: TObject);
var
  i, l: integer;
begin
  Mng.LUpdate;
  Mng.RUpdate;
  StringGrid1.RowCount := 1;
  for i := 0 to Mng.RScriptCount - 1 do
  begin
    StringGrid1.RowCount := i + 2;
    StringGrid1.Cells[0, i + 1] := Mng.SimbaScript[i].Name;
    StringGrid1.Cells[1, i + 1] := Mng.SimbaScript[i].Version;
    StringGrid1.Cells[2, i + 1] := Mng.SimbaScript[i].Author;
    StringGrid1.Cells[3, i + 1] := Mng.SimbaScript[i].Description;
  end;
end;





procedure TScriptManagerForm.Edit1Change(Sender: TObject);
var
  i, c: Integer;
begin
  StringGrid1.RowCount := 1;
  c := 0;
  for i := 0 to Mng.RScriptCount - 1 do
  begin
    if AnsiContainsText(Mng.SimbaScript[i].Name, Edit1.Caption) or AnsiContainsText(Mng.SimbaScript[i].Author, Edit1.Caption) or (Edit1.Caption = '') or (Edit1.Caption = 'Search') then
    begin
      StringGrid1.RowCount := c + 2;

      StringGrid1.Cells[0, c + 1] := Mng.SimbaScript[i].Name;
      StringGrid1.Cells[1, c + 1] := Mng.SimbaScript[i].Version;
      StringGrid1.Cells[2, c + 1] := Mng.SimbaScript[i].Author;
      StringGrid1.Cells[3, c + 1] := Mng.SimbaScript[i].Description;
      c := c + 1;
    end;
  end;
end;

procedure TScriptManagerForm.Edit1Enter(Sender: TObject);
begin
  Edit1.Caption := '';
end;

procedure TScriptManagerForm.Edit1Exit(Sender: TObject);
begin
  if Edit1.Caption = '' then
    Edit1.Caption := 'Search'
end;



 // Install Script
{procedure TScriptManagerForm.Button2Click(Sender: TObject);
var
  Script : TSimbaScript;
begin
  {if (ListView1.Selected <> nil) and (ListView1.Selected.Data <> nil) then
  begin
    Script := TSimbaScript(ListView1.Selected.Data);
    if Script.IsInstalled then
    begin
//      ShowMessage('Updating Script "' + Script.Name + '"');
      Mng.UpdateLScript(mng.FindRScriptByName(Script.Name));
      ShowMessage('Finished Updating Script "' + Script.Name + '"');
    end else
    begin
//      ShowMessage('Installing Script "' + Script.Name + '"');
      Mng.InstallNewRScript(mng.FindRScriptByName(Script.Name));
      ShowMessage('Finished Installing Script "' + Script.Name + '"');
    end;
  end;       }
end;          }

{ TSimbaScript }

function TSimbaScript.IsInstalled: boolean;
begin
  Result := Self.Installed;
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
    begin
      Result.Free;
      exit;
    end;
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
  Installed := False;
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

function TScriptManager.GetLScriptByName(name: string): TLSimbaScript;
var
  I : integer;
  Scr : TLSimbaScript;
begin
  result := nil;
  for i := FLScripts.Count - 1 downto 0 do
    scr := TLSimbaScript(FLScripts[i]);
    if scr.Name = Name then
      result := scr;
      exit();
end;

function TScriptManager.FindRScriptByName(name: string): Integer;
var
  I : integer;
begin
  for i := FRScripts.Count - 1 downto 0 do
    if TSimbaScript(FRScripts[i]).Name = Name then
      exit(i);
  result := -1;
end;

function TScriptManager.FindLScriptByName(name: string): Integer;
var
  I : integer;
begin
  for i := FLScripts.Count - 1 downto 0 do
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
  result := TSimbaScript(FRScripts[index]);
end;

function TScriptManager.GetRScriptCount: integer;
begin
  result := FRScripts.Count;
end;

procedure TScriptManager.RUpdate;
var
  Databs : TStringList;
  X : integer;
begin
  Databs := TStringList.Create;
  Databs.Add('http://old.villavu.com/sm');
  //Databs.Add('http://tootoot222.hopto.org:8080/~mcteo/scriptman2/scripts.xml');
  //Databs.Add('http://tootoot222.hopto.org:8080/~mcteo/secretrepo/scripts.cgi?user=user&pass=pass');

  //TODO: Load list of repositories

//  ShowMessage(SettingsForm.Settings.GetKeyValueDefLoad('Settings/SourceEditor/DefScriptPath', ,SimbaSettingsFile));
//  ShowMessage(LoadSettingDef('Settings/SourceEditor/DefScriptPath', ExpandFileName(MainDir+DS+'default.simba')));
  FRScripts.Clear();
  //Form1.Label1.Caption := 'Updating from Repository';
  for X := 0 to Databs.Count-1 do
  begin
    //Form1.Label1.Caption := 'Updating from Repository ' + inttostr(X+1) +
    //                                '/' + inttostr(Databs.Count);
    AppendRemoteDB(Databs[X]);
  end;
  Databs.Free;

  //Form1.Label1.Caption := 'Finished updating from Repository';
end;

{ Downloads online scripts.xml and parses it, populating FRScripts }
procedure TScriptManager.AppendRemoteDB(url: string);
var
  XMLFile : string;
  Stream  : TStringStream;
  XMLDoc  : TXMLDocument;
  Node,Script : TDOMNode;
  Subs : TStringList;
  Down : TDownloadThread;
  SScript : TSimbaScript;
  LScr : TLSimbaScript;
  I : integer;
begin
  if FUpdating then
    exit;
  FUpdating := True;
  Down := TDownloadThread.Create('http://old.villavu.com/sm',@XMLFile);
//  Down := TDownloadThread.Create(url, @XMLFile);
//  Down := TDownloadThread.Create('http://tootoot222.hopto.org:8080/~mcteo/scriptman/scripts.xml',@XMLFile);
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
    FRVersion:= Node.TextContent;
  Node := XMLDoc.FirstChild.FindNode('ScriptList');
  if node <> nil then
  begin
    script := Node.FirstChild;
    WriteLn('loading remote script from online db');
    while Script <> nil do
    begin
      SScript := TSimbaScript.Create;
      SScript.LoadFromNode(Script);
      i := FindRScriptByName(SScript.Name);
      if (i = -1) then  // if no remote scripts with same name
      begin
        i := FindLScriptByName(SScript.Name);
        if (i = -1) then  // if no local scripts with same name
        begin
          FRScripts.Add(SScript);
        end else  // there is a local script with same name
        begin
          LScr := GetLScriptByName(SScript.Name);
          if (LScr <> nil) then
          begin
            if isNewerVersion(SScript.Version, LScr.Version) then // if local script is older
            begin
              SScript.Installed := True;
              FRScripts.Add(SScript); // add newer version of script
            end;
          end;
        end;
      end else
      begin
        SScript.free;
//        TSimbaScript(FScripts[i]).LoadFromNode(Script);
      end;
      Script := Script.NextSibling;
    end;
    WriteLn('finished loading remote script from online db');
  end;
  XMLDoc.Free;
  FUpdating := false;
//  MatchLocalOnline;
end;

{ Opens local scripts.xml and parses it, populating FLScripts }
procedure TScriptManager.LUpdate;
var
  XMLDoc  : TXMLDocument;
  Node,Script,tmpNode : TDOMNode;
  Subs : TStringList;
  Down : TDownloadThread;
  SScript : TLSimbaScript;
  I : integer;
begin
  if DirectoryExists(MainDir) = false then
    exit;
  WriteLn('Reading XML Config file from "' + maindir + 'General' + DirectorySeparator+ 'scripts.xml"');
  if FileExists(maindir + 'General' + DirectorySeparator+  'scripts.xml') then
  begin
    ReadXMLFile(XMLDoc,maindir + 'General' + DirectorySeparator+ 'scripts.xml');
//    Node := XMLDoc.FirstChild.FindNode('ScriptList');
    Node := XMLDoc.FindNode('ScriptList');
    if node <> nil then
    begin
      FLScripts.Clear();
      script := Node.FirstChild;
      WriteLn('loading local script from local db');
      while script <> nil do
      begin
        SScript := TLSimbaScript.Create;
        tmpNode := script.FindNode('Name');
        if tmpNode <> nil then
        begin
          SScript.LoadFromName(Trim(tmpNode.TextContent), maindir);
          i := FindLScriptByName(SScript.Name);
          if (i = -1) then
          begin
            FLScripts.Add(SScript);
          end
          else
          begin
            SScript.free;
            TLSimbaScript(FLScripts[i]).LoadFromName(Node.TextContent,maindir);
          end;
        end;
        script := script.NextSibling;
      end;
      WriteLn('finished loading local scripts from local db');
    end;
    XMLDoc.Free;
  end;
end;

{ Compares versions and returns true if ver1 > ver2 }
function TScriptManager.isNewerVersion(ver1, ver2: string): boolean;
var
  v1, v2: Extended;
begin
  v1 := StrToFloat(ver1);
  v2 := StrToFloat(ver2);
  result := (v1 > v2);
end;

function TScriptManager.NewVersion(Script: integer): boolean;
begin
//  MatchLocalOnline;
  with TLSimbaScript(FLScripts[Script]) do
    result := OnlineScript.Version <> Version;
end;

{ Sets up some vars/dirs and hands installation to update script func }
procedure TScriptManager.InstallNewRScript(Script: integer);
var
  Scrpt : TSimbaScript;
  LScrpt: TLSimbaScript;
  Dir : string;
begin
  if FUpdating then
    exit;
  FUpdating := true;
  Scrpt := TSimbaScript(FRScripts[Script]);
  LScrpt := TLSimbaScript.create;
  LScrpt.Name:= Scrpt.Name;
  LScrpt.OnlineScript := Scrpt;
  Dir := MainDir + LScrpt.Name + DirectorySeparator;
  if DirectoryExists(dir) then
    Writeln('Directory already exists, yet continue?');
  if not CreateDir(Dir) then
    Writeln('Failed to create dir..');
  FLScripts.Add(LScrpt);
  UpdateLScript(FLScripts.Count - 1, true);
end;

{ Downloads script files and puts them in their own directory,
 then triggers updating of local scripts.xml to match FLScripts }
function TScriptManager.UpdateLScript(Script: integer; ignoreupdating : boolean = false) : boolean;
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
    Description:= Scrpt.Description;
    Tags.Assign(Scrpt.Tags);
    Files.Assign(Scrpt.Files);
    URL := 'http://old.villavu.com/sm/scripts/'+name+ '.tar.bz2';
 //   URL := 'http://tootoot222.hopto.org:8080/~mcteo/scriptman/'+name+'.tar.bz2';
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

{ Updates local scripts.xml to match FLScripts }
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
  Node, ChildNode : TDOMNode;
  i : integer;
begin
  if DirectoryExists(MainDir) = false then
    exit;
  XMLDoc := TXMLDocument.Create;
  Node := XMLDoc.CreateElement('ScriptList');
  XMLDoc.AppendChild(node);
  for i := 0 to FLScripts.Count - 1 do
  begin
    TLSimbaScript(FLScripts[i]).Dbg;
    ChildNode := XMLDoc.CreateElement('Script');
    AddTextElement(childnode,'Name', TLSimbaScript(FLScripts[i]).Name);
    Node.AppendChild(ChildNode);
  end;
  WriteXMLFile(XMLDoc,maindir + 'General' + DirectorySeparator+  'scripts.xml');
  XMLDoc.Free;
end;

constructor TScriptManager.Create;
begin
  inherited;
  FLScripts := TList.Create;
  FRScripts := TList.Create;
  FRVersion := '';
  FUpdating:= False;
  FMainDir:= ExtractFileDir(Application.ExeName);
  CreateDir(MainDir + 'General');
end;

destructor TScriptManager.Destroy;
begin
  while FRScripts.Count > 0 do
  begin
    TSimbaScript(FRScripts[0]).Free;
    FRScripts.Delete(0);
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

function TLSimbaScript.LoadFromName(const ScriptName, MainDir : string) : boolean;
begin
  Result := false;
  if FileExists(MainDir + 'General' + DirectorySeparator + ScriptName +
                        DirectorySeparator + 'info.xml') then
  begin
    Result := true;
    LoadFromFile(MainDir + 'General' + DirectorySeparator + ScriptName +
                         DirectorySeparator + 'info.xml');

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
  AddTextElement(node,'Description',Description);
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

initialization
  {$I scriptmanager.lrs}

end.

