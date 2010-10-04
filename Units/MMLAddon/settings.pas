{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    Settings class for Mufasa Macro Library
}

unit settings;

{$mode objfpc}{$M+}

interface

uses
  Classes, SysUtils, ComCtrls, xmlread, xmlwrite, DOM,mufasatypes,MufasaBase;



type
  TSettingData = class(TObject)
    public
      Val: String;

      constructor Create;
      destructor Destroy; override;

  end;

  {
    TMMLSettings; class to manage settings with XML.
        Features:
            - Loading and Saving to XML.
            - Showing the settings as a tree.
            - Changing, Creating settings. (No Deleting yet.)
            - Bad naming conventions.
        Bugs:
            - Don't use '/' as a *name* for a node. It WILL fuck up.
                It is no problem in values, but in NAMES for nodes, it will
                simply not work.
            - Poor naming.
  }

  TMMLSettings = class(TObject)
  private
    Nodes: TTreeNodes;
    function KeyNameToKeys(KeyName: String): TStringArray;

    procedure InternalLoadFromXML(XMLDoc: TXMLDocument);
    procedure WriteXMLData(n: TTreeNode;
                     XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                     var XMLChild: TDOMNode; var C: Integer);
    procedure WalkTree(Node: TTreeNode; XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                 var C: Integer);
    function GetKeyValueDefLoadFirst(KeyName, defVal, fileName: String): String;
  public
    constructor Create(aNodes: TTreeNodes);
    destructor Destroy; override;

    function WalkToNode(KeyName: String): TTreeNode;
    function GetNodePath(Node: TTreeNode): String;
    function ListKeys(KeyName: String; out Keys :TStringArray) : boolean;

    function KeyExists(KeyName: String): Boolean;
    function IsKey(KeyName: String): Boolean;
    function IsDirectory(KeyName: String): Boolean;

    function DeleteKey(KeyName: String): Boolean;
    function DeleteSubKeys(KeyName: String): Boolean;

    function CreateKey(KeyName: String; CreatePath: Boolean = True): Boolean;
    function SetKeyValue(KeyName: String; KeyValue: String; CreatePath : boolean = true) : boolean;
    function GetKeyValue(KeyName: String): String;
    function GetKeyValueDef(KeyName, defVal: String): String;
    function RenameKey(oldKey,newKey : string) : boolean;
    function GetKeyValueDefLoad(KeyName, defVal, fileName: String): String;

    function LoadFromXML(fileName: String): Boolean;
    function SaveToXML(fileName: String): Boolean;
  end;

implementation
uses
  stringutil,fileutil;

constructor TSettingData.Create;
begin
  inherited;
  Val := '';
end;

destructor TSettingData.Destroy;
begin
  Val := '';
  inherited;
end;


constructor TMMLSettings.Create(aNodes: TTreeNodes);
begin
  inherited Create;
  Self.Nodes := aNodes;
end;

destructor TMMLSettings.Destroy;
var
  i : integer;
begin
  for i := 0 to Nodes.Count - 1 do
    if Nodes[i].data <> nil then
      TSettingData(Nodes[i].Data).Free;
  Nodes := nil;

  inherited;
end;

procedure TMMLSettings.InternalLoadFromXML(XMLDoc: TXMLDocument);
var
  iNode: TDOMNode;

  procedure ProcessNode(Node: TDOMNode; TreeNode: TTreeNode);
  var
    cNode: TDOMNode;
    s: string;
    d: TSettingData;

  begin
    if Node = nil then Exit; // Stops if reached a leaf

    // Adds a node to the tree
    s := Node.NodeName;

    TreeNode := Nodes.AddChild(TreeNode, s);
    if (Node.NodeType = 3) then
    begin
      d := TSettingData.Create;
      D.Val := Node.NodeValue;
      TreeNode.Data := D;

      TreeNode.Text := Node.NodeValue;
    end;
    // Goes to the child node
    cNode := Node.FirstChild;

    // Processes all child nodes
    while cNode <> nil do
    begin
      ProcessNode(cNode, TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  iNode := XMLDoc.DocumentElement;
  if iNode.NodeName = 'Simba' then
    iNode := iNode.ChildNodes[0];
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil); // Recursive
    iNode := iNode.NextSibling;
  end;
end;

function TMMLSettings.KeyNameToKeys(KeyName: String): TStringArray;
begin
  Result := Explode('/',KeyName);
end;

function TMMLSettings.WalkToNode(KeyName: String): TTreeNode;
var
  N: TTreeNode;
  i: Integer;
  S: TStringArray;
begin
  Result := nil;

  if KeyName[length(KeyName)]='/' then setlength(KeyName,length(KeyName)-1);
  S := KeyNameToKeys(KeyName);

  if not assigned(s) then
    Exit(nil);

  N := Nodes.GetFirstNode;
  i := 0;
  while N <> nil do
  begin
    if N.Text = s[i] then
    begin
      inc(i);
      if i = length(s) then
        break;
      N := N.GetFirstChild;
    end else
      N := N.GetNextSibling;
  end;
  Result := N;
end;

{
    Return the "path" of the given Node.
    The node should be in Nodes. (TreeView.Items)
}

function TMMLSettings.GetNodePath(Node: TTreeNode): String;
var
  N: TTreeNode;
  s: TStringArray;
  i: Integer;
begin
  if Node = nil then
    Exit('');

  N := Node;
  setlength(s, 0);
  while N <> nil do
  begin
    setlength(s,length(s) + 1);
    s[high(s)] := N.Text;
    N := N.Parent;
  end;

  result := '';
  for i := high(s) downto 0 do
    result := result + s[i] + '/';
  if length(result) > 1 then
    setlength(result,length(result)-1);
end;

{
    Equivalent to 'ls' or 'dir'. It lists the keys in a certain key (directory)
}

function TMMLSettings.ListKeys(KeyName: String; out Keys: TStringArray): boolean;
var
  N: TTreeNode;
begin
  SetLength(Keys, 0);
  N := WalkToNode(KeyName);
  if N <> nil then
    N := N.GetFirstChild;
  result := n <> nil;
  while N <> nil do
  begin
    setlength(Keys,length(Keys)+1);
    Keys[high(Keys)] := N.Text;
    N := N.GetNextSibling;
  end;
end;

{
    Return wether the given key exists or not
}

function TMMLSettings.KeyExists(KeyName: String): Boolean;

begin
  Result := WalkToNode(KeyName) <> nil;
end;

{
    Return wether the given key is a key. (again, bad naming)
    What I mean is, a 'key' only has a 'Value', which a 'directory key' has other
    keys (or none) as childs.
}

function TMMLSettings.IsKey(KeyName: String): Boolean;
var
  N: TTreeNode;

begin
  N := WalkToNode(KeyName);
  if N = nil then
    Exit(False);

  n := N.GetFirstChild;
  while n <> nil do
  begin
    if n.HasChildren then
      exit(false);
    n := N.GetNextSibling;
  end;
  exit(true);
end;

{
    Perhaps this should just do Exit(not IsKey(KeyName))
}

function TMMLSettings.IsDirectory(KeyName: String): Boolean;
var
  N: TTreeNode;
begin
  N := WalkToNode(KeyName);
  if N <> nil then
    Exit(N.HasChildren);
  Exit(False);
end;

{
    Get the value of a Key. (String)
}

function TMMLSettings.GetKeyValue(KeyName: String): String;
var
    N: TTreeNode;
begin
  if not KeyExists(KeyName) then
    Exit('');
  N := WalkToNode(KeyName);
  if N <> nil then
    N := N.GetFirstChild;
  while N <> nil do
  begin
    if assigned(n.Data) then
    begin
      Exit(TSettingData(n.Data).Val);
    end;
    N := N.GetNextSibling;
  end;
  Exit('');
end;

{
  newKey is not the entire key path.
}
function TMMLSettings.RenameKey(oldKey, newKey: string): boolean;
var
  Node : TTreeNode;
  Path : TstringArray;
  newstr : string;
  i : integer;
begin
  result := false;
  writeln(oldkey);
  Path := KeyNameToKeys(OldKey);
  if length(path) = 1 then
    exit;

  if pos('/',newKey) > 0 then
    exit;

  if not KeyExists(oldkey) then
    exit;

{  for i := length(oldKey) downto 1 do
    if oldkey[i] = '/' then
    begin
      newstr:= Copy(oldkey,1,i) + NewKey;
      break;
    end;
  if KeyExists(NewKey) then
    exit;   }
  Node := WalkToNode(oldKey);
  Node.Text:= NewKey;
  result := true;
end;

{
    If the key exists - return the value.
    If it does not exist, create the key - with a possible path, set it to
    defVal and return defVal.
}

function TMMLSettings.GetKeyValueDef(KeyName, defVal: String): String;
var
    Res: String;
begin
  if not IsKey(KeyName) then
  begin
    CreateKey(KeyName, True);
    SetKeyValue(KeyName, defVal);
    exit(defVal);
  end;
  Res := GetKeyValue(KeyName);
  if Res = '' then
  begin
    SetKeyValue(KeyName, defVal);
    exit(defVal);
  end;
  Exit(Res);
end;

function TMMLSettings.DeleteKey(KeyName: String): Boolean;
var
    Node, C: TTreeNode;
begin
  if not isKey(KeyName) and not IsDirectory(KeyName) then
    exit(false);
  Node := WalkToNode(KeyName);

  if Node = nil then  // This should not happen
    exit;

  C := Node.GetFirstChild;
  if C = nil then
  begin
    TSettingData(Node.Data).Free;
    Node.Delete;
    exit;
  end;

  While C <> nil do
  begin
    TSettingData(C.Data).Free;
    C := C.GetNextSibling;
  end;
  Node.DeleteChildren;

  TSettingData(Node.Data).Free;
  Node.Delete;
end;

function TMMLSettings.DeleteSubKeys(KeyName: String): Boolean;
var
    Node, C: TTreeNode;
begin
  if not isKey(KeyName) and not IsDirectory(KeyName) then
    exit(false);
  Node := WalkToNode(KeyName);
  if Node = nil then  // This should not happen
    exit;

  C := Node.GetFirstChild;
  While C <> nil do
  begin
    TSettingData(C.Data).Free;
    C := C.GetNextSibling;
  end;

  Node.DeleteChildren;
end;

{
    Clear the entire tree. Load from fileName. call GetKeyValueDef.
}

function TMMLSettings.GetKeyValueDefLoadFirst(KeyName, defVal, fileName: String): String;
begin
  Nodes.Clear;
  LoadFromXML(fileName);
  Result := GetKeyValueDef(KeyName, defVal);
  SaveToXML(fileName);
end;

{
    If Key exists, call GetKeyValueDef, else call GetSetLoadSaveDefaultKeyValue
}
function TMMLSettings.GetKeyValueDefLoad(KeyName, defVal, fileName: String): String;
begin
  if KeyExists(KeyName) then
    Exit(GetKeyValueDef(KeyName, defVal))
  else
    Exit(GetKeyValueDefLoadFirst(KeyName, defVal, fileName));
end;

{
    Create the given key. If CreatePath = true, then create every key that is
    required to create the key. (Say KeyName = 'a/b/c/d/e' and only key a exists,
    and CreatePath = True, then b,c,d and e are all created.
}

function TMMLSettings.CreateKey(KeyName: String; CreatePath: Boolean = True): Boolean;
var
    N, newN, nParent: TTreeNode;
    Path: TStringArray;
    NewPath: String;
    i: Integer;

begin
  if KeyExists(KeyName) then
  begin
    Exit(False);
  end;
  NewPath := '';
  N := nil;
  Path := KeyNameToKeys(KeyName);

  if length(path) < 2 then
  begin
    mDebugLn('CreateKey - Path too short!');
    exit(false);
  end;
  nParent := WalkToNode(path[0]);
  if nParent = nil then
    nParent := Nodes.Add(nil,path[0]);
 { if path[0] <> nParent.Text then
  begin
    writeln('First key doesn''t match. First key should always match');
    exit(false);
  end;}
  for i := 0 to length(Path) - 2 do
  begin
    if Path[i] = '' then
    begin
      mDebugLn('CreateKey - Invalid Key Path / Name');
      exit(false);
    end;
    NewPath := NewPath + Path[i] + '/';
    N := WalkToNode(NewPath);

    if (N = nil) and (not CreatePath) then
      exit(false);

    if (N = nil) and CreatePath then
    begin
      newN := TTreeNode.Create(Nodes);
      newN.Text := Path[i];
      if (nParent = nil) then
      begin
        mDebugLn('CreateKey - This shouldn''t happen...');
        newN.MoveTo(Nodes.GetFirstNode, naAddChild);
        nParent := newN;
      end
      else
      begin
        newN.MoveTo(nParent, naAddChild);
        nParent := newN;
      end;
    end;

    if N <> nil then
      nParent := N;
  end;

  if nParent = nil then
    exit(false);

  newN := TTreeNode.Create(Nodes);
  newN.Text := Path[High(Path)];
  newN.MoveTo(nParent, naAddChild);
end;

{
    Set the value of a key.
}

function TMMLSettings.SetKeyValue(KeyName: String; KeyValue: String; CreatePath: boolean) : boolean;
var
    N, NN: TTreeNode;
begin
  result := false;
  if not KeyExists(KeyName) and not CreatePath then
  begin
    mDebugLn('SetKeyValue - Key does not exist');
    Exit;
  end else
    if not KeyExists(keyname) then
      CreateKey(keyname);
  if not IsKey(KeyName) then
  begin
    mDebugLn('SetKeyValue - IsKey returned false');
    Exit;
  end;
  N := WalkToNode(KeyName);

  if not N.HasChildren then
  begin
    NN := TTreeNode.Create(Nodes);
    NN.Text := KeyValue;
    NN.MoveTo(N, naAddChild);
  end;

  if n <> nil then
    N := N.GetFirstChild;
   while N <> nil do
   begin
     if Assigned(N.Data) then
       TSettingData(N.Data).Free;
     N.Data := TSettingData.Create;
     TSettingData(N.Data).Val := KeyValue;
     mDebugLn('Setting ' + KeyName + ' to ' + KeyValue);
     N := N.GetNextSibling;
   end;
   result := true;
end;


{ load from xml }
function TMMLSettings.LoadFromXML(fileName: String): Boolean;
var
    Doc: TXMLDocument;
begin
  Nodes.Clear;
  if not fileExistsUTF8(fileName) then
  begin
    mDebugLn('SettingsFile hasn''t been created yet.');
    // create file.
    SaveToXML(fileName);
  end;

  try
    ReadXMLFile(Doc, utf8tosys(fileName));
    InternalLoadFromXML(Doc);
    Doc.Free;
    Result := True;
  except
    Result := False;
  end;

end;

procedure TMMLSettings.WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       var XMLChild: TDOMNode; var C: Integer);
begin
  if assigned(n.data) and (n.HasChildren) then
    mDebugLn('WriteXMLData - Has data and children! Please close simba and remove settings.xml. if problem persists, please report your settings.xml');
  if assigned(n.Data) then
  begin
    XMLChild := XMLDoc.CreateTextNode(TSettingData(N.Data).Val);
  end else
  begin
    XMLChild := XMLDoc.CreateElement(n.Text);
  end;
  Inc(C);
  XMLNode.AppendChild(XMLChild);
end;

procedure TMMLSettings.WalkTree(Node: TTreeNode; XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                   var C: Integer);
var
   N: TTreeNode;
   XMLChild: TDOMNode;

begin
  N := Node.GetFirstChild;

  while assigned(n) do
  begin
    WriteXMLData(n, XMLNode, XMLDoc, XMLChild, C);

    WalkTree(n, XMLChild, XMLDoc, C);
    n := n.GetNextSibling;
  end;
end;

{ save to xml }

function TMMLSettings.SaveToXML(fileName: String): Boolean;
var
   XMLDoc: TXMLDocument;
   Simba,DOMNode: TDOMNode;
   TreeNode : TTreeNode;
   C: Integer;
begin
  XMLDoc := TXMLDocument.Create;

  C := 0;

  Simba := XMLDoc.CreateElement('Simba');
  Simba := XMLDoc.AppendChild(Simba);

  TreeNode := Nodes.GetFirstNode;
  while TreeNode <> nil do
  begin;
    DOMNode := XMLDoc.CreateElement(TreeNode.Text);
    Simba.AppendChild(DOMNode);
    WalkTree(TreeNode, DOMNode, XMLDoc, C);
    TreeNode := TreeNode.GetNextSibling;
  end;

  try
    WriteXMLFile(XMLDoc, utf8tosys(fileName));
    Result := True;
  except
    mDebugLn('Failed to write ' + fileName);
    Result := False;
  end;
  XMLDoc.Free;
end;

end.

