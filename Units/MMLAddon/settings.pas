unit settings;

{$mode objfpc}{$M+}

interface

uses
  Classes, SysUtils, ComCtrls, xmlread, xmlwrite, DOM;



type
  // remove later
  TStringArray = Array Of String;

  TSettingData = class(TObject)
    public
      Val: String;

      constructor Create;
      destructor Destroy; override;

  end;

  TMMLSettings = class(TObject)

    public
      constructor Create(aNodes: TTreeNodes);
      destructor Destroy; override;

    private
      Nodes: TTreeNodes;
      function KeyNameToKeys(KeyName: String): TStringArray;
      function WalkToNode(KeyName: String): TTreeNode;

      procedure InternalLoadFromXML(XMLDoc: TXMLDocument);
      procedure WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       var XMLChild: TDOMNode; var C: Integer);
      procedure WalkTree(Node: TTreeNode; XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                   var C: Integer);

    public
      function GetNodePath(Node: TTreeNode): String;
      function ListKeys(KeyName: String): TStringArray;
      function KeyExists(KeyName: String): Boolean;
      function IsKey(KeyName: String): Boolean;
      function IsDirectory(KeyName: String): Boolean;
      procedure SetKeyValue(KeyName: String; KeyValue: String);
      function CreateKey(KeyName: String; CreatePath: Boolean = False): Boolean;
      function GetKeyValue(KeyName: String): String;

      // Horrible name
      function GetSetDefaultKeyValue(KeyName, defVal: String): String;

      // /facepalm
      function GetSetLoadSaveDefaultKeyValue(KeyName, defVal, fileName: String): String;

      // AAAAAAAAAAAHG??
      function GetSetLoadSaveDefaultKeyValueIfNotExists(KeyName, defVal, fileName: String): String;

    public
      procedure LoadFromXML(fileName: String);
      procedure SaveToXML(fileName: String);

  end;

implementation
uses
  strutils;

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
  Self.Nodes := aNodes;
end;

destructor TMMLSettings.Destroy;
begin
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
    if (Node.NodeType = 3) then
      s := 'Data'
    else
      s := Node.NodeName;

    TreeNode := Nodes.AddChild(TreeNode, s);
    if (Node.NodeType = 3) then
    begin
      d := TSettingData.Create;
      D.Val := Node.NodeValue;
      TreeNode.Data := D;

      TreeNode.Text := 'Value';
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
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil); // Recursive
    iNode := iNode.NextSibling;
  end;
end;

function TMMLSettings.KeyNameToKeys(KeyName: String): TStringArray;
  // yay for SRL!
  function srl_Explode(str, del: string): TStringArray;
  var
    i, l, dL: Integer;
  begin
    i := 0;
    l := -1;
    SetLength(Result, 0);
    if (str = '') then
      Exit;
    dL := Length(del) - 1;
    repeat
      Inc(l);
      SetLength(Result, l + 1);
      i := Pos(del, str);
      if i <= 0 then
        Break;
      Result[l] := Copy(str, 1, i - 1);
      Delete(str, 1, i + dL);
    until false;
    Result[l] := Copy(str, 1, Length(str));
  end;
begin
  Result := srl_Explode(KeyName, '/');
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
end;

function TMMLSettings.ListKeys(KeyName: String): TStringArray;
var
  N: TTreeNode;
  i: Integer;
  S: TStringArray;
begin
  SetLength(Result, 0);
  N := WalkToNode(KeyName);
  if N <> nil then
    N := N.GetFirstChild;

  while N <> nil do
  begin
    setlength(result,length(result)+1);
    result[high(result)] := N.Text;
    N := N.GetNextSibling;
  end;
end;

function TMMLSettings.KeyExists(KeyName: String): Boolean;

begin
  Result := WalkToNode(KeyName) <> nil;
end;

function TMMLSettings.IsKey(KeyName: String): Boolean;
var
  N: TTreeNode;
  i: Integer;
begin
  N := WalkToNode(KeyName);
  if N = nil then
    Exit(False);

  i := 0;

  N := N.GetNextSibling;
  while N <> nil do
  begin
    if N.Text <> 'Value' then
      inc(i);
    N := N.GetNextSibling;
  end;

  Exit(i = 0);
end;

function TMMLSettings.IsDirectory(KeyName: String): Boolean;
var
  N: TTreeNode;
begin
  N := WalkToNode(KeyName);
  if N <> nil then
    Exit(N.HasChildren);
  Exit(False);
end;

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
    if N.Text = 'Value' then
      if assigned(n.Data) then
        Exit(TSettingData(n.Data).Val);
    N := N.GetNextSibling;
  end;
  Exit('');
end;

function TMMLSettings.GetSetDefaultKeyValue(KeyName, defVal: String): String;
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

function TMMLSettings.GetSetLoadSaveDefaultKeyValue(KeyName, defVal, fileName: String): String;
begin
  LoadFromXML(fileName);
  Result := GetSetDefaultKeyValue(KeyName, defVal);
  SaveToXML(fileName);
end;

function TMMLSettings.GetSetLoadSaveDefaultKeyValueIfNotExists(KeyName, defVal, fileName: String): String;
begin
  if KeyExists(KeyName) then
    exit(GetSetDefaultKeyValue(KeyName, defVal))
  else
    Exit(GetSetLoadSaveDefaultKeyValue(KeyName, defVal, fileName));
end;

function TMMLSettings.CreateKey(KeyName: String; CreatePath: Boolean = False): Boolean;
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
  nParent := Nodes.GetFirstNode;

  Path := KeyNameToKeys(KeyName);
  if length(path) < 2 then
  begin
    writeln('Path too short!');
    exit(false);
  end;

  if path[0] <> nParent.Text then
  begin
    writeln('First key doesn''t match. First key should always match');
    exit(false);
  end;
  for i := 0 to length(Path) - 2 do
  begin
    if Path[i] = '' then
    begin
      writeln('Invalid Key Path / Name');
      exit(false);
    end;
    NewPath := NewPath + Path[i] + '/';
    N := WalkToNode(NewPath);

    if (N = nil) and (not CreatePath) then
    begin
      writeln('(N = nil) and (not CreatePath)');
      exit(false);
    end;

    if (N = nil) and CreatePath then
    begin
      newN := TTreeNode.Create(Nodes);
      newN.Text := Path[i];
      if (nParent = nil) then
      begin
        writeln('This shouldn''t happen...');
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

procedure TMMLSettings.SetKeyValue(KeyName: String; KeyValue: String);
var
    N, NN: TTreeNode;
begin
  if not KeyExists(KeyName) then
    Exit;
  if not IsKey(KeyName) then
    Exit;

  N := WalkToNode(KeyName);

  if not N.HasChildren then
  begin
    NN := TTreeNode.Create(Nodes);
    NN.Text := 'Value';
    NN.MoveTo(N, naAddChild);
  end;

  if n <> nil then
    N := N.GetFirstChild;
   while N <> nil do
   begin
     if N.Text = 'Value' then
     begin
       if Assigned(N.Data) then
         TSettingData(N.Data).Free;
       N.Data := TSettingData.Create;
       TSettingData(N.Data).Val := KeyValue;
     end;
     N := N.GetNextSibling;
   end;
end;

procedure TMMLSettings.LoadFromXML(fileName: String);
var
    Doc: TXMLDocument;
begin
  ReadXMLFile(Doc, fileName);
  InternalLoadFromXML(Doc);
  Doc.Free;
end;

procedure TMMLSettings.WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       var XMLChild: TDOMNode; var C: Integer);

var
   DDataNode, DataNode: TDOMNode;

begin
  if n.Text = 'Value' then
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

procedure TMMLSettings.SaveToXML(fileName: String);
var
   XMLDoc: TXMLDocument;
   RootNode: TDOMNode;
   C: Integer;
begin
  XMLDoc := TXMLDocument.Create;

  RootNode := XMLDoc.CreateElement('Settings');
  XMLDoc.AppendChild(RootNode);

  RootNode := XMLDoc.DocumentElement;

  C := 0;
  if Nodes.GetFirstNode <> nil then
    WalkTree(Nodes.GetFirstNode, RootNode, XMLDoc, C);

  WriteXMLFile(XMLDoc, fileName);

  XMLDoc.Free;
end;

end.

