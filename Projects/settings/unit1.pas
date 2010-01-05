unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, xml2, XMLRead, XMLWrite, DOM;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListView1: TListView;
    SaveButton: TButton;
    LoadButton: TButton;
    TreeView1: TTreeView;
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
  private
    { private declarations }

    procedure LoadFromXML(tree: TTreeView; XMLDoc: TXMLDocument);
    procedure SaveToXML(s: String);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  settings;


procedure WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       var XMLChild: TDOMNode; var C: Integer);

var
   DDataNode, DataNode: TDOMNode;

begin
  XMLChild := XMLDoc.CreateElement(n.Text);
  Inc(C);
  XMLNode.AppendChild(XMLChild);
end;

procedure WalkTree(Node: TTreeNode; XMLNode: TDOMNode; XMLDoc: TXMLDocument;
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

procedure TForm1.SaveToXML(s: String);
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
  if TreeView1.Items.GetFirstNode <> nil then
    WalkTree(TreeView1.Items.GetFirstNode, RootNode, XMLDoc, C);

 { writeln(TreeView1.Items.TopLvlCount);
  for i := 0 to TreeView1.Items.TopLvlCount -1 do
  begin
    WriteXMLData(TreeView1.Items.TopLvlItems[i], RootNode, XMLDoc, XMLChild, C);
    WalkTree(TreeView1.Items.TopLvlItems[i], XMLChild, XMLDoc, C);
  end;   }

  WriteXMLFile(XMLDoc, s);

  XMLDoc.Free;
end;

procedure TForm1.LoadFromXML(tree: TTreeView; XMLDoc: TXMLDocument);
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

    TreeNode := tree.Items.AddChild(TreeNode, s);
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


{ TForm1 }

procedure TForm1.SaveButtonClick(Sender: TObject);
var
  sett: TMMLSettings;
  s: TStringArray;
  i: Integer;

begin
  sett := TMMLSettings.Create(TreeView1.Items);

  if sett.KeyExists('') then
    writeln('Tree exists');
  if sett.KeyExists('Settings/a/b/c') then
    writeln('Settings exists');

  {writeln(sett.GetKeyValue('Settings/hoi0/Item1/Item2/Item3/wattt'));     }
  sett.CreateKey('Settings/a/b/c', true);

  if sett.iskey('Settings/a/b/c') then
    writeln('is key');

  s := sett.ListKeys('Settings');
  for i := 0 to high(s) do
    writeln(s[i]);


 { ss:='Settings' + '/' + s[0];
  while length(s) > 0 do
  begin
    s := sett.ListKeys(ss);
    if(length(s)>0) then
      ss:=ss +'/' + s[0];
  end;   }

  sett.Free;
  SaveToXML('settings.xml');
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
var
  doc: TXMLDocument;
begin
  if not FileExists('settings.xml') then
  begin
    TreeView1.Items.Clear;
    SaveToXML('settings.xml');
  end;
  ReadXMLFile(doc, 'settings.xml');

  TreeView1.Items.Clear;
  LoadFromXML(TreeView1, doc);
end;

initialization
  {$I unit1.lrs}

end.

