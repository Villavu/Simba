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

    Coliur History window for Mufasa Macro Library
}
unit colourhistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus,  DOM, XMLWrite, XMLRead;

type
  TColourPickerObject = class(TObject)
          constructor Create(C: Integer; P: TPoint; N: String);
          destructor Destroy; override;

        public
          Colour: Integer;
          Pos: TPoint;
          Name: String;
  end;

  { TColourHistoryForm }

  TColourHistoryForm = class(TForm)
    CH_RGB_Label: TLabel;
    CHImages: TImageList;
    CHMainMenu: TMainMenu;
    CHFile: TMenuItem;
    CHHelp: TMenuItem;
    CHClear: TMenuItem;
    CHLoad: TMenuItem;
    CHSave: TMenuItem;
    CHAbout: TMenuItem;
    OkButton: TButton;
    ColourValue: TEdit;
    CoordValue: TLabel;
    ColourImage: TImage;
    CHOpenDialog: TOpenDialog;
    PickNewColourButton: TButton;
    DeleteButton: TButton;
    CHSaveDialog: TSaveDialog;
    SelectionName: TEdit;
    ColourTree: TTreeView;
    procedure CHAboutClick(Sender: TObject);
    procedure ChangeName(Sender: TObject);
    procedure CHClearClick(Sender: TObject);
    procedure CHLoadClick(Sender: TObject);
    procedure ColourTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ColourTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ColourTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DeleteSelected(Sender: TObject);
    procedure AddColObj(c: TColourPickerObject; autoName: Boolean);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CHSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure SelectionNameKeyPress(Sender: TObject; var Key: char);
    procedure SetCHShowMenu(Sender: TObject);
    procedure UnSetCHShowMenu(Sender: TObject);
  private
    TreeChanged: Boolean;
    { private declarations }
  protected
    procedure AddColObj(c: TColourPickerObject);
    procedure SetNodeBitmap(N: TTreeNode);
    procedure SaveToXML(s: String);
    procedure XML2Tree(XMLDoc: TXMLDocument);
  public
    IndexSelected: Integer;
    { public declarations }
  end; 



var
  ColourHistoryForm: TColourHistoryForm;

implementation
uses
  colour_conv, TestUnit, lclintf, lcltype;

constructor TColourPickerObject.Create(C: Integer; P: TPoint; N: String);
begin
  inherited Create;
  Self.Colour := C;
  Self.Pos := P;
  Self.Name:= N;
end;

destructor TColourPickerObject.Destroy;
begin

  inherited Destroy;
end;

{ TColourHistoryForm }

procedure TColourHistoryForm.AddColObj(c: TColourPickerObject; autoName: Boolean);
begin
  if autoName then
  begin
    // TODO: Proper name
    c.Name := IntToStr(c.Colour);
  end;
  Self.AddColObj(c);
end;

procedure TColourHistoryForm.SetNodeBitmap(N: TTreeNode);
var
  bmp: TBitmap;
begin
  bmp:=TBitmap.Create;
  bmp.SetSize(16,16);
  bmp.Canvas.Brush.Color:=TColourPickerObject(n.Data).Colour;
  bmp.Canvas.Rectangle(0,0,16,16);

  n.ImageIndex:=CHImages.Add(bmp, nil);
  n.SelectedIndex:=n.ImageIndex;

  bmp.Free;
end;

procedure TColourHistoryForm.AddColObj(c: TColourPickerObject);

var
   it: TTreeNode;

begin
  TreeChanged:=True;
  it := ColourTree.Items.Add(nil, c.Name);
  it.Data := c;
  ColourTree.Selected := it;
  SetNodeBitmap(it);
end;

procedure WalkDeleteTree(Node: TTreeNode; Img: TImageList);
var
   N: TTreeNode;

begin
  N := Node.GetFirstChild;

  while assigned(n) do
  begin
    If Assigned(N.Data) then
      TColourPickerObject(N.Data).Free;
    WriteLn('Deleting ImageIndex: ' + IntToStr(n.ImageIndex) + '; Text: ' + N.Text);
    Img.Delete(n.ImageIndex);
    WalkDeleteTree(n, img);
    n := n.GetNextSibling;
  end;
end;

procedure TColourHistoryForm.DeleteSelected(Sender: TObject);

var
   i:integer;
   e: TTreeNodesEnumerator;

begin
  if (Assigned(ColourTree.Selected)) then
  begin
    if Assigned(ColourTree.Selected.Data) then
      TColourPickerObject(ColourTree.Selected.Data).Free;

    WalkDeleteTree(ColourTree.Selected, CHImages);

    WriteLn('Deleting ImageIndex: ' + IntToStr(ColourTree.Selected.ImageIndex) + '; Text: ' + ColourTree.Selected.Text);
    CHImages.Delete(ColourTree.Selected.ImageIndex);

    ColourTree.Selected.Delete;
    TreeChanged := True;

    { Now, we have to recreate all images and their indices... Since the TImageList
      fiddles with it's indices if one is deleted... Wtf? }
    CHImages.Clear;

    e := ColourTree.Items.GetEnumerator;
    while e.MoveNext do
      SetNodeBitmap(e.Current);
  end;
end;

procedure WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       var XMLChild: TDOMNode; var C: Integer);

var
   DDataNode, DataNode, Data: TDOMNode;

begin
  XMLChild := XMLDoc.CreateElement('Item' + IntToStr(C));
  Inc(C);
  XMLNode.AppendChild(XMLChild);


  DDataNode := XMLDoc.CreateElement('Data');
  XMLChild.AppendChild(DDataNode);

  DataNode := XMLDoc.CreateElement('Name');
  DDataNode.AppendChild(DataNode);
  Data := XMLDoc.CreateTextNode(TColourPickerObject(n.Data).Name);
  DataNode.AppendChild(Data);
  DataNode := XMLDoc.CreateElement('Colour');
  DDataNode.AppendChild(DataNode);
  Data := XMLDoc.CreateTextNode(IntToStr(TColourPickerObject(n.Data).Colour));
  DataNode.AppendChild(Data);

  DataNode := XMLDoc.CreateElement('CoordX');
  DDataNode.AppendChild(DataNode);
  Data := XMLDoc.CreateTextNode(IntToStr(TColourPickerObject(n.Data).Pos.X));
  DataNode.AppendChild(Data);
  DataNode := XMLDoc.CreateElement('CoordY');
  DDataNode.AppendChild(DataNode);
  Data := XMLDoc.CreateTextNode(IntToStr(TColourPickerObject(n.Data).Pos.Y));
  DataNode.AppendChild(Data);
end;

procedure WalkTree(Node: TTreeNode; XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                   var C: Integer);
var
   N: TTreeNode;
   XMLChild, DDataNode, DataNode, Data: TDOMNode;

begin
  N := Node.GetFirstChild;

  while assigned(n) do
  begin
    WriteXMLData(n, XMLNode, XMLDoc, XMLChild, C);

    WalkTree(n, XMLChild, XMLDoc, C);
    n := n.GetNextSibling;
  end;
end;

procedure TColourHistoryForm.SaveToXML(s: String);
var
   XMLDoc: TXMLDocument;
   RootNode, XMLChild: TDOMNode;
   C, i: Integer;

begin
  XMLDoc := TXMLDocument.Create;

  RootNode := XMLDoc.CreateElement('Tree');
  XMLDoc.AppendChild(RootNode);

  RootNode := XMLDoc.DocumentElement;

  C := 0;

  writeln(Colourtree.Items.TopLvlCount);
  for i := 0 to Colourtree.Items.TopLvlCount -1 do
  begin
    WriteXMLData(ColourTree.Items.TopLvlItems[i], RootNode, XMLDoc, XMLChild, C);
    WalkTree(ColourTree.Items.TopLvlItems[i], XMLChild, XMLDoc, C);
  end;

  WriteXMLFile(XMLDoc, s);

  XMLDoc.Free;
end;

procedure TColourHistoryForm.ColourTreeChange(Sender: TObject; Node: TTreeNode);
var
   r,g,b:integer;
begin
  if not Assigned(Node) then
    exit;
  if not Node.Selected then
    exit;

  { This only occurs when we have manually added an item with the Form Editor }
  if not Assigned(Node.Data) then
    exit;

  colour_conv.ColorToRGB(TColourPickerObject(Node.Data).Colour, r, g, b);

  { Change Form Text / Values }
  ColourValue.Caption := IntToStr(TColourPickerObject(Node.Data).Colour);
  CoordValue.Caption := 'Coords: ' + IntToStr(TColourPickerObject(Node.Data).Pos.X) +
                        ', ' + IntToStr(TColourPickerObject(Node.Data).Pos.Y);
  SelectionName.Text := TColourPickerObject(Node.Data).Name;

  CH_RGB_Label.Caption:=Format('RGB:%d,%d,%d', [r,g,b]);

  { Draw the Image }
  ColourImage.Canvas.Brush.Color := TColourPickerObject(Node.Data).Colour;
  ColourImage.Canvas.Rectangle(0,0,ColourImage.Width, ColourImage.Height);

  if Self.Visible then
  begin
    try
      SelectionName.SetFocus;
    finally
    end;
  end;
end;

procedure TColourHistoryForm.ColourTreeDragDrop(Sender, Source: TObject; X,
  Y: Integer);
Var
   Node: TTreeNode;

begin
  if Sender <> ColourTree then
    Exit;
  Node := ColourTree.GetNodeAt(X, Y);
  if not assigned(ColourTree.Selected) then
  begin
    writeln('No valid node is currently selected');
    exit;
  end;

  if not assigned(Node) then
  begin
    ColourTree.Selected.MoveTo(nil, naAdd);
    TreeChanged:=True;
    exit;
  end;
  ColourTree.Selected.MoveTo(Node, naAddChild);
  TreeChanged:=True;
  writeln('Dragging from: ' + ColourTree.Selected.Text);
  writeln('Dragging to: ' + Node.Text);
end;

procedure TColourHistoryForm.ColourTreeDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

begin
  Accept:=True;
end;

procedure TColourHistoryForm.ChangeName(Sender: TObject);
begin
  if not Assigned(ColourTree.Selected) then
  begin
    WriteLn('We double clicked but have nothing Selected?');
    exit;
  end;
  ColourTree.Selected.Text := SelectionName.Text;
  TColourPickerObject(ColourTree.Selected.Data).Name := SelectionName.Text;
end;

procedure TColourHistoryForm.CHClearClick(Sender: TObject);
begin
  TreeChanged:=False;
  ColourTree.BeginUpdate;
    ColourTree.Items.Clear;
    CHImages.Clear;
  ColourTree.EndUpdate;
end;

//heavily modded from http://wiki.lazarus.freepascal.org/XML_Tutorial
procedure TColourHistoryForm.XML2Tree(XMLDoc: TXMLDocument);
var
  iNode: TDOMNode;

  procedure ProcessNode(Node: TDOMNode; TreeNode: TTreeNode);
  var
    cNode, dNode: TDOMNode;
    OP: TColourPickerObject;
    pos: TPoint;
    Colour: Integer;
    Name: String;

  begin
    if Node = nil then Exit;

    If Node.NodeName = 'Data' Then
      exit;

    TreeNode := ColourTree.Items.AddChild(TreeNode, 'ERROR');
    writeln(Node.NodeName);
    cNode := Node.FindNode('Data');
    if Assigned(cNode) then
    begin
      dNode := cNode.FindNode('Name');
      if assigned(dNode) then
        Name := dNode.TextContent
      else
        Name := 'Error';
      dNode := cNode.FindNode('Colour');
      if assigned(dNode) then
        Colour := StrToIntDef(dNode.TextContent, -1);

      dNode := cNode.FindNode('CoordX');
      if assigned(dNode) then
        Pos.X := StrToIntDef(dNode.TextContent, -1);

      dNode := cNode.FindNode('CoordY');
      if assigned(dNode) then
        Pos.Y := StrToIntDef(dNode.TextContent, -1);

      OP := TColourPickerObject.Create(Colour, Pos, Name);
      TreeNode.Text := Name;
      TreeNode.Data := Pointer(OP);

      SetNodeBitmap(TreeNode);
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
  iNode := XMLDoc.DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil); // Recursive
    iNode := iNode.NextSibling;
  end;
end;


procedure TColourHistoryForm.CHLoadClick(Sender: TObject);

var
   XMLDoc: TXMLDocument;

begin
  if TreeChanged then
  begin
    case MessageBox(0,pchar('Do you want to save the colours?'), Pchar('Colours have been modified.'),
                    MB_YESNOCANCEL or MB_ICONQUESTION) of
        IDCANCEL :
              Exit;
        IDYES :
            begin
              if CHSaveDialog.Execute then
                ColourTree.SaveToFile(CHSaveDialog.FileName)
              else
                Exit;
            end;
    end;
  end;

  if CHOpenDialog.Execute then
    if FileExists(CHOpenDialog.FileName) then
    begin
      ReadXMLFile(XMLDoc, CHOpenDialog.FileName);

      // Clear Tree and Images
      ColourTree.BeginUpdate;
        ColourTree.Items.Clear;
        CHImages.Clear;
        XML2Tree(XMLDoc);
      ColourTree.EndUpdate;
      TreeChanged:=False;
      XMLDoc.Free;
    end;
end;

procedure TColourHistoryForm.CHAboutClick(Sender: TObject);
begin

end;

constructor TColourHistoryForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  PickNewColourButton.OnClick:= @Form1.ButtonPickClick;
end;

destructor TColourHistoryForm.Destroy;
begin
  PickNewColourButton.OnClick := nil;

  inherited Destroy;
end;

procedure TColourHistoryForm.CHSaveClick(Sender: TObject);

begin
   if CHSaveDialog.Execute then
  begin
    SaveToXML(CHSaveDialog.FileName);
    TreeChanged:=False;
  end;
end;

procedure TColourHistoryForm.FormCreate(Sender: TObject);
begin
  TreeChanged:=False;
end;

procedure TColourHistoryForm.OkButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TColourHistoryForm.SelectionNameKeyPress(Sender: TObject;
  var Key: char);
begin
  if key = #13 then
  begin
    key := #0;
    Self.close;
  end;
end;

procedure TColourHistoryForm.SetCHShowMenu(Sender: TObject);
begin
  Form1.MenuItemColourHistory.Checked := True;
end;

procedure TColourHistoryForm.UnSetCHShowMenu(Sender: TObject);
begin
  Form1.MenuItemColourHistory.Checked := False;
end;

initialization
  {$I colourhistory.lrs}

end.

