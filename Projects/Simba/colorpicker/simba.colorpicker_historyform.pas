{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

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

    Colour History window for Mufasa Macro Library
}
unit simba.colorpicker_historyform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus,  DOM, XMLWrite, XMLRead;

type
  TColourPickerObject = class(TObject)
  public
    Colour: Integer;
    Pos: TPoint;
    Name: String;

    constructor Create(C: Integer; P: TPoint; N: String);
    destructor Destroy; override;
  end;

  { TSimbaColorHistoryForm }

  TSimbaColorHistoryForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CH_RGB_Label: TLabel;
    CHImages: TImageList;
    OkButton: TButton;
    ColourValue: TEdit;
    CoordValue: TLabel;
    CHOpenDialog: TOpenDialog;
    PickNewColourButton: TButton;
    DeleteButton: TButton;
    CHSaveDialog: TSaveDialog;
    SelectionName: TEdit;
    ColourTree: TTreeView;
    procedure ChangeName(Sender: TObject);
    procedure CHClearClick(Sender: TObject);
    procedure CHLoadClick(Sender: TObject);
    procedure ColourTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ColourTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ColourTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure ColourTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ColourTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DeleteSelected(Sender: TObject);
    procedure AddColObj(c: TColourPickerObject; autoName: Boolean);


    procedure CHSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure SelectionNameKeyPress(Sender: TObject; var Key: char);
  private
    TreeChanged: Boolean;
    { private declarations }
  protected
    procedure AddColObj(c: TColourPickerObject);
    procedure SaveToXML(s: String);
    procedure XML2Tree(XMLDoc: TXMLDocument);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    IndexSelected: Integer;
    { public declarations }
  end;

var
  SimbaColorHistoryForm: TSimbaColorHistoryForm;

implementation

uses
  lclintf, lcltype, AnchorDocking,
  simba.colormath, simba.main;

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

{ TSimbaColorHistoryForm }

procedure TSimbaColorHistoryForm.AddColObj(c: TColourPickerObject; autoName: Boolean);
begin
  if autoName then
  begin
    // TODO: Proper name
    c.Name := IntToStr(c.Colour);
  end;
  Self.AddColObj(c);
end;

procedure TSimbaColorHistoryForm.AddColObj(c: TColourPickerObject);
begin
  TreeChanged := True;

  ColourTree.Selected := ColourTree.Items.Add(nil, c.Name);
  ColourTree.Selected.ImageIndex := 0;
  ColourTree.Selected.SelectedIndex := 0;
  ColourTree.Selected.Data := c;
end;

procedure TSimbaColorHistoryForm.DeleteSelected(Sender: TObject);
begin
  if ColourTree.Selected <> nil then
  begin
    ColourTree.Selected.Delete();
    TreeChanged := True;
  end;
end;

procedure WriteXMLData(n: TTreeNode;
                       XMLNode: TDOMNode; XMLDoc: TXMLDocument;
                       out XMLChild: TDOMNode; var C: Integer);

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

procedure TSimbaColorHistoryForm.SaveToXML(s: String);
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

  for i := 0 to Colourtree.Items.TopLvlCount -1 do
  begin
    WriteXMLData(ColourTree.Items.TopLvlItems[i], RootNode, XMLDoc, XMLChild, C);
    WalkTree(ColourTree.Items.TopLvlItems[i], XMLChild, XMLDoc, C);
  end;

  WriteXMLFile(XMLDoc, s);

  XMLDoc.Free;
end;

procedure TSimbaColorHistoryForm.ColourTreeChange(Sender: TObject; Node: TTreeNode);
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

  ColorToRGB(TColourPickerObject(Node.Data).Colour, r, g, b);

  { Change Form Text / Values }
  ColourValue.Caption := IntToStr(TColourPickerObject(Node.Data).Colour);
  CoordValue.Caption := 'Coords: ' + IntToStr(TColourPickerObject(Node.Data).Pos.X) +
                        ', ' + IntToStr(TColourPickerObject(Node.Data).Pos.Y);
  SelectionName.Text := TColourPickerObject(Node.Data).Name;

  CH_RGB_Label.Caption := Format('RGB: %d, %d, %d', [r,g,b]);
end;

procedure TSimbaColorHistoryForm.ColourTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data <> nil then
    TColourPickerObject(Node.Data).Free();
end;

procedure TSimbaColorHistoryForm.ColourTreeDragDrop(Sender, Source: TObject; X,
  Y: Integer);
Var
   Node: TTreeNode;

begin
  if Sender <> ColourTree then
    Exit;
  Node := ColourTree.GetNodeAt(X, Y);
  if not assigned(ColourTree.Selected) then
    exit;

  if not assigned(Node) then
  begin
    ColourTree.Selected.MoveTo(nil, naAdd);
    TreeChanged:=True;
    exit;
  end;
  ColourTree.Selected.MoveTo(Node, naAddChild);
  TreeChanged:=True;
end;

procedure TSimbaColorHistoryForm.ColourTreeDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

begin
  Accept:=True;
end;

procedure TSimbaColorHistoryForm.ChangeName(Sender: TObject);
begin
  if not Assigned(ColourTree.Selected) then
    exit;
  ColourTree.Selected.Text := SelectionName.Text;
  TColourPickerObject(ColourTree.Selected.Data).Name := SelectionName.Text;
end;

procedure TSimbaColorHistoryForm.CHClearClick(Sender: TObject);
begin
  TreeChanged:=False;
  ColourTree.BeginUpdate;
  ColourTree.Items.Clear;
  ColourTree.EndUpdate;
end;

//heavily modded from http://wiki.lazarus.freepascal.org/XML_Tutorial
procedure TSimbaColorHistoryForm.XML2Tree(XMLDoc: TXMLDocument);
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
      TreeNode.ImageIndex := 0;
      TreeNode.SelectedIndex := 0;
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


procedure TSimbaColorHistoryForm.CHLoadClick(Sender: TObject);

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
      XML2Tree(XMLDoc);
      ColourTree.EndUpdate;
      TreeChanged:=False;
      XMLDoc.Free;
    end;
end;

procedure TSimbaColorHistoryForm.ColourTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  Sender.Canvas.Brush.Color := TColourPickerObject(Node.Data).Colour;
  Sender.Canvas.Pen.Color := clBlack;
  Sender.Canvas.Rectangle(
    Node.DisplayIconLeft,
    Node.DisplayRect(False).Top + 2,
    Node.DisplayTextLeft - 2,
    Node.DisplayRect(False).Bottom - 2
  );

  PaintImages := False;
end;

constructor TSimbaColorHistoryForm.Create(TheOwner: TComponent);
var
  Template: TBitmap;
begin
  inherited Create(TheOwner);

  PickNewColourButton.OnClick:= @SimbaForm.ButtonPickClick;

  Template := TBitmap.Create();
  Template.SetSize(16, 16);

  CHImages.Add(Template, nil);
end;

destructor TSimbaColorHistoryForm.Destroy;
begin
  PickNewColourButton.OnClick := nil;

  inherited Destroy;
end;

procedure TSimbaColorHistoryForm.CHSaveClick(Sender: TObject);
begin
  if CHSaveDialog.Execute then
  begin
    SaveToXML(CHSaveDialog.FileName);
    TreeChanged := False;
  end;
end;

procedure TSimbaColorHistoryForm.FormCreate(Sender: TObject);
begin
  TreeChanged := False;
end;

procedure TSimbaColorHistoryForm.FormResize(Sender: TObject);
begin
  Constraints.MinWidth := Button1.Width + Button2.Width + Button3.Width + DeleteButton.Width + OkButton.Width + 50;
end;

procedure TSimbaColorHistoryForm.OkButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TSimbaColorHistoryForm.SelectionNameKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Self.close;
  end;
end;

initialization
  {$R *.lfm}

end.

