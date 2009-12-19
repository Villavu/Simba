unit colourhistory; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

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
    OkButton: TButton;
    ColourValue: TEdit;
    CoordValue: TLabel;
    ColourImage: TImage;
    PickNewColourButton: TButton;
    DeleteButton: TButton;
    SelectionName: TEdit;
    ColourTree: TTreeView;
    procedure ChangeName(Sender: TObject);
    procedure ColourTreeChange(Sender: TObject; Node: TTreeNode);
    procedure DeleteSelected(Sender: TObject);
    procedure AddColObj(c: TColourPickerObject; autoName: Boolean);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OkButtonClick(Sender: TObject);
    procedure SelectionNameKeyPress(Sender: TObject; var Key: char);
    procedure SetCHShowMenu(Sender: TObject);
    procedure UnSetCHShowMenu(Sender: TObject);
  private
    { private declarations }
  protected
    procedure AddColObj(c: TColourPickerObject);
  public
    IndexSelected: Integer;
    { public declarations }
  end; 



var
  ColourHistoryForm: TColourHistoryForm;

implementation
uses
  colour_conv, TestUnit;

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

procedure TColourHistoryForm.AddColObj(c: TColourPickerObject);

var
   it: TTreeNode;
   bmp: TBitmap;
begin
  it := ColourTree.Items.Add(nil, c.Name);
  it.Data := c;
  ColourTree.Selected := it;

  bmp:=TBitmap.Create;
  bmp.SetSize(16,16);
  bmp.Canvas.Brush.Color:=c.Colour;
  bmp.Canvas.Rectangle(0,0,16,16);

  it.ImageIndex:= CHImages.Add(bmp,nil);
  it.SelectedIndex:=it.ImageIndex;

  bmp.Free;
end;

procedure TColourHistoryForm.DeleteSelected(Sender: TObject);

var
   i:integer;

begin
  if (Assigned(ColourTree.Selected)) then
  begin
    if Assigned(ColourTree.Selected.Data) then
      TColourPickerObject(ColourTree.Selected.Data).Free;
    i:=ColourTree.Selected.ImageIndex;
    ColourTree.Selected.ImageIndex:=0;
    ColourTree.Selected.SelectedIndex:=-0;
    CHImages.Delete(i);

    ColourTree.Selected.Delete;
  end;
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

