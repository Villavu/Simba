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
    OkButton: TButton;
    ColourValue: TEdit;
    CoordValue: TLabel;
    ColourImage: TImage;
    PickNewColourButton: TButton;
    DeleteButton: TButton;
    ColourList: TListView;
    SelectionName: TEdit;
    procedure ChangeName(Sender: TObject);
    procedure ChangeViewData(Sender: TObject; Item: TListItem; Selected: Boolean
      );
    procedure DeleteSelected(Sender: TObject);
    procedure AddColObj(c: TColourPickerObject; autoName: Boolean);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OkButtonClick(Sender: TObject);
    procedure SelectionNameKeyPress(Sender: TObject; var Key: char);
    procedure SetCHShowMenu(Sender: TObject);
    procedure UnSetCHShowMenu(Sender: TObject);
  private
    Colour_Count: Integer;
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
    Inc(Colour_Count);
    // TODO: Proper name
    c.Name := IntToStr(c.Colour) + ' (' + IntToStr(Colour_Count) + ')';
  end;
  Self.AddColObj(c);
end;

procedure TColourHistoryForm.AddColObj(c: TColourPickerObject);

var
   it: TListItem;
begin
  it := ColourList.Items.Add;
  it.Data := c;
  it.Caption:= c.Name;
  ColourList.Selected := it;
end;

procedure TColourHistoryForm.DeleteSelected(Sender: TObject);

begin
  if (Assigned(ColourList.Selected)) then
  begin
    TColourPickerObject(ColourList.Selected.Data).Free;
    ColourList.Selected.Delete;
  end;
end;

procedure TColourHistoryForm.ChangeViewData(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
   r,g,b:integer;
begin
  if not Assigned(Item) then
    exit;
  if not Item.Selected then
    exit;

  { This only occurs when we have manually added an item with the Form Editor }
  if not Assigned(Item.Data) then
    exit;

  colour_conv.ColorToRGB(TColourPickerObject(Item.Data).Colour, r, g, b);

  { Change Form Text / Values }
  ColourValue.Caption := IntToStr(TColourPickerObject(Item.Data).Colour);
  CoordValue.Caption := 'Coords: ' + IntToStr(TColourPickerObject(Item.Data).Pos.X) +
                        ', ' + IntToStr(TColourPickerObject(Item.Data).Pos.Y);
  SelectionName.Text := TColourPickerObject(Item.Data).Name;

  CH_RGB_Label.Caption:=Format('RGB:%d,%d,%d', [r,g,b]);

  { Draw the Image }
  ColourImage.Canvas.Brush.Color := TColourPickerObject(Item.Data).Colour;
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
  if not Assigned(ColourList.Selected) then
  begin
    WriteLn('We double clicked but have nothing Selected?');
    exit;
  end;
  ColourList.Selected.Caption := SelectionName.Text;
  TColourPickerObject(ColourList.Selected.Data).Name := SelectionName.Text;
end;

constructor TColourHistoryForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Colour_Count := 0;
  PickNewColourButton.OnClick:= @Form1.ButtonPickClick;
end;

destructor TColourHistoryForm.Destroy;
begin
  PickNewColourButton.OnClick := nil;
  Colour_Count := 0;

  inherited Destroy;
end;

procedure TColourHistoryForm.OkButtonClick(Sender: TObject);
begin
  Self.close;
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

