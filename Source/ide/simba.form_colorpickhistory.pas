{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_colorpickhistory;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus,
  simba.base, simba.component_treeview, simba.component_buttonpanel, simba.ide_events;

type
  TSimbaColorPickHistoryForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemClear: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemCopyPoint: TMenuItem;
    MenuItemCopyRGB: TMenuItem;
    MenuItemCopyHSL: TMenuItem;
    MenuItemCopy: TMenuItem;
    Separator1: TMenuItem;

    procedure ContextMenuClearClick(Sender: TObject);
    procedure ContextMenuCopyClick(Sender: TObject);
    procedure ContextMenuCopyHSLClick(Sender: TObject);
    procedure ContextMenuCopyPointClick(Sender: TObject);
    procedure ContextMenuCopyRGB(Sender: TObject);
    procedure ContextMenuDeleteClick(Sender: TObject);
    procedure ContextMenuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FColorList: TSimbaTreeView;
    FButtonPanel: TSimbaButtonPanel;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoKeyDelete(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyCopy(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoPickColorClick(Sender: TObject);
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
  public
    procedure Add(APoint: TPoint; AColor: TColor; ExpandAndScroll: Boolean = False);
    procedure LoadColors;
    procedure SaveColors;
  end;

var
  SimbaColorPickHistoryForm: TSimbaColorPickHistoryForm;

implementation

uses
  Clipbrd, LCLType, AnchorDocking,
  simba.dialog,
  simba.colormath, simba.component_theme, simba.settings, simba.vartype_string,
  simba.ide_docking;

type
  TColorHistoryValue = packed record
    Point: TPoint;
    Color: TColor;
  end;

  TColorNode = class(TTreeNode)
  public
    Color: TColor;
    Point: TPoint;

    PointStr: String;
    RGBStr: String;
    HSLStr: String;
  end;

procedure TSimbaColorPickHistoryForm.FormCreate(Sender: TObject);
begin
  Width := Scale96ToScreen(500);
  Height := Scale96ToScreen(400);

  FColorList := TSimbaTreeView.Create(Self, TColorNode);
  FColorList.Parent := Self;
  FColorList.Align := alClient;
  FColorList.OnPaintNode := @DoPaintNode;
  FColorList.FilterVisible := False;
  FColorList.PopupMenu := ContextMenu;
  FColorList.AddKeyEvent(VK_C, [ssCtrl], @DoKeyCopy);
  FColorList.AddKeyEvent(VK_DELETE, [], @DoKeyDelete);

  FButtonPanel := TSimbaButtonPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.ButtonCancel.Visible := False;
  FButtonPanel.ButtonOk.Visible := False;
  FButtonPanel.ButtonClose.Visible := True;
  with FButtonPanel.AddButton do
  begin
    Parent := FButtonPanel;
    Align := alLeft;
    Caption := 'Pick Color';
    OnClick := @DoPickColorClick;
  end;

  LoadColors();

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.COLOR_PICKED,ESimbaEvent.ACTION_VIEW_COLORHISTORY]);
end;

procedure TSimbaColorPickHistoryForm.FormDestroy(Sender: TObject);
begin
  SaveColors();
end;

procedure TSimbaColorPickHistoryForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoColorPicked(Picked: TSimbaEvents.TColorPicked);
  begin
    Add(Picked.Point, Picked.Color, True);
    SimbaDocking.Show(Self);
  end;

  procedure DoViewColorHistory(Item: TMenuItem);
  begin
    SimbaDocking.Show(Self);
  end;

begin
  case Event of
    ESimbaEvent.COLOR_PICKED:             DoColorPicked(TSimbaEvents.TColorPicked(Data^));
    ESimbaEvent.ACTION_VIEW_COLORHISTORY: DoViewColorHistory(TMenuItem(Data));
  end;
end;

procedure TSimbaColorPickHistoryForm.DoKeyDelete(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ContextMenuDeleteClick(nil);
end;

procedure TSimbaColorPickHistoryForm.DoKeyCopy(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ContextMenuCopyClick(nil);
end;

procedure TSimbaColorPickHistoryForm.ContextMenuCopyClick(Sender: TObject);
var
  NodeText: String;
begin
  if FColorList.Selected <> nil then
  begin
    NodeText := FColorList.Selected.Text;
    if NodeText.After(': ') <> '' then
      Clipboard.AsText := NodeText.After(': ')
    else
      Clipboard.AsText := NodeText;
  end;
end;

procedure TSimbaColorPickHistoryForm.ContextMenuClearClick(Sender: TObject);
begin
  if ShowQuestionDialog('Simba', 'Clear the entire list?', []) = ESimbaDialogButton.YES then
    FColorList.Clear();
end;

procedure TSimbaColorPickHistoryForm.ContextMenuCopyHSLClick(Sender: TObject);
var
  Node: TColorNode;
begin
  Node := TColorNode(FColorList.Selected);
  if (Node <> nil) then
    Clipboard.AsText := Node.HSLStr;
end;

procedure TSimbaColorPickHistoryForm.ContextMenuCopyPointClick(Sender: TObject);
var
  Node: TColorNode;
begin
  Node := TColorNode(FColorList.Selected);
  if (Node <> nil) then
    Clipboard.AsText := Node.PointStr;
end;

procedure TSimbaColorPickHistoryForm.ContextMenuCopyRGB(Sender: TObject);
var
  Node: TColorNode;
begin
  Node := TColorNode(FColorList.Selected);
  if (Node <> nil) then
    Clipboard.AsText := Node.RGBStr;
end;

procedure TSimbaColorPickHistoryForm.ContextMenuDeleteClick(Sender: TObject);
var
  Node: TColorNode;
begin
  Node := TColorNode(FColorList.Selected);
  if (Node <> nil) and (Node.Level = 0) then
    FColorList.Items.Delete(Node);
end;

procedure TSimbaColorPickHistoryForm.ContextMenuPopup(Sender: TObject);
begin
  if (FColorList.Selected <> nil) then
  begin
    MenuItemCopyPoint.Enabled := FColorList.Selected.Level = 0;
    MenuItemCopyRGB.Enabled   := FColorList.Selected.Level = 0;
    MenuItemCopyHSL.Enabled   := FColorList.Selected.Level = 0;
    MenuItemDelete.Enabled    := FColorList.Selected.Level = 0;
  end;
end;

procedure TSimbaColorPickHistoryForm.DoPickColorClick(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent.ACTION_PICKCOLOR, nil);
end;

procedure TSimbaColorPickHistoryForm.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
var
  BaseRect, ColorRect: TRect;
  S: TTextStyle;
begin
  if (Node.Level <> 0) then
    Exit;

  BaseRect := Node.DisplayRect(True);

  ColorRect := BaseRect;
  ColorRect.Top += 5;
  ColorRect.Bottom -= 5;
  ColorRect.Left += 5;
  ColorRect.Right := BaseRect.Left + Round(ColorRect.Height * 1.5);

  ACanvas.FillRect(BaseRect);

  BaseRect.Left := ColorRect.Right + 5;
  BaseRect.Right += ColorRect.Width + 5;

  if Node.Selected then
  begin
    ACanvas.Brush.Color := SimbaComponentTheme.ColorActive;
    ACanvas.FillRect(BaseRect);
  end;

  ACanvas.Brush.Color := TColorNode(Node).Color;
  ACanvas.Pen.Color := SimbaComponentTheme.ColorFont;
  ACanvas.Pen.Width := 1;
  ACanvas.Rectangle(ColorRect);

  S := ACanvas.TextStyle;
  S.Layout := tlCenter;
  S.Clipping := False;

  ACanvas.TextRect(BaseRect, BaseRect.Left, BaseRect.Top, Node.Text, S);
end;

procedure TSimbaColorPickHistoryForm.Add(APoint: TPoint; AColor: TColor; ExpandAndScroll: Boolean);
var
  Node: TColorNode;
begin
  Node := TColorNode(FColorList.AddNode(ColorToStr(AColor)));
  Node.Color := AColor;
  Node.Point := APoint;
  Node.PointStr := Format('%d, %d', [APoint.X, APoint.Y]);
  with ColorToRGB(AColor) do
    Node.RGBStr := Format('%d, %d, %d', [R,G,B]);
  with ColorToHSL(AColor) do
    Node.HSLStr := Format('%f, %f, %f', [H,S,L]);

  FColorList.AddNode(Node, '- Point: ' + Node.PointStr);
  FColorList.AddNode(Node, '- RGB: ' + Node.RGBStr);
  FColorList.AddNode(Node, '- HSL: ' + Node.HSLStr);

  if ExpandAndScroll then
  begin
    FColorList.Selected := Node;
    Node.Expanded := True;
  end;
end;

procedure TSimbaColorPickHistoryForm.LoadColors;
var
  Value: TColorHistoryValue;
  Stream: TMemoryStream;
begin
  FColorList.BeginUpdate();

  Stream := TStringStream.Create(AnsiString(SimbaSettings.General.ColorPickerHistory.Value));
  while Stream.Read(Value, SizeOf(TColorHistoryValue)) = SizeOf(TColorHistoryValue) do
    Add(Value.Point, Value.Color);
  Stream.Free();

  FColorList.EndUpdate();
end;

procedure TSimbaColorPickHistoryForm.SaveColors;
var
  I: Integer;
  Value: TColorHistoryValue;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(AnsiString(''));
  for I := 0 to FColorList.Items.TopLvlCount - 1 do
  begin
    Value.Color := TColorNode(FColorList.Items.TopLvlItems[i]).Color;
    Value.Point := TColorNode(FColorList.Items.TopLvlItems[i]).Point;

    Stream.Write(Value, SizeOf(TColorHistoryValue));
  end;

  SimbaSettings.General.ColorPickerHistory.Value := Stream.DataString;

  Stream.Free();
end;

{$R *.lfm}

end.

