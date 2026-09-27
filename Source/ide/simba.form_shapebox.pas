{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, ComCtrls, Graphics,
  simba.base,
  simba.toolform,
  simba.component_imagebox,
  simba.component_shapebox,
  simba.component_treeview,
  simba.component_button;

type
  TSimbaShapeBoxForm = class(TSimbaToolForm)
  protected
    FShapeBox: TSimbaShapeBox;
    FList: TSimbaTreeView;
    FSyncing: Boolean;

    function CreateImageBox: TSimbaImageBox; override;

    // the list, in step with the shape box
    procedure DoShapesChange(Sender: TObject);
    procedure DoListSelectionChange(Sender: TObject);
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoKindClick(Sender: TObject);

    procedure DoDeleteClick(Sender: TObject);
    procedure DoDuplicateClick(Sender: TObject);
    procedure DoNameClick(Sender: TObject);
    procedure DoPrintClick(Sender: TObject);
    procedure DoClearShapesClick(Sender: TObject);
    procedure DoPrintShapesClick(Sender: TObject);

    procedure DoLoadShapesClick(Sender: TObject);
    procedure DoSaveShapesClick(Sender: TObject);
    procedure DoUndoClick(Sender: TObject);
    procedure DoRedoClick(Sender: TObject);

    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoFirstShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(ImageSupplier: TImageSupplier); override;
  end;

implementation

uses
  Dialogs, LCLType,
  simba.env, simba.dialog, simba.component_theme;

function TSimbaShapeBoxForm.CreateImageBox: TSimbaImageBox;
begin
  FShapeBox := TSimbaShapeBox.Create(Self);

  Result := FShapeBox;
end;

procedure TSimbaShapeBoxForm.DoShapesChange(Sender: TObject);
var
  I: Integer;
begin
  FSyncing := True;
  FList.BeginUpdate();
  try
    while (FList.TopLevelCount > FShapeBox.Count) do
      FList.Items.Delete(FList.TopLevelItem[FList.TopLevelCount - 1]);
    while (FList.TopLevelCount < FShapeBox.Count) do
      FList.AddNode('');
    for I := 0 to FShapeBox.Count - 1 do
      if (FList.TopLevelItem[I].Text <> FShapeBox.ListText(I)) then
        FList.TopLevelItem[I].Text := FShapeBox.ListText(I);

    if FShapeBox.HasSelection() then
      FList.Selected := FList.TopLevelItem[FShapeBox.SelectedIndex]
    else
      FList.Selected := nil;
  finally
    FList.EndUpdate();
    FSyncing := False;
  end;
  FList.Invalidate();
end;

// the user picked a shape: it is brought into view
procedure TSimbaShapeBoxForm.DoListSelectionChange(Sender: TObject);
begin
  if FSyncing then
    Exit;

  if (FList.Selected <> nil) then
    FShapeBox.SelectedIndex := FList.Selected.Index
  else
    FShapeBox.SelectedIndex := -1;
  FShapeBox.MakeSelectionVisible();
end;

// the description, dimmer, after the kind and name the tree drew
procedure TSimbaShapeBoxForm.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
var
  Str: String;
  R: TRect;
  OldColor: TColor;
  OldStyle: TBrushStyle;
begin
  if (Node.Index >= FShapeBox.Count) then
    Exit;

  Str := FShapeBox.Describe(Node.Index);

  OldColor := ACanvas.Font.Color;
  OldStyle := ACanvas.Brush.Style;
  ACanvas.Font.Color := SimbaComponentTheme.ColorLine;
  ACanvas.Brush.Style := bsClear;

  R := Node.DisplayRect(True);
  ACanvas.TextOut(R.Right + ACanvas.TextWidth(' '), R.Top + (R.Height - ACanvas.TextHeight(Str)) div 2, Str);

  ACanvas.Font.Color := OldColor;
  ACanvas.Brush.Style := OldStyle;
end;

// the keys that place it (Esc, Enter, Delete) go to the shape box straight away
procedure TSimbaShapeBoxForm.DoKindClick(Sender: TObject);
begin
  FShapeBox.Place(EShapeBoxKind(TSimbaButton(Sender).Tag));
  if FShapeBox.CanSetFocus() then
    FShapeBox.SetFocus();
end;

procedure TSimbaShapeBoxForm.DoDeleteClick(Sender: TObject);
begin
  FShapeBox.Delete(FShapeBox.SelectedIndex);
end;

procedure TSimbaShapeBoxForm.DoDuplicateClick(Sender: TObject);
begin
  FShapeBox.Copy(FShapeBox.SelectedIndex);
end;

procedure TSimbaShapeBoxForm.DoNameClick(Sender: TObject);
var
  NewName: String;
begin
  if not FShapeBox.HasSelection() then
    Exit;

  NewName := FShapeBox.ShapeName[FShapeBox.SelectedIndex];
  if InputQuery('Shape name', 'Enter shape name', NewName) then
    FShapeBox.ShapeName[FShapeBox.SelectedIndex] := NewName;
end;

procedure TSimbaShapeBoxForm.DoPrintClick(Sender: TObject);
begin
  FShapeBox.Print(FShapeBox.SelectedIndex);
end;

procedure TSimbaShapeBoxForm.DoClearShapesClick(Sender: TObject);
begin
  if (FShapeBox.Count > 0) and (ShowQuestionDialog('Simba', 'Delete all shapes?', []) = ESimbaDialogButton.YES) then
    FShapeBox.Clear();
end;

procedure TSimbaShapeBoxForm.DoPrintShapesClick(Sender: TObject);
begin
  FShapeBox.Print();
end;

procedure TSimbaShapeBoxForm.DoLoadShapesClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    if Execute() and FileExists(FileName) then
      FShapeBox.Load(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaShapeBoxForm.DoSaveShapesClick(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  try
    Options := Options + [ofOverwritePrompt];
    DefaultExt := 'json';
    if Execute() then
      FShapeBox.Save(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaShapeBoxForm.DoUndoClick(Sender: TObject);
begin
  FShapeBox.Undo();
end;

procedure TSimbaShapeBoxForm.DoRedoClick(Sender: TObject);
begin
  FShapeBox.Redo();
end;

// undo and redo whether the image or the list has the focus
procedure TSimbaShapeBoxForm.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited DoFormKeyDown(Sender, Key, Shift);

  FShapeBox.UndoKeyDown(Key, Shift);
end;

procedure TSimbaShapeBoxForm.DoFirstShow;
begin
  inherited DoFirstShow();

  FShapeBox.Load(SimbaEnv.DataPath + 'shapes.json', SimbaEnv.DataPath + 'shapes_history.json');
end;

procedure TSimbaShapeBoxForm.DoClose(var CloseAction: TCloseAction);
begin
  FShapeBox.Print();
  FShapeBox.Save(SimbaEnv.DataPath + 'shapes.json', SimbaEnv.DataPath + 'shapes_history.json');

  inherited DoClose(CloseAction);
end;

constructor TSimbaShapeBoxForm.Create(ImageSupplier: TImageSupplier);
var
  Items: TMenuItem;
  ListMenu: TPopupMenu;
  Buttons: TSimbaButtonGrid;
  K: EShapeBoxKind;
begin
  inherited Create(ImageSupplier);

  Caption := 'Shape Box';
  FUpdateImageOnFirstShow := False; // the shape box's own black image, until Update Image
  ShowButtonDivider := False;
  ShowZoom := False;

  addImageMenu();

  // the keys themselves are DoFormKeyDown's
  Items := addMenu('Shapes');
  addMenuItem(Items, 'Load Shapes', @DoLoadShapesClick);
  addMenuItem(Items, 'Save Shapes', @DoSaveShapesClick);
  addMenuItem(Items, '-');
  addMenuItem(Items, 'Undo', @DoUndoClick, ShortCut(VK_Z, [ssCtrl]));
  addMenuItem(Items, 'Redo', @DoRedoClick, ShortCut(VK_Y, [ssCtrl]));

  FShapeBox.OnShapesChange := @DoShapesChange;
  FShapeBox.OnSelectionChange := @DoShapesChange;

  Buttons := TSimbaButtonGrid.Create(Self);
  Buttons.Parent := FSidePanel;
  Buttons.Align := alTop;
  Buttons.BorderSpacing.Around := 5;
  Buttons.ChildSizing.ControlsPerLine := Ord(High(EShapeBoxKind)) + 1;

  for K in EShapeBoxKind do
    with TSimbaButton.Create(Self) do
    begin
      Parent := Buttons;
      Caption := TSimbaShapeBox.KindName(K);
      XPadding := 4;
      Tag := Ord(K);
      OnClick := @DoKindClick;
    end;

  ListMenu := TPopupMenu.Create(Self);
  addMenuItem(ListMenu.Items, 'Delete', @DoDeleteClick);
  addMenuItem(ListMenu.Items, 'Duplicate', @DoDuplicateClick);
  addMenuItem(ListMenu.Items, 'Name', @DoNameClick);
  addMenuItem(ListMenu.Items, 'Print', @DoPrintClick);

  FList := TSimbaTreeView.Create(Self);
  FList.Parent := FSidePanel;
  FList.Align := alClient;
  FList.FilterVisible := False;
  FList.HideRoot();
  FList.OnSelectionChange := @DoListSelectionChange;
  FList.OnPaintNode := @DoPaintNode;
  FList.PopupMenu := ListMenu;

  addButton('Clear Shapes', @DoClearShapesClick);
  addButton('Print Shapes', @DoPrintShapesClick);
end;

end.
