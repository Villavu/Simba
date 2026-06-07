{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.aca;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus, Graphics, ComCtrls,
  simba.base,
  simba.image,
  simba.colormath,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.component_treeview,
  simba.component_edit,
  simba.component_button,
  simba.toolform;

type
  TSimbaACA = class(TSimbaToolForm)
  protected
    FColorList: TSimbaTreeView;
    FEditColor: TSimbaLabeledEdit;
    FEditTol: TSimbaLabeledEdit;
    FEditMulti1: TSimbaLabeledEdit;
    FEditMulti2: TSimbaLabeledEdit;
    FEditMulti3: TSimbaLabeledEdit;
    FColorSpaces: TSimbaLabeledToggleButtonGroup;

    FButtonFindColor: TSimbaButton;
    FButtonMatchColor: TSimbaButton;
    FButtonClearImg: TSimbaButton;
    FButtonUpdateImg: TSimbaButton;

    FDebugTPA: TPointArray;
    FDebugMat: TSingleMatrix;

    procedure FillLoadDeleteColorMenus(LoadItem, DeleteItem: TMenuItem);

    function GetColorSpace: EColorSpace;
    procedure CalcBestColor;

    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect); override;
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoListModify(Sender: TObject);
    procedure DoListSelectionChange(Sender: TObject);
    procedure DoListDeleteKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoColorSpaceChange(Sender: TObject);

    procedure DoFindColorClick(Sender: TObject);
    procedure DoClearImageClick(Sender: TObject);
    procedure DoMatchColorClick(Sender: TObject);
    procedure DoUpdateImgClick(Sender: TObject);
    procedure DoLoadHSLCircleClick(Sender: TObject);

    procedure DoDeleteSelectedClick(Sender: TObject);
    procedure DoAddFromClipboardClick(Sender: TObject);
    procedure DoClearColorsClick(Sender: TObject);
    procedure DoSaveColorsClick(Sender: TObject);
    procedure DoLoadColorsClick(Sender: TObject);
    procedure DoDeleteColorsClick(Sender: TObject);
    procedure DoColorListPopup(Sender: TObject);
    procedure DoCopyBestColorClick(Sender: TObject);
    procedure DoColorMenuPopup(Sender: TObject);

    procedure AddColor(NewColor: TColor);
    function GetBest: TColorTolerance;
    function GetColors: TColorArray;
    procedure SetColors(Colors: TColorArray);
  public
    constructor Create(ImageSupplier: TImageSupplier); override;

    property Colors: TColorArray read GetColors write SetColors;
    property BestColor: TColorTolerance read GetBest;

    property ButtonFindColor: TSimbaButton read FButtonFindColor;
    property ButtonMatchColor: TSimbaButton read FButtonMatchColor;
    property ButtonClearImg: TSimbaButton read FButtonClearImg;
    property ButtonUpdateImg: TSimbaButton read FButtonUpdateImg;
  end;

implementation

uses
  IniFiles, Clipbrd, LCLType, TypInfo, LCLIntf, Dialogs, ExtCtrls,
  simba.env,
  simba.colormath_aca,
  simba.vartype_matrix,
  simba.vartype_string,
  simba.array_algorithm,
  simba.component_theme,
  simba.dialog;

type
  TColorNode = class(TTreeNode)
  public
    Color: TColor;
  end;

function GetColorsINI: TINIFile;
begin
  Result := TINIFile.Create(SimbaEnv.DataPath + 'aca.ini');
end;

procedure TSimbaACA.FillLoadDeleteColorMenus(LoadItem, DeleteItem: TMenuItem);
var
  Sections: TStringList;
  I: Integer;
begin
  if (LoadItem = nil) or (DeleteItem = nil) then
    Exit;
  LoadItem.Clear();
  DeleteItem.Clear();

  Sections := TStringList.Create();
  with GetColorsINI() do
  try
    ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      LoadItem.Add(NewItem(Sections[I], scNone, False, True, @DoLoadColorsClick, 0, ''));
      DeleteItem.Add(NewItem(Sections[I], scNone, False, True, @DoDeleteColorsClick, 0, ''));
    end;
  finally
    Free();
  end;
  Sections.Free();
end;

function TSimbaACA.GetColors: TColorArray;
var
  I: Integer;
begin
  SetLength(Result, FColorList.TopLevelCount);
  for I := 0 to High(Result) do
    Result[I] := TColorNode(FColorList.TopLevelItem[I]).Color;
end;

procedure TSimbaACA.SetColors(Colors: TColorArray);
var
  Col: TColor;
begin
  FColorList.BeginUpdate();
  FColorList.Clear();
  for Col in specialize TArrayUnique<TColor>.Unique(Colors) do
    TColorNode(FColorList.AddNode(ColorToStr(Col))).Color := Col;
  FColorList.EndUpdate();
end;

function TSimbaACA.GetColorSpace: EColorSpace;
begin
  case FColorSpaces.ToggleButtons.SelectedText of
    'RGB':    Result := EColorSpace.RGB;
    'HSV':    Result := EColorSpace.HSV;
    'HSL':    Result := EColorSpace.HSL;
    'XYZ':    Result := EColorSpace.XYZ;
    'LAB':    Result := EcolorSpace.LAB;
    'LCH':    Result := EColorSpace.LCH;
    'DeltaE': Result := EColorSpace.DELTAE;
    else
      Result := EColorSpace.RGB;
  end;
end;

procedure TSimbaACA.CalcBestColor;
var
  Best: TBestColor;
  FormatSettingsDot: TFormatSettings;
begin
  if (FColorList.Items.Count > 0) then
  begin
    Best := GetBestColor(GetColorSpace(), GetColors());

    FormatSettingsDot := FormatSettings;
    FormatSettingsDot.DecimalSeparator := '.';

    FEditColor.Edit.Text  := ColorToStr(Best.Color);
    FEditTol.Edit.Text    := Format('%.3f', [Best.Tolerance], FormatSettingsDot);
    FEditMulti1.Edit.Text := Format('%.3f', [Best.Mods[0]], FormatSettingsDot);
    FEditMulti2.Edit.Text := Format('%.3f', [Best.Mods[1]], FormatSettingsDot);
    FEditMulti3.Edit.Text := Format('%.3f', [Best.Mods[2]], FormatSettingsDot);
  end else
  begin
    FEditColor.Edit.Clear();
    FEditTol.Edit.Clear();
    FEditMulti1.Edit.Clear();
    FEditMulti2.Edit.Clear();
    FEditMulti3.Edit.Clear();
  end;
end;

procedure TSimbaACA.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_V) and (Shift = [ssCtrl]) then
  begin
    Key := 0;
    DoAddFromClipboardClick(nil);
  end
  else if (Key = VK_C) and (Shift = [ssCtrl]) and (not (ActiveControl is TSimbaEdit)) then
  begin
    Key := 0;
    DoCopyBestColorClick(nil);
  end else
    inherited DoFormKeyDown(Sender, Key, Shift);
end;

procedure TSimbaACA.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
begin
  inherited DoImgPaintArea(Sender, ACanvas, R);

  if (Length(FDebugTPA) > 0) then
    ACanvas.DrawPoints(FDebugTPA, FDrawColor)
  else if (Length(FDebugMat) > 0) then
    ACanvas.DrawHeatmap(FDebugMat);
end;

procedure TSimbaACA.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    AddColor(FImageBox.Background.Canvas.Pixels[X, Y]);
end;

// stolen from colorpickerhistory
procedure TSimbaACA.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
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
  BaseRect.Right += ColorRect.Width + 8;

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

procedure TSimbaACA.DoListModify(Sender: TObject);
begin
  CalcBestColor();
  FButtonClearImg.Click();
end;

procedure TSimbaACA.DoListSelectionChange(Sender: TObject);
begin
  if (FColorList.Selected <> nil) then
    FImageBoxZoom.Fill(TColorNode(FColorList.Selected).Color);
end;

procedure TSimbaACA.DoListDeleteKey(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  FColorList.DeleteSelection();
end;

procedure TSimbaACA.DoColorSpaceChange(Sender: TObject);
begin
  CalcBestColor();
  if (Length(FDebugTPA) > 0) then
    FButtonFindColor.Click()
  else if (Length(FDebugMat) > 0) then
    FButtonMatchColor.Click();
end;

procedure TSimbaACA.DoFindColorClick(Sender: TObject);
begin
  FDebugMat := [];
  FDebugTPA := FImageBox.FindColor(BestColor);

  FImageBox.Status := Format('Found %.0n matches', [Double(Length(FDebugTPA))]);
  FImageBox.RePaint();
end;

procedure TSimbaACA.DoClearImageClick(Sender: TObject);
begin
  if (Length(FDebugTPA) > 0) or (FDebugMat.Area > 0) then
  begin
    FDebugTPA := [];
    FDebugMat := [];
    FImageBox.Repaint();
  end;
end;

procedure TSimbaACA.DoMatchColorClick(Sender: TObject);
begin
  FDebugTPA := [];
  FDebugMat := FImageBox.MatchColor(BestColor).NormMinMax(0, 1);
  FImageBox.Repaint();
end;

procedure TSimbaACA.DoUpdateImgClick(Sender: TObject);

  procedure SetImageNormal;
  begin
    FImageBox.SetImage(FImageSupplier());
  end;

  procedure SetImageFromLape;
  var
    LapeObject: TByteArray;
  begin
    LapeObject := FImageSupplierLape();

    FImageBox.SetImage(
      PSimbaImage(LapeObject)^,
      PSizeInt(LapeObject)[-2] <= 1 // If has no image references, we need to free it
    );
  end;

begin
  if Assigned(FImageSupplierLape) then
    SetImageFromLape()
  else if Assigned(FImageSupplier) then
    SetImageNormal();

  if (Length(FDebugTPA) > 0) then
    FButtonFindColor.Click()
  else if (Length(FDebugMat) > 0) then
    FButtonMatchColor.Click();
end;

procedure TSimbaACA.DoLoadHSLCircleClick(Sender: TObject);
var
  Value: String;
  Img: TSimbaImage;
begin
  if InputQuery('Simba - ACA', 'HSL Circle Radius?', Value) and Value.IsNumeric then
  begin
    FButtonClearImg.Click();

    Img := TSimbaImage.Create(Value.ToInt()*2, Value.ToInt()*2);
    Img.DrawHSLCircle(Img.Center, Value.ToInt());

    FImageBox.SetImage(Img);
  end;
end;

procedure TSimbaACA.DoDeleteSelectedClick(Sender: TObject);
begin
  FColorList.DeleteSelection();
end;

procedure TSimbaACA.DoAddFromClipboardClick(Sender: TObject);
var
  Str: String;
  Numbers: TStringArray;
begin
  try
    Str := Clipboard.AsText;
    Numbers := Str.ExtractNumbers();

    FColorList.Items.BeginUpdate();
    try
      for Str in Numbers do
        if Str.IsInteger and (Str.ToInt >= 0) and (Str.ToInt <= $FFFFFFFF) then
          AddColor(StrToColor(Str));
    finally
      FColorList.Items.EndUpdate();
    end;
  except
  end;
end;

procedure TSimbaACA.DoClearColorsClick(Sender: TObject);
begin
  FColorList.Clear();
end;

procedure TSimbaACA.DoSaveColorsClick(Sender: TObject);
var
  Col: TColor;
  ColorName, ColorsString: String;
begin
  if InputQuery('Auto Color Aid', 'Save under what name?', ColorName) then
    with GetColorsINI() do
    try
      ColorsString := '';
      for Col in GetColors() do
        ColorsString := ColorsString + ColorToStr(Col) + ',';
      WriteString(ColorName, 'Colors', ColorsString);
    finally
      Free();
    end;
end;

procedure TSimbaACA.DoLoadColorsClick(Sender: TObject);
var
  Str: String;
begin
  FColorList.BeginUpdate();
  FColorList.Clear();

  with GetColorsINI() do
  try
    for Str in ReadString(TMenuItem(Sender).Caption, 'Colors', '').ExtractNumbers() do
      if Str.IsInteger and (Str.ToInt >= 0) and (Str.ToInt <= $FFFFFFFF) then
        AddColor(StrToColor(Str));
  finally
    Free();
  end;

  FColorList.EndUpdate();
end;

procedure TSimbaACA.DoDeleteColorsClick(Sender: TObject);
begin
  with GetColorsINI() do
  try
    EraseSection(TMenuItem(Sender).Caption);
  finally
    Free();
  end;

  FColorList.PopupMenu.Close();
end;

procedure TSimbaACA.DoColorListPopup(Sender: TObject);
begin
  FillLoadDeleteColorMenus(
    TPopupMenu(Sender).Items.Find('Load'),
    TPopupMenu(Sender).Items.Find('Delete')
  );
end;

procedure TSimbaACA.DoCopyBestColorClick(Sender: TObject);
begin
  Clipboard.AsText := Format('[%s, %s, %s, [%s, %s, %s]]', [
    ColorToStr(StrToColor(FEditColor.Edit.Text)),
    FEditTol.Edit.Text,
    'EColorSpace.' + GetEnumName(TypeInfo(EColorSpace), Ord(GetColorSpace())),
    FEditMulti1.Edit.Text,
    FEditMulti2.Edit.Text,
    FEditMulti3.Edit.Text
  ]);
end;

procedure TSimbaACA.DoColorMenuPopup(Sender: TObject);
begin
  FillLoadDeleteColorMenus(
    TPopupMenu(Sender).Items.Find('Load Colors'),
    TPopupMenu(Sender).Items.Find('Delete Colors')
  );
end;

constructor TSimbaACA.Create(ImageSupplier: TImageSupplier);

  function CreateColorMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    Result.OnPopup := @DoColorMenuPopup;
    Result.Items.Add(NewItem('Clear Color List', scNone, False, True, @DoClearColorsClick, 0, ''));
    Result.Items.Add(NewItem('Add Colors From Clipboard', ShortCut(VK_V, [ssCtrl]), False, True, @DoAddFromClipboardClick, 0, ''));
    Result.Items.Add(NewLine());
    Result.Items.Add(NewItem('Load Colors', scNone, False, True, nil, 0, ''));
    Result.Items.Add(NewItem('Save Colors ...', scNone, False, True, @DoSaveColorsClick, 0, ''));
    Result.Items.Add(NewItem('Delete Colors', scNone, False, True, @DoDeleteColorsClick, 0, ''));
    Result.Items.Add(NewLine());
    Result.Items.Add(NewItem('Copy Best Color', ShortCut(VK_C, [ssCtrl]), False, True, @DoCopyBestColorClick, 0, ''));
  end;

  function CreateListPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    Result.OnPopup := @DoColorListPopup;
    Result.Items.Add(NewItem('Delete Selected', scNone, False, True, @DoDeleteSelectedClick, 0, ''));
    Result.Items.Add(NewLine);
    Result.Items.Add(NewItem('Clear', scNone, False, True, @DoClearColorsClick, 0, ''));
    Result.Items.Add(NewItem('Add From Clipboard', scNone, False, True, @DoAddFromClipboardClick, 0, ''));
    Result.Items.Add(NewLine);
    Result.Items.Add(NewItem('Save ...', scNone, False, True, @DoSaveColorsClick, 0, ''));
    Result.Items.Add(NewItem('Load', scNone, False, True, nil, 0, ''));
    Result.Items.Add(NewItem('Delete', scNone, False, True, @DoDeleteColorsClick, 0, ''));
  end;

var
  SubSidePanel: TPanel;
begin
  inherited Create(ImageSupplier);

  Caption := 'Auto Color Aid';

  FMenuBar.AddMenu('Colors', CreateColorMenu());
  FImageMenu.Items.Insert(2, NewItem('Load HSL Circle', scNone, False, True, @DoLoadHSLCircleClick, 0, ''));

  SubSidePanel := TPanel.Create(Self);
  SubSidePanel.Parent := FSidePanel;
  SubSidePanel.Align := alClient;
  SubSidePanel.BevelOuter := bvNone;
  SubSidePanel.ChildSizing.VerticalSpacing := 5;

  FColorList := TSimbaTreeView.Create(Self, TColorNode);
  FColorList.Parent := SubSidePanel;
  FColorList.Align := alClient;
  FColorList.OnModify := @DoListModify;
  FColorList.OnPaintNode := @DoPaintNode;
  FColorList.OnSelectionChange := @DoListSelectionChange;
  FColorList.AddKeyEvent(VK_DELETE, [], @DoListDeleteKey);
  FColorList.FilterVisible := False;
  FColorList.BorderSpacing.Right := 5;
  FColorList.BorderSpacing.Bottom := 2;
  FColorList.PopupMenu := CreateListPopupMenu();
  FColorList.HideRoot();

  FColorSpaces := TSimbaLabeledToggleButtonGroup.Create(SubSidePanel);
  with FColorSpaces do
  begin
    Parent := SubSidePanel;
    Align := alBottom;
    Color := SimbaComponentTheme.ColorFrame;

    ToggleButtons.Add('RGB').MeasureText := 'DeltaE';
    ToggleButtons.Add('HSL').MeasureText := 'DeltaE';
    ToggleButtons.Add('HSV').MeasureText := 'DeltaE';
    ToggleButtons.Add('LCH').MeasureText := 'DeltaE';
    ToggleButtons.Add('DeltaE').MeasureText := 'DeltaE';
    //ToggleButtons.Add('XYZ'); not implemented properly yet
    //ToggleButtons.Add('LAB'); ...

    Caption := 'Color Space';

    ToggleButtons.OnChange := @DoColorSpaceChange;
  end;

  FEditMulti3 := TSimbaLabeledEdit.Create(SubSidePanel);
  FEditMulti3.Parent := SubSidePanel;
  FEditMulti3.Align := alBottom;
  FEditMulti3.Caption := 'Best Multiplier[2]';
  FEditMulti3.LabelMeasure := 'Best Multiplier[2]';
  FEditMulti3.BorderSpacing.Top := 4;
  FEditMulti3.Color := SimbaComponentTheme.ColorFrame;

  FEditMulti2 := TSimbaLabeledEdit.Create(SubSidePanel);
  FEditMulti2.Parent := SubSidePanel;
  FEditMulti2.Align := alBottom;
  FEditMulti2.Caption := 'Best Multiplier[1]';
  FEditMulti2.LabelMeasure := 'Best Multiplier[1]';
  FEditMulti2.BorderSpacing.Top := 4;
  FEditMulti2.Color := SimbaComponentTheme.ColorFrame;

  FEditMulti1 := TSimbaLabeledEdit.Create(SubSidePanel);
  FEditMulti1.Parent := SubSidePanel;
  FEditMulti1.Align := alBottom;
  FEditMulti1.Caption := 'Best Multiplier[0]';
  FEditMulti1.LabelMeasure := 'Best Multiplier[0]';
  FEditMulti1.BorderSpacing.Top := 4;
  FEditMulti1.Color := SimbaComponentTheme.ColorFrame;

  FEditTol := TSimbaLabeledEdit.Create(SubSidePanel);
  FEditTol.Parent := SubSidePanel;
  FEditTol.Align := alBottom;
  FEditTol.Caption := 'Best Tolerance';
  FEditTol.LabelMeasure := 'Best Multiplier[0]';
  FEditTol.BorderSpacing.Top := 4;
  FEditTol.Color := SimbaComponentTheme.ColorFrame;

  FEditColor := TSimbaLabeledEdit.Create(SubSidePanel);
  FEditColor.Parent := SubSidePanel;
  FEditColor.Align := alBottom;
  FEditColor.Caption := 'Best Color';
  FEditColor.LabelMeasure := 'Best Multiplier[0]';
  FEditColor.BorderSpacing.Top := 4;
  FEditColor.Color := SimbaComponentTheme.ColorFrame;

  FButtonFindColor  := addButton('Find Color', @DoFindColorClick);
  FButtonMatchColor := addButton('Match Color', @DoMatchColorClick);
  FButtonUpdateImg  := addButton('Update Image', @DoUpdateImgClick);
  FButtonClearImg   := addButton('Clear Image', @DoClearImageClick);
end;

procedure TSimbaACA.AddColor(NewColor: TColor);
var
  I: Integer;
begin
  // check duplicate
  for I := 0 to FColorList.TopLevelCount - 1 do
    if (TColorNode(FColorList.TopLevelItem[I]).Color = NewColor)then
      Exit;

  // Wrap with begin/endupdate so modify event doesnt fire before .Color is assigned
  FColorList.BeginUpdate();
  TColorNode(FColorList.AddNode(ColorToStr(NewColor))).Color := NewColor;
  FColorList.EndUpdate();
end;

function TSimbaACA.GetBest: TColorTolerance;
begin
  Result.ColorSpace := GetColorSpace();
  Result.Color := String(FEditColor.Edit.Text).ToInt(0);
  Result.Tolerance := String(FEditTol.Edit.Text).ToFloat(0);
  Result.Multipliers[0] := String(FEditMulti1.Edit.Text).ToFloat(0);
  Result.Multipliers[1] := String(FEditMulti2.Edit.Text).ToFloat(0);
  Result.Multipliers[2] := String(FEditMulti3.Edit.Text).ToFloat(0);
end;

end.

