{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.aca;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Forms, Graphics, Menus,
  simba.base, simba.colormath, simba.target;

  // ShowOnTop
  procedure ShowACA(Target: TSimbaTarget; ManageTarget: Boolean); overload;
  // ShowModal
  procedure ShowACA(Target: TSimbaTarget; ManageTarget: Boolean; out Color: TColorTolerance); overload;

implementation

uses
  IniFiles, Dialogs, Clipbrd, LCLType, TypInfo,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.component_imageboxzoom,
  simba.component_treeview,
  simba.component_button,
  simba.component_edit,
  simba.component_menubar,
  simba.env,
  simba.image,
  simba.component_divider,
  simba.colormath_aca,
  simba.ide_theme,
  simba.vartype_string,
  simba.vartype_matrix,
  simba.threading;

type
  TSimbaACA = class
  protected
    FForm: TForm;
    FMenuBar: TSimbaMenuBar;
    FPanel: TPanel;
    FImageBox: TSimbaImageBox;
    FImageBoxZoom: TSimbaImageBoxZoomPanel;
    FColorList: TSimbaTreeView;
    FColorListPopup: TPopupMenu;
    FDrawColorMenu: TMenuItem;
    FDrawColor: TColor;
    FTarget: TSimbaTarget;
    FManageTarget: Boolean;

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

    function GetColors: TColorArray;
    function GetColorSpace: EColorSpace;
    procedure CalcBestColor;

    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure DoLoadImageClick(Sender: TObject);

    procedure DoDeleteSelectedClick(Sender: TObject);
    procedure DoAddFromClipboardClick(Sender: TObject);
    procedure DoClearColorsClick(Sender: TObject);
    procedure DoSaveColorsClick(Sender: TObject);
    procedure DoLoadColorsClick(Sender: TObject);
    procedure DoDeleteColorsClick(Sender: TObject);
    procedure DoColorListPopup(Sender: TObject);
    procedure DoDrawColorChange(Sender: TObject);
    procedure DoCopyBestColorClick(Sender: TObject);
    procedure DoColorMenuPopup(Sender: TObject);

    procedure Add(Color: TColor);
    function GetBest: TColorTolerance;
  public
    constructor Create(Target: TSimbaTarget; ManageTarget: Boolean = True); reintroduce;
    destructor Destroy; override;

    property BestColor: TColorTolerance read GetBest;
    property Form: TForm read FForm;
  end;

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
  Colors: TColorArray;
  Best: TBestColor;
  FormatSettingsDot: TFormatSettings;
begin
  Colors := GetColors();

  if (Length(GetColors) > 0) then
  begin
    Best := GetBestColor(GetColorSpace(), Colors);

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
  else if (Key = VK_C) and (Shift = [ssCtrl]) and (not (FForm.ActiveControl is TSimbaEdit)) then
  begin
    Key := 0;
    DoCopyBestColorClick(nil);
  end
  else if (Key = VK_F5) then
  begin
    Key := 0;
    DoUpdateImgClick(nil);
  end;
end;

procedure TSimbaACA.DoFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not (fsModal in FForm.FormState)) then // non modal, free
    QueueOnMainThread(@Self.Free);
end;

procedure TSimbaACA.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
begin
  if (Length(FDebugTPA) > 0) then
    ACanvas.DrawPoints(FDebugTPA, FDrawColor)
  else if (Length(FDebugMat) > 0) then
    ACanvas.DrawHeatmap(FDebugMat);
end;

procedure TSimbaACA.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  FImageBoxZoom.Move(FImageBox.Background.Canvas, X ,Y);
end;

procedure TSimbaACA.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    Add(FImageBox.Background.Canvas.Pixels[X, Y]);
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
    ACanvas.Brush.Color := SimbaTheme.ColorActive;
    ACanvas.FillRect(BaseRect);
  end;

  ACanvas.Brush.Color := TColorNode(Node).Color;
  ACanvas.Pen.Color := SimbaTheme.ColorFont;
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
begin
  FImageBox.SetImage(FTarget.GetImage());
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

procedure TSimbaACA.DoLoadImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute and FileExists(FileName) then
    try
      FImageBox.SetImage(TSimbaImage.Create(FileName));
    except
    end;
  finally
    Free();
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
          Add(StrToColor(Str));
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
  col: TColor;
  Name,s: String;
begin
  if InputQuery('Auto Color Aid', 'Save under what name?', Name) then
    with GetColorsINI() do
    try
      s := '';
      for col in GetColors() do
        s += ColorToStr(col) + ',';
      WriteString(Name, 'Colors', s);
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
        Add(StrToColor(Str));
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

  FColorListPopup.Close();
end;

procedure TSimbaACA.DoColorListPopup(Sender: TObject);
begin
  FillLoadDeleteColorMenus(
    TPopupMenu(Sender).Items.Find('Load'),
    TPopupMenu(Sender).Items.Find('Delete')
  );
end;

procedure TSimbaACA.DoDrawColorChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FDrawColorMenu.Count - 1 do
    FDrawColorMenu.Items[I].Checked := FDrawColorMenu.Items[I] = Sender;

  case TMenuItem(Sender).Caption of
    'Red':    FDrawColor := clRed;
    'Green':  FDrawColor := clGreen;
    'Blue':   FDrawColor := clBlue;
    'Yellow': FDrawColor := clYellow;
    'Aqua':   FDrawColor := clAqua;
  end;

  FImageBox.Repaint();
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

constructor TSimbaACA.Create(Target: TSimbaTarget; ManageTarget: Boolean);

  function CreateColorMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(FForm);
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

  function CreateImageMenu: TPopupMenu;
  begin
    FDrawColorMenu := NewItem('Draw Color', scNone, False, True, nil, 0, '');
    FDrawColorMenu.Add(NewItem('Red', scNone, True, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Green', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Blue', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Yellow', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Aqua', scNone, False, True, @DoDrawColorChange, 0, ''));

    Result := TPopupMenu.Create(FForm);
    Result.Items.Add(NewItem('Load HSL Circle', scNone, False, True, @DoLoadHSLCircleClick, 0, ''));
    Result.Items.Add(NewItem('Load Image', scNone, False, True, @DoLoadImageClick, 0, ''));
    Result.Items.Add(NewItem('Update Image', ShortCut(VK_F5, []), False, True, nil, 0, ''));
    Result.Items.Add(NewLine());
    Result.Items.Add(FDrawColorMenu);
  end;

  function CreateListPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(FForm);
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
  BottomPanel, ButtonPanel: TPanel;
begin
  inherited Create();

  FTarget := Target;
  FManageTarget := ManageTarget;
  FDrawColor := clRed;

  FForm := TForm.Create(nil);
  FForm.Caption := 'Auto Color Aid';
  FForm.Position := poScreenCenter;
  FForm.Width := FForm.Scale96ToScreen(1200);
  FForm.Height := FForm.Scale96ToScreen(800);
  FForm.Font.Color := SimbaTheme.ColorFont;
  FForm.KeyPreview := True;
  FForm.OnKeyDown := @DoFormKeyDown;
  FForm.OnClose := @DoFormClose;

  FMenuBar := TSimbaMenuBar.Create(FForm);
  FMenuBar.Parent := FForm;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('Colors', CreateColorMenu());
  FMenuBar.AddMenu('Image', CreateImageMenu());

  FPanel := TPanel.Create(FForm);
  FPanel.Parent := FForm;
  FPanel.Align := alRight;
  FPanel.AutoSize := True;
  FPanel.BevelOuter := bvNone;
  FPanel.BevelInner := bvNone;
  FPanel.Color := SimbaTheme.ColorFrame;

  FImageBox := TSimbaImageBox.Create(FForm);
  FImageBox.Parent := FForm;
  FImageBox.Align := alClient;
  FImageBox.SetImage(FTarget.GetImage());
  FImageBox.OnImgMouseMove := @DoImgMouseMove;
  FImageBox.OnImgMouseDown := @DoImgMouseDown;
  FImageBox.OnImgPaint := @DoImgPaintArea;

  FImageBoxZoom := TSimbaImageBoxZoomPanel.Create(FPanel);
  FImageBoxZoom.Parent := FPanel;
  FImageBoxZoom.Align := alTop;
  FImageBoxZoom.Font.Color := SimbaTheme.ColorFont;
  FImageBoxZoom.FrameColor := SimbaTheme.ColorScrollBarActive;

  FColorListPopup := CreateListPopupMenu();

  FColorList := TSimbaTreeView.Create(FForm, TColorNode);
  FColorList.Parent := FPanel;
  FColorList.Align := alClient;
  FColorList.OnModify := @DoListModify;
  FColorList.OnPaintNode := @DoPaintNode;
  FColorList.OnSelectionChange := @DoListSelectionChange;
  FColorList.AddKeyEvent(VK_DELETE, [], @DoListDeleteKey);
  FColorList.FilterVisible := False;
  FColorList.BorderSpacing.Left := 5;
  FColorList.BorderSpacing.Right := 5;
  FColorList.BorderSpacing.Bottom := 2;
  FColorList.PopupMenu := FColorListPopup;
  FColorList.HideRoot();

  BottomPanel := TPanel.Create(FPanel);
  BottomPanel.Parent := FPanel;
  BottomPanel.Align := alBottom;
  BottomPanel.AutoSize := True;
  BottomPanel.BevelOuter := bvNone;

  ButtonPanel := TPanel.Create(BottomPanel);
  ButtonPanel.Parent := BottomPanel;
  ButtonPanel.Align := alTop;
  ButtonPanel.AutoSize := True;
  ButtonPanel.ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ButtonPanel.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  ButtonPanel.ChildSizing.ControlsPerLine := 2;
  ButtonPanel.BevelOuter := bvNone;

  with TSimbaDivider.Create(FPanel) do
  begin
    Parent := BottomPanel;
    BorderSpacing.Top := 5;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
    Align := alTop;
  end;

  FButtonFindColor := TSimbaButton.Create(FPanel);
  FButtonFindColor.Parent := ButtonPanel;
  FButtonFindColor.Caption := 'Find Color';
  FButtonFindColor.BorderSpacing.Around := 5;
  FButtonFindColor.OnClick := @DoFindColorClick;

  FButtonClearImg := TSimbaButton.Create(FPanel);
  FButtonClearImg.Parent := ButtonPanel;
  FButtonClearImg.Caption := 'Clear Image';
  FButtonClearImg.BorderSpacing.Around := 5;
  FButtonClearImg.OnClick := @DoClearImageClick;

  FButtonMatchColor := TSimbaButton.Create(FPanel);
  FButtonMatchColor.Parent := ButtonPanel;
  FButtonMatchColor.Caption := 'Match Color';
  FButtonMatchColor.BorderSpacing.Around := 5;
  FButtonMatchColor.OnClick := @DoMatchColorClick;

  FButtonUpdateImg := TSimbaButton.Create(FPanel);
  FButtonUpdateImg.Parent := ButtonPanel;
  FButtonUpdateImg.Caption := 'Update Image';
  FButtonUpdateImg.BorderSpacing.Around := 5;
  FButtonUpdateImg.OnClick := @DoUpdateImgClick;

  FEditMulti3 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti3.Parent := BottomPanel;
  FEditMulti3.Align := alTop;
  FEditMulti3.Caption := 'Best B Multiplier';
  FEditMulti3.LabelMeasure := 'Best R Multipliers';
  FEditMulti3.BorderSpacing.Top := 4;
  FEditMulti3.Color := SimbaTheme.ColorFrame;

  FEditMulti2 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti2.Parent := BottomPanel;
  FEditMulti2.Align := alTop;
  FEditMulti2.Caption := 'Best G Multiplier';
  FEditMulti2.LabelMeasure := 'Best R Multipliers';
  FEditMulti2.BorderSpacing.Top := 4;
  FEditMulti2.Color := SimbaTheme.ColorFrame;

  FEditMulti1 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti1.Parent := BottomPanel;
  FEditMulti1.Align := alTop;
  FEditMulti1.Caption := 'Best R Multiplier';
  FEditMulti1.LabelMeasure := 'Best R Multipliers';
  FEditMulti1.BorderSpacing.Top := 4;
  FEditMulti1.Color := SimbaTheme.ColorFrame;

  FEditTol := TSimbaLabeledEdit.Create(FPanel);
  FEditTol.Parent := BottomPanel;
  FEditTol.Align := alTop;
  FEditTol.Caption := 'Best Tolerance';
  FEditTol.LabelMeasure := 'Best R Multipliers';
  FEditTol.BorderSpacing.Top := 4;
  FEditTol.Color := SimbaTheme.ColorFrame;

  FEditColor := TSimbaLabeledEdit.Create(FPanel);
  FEditColor.Parent := BottomPanel;
  FEditColor.Align := alTop;
  FEditColor.Caption := 'Best Color';
  FEditColor.LabelMeasure := 'Best R Multipliers';
  FEditColor.BorderSpacing.Top := 4;
  FEditColor.Color := SimbaTheme.ColorFrame;

  with TSimbaDivider.Create(FPanel) do
  begin
    Parent := BottomPanel;
    Align := alTop;
    BorderSpacing.Top := 2;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
    BorderSpacing.Bottom := 5;
  end;

  FColorSpaces := TSimbaLabeledToggleButtonGroup.Create(FPanel);
  with FColorSpaces do
  begin
    Parent := BottomPanel;
    Align := alTop;
    Color := SimbaTheme.ColorFrame;

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
end;

destructor TSimbaACA.Destroy;
begin
  if (FForm <> nil) then
    FreeAndNil(FForm);
  if (FTarget <> nil) and FManageTarget then
    FreeAndNil(FTarget);

  inherited Destroy();
end;

procedure TSimbaACA.Add(Color: TColor);
var
  I: Integer;
begin
  // check duplicate
  for I := 0 to FColorList.TopLevelCount - 1 do
    if (TColorNode(FColorList.TopLevelItem[I]).Color = Color)then
      Exit;

  TColorNode(FColorList.AddNode(ColorToStr(Color))).Color := Color;
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

procedure ShowACA(Target: TSimbaTarget; ManageTarget: Boolean);
begin
  with TSimbaACA.Create(Target, ManageTarget) do
    Form.ShowOnTop();
end;

procedure ShowACA(Target: TSimbaTarget; ManageTarget: Boolean; out Color: TColorTolerance);
begin
  with TSimbaACA.Create(Target, ManageTarget) do
  try
    Form.ShowModal();

    Color := BestColor;
  finally
    Free();
  end;
end;

end.

