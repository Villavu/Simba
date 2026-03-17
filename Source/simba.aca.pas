{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.aca;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Forms, Menus, Graphics,
  simba.base,
  simba.image,
  simba.colormath,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.component_imageboxzoom,
  simba.component_treeview,
  simba.component_button,
  simba.component_edit,
  simba.component_menubar,
  simba.component_splitter;

type
  TACAImageSupplier = function(): TSimbaImage of object;
  TACAImageSupplierLape = function(): TByteArray of object;

  TSimbaACA = class(TForm)
  private const
    DEF_WIDTH = 1100;
    DEF_HEIGHT = 700;
  protected
    FMenuBar: TSimbaMenuBar;
    FPanel: TPanel;
    FImageBox: TSimbaImageBox;
    FImageBoxZoom: TSimbaImageBoxZoomPanel;
    FColorList: TSimbaTreeView;
    FColorListPopup: TPopupMenu;
    FDrawColorMenu: TMenuItem;
    FDrawColor: TColor;
    FImageSupplier: TACAImageSupplier;
    FImageSupplierLape: TACAImageSupplierLape;

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

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoFirstShow; override;

    procedure FillLoadDeleteColorMenus(LoadItem, DeleteItem: TMenuItem);

    function GetColorSpace: EColorSpace;
    procedure CalcBestColor;

    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

    procedure AddColor(NewColor: TColor);
    function GetBest: TColorTolerance;
    function GetUserPanel: TPanel;
    function GetColors: TColorArray;
    procedure SetColors(Colors: TColorArray);
    procedure SetImage(Image: TSimbaImage);
  public
    FreeOnClose: Boolean;

    constructor Create(ImageSupplier: TACAImageSupplier); reintroduce;
    constructor CreateLape(ImageSupplier: TACAImageSupplierLape); reintroduce;

    property UserPanel: TPanel read GetUserPanel;
    property Image: TSimbaImage write SetImage;
    property ImageBox: TSimbaImageBox read FImageBox;
    property DrawColor: TColor read FDrawColor write FDrawColor;
    property Colors: TColorArray read GetColors write SetColors;
    property BestColor: TColorTolerance read GetBest;

    property ButtonFindColor: TSimbaButton read FButtonFindColor;
    property ButtonMatchColor: TSimbaButton read FButtonMatchColor;
    property ButtonClearImg: TSimbaButton read FButtonClearImg;
    property ButtonUpdateImg: TSimbaButton read FButtonUpdateImg;
  end;

implementation

uses
  IniFiles, Dialogs, Clipbrd, LCLType, TypInfo, LCLIntf,
  simba.env,
  simba.component_divider,
  simba.colormath_aca,
  simba.component_theme,
  simba.vartype_string,
  simba.vartype_matrix,
  simba.threading,
  simba.array_algorithm;

type
  TColorNode = class(TTreeNode)
  public
    Color: TColor;
  end;

function GetColorsINI: TINIFile;
begin
  Result := TINIFile.Create(SimbaEnv.DataPath + 'aca.ini');
end;

function TSimbaACA.GetUserPanel: TPanel;
begin
  Result := FImageBox.UserPanel;
end;

procedure TSimbaACA.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if FreeOnClose then
    CloseAction := caFree;
end;

procedure TSimbaACA.DoFirstShow;
begin
  inherited DoFirstShow();

  DoUpdateImgClick(Self);
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

procedure TSimbaACA.SetImage(Image: TSimbaImage);
begin
  FImageBox.SetImage(Image);
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
  end
  else if (Key = VK_F5) then
  begin
    Key := 0;
    DoUpdateImgClick(nil);
  end;
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
  if FImageBox.MouseInClient then
    FImageBoxZoom.Move(FImageBox.Background.Canvas, X ,Y);
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

constructor TSimbaACA.Create(ImageSupplier: TACAImageSupplier);

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

  function CreateImageMenu: TPopupMenu;
  begin
    FDrawColorMenu := NewItem('Draw Color', scNone, False, True, nil, 0, '');
    FDrawColorMenu.Add(NewItem('Red', scNone, True, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Green', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Blue', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Yellow', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Aqua', scNone, False, True, @DoDrawColorChange, 0, ''));

    Result := TPopupMenu.Create(Self);
    Result.Items.Add(NewItem('Load HSL Circle', scNone, False, True, @DoLoadHSLCircleClick, 0, ''));
    Result.Items.Add(NewItem('Load Image', scNone, False, True, @DoLoadImageClick, 0, ''));
    Result.Items.Add(NewItem('Update Image', ShortCut(VK_F5, []), False, True, nil, 0, ''));
    Result.Items.Add(NewLine());
    Result.Items.Add(FDrawColorMenu);
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
  BottomPanel, ButtonPanel: TPanel;
begin
  inherited CreateNew(nil);

  FImageSupplier := ImageSupplier;
  FDrawColor := clRed;

  Caption := 'Auto Color Aid';
  Width := Min(Scale96ToScreen(DEF_WIDTH), Monitor.WorkareaRect.Width - 200);
  Height := Min(Scale96ToScreen(DEF_HEIGHT), Monitor.WorkareaRect.Height - 100);
  Position := poScreenCenter;

  Font.Color := SimbaComponentTheme.ColorFont;
  KeyPreview := True;
  OnKeyDown := @DoFormKeyDown;
  ShowInTaskBar := stAlways;

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := Self;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('Colors', CreateColorMenu());
  FMenuBar.AddMenu('Image', CreateImageMenu());

  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alRight;
  FPanel.BevelOuter := bvNone;
  FPanel.BevelInner := bvNone;
  FPanel.Color := SimbaComponentTheme.ColorFrame;
  FPanel.Constraints.MinWidth := 150;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    FPanel.Width := Canvas.TextWidth('Best R Multipliers') * 3;
  finally
    Free();
  end;

  with TSimbaSplitter.Create(Self) do
  begin
    Parent := Self;
    Align := alRight;
  end;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnImgMouseMove := @DoImgMouseMove;
  FImageBox.OnImgMouseDown := @DoImgMouseDown;
  FImageBox.OnImgPaint := @DoImgPaintArea;

  FImageBoxZoom := TSimbaImageBoxZoomPanel.Create(FPanel);
  FImageBoxZoom.Parent := FPanel;
  FImageBoxZoom.Align := alTop;
  FImageBoxZoom.BorderSpacing.Top := 5;
  FImageBoxZoom.BorderSpacing.Bottom := 5;
  FImageBoxZoom.Font.Color := SimbaComponentTheme.ColorFont;
  FImageBoxZoom.FrameColor := SimbaComponentTheme.ColorScrollBarActive;

  FColorListPopup := CreateListPopupMenu();

  FColorList := TSimbaTreeView.Create(Self, TColorNode);
  FColorList.Parent := FPanel;
  FColorList.Align := alClient;
  FColorList.OnModify := @DoListModify;
  FColorList.OnPaintNode := @DoPaintNode;
  FColorList.OnSelectionChange := @DoListSelectionChange;
  FColorList.AddKeyEvent(VK_DELETE, [], @DoListDeleteKey);
  FColorList.FilterVisible := False;
  FColorList.BorderSpacing.Right := 5;
  FColorList.BorderSpacing.Bottom := 2;
  FColorList.PopupMenu := FColorListPopup;
  FColorList.HideRoot();

  BottomPanel := TPanel.Create(FPanel);
  BottomPanel.Parent := FPanel;
  BottomPanel.Align := alBottom;
  BottomPanel.AutoSize := True;
  BottomPanel.BevelOuter := bvNone;
  BottomPanel.BorderSpacing.Right := 5;

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
    Align := alTop;
  end;

  FButtonFindColor := TSimbaButton.Create(FPanel);
  FButtonFindColor.Parent := ButtonPanel;
  FButtonFindColor.Caption := 'Find Color';
  FButtonFindColor.BorderSpacing.Top := 5;
  FButtonFindColor.BorderSpacing.Bottom := 5;
  FButtonFindColor.BorderSpacing.Right := 5;
  FButtonFindColor.OnClick := @DoFindColorClick;

  FButtonClearImg := TSimbaButton.Create(FPanel);
  FButtonClearImg.Parent := ButtonPanel;
  FButtonClearImg.Caption := 'Clear Image';
  FButtonClearImg.BorderSpacing.Top := 5;
  FButtonClearImg.BorderSpacing.Bottom := 5;
  FButtonClearImg.BorderSpacing.Right := 5;
  FButtonClearImg.OnClick := @DoClearImageClick;

  FButtonMatchColor := TSimbaButton.Create(FPanel);
  FButtonMatchColor.Parent := ButtonPanel;
  FButtonMatchColor.Caption := 'Match Color';
  FButtonMatchColor.BorderSpacing.Top := 5;
  FButtonMatchColor.BorderSpacing.Bottom := 5;
  FButtonMatchColor.BorderSpacing.Left := 5;
  FButtonMatchColor.OnClick := @DoMatchColorClick;

  FButtonUpdateImg := TSimbaButton.Create(FPanel);
  FButtonUpdateImg.Parent := ButtonPanel;
  FButtonUpdateImg.Caption := 'Update Image';
  FButtonUpdateImg.BorderSpacing.Top := 5;
  FButtonUpdateImg.BorderSpacing.Bottom := 5;
  FButtonUpdateImg.BorderSpacing.Left := 5;
  FButtonUpdateImg.OnClick := @DoUpdateImgClick;

  FEditMulti3 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti3.Parent := BottomPanel;
  FEditMulti3.Align := alTop;
  FEditMulti3.Caption := 'Best Multiplier[2]';
  FEditMulti3.LabelMeasure := 'Best Multiplier[2]';
  FEditMulti3.BorderSpacing.Top := 4;
  FEditMulti3.Color := SimbaComponentTheme.ColorFrame;

  FEditMulti2 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti2.Parent := BottomPanel;
  FEditMulti2.Align := alTop;
  FEditMulti2.Caption := 'Best Multiplier[1]';
  FEditMulti2.LabelMeasure := 'Best Multiplier[1]';
  FEditMulti2.BorderSpacing.Top := 4;
  FEditMulti2.Color := SimbaComponentTheme.ColorFrame;

  FEditMulti1 := TSimbaLabeledEdit.Create(FPanel);
  FEditMulti1.Parent := BottomPanel;
  FEditMulti1.Align := alTop;
  FEditMulti1.Caption := 'Best Multiplier[0]';
  FEditMulti1.LabelMeasure := 'Best Multiplier[0]';
  FEditMulti1.BorderSpacing.Top := 4;
  FEditMulti1.Color := SimbaComponentTheme.ColorFrame;

  FEditTol := TSimbaLabeledEdit.Create(FPanel);
  FEditTol.Parent := BottomPanel;
  FEditTol.Align := alTop;
  FEditTol.Caption := 'Best Tolerance';
  FEditTol.LabelMeasure := 'Best Multiplier[0]';
  FEditTol.BorderSpacing.Top := 4;
  FEditTol.Color := SimbaComponentTheme.ColorFrame;

  FEditColor := TSimbaLabeledEdit.Create(FPanel);
  FEditColor.Parent := BottomPanel;
  FEditColor.Align := alTop;
  FEditColor.Caption := 'Best Color';
  FEditColor.LabelMeasure := 'Best Multiplier[0]';
  FEditColor.BorderSpacing.Top := 4;
  FEditColor.Color := SimbaComponentTheme.ColorFrame;

  with TSimbaDivider.Create(FPanel) do
  begin
    Parent := BottomPanel;
    Align := alTop;
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := 5;
  end;

  FColorSpaces := TSimbaLabeledToggleButtonGroup.Create(FPanel);
  with FColorSpaces do
  begin
    Parent := BottomPanel;
    Align := alTop;
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
end;

constructor TSimbaACA.CreateLape(ImageSupplier: TACAImageSupplierLape);
begin
  Create(nil);

  FImageSupplierLape := ImageSupplier;
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

