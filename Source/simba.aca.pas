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
  simba.component_treeview,
  simba.component_edit,
  simba.component_button,
  simba.toolform;

type
  {$push}
  {$scopedenums on}
  EACASearch = (NONE, FIND_COLOR, MATCH_COLOR);
  {$pop}

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

    FSearch: EACASearch;
    FDrawColor: TColor;
    FDrawAlpha: Byte;
    FDrawColorMenu: TMenuItem;
    FDrawAlphaMenu: TMenuItem;

    procedure FillLoadDeleteColorMenus(LoadItem, DeleteItem: TMenuItem);

    function GetColorSpace: EColorSpace;
    procedure CalcBestColor;
    procedure AddColor(NewColor: TColor);
    procedure AddColors(Str: String);
    function HasBestColor: Boolean;
    // Value's results for the best colour, over the image in place of the last search's.
    // Search(FSearch) does the last one again: for a new image, best colour or draw colour.
    procedure Search(Value: EACASearch);
    procedure SetDrawColor(Value: TColor);
    procedure SetDrawAlpha(Value: Byte);
    procedure DoDrawMenuClick(Sender: TObject);
    procedure DoClearImageClick(Sender: TObject);

    procedure SetImage(Value: TSimbaImage); override;
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoListModify(Sender: TObject);
    procedure DoListSelectionChange(Sender: TObject);
    procedure DoListDeleteKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoColorSpaceChange(Sender: TObject);

    procedure DoFindColorClick(Sender: TObject);
    procedure DoMatchColorClick(Sender: TObject);
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

    function GetBest: TColorTolerance;
    function GetColors: TColorArray;
    procedure SetColors(Colors: TColorArray);
  public
    constructor Create(ImageSupplier: TImageSupplier); override;

    property Colors: TColorArray read GetColors write SetColors;
    property BestColor: TColorTolerance read GetBest;
    property DrawColor: TColor read FDrawColor write SetDrawColor;
    property DrawAlpha: Byte read FDrawAlpha write SetDrawAlpha;

    property ButtonFindColor: TSimbaButton read FButtonFindColor;
    property ButtonMatchColor: TSimbaButton read FButtonMatchColor;
    property ButtonClearImg: TSimbaButton read FButtonClearImg;
    property ButtonUpdateImg: TSimbaButton read FButtonUpdateImg;
  end;

implementation

uses
  IniFiles, Clipbrd, LCLType, TypInfo, Dialogs, ExtCtrls, Math,
  simba.env,
  simba.colormath_aca,
  simba.vartype_matrix,
  simba.vartype_string,
  simba.vartype_box,
  simba.array_algorithm,
  simba.component_theme;

type
  TColorNode = class(TTreeNode)
  public
    Color: TColor;
  end;

function GetColorsINI: TINIFile;
begin
  Result := TINIFile.Create(SimbaEnv.DataPath + 'aca.ini');
end;

// Fills a disc with an HSL colour wheel (hue by angle, saturation by radius) at lightness 50.
procedure DrawHSLCircle(Img: TSimbaImage; Center: TPoint; Radius: Integer);

  // One channel at lightness 50. Hue in turns, Sat 0..1
  function Channel(Hue, Sat: Single): Byte;
  begin
    Hue := Hue - Floor(Hue);
    if (Hue < 1/6) then
      Result := Round(127.5 + Sat * 127.5 * (12 * Hue - 1))
    else if (Hue < 1/2) then
      Result := Round(127.5 + Sat * 127.5)
    else if (Hue < 2/3) then
      Result := Round(127.5 + Sat * 127.5 * (7 - 12 * Hue))
    else
      Result := Round(127.5 - Sat * 127.5);
  end;

var
  Hue, Sat: Single;
  X, Y: Integer;
begin
  for Y := Max(Center.Y - Radius, 0) to Min(Center.Y + Radius, Img.Height - 1) do
    for X := Max(Center.X - Radius, 0) to Min(Center.X + Radius, Img.Width - 1) do
    begin
      Hue := ArcTan2(Y - Center.Y, X - Center.X) / (2 * PI);
      Sat := Hypot(Center.X - X, Center.Y - Y) / Radius;
      if (Sat < 1) then
        with Img.PixelPtr[X, Y]^ do
        begin
          R := Channel(Hue + 1/3, Sat);
          G := Channel(Hue, Sat);
          B := Channel(Hue - 1/3, Sat);
          A := ALPHA_OPAQUE;
        end;
    end;
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
      addMenuItem(LoadItem, Sections[I], @DoLoadColorsClick);
      addMenuItem(DeleteItem, Sections[I], @DoDeleteColorsClick);
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
    'LAB':    Result := EColorSpace.LAB;
    'LCH':    Result := EColorSpace.LCH;
    'DeltaE': Result := EColorSpace.DELTAE;
    else
      Result := EColorSpace.RGB;
  end;
end;

procedure TSimbaACA.CalcBestColor;
var
  Best: TBestColor;
begin
  if (FColorList.TopLevelCount > 0) then
  begin
    Best := GetBestColor(GetColorSpace(), GetColors());

    FEditColor.Edit.Text  := ColorToStr(Best.Color);
    FEditTol.Edit.Text    := Format('%.3f', [Best.Tolerance]);
    FEditMulti1.Edit.Text := Format('%.3f', [Best.Mods[0]]);
    FEditMulti2.Edit.Text := Format('%.3f', [Best.Mods[1]]);
    FEditMulti3.Edit.Text := Format('%.3f', [Best.Mods[2]]);
  end else
  begin
    FEditColor.Edit.Clear();
    FEditTol.Edit.Clear();
    FEditMulti1.Edit.Clear();
    FEditMulti2.Edit.Clear();
    FEditMulti3.Edit.Clear();
  end;
end;

procedure TSimbaACA.AddColor(NewColor: TColor);
var
  I: Integer;
begin
  for I := 0 to FColorList.TopLevelCount - 1 do
    if (TColorNode(FColorList.TopLevelItem[I]).Color = NewColor) then
      Exit;

  // in an update, so OnModify waits for the colour
  FColorList.BeginUpdate();
  TColorNode(FColorList.AddNode(ColorToStr(NewColor))).Color := NewColor;
  FColorList.EndUpdate();
end;

// every number in Str that is a colour
procedure TSimbaACA.AddColors(Str: String);
var
  Number: String;
  Value: Int64;
begin
  FColorList.BeginUpdate();
  try
    for Number in Str.ExtractNumbers() do
    begin
      Value := Number.ToInt(-1);
      if (Value >= 0) and (Value <= $FFFFFF) then
        AddColor(Value);
    end;
  finally
    FColorList.EndUpdate();
  end;
end;

// the edits can be typed in, with or without colours in the list
function TSimbaACA.HasBestColor: Boolean;
begin
  Result := FEditColor.Edit.Text <> '';
end;

procedure TSimbaACA.Search(Value: EACASearch);
var
  Best: TColorTolerance;
  TPA: TPointArray;
  Matches: TSingleMatrix;
begin
  FSearch := Value;

  TopLayer.Clear();
  TopLayer.Opacity := FDrawAlpha;
  FImageBox.Status := '';

  if HasBestColor() then
  begin
    Best := BestColor;

    case FSearch of
      EACASearch.FIND_COLOR:
        begin
          TPA := FImageBox.Background.Finder.FindColor(Best, TBox.Create(-1, -1, -1, -1));
          if (FDrawColor = clNone) then
            TopLayer.DrawColor := GetContrastingColor(FImageBox.Background.GetPixels(TPA), [clRed, clLime, clBlue, clYellow, clAqua, clFuchsia])
          else
            TopLayer.DrawColor := FDrawColor;
          TopLayer.DrawTPA(TPA);

          FImageBox.Status := Format('Found %.0n matches', [Double(Length(TPA))]);
        end;

      EACASearch.MATCH_COLOR:
        begin
          Matches := FImageBox.Background.Finder.MatchColor(Best.Color, Best.ColorSpace, Best.Multipliers, TBox.Create(-1, -1, -1, -1));
          Matches.NormMinMax(1, 0);
          TopLayer.DrawHeatmap(Matches);
        end;
    end;
  end;

  FImageBox.Invalidate();
end;

// the item's Tag is the colour or the alpha, by which menu it is in
procedure TSimbaACA.DoDrawMenuClick(Sender: TObject);
begin
  if (TMenuItem(Sender).Parent = FDrawColorMenu) then
    SetDrawColor(TMenuItem(Sender).Tag)
  else
    SetDrawAlpha(TMenuItem(Sender).Tag);
end;

procedure TSimbaACA.SetDrawColor(Value: TColor);
var
  I: Integer;
begin
  FDrawColor := Value;
  for I := 0 to FDrawColorMenu.Count - 1 do
    if not FDrawColorMenu.Items[I].IsLine then // its Tag is 0, which is black
      FDrawColorMenu.Items[I].Checked := (FDrawColorMenu.Items[I].Tag = Value);

  if (FTopLayer <> nil) then
    Search(FSearch);
end;

procedure TSimbaACA.SetDrawAlpha(Value: Byte);
var
  I: Integer;
begin
  FDrawAlpha := Value;
  for I := 0 to FDrawAlphaMenu.Count - 1 do
    FDrawAlphaMenu.Items[I].Checked := (FDrawAlphaMenu.Items[I].Tag = Value);

  if (FTopLayer <> nil) then
    FTopLayer.Opacity := Value;
end;

procedure TSimbaACA.DoClearImageClick(Sender: TObject);
begin
  Search(EACASearch.NONE);
end;

procedure TSimbaACA.SetImage(Value: TSimbaImage);
begin
  inherited SetImage(Value);

  Search(FSearch);
end;

// Ctrl+V and Ctrl+C are the colour list's, unless an edit has the focus
procedure TSimbaACA.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key in [VK_C, VK_V]) and (not (ActiveControl is TSimbaEdit)) then
  begin
    if (Key = VK_V) then
      DoAddFromClipboardClick(nil)
    else
      DoCopyBestColorClick(nil);
    Key := 0;
  end else
    inherited DoFormKeyDown(Sender, Key, Shift);
end;

procedure TSimbaACA.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FImageBox.Background.InImage(X, Y) then
    AddColor(FImageBox.Background.Pixel[X, Y]);
end;

procedure TSimbaACA.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
begin
  PaintColorNode(ACanvas, Node, TColorNode(Node).Color);
end;

procedure TSimbaACA.DoListModify(Sender: TObject);
begin
  CalcBestColor();
  Search(FSearch);
end;

procedure TSimbaACA.DoListSelectionChange(Sender: TObject);
begin
  if (FColorList.Selected <> nil) then
    FImageBoxZoom.Fill(TColorNode(FColorList.Selected).Color);
end;

procedure TSimbaACA.DoListDeleteKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FColorList.DeleteSelection();
end;

// with no colours to work it out from, a best colour typed in stays
procedure TSimbaACA.DoColorSpaceChange(Sender: TObject);
begin
  if (FColorList.TopLevelCount > 0) then
    CalcBestColor();
  Search(FSearch);
end;

procedure TSimbaACA.DoFindColorClick(Sender: TObject);
begin
  Search(EACASearch.FIND_COLOR);
end;

procedure TSimbaACA.DoMatchColorClick(Sender: TObject);
begin
  Search(EACASearch.MATCH_COLOR);
end;

// a radius past MAX_RADIUS is MAX_RADIUS: bigger only takes longer to draw
procedure TSimbaACA.DoLoadHSLCircleClick(Sender: TObject);
const
  MAX_RADIUS = 1000;
var
  Value: String;
  Radius: Integer;
  Img: TSimbaImage;
begin
  Value := '50';
  if not InputQuery('Simba - ACA', Format('HSL Circle Radius (1 to %d)?', [MAX_RADIUS]), Value) then
    Exit;
  Radius := StrToIntDef(Value, 0);
  if (Radius < 1) then
    Exit;
  Radius := Min(Radius, MAX_RADIUS);

  Img := TSimbaImage.Create(Radius * 2, Radius * 2);
  DrawHSLCircle(Img, Img.Center, Radius);

  SetImage(Img);
end;

procedure TSimbaACA.DoDeleteSelectedClick(Sender: TObject);
begin
  FColorList.DeleteSelection();
end;

procedure TSimbaACA.DoAddFromClipboardClick(Sender: TObject);
begin
  AddColors(Clipboard.AsText);
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
  ColorName := '';
  if (not InputQuery('Auto Color Aid', 'Save under what name?', ColorName)) or (ColorName = '') then
    Exit;

  ColorsString := '';
  for Col in GetColors() do
    ColorsString := ColorsString + ColorToStr(Col) + ',';

  with GetColorsINI() do
  try
    WriteString(ColorName, 'Colors', ColorsString);
  finally
    Free();
  end;
end;

// the colours saved under the item's caption, in place of the list
procedure TSimbaACA.DoLoadColorsClick(Sender: TObject);
var
  ColorsString: String;
begin
  with GetColorsINI() do
  try
    ColorsString := ReadString(TMenuItem(Sender).Caption, 'Colors', '');
  finally
    Free();
  end;

  FColorList.BeginUpdate();
  try
    FColorList.Clear();
    AddColors(ColorsString);
  finally
    FColorList.EndUpdate();
  end;
end;

procedure TSimbaACA.DoDeleteColorsClick(Sender: TObject);
begin
  with GetColorsINI() do
  try
    EraseSection(TMenuItem(Sender).Caption);
  finally
    Free();
  end;
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
  if not HasBestColor() then
    Exit;

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

function TSimbaACA.GetBest: TColorTolerance;
begin
  Result.ColorSpace := GetColorSpace();
  Result.Color := StrToColor(FEditColor.Edit.Text);
  Result.Tolerance := StrToFloatDef(FEditTol.Edit.Text, 0);
  Result.Multipliers[0] := StrToFloatDef(FEditMulti1.Edit.Text, 0);
  Result.Multipliers[1] := StrToFloatDef(FEditMulti2.Edit.Text, 0);
  Result.Multipliers[2] := StrToFloatDef(FEditMulti3.Edit.Text, 0);
end;

constructor TSimbaACA.Create(ImageSupplier: TImageSupplier);
var
  SubSidePanel: TPanel;
  Items: TMenuItem;

  procedure AddDrawItem(AParent: TMenuItem; ACaption: String; Value, Current: PtrInt);
  var
    Item: TMenuItem;
  begin
    Item := addMenuItem(AParent, ACaption, @DoDrawMenuClick);
    Item.Checked := (Value = Current);
    Item.Tag := Value;
  end;

  function CreateListPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    Result.OnPopup := @DoColorListPopup;
    addMenuItem(Result.Items, 'Delete Selected', @DoDeleteSelectedClick);
    addMenuItem(Result.Items, '-');
    addMenuItem(Result.Items, 'Clear', @DoClearColorsClick);
    addMenuItem(Result.Items, 'Add From Clipboard', @DoAddFromClipboardClick);
    addMenuItem(Result.Items, '-');
    addMenuItem(Result.Items, 'Save ...', @DoSaveColorsClick);
    addMenuItem(Result.Items, 'Load');
    addMenuItem(Result.Items, 'Delete');
  end;

  // made top to bottom
  function NewEdit(ACaption: String): TSimbaLabeledEdit;
  begin
    Result := TSimbaLabeledEdit.Create(SubSidePanel);
    Result.Parent := SubSidePanel;
    Result.Align := alBottom;
    Result.Caption := ACaption;
    Result.LabelMeasure := 'Best Multiplier[0]';
    Result.BorderSpacing.Top := 4;
    Result.Color := SimbaComponentTheme.ColorFrame;
  end;

begin
  inherited Create(ImageSupplier);

  Caption := 'Auto Color Aid';

  FDrawColor := clNone;
  FDrawAlpha := ALPHA_OPAQUE;

  Items := addImageMenu();
  addMenuItem(Items, 'Load HSL Circle', @DoLoadHSLCircleClick);
  addMenuItem(Items, '-');

  FDrawColorMenu := addMenuItem(Items, 'Draw Color');
  AddDrawItem(FDrawColorMenu, 'Auto', clNone, FDrawColor);
  addMenuItem(FDrawColorMenu, '-');
  AddDrawItem(FDrawColorMenu, 'Red', clRed, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Green', clGreen, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Blue', clBlue, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Yellow', clYellow, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Aqua', clAqua, FDrawColor);

  FDrawAlphaMenu := addMenuItem(Items, 'Draw Alpha');
  AddDrawItem(FDrawAlphaMenu, '100%', 255, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '75%', 191, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '50%', 128, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '25%', 64, FDrawAlpha);

  // the items that hold saved colour lists are filled when the menu opens
  Items := addMenu('Colors', @DoColorMenuPopup);
  addMenuItem(Items, 'Clear Color List', @DoClearColorsClick);
  addMenuItem(Items, 'Add Colors From Clipboard', @DoAddFromClipboardClick, ShortCut(VK_V, [ssCtrl]));
  addMenuItem(Items, '-');
  addMenuItem(Items, 'Load Colors');
  addMenuItem(Items, 'Save Colors ...', @DoSaveColorsClick);
  addMenuItem(Items, 'Delete Colors');
  addMenuItem(Items, '-');
  addMenuItem(Items, 'Copy Best Color', @DoCopyBestColorClick, ShortCut(VK_C, [ssCtrl]));

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
  FColorSpaces.Parent := SubSidePanel;
  FColorSpaces.Align := alBottom;
  FColorSpaces.Color := SimbaComponentTheme.ColorFrame;
  FColorSpaces.Caption := 'Color Space';
  FColorSpaces.ToggleButtons.Add('RGB').MeasureText := 'DeltaE';
  FColorSpaces.ToggleButtons.Add('HSL').MeasureText := 'DeltaE';
  FColorSpaces.ToggleButtons.Add('HSV').MeasureText := 'DeltaE';
  FColorSpaces.ToggleButtons.Add('LCH').MeasureText := 'DeltaE';
  FColorSpaces.ToggleButtons.Add('DeltaE').MeasureText := 'DeltaE';
  // XYZ and LAB are not offered yet
  FColorSpaces.ToggleButtons.OnChange := @DoColorSpaceChange;

  FEditColor := NewEdit('Best Color');
  FEditTol := NewEdit('Best Tolerance');
  FEditMulti1 := NewEdit('Best Multiplier[0]');
  FEditMulti2 := NewEdit('Best Multiplier[1]');
  FEditMulti3 := NewEdit('Best Multiplier[2]');

  FButtonFindColor  := addButton('Find Color', @DoFindColorClick);
  FButtonMatchColor := addButton('Match Color', @DoMatchColorClick);
  FButtonUpdateImg  := addButton('Update Image', @DoUpdateImageClick);
  FButtonClearImg   := addButton('Clear Image', @DoClearImageClick);
end;

end.

