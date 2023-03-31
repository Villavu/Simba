{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.aca;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, DividerBevel, Graphics, ExtCtrls, ComCtrls, StdCtrls, Menus, ColorBox,
  simba.mufasatypes,
  simba.imagebox, simba.imagebox_zoom, simba.imagebox_bitmap,
  simba.colormath, simba.colormath_distance;

type
  TACABestColorEvent = procedure(CTS: Integer; Color, Tolerance: Integer; Hue, Sat: Extended) of object;
  TACABestColorEventEx = procedure(CTS: Integer; Color, Tolerance: Integer; Hue, Sat: Extended) is nested;

  TSimbaACAForm = class(TForm)
    ButtonRemoveColor: TButton;
    ButtonRemoveAllColors: TButton;
    ButtonFindColor: TButton;
    ButtonMatchColor: TButton;
    ButtonClearImage: TButton;
    ColorListBox: TColorListBox;
    Divider2: TDividerBevel;
    BestColorEdit: TEdit;
    BestToleranceEdit: TEdit;
    BestMulti1Edit: TEdit;
    BestMulti2Edit: TEdit;
    BestMulti3Edit: TEdit;
    Divider1: TDividerBevel;
    GroupBoxColorSpace: TGroupBox;
    LabelBestColor: TLabel;
    LabelBestTolerance: TLabel;
    LabelMulti1: TLabel;
    LabelMulti2: TLabel;
    LabelMulti3: TLabel;
    MainMenu: TMainMenu;
    MenuImage: TMenuItem;
    MenuColors: TMenuItem;
    MenuItemLoadHSLCircleEx: TMenuItem;
    MenuItemLoadHSLCircle: TMenuItem;
    MenuItemUpdateImage: TMenuItem;
    MenuItemLoadColors: TMenuItem;
    MenuItemSaveColors: TMenuItem;
    MenuItemCopyBestColor: TMenuItem;
    Panel1: TPanel;
    AlignmentPanel: TPanel;
    PanelTop: TPanel;
    PanelAlignment: TPanel;
    MenuItemLoadImage: TMenuItem;
    MenuItemClearImage: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemDrawColor: TMenuItem;
    MenuItemColorRed: TMenuItem;
    MenuItemColorGreen: TMenuItem;
    MenuItemColorBlue: TMenuItem;
    MenuItemColorYellow: TMenuItem;
    PanelMain: TPanel;
    PanelRight: TPanel;
    ButtonRGB: TRadioButton;
    ButtonHSL: TRadioButton;
    ButtonHSV: TRadioButton;
    ButtonXYZ: TRadioButton;
    ButtonLAB: TRadioButton;
    ButtonLCH: TRadioButton;
    ButtonDeltaE: TRadioButton;
    Separator1: TMenuItem;

    procedure ButtonRemoveAllColorsClick(Sender: TObject);
    procedure ButtonRemoveColorClick(Sender: TObject);
    procedure ButtonFindColorClick(Sender: TObject);
    procedure ButtonMatchColorClick(Sender: TObject);
    procedure ButtonLoadImageClick(Sender: TObject);
    procedure ButtonSaveColorsClick(Sender: TObject);
    procedure DoButtonClearImageClick(Sender: TObject);
    procedure DoColorSpaceChanged(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonLoadColorsClick(Sender: TObject);
    procedure ButtonCTSClick(Sender: TObject);
    procedure ColorSelectionChanged(Sender: TObject; User: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ButtonUpdateImageClick(Sender: TObject);
    procedure ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyBestColorClick(Sender: TObject);
    procedure MenuItemLoadHSLCircleClick(Sender: TObject);
    procedure MenuItemLoadHSLCircleExClick(Sender: TObject);
    procedure PanelRightResize(Sender: TObject);
  protected
    FOnCalculateBestColor: TACABestColorEvent;
    FOnCalculateBestColorEx: TACABestColorEventEx;

    FWindow: TWindowHandle;
    FImageBox: TSimbaImageBox;
    FImageZoom: TSimbaImageBoxZoom;
    FZoomInfo: TLabel;
    FDebugTPA: TPointArray;
    FDrawColor: TColor;

    procedure LoadHSLCircle(Radius: Integer);
    procedure DoPaintArea(Sender: TObject; Bitmap: TSimbaImageBoxBitmap; R: TRect);
    procedure CalculateBestColor;

    procedure GetColorStuff(out ColorSpace: EColorSpace; out Col: Integer; out Tol: Single; out Mods: TChannelMultipliers);

    function GetColorSpace: EColorSpace;
    function GetColorSpaceStr: String;
    function GetColors: TColorArray;
  public
    constructor Create(Window: TWindowHandle); reintroduce;

    property OnCalculateBestColor: TACABestColorEvent read FOnCalculateBestColor write FOnCalculateBestColor;
    property OnCalculateBestColorEx: TACABestColorEventEx read FOnCalculateBestColorEx write FOnCalculateBestColorEx;
  end;

implementation

{$R *.lfm}

uses
  Clipbrd, TypInfo, LCLType,
  simba.windowhandle, simba.bitmap, simba.colormath_aca;

procedure TSimbaACAForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FImageZoom.SetTempColor(-1);
  FImageZoom.MoveTest(FImageBox, X, Y);

  with FImageBox.Background.Canvas.Pixels[X, Y].ToRGB(), FImageBox.Background.Canvas.Pixels[X, Y].ToHSL() do
    FZoomInfo.Caption := Format('Color: %d', [FImageBox.Background.Canvas.Pixels[X, Y]]) + LineEnding +
                         Format('RGB: %d, %d, %d', [R, G, B])                            + LineEnding +
                         Format('HSL: %.2f, %.2f, %.2f', [H, S, L])                      + LineEnding;
end;

procedure TSimbaACAForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixel: Integer;
begin
  if (Button = mbLeft) then
  begin
    Pixel := FImageBox.Background.Canvas.Pixels[X, Y];

    if ColorListBox.Items.IndexOf(Pixel.ToString()) = -1 then
      ColorListBox.ItemIndex := ColorListBox.Items.AddObject(Pixel.ToString(), TObject(PtrUInt(Pixel)));
  end;
end;

procedure TSimbaACAForm.MenuItemCopyBestColorClick(Sender: TObject);
begin
  Clipboard.AsText := Format('ColorTolerance($%s, %s, %s, [%s, %s, %s])', [IntToHex(StrToIntDef(BestColorEdit.Text, 0), 6), BestToleranceEdit.Text, GetColorSpaceStr(), BestMulti1Edit.Text, BestMulti2Edit.Text, BestMulti3Edit.Text]);
end;

procedure TSimbaACAForm.MenuItemLoadHSLCircleClick(Sender: TObject);
begin
  LoadHSLCircle(350);
end;

procedure TSimbaACAForm.MenuItemLoadHSLCircleExClick(Sender: TObject);
var
  Value: String;
begin
  if InputQuery('ACA', 'HSL Circle Radius (Max 2000)', Value) and Value.IsInteger() then
    LoadHSLCircle(Min(Value.ToInteger(), 2000));
end;

procedure TSimbaACAForm.PanelRightResize(Sender: TObject);
begin
  PanelRight.Constraints.MinWidth := PanelRight.Width;
end;

procedure TSimbaACAForm.LoadHSLCircle(Radius: Integer);
var
  Bitmap: TMufasaBitmap;
begin
  Bitmap := TMufasaBitmap.Create(Radius*2, Radius*2);
  Bitmap.DrawHSLCircle(Bitmap.Center, Radius);

  FImageBox.SetBackground(Bitmap);

  Bitmap.Free();
end;

procedure TSimbaACAForm.DoPaintArea(Sender: TObject; Bitmap: TSimbaImageBoxBitmap; R: TRect);
begin
  if (Length(FDebugTPA) > 0) then
    Bitmap.DrawPoints(FDebugTPA, FDrawColor);
end;

procedure TSimbaACAForm.ChangeDrawColor(Sender: TObject);
var
  i: Integer;
begin
  with Sender as TMenuItem do
  begin
    for i := 0 to Parent.Count - 1 do
      if (Parent[i] <> Sender) then
        Parent[i].Checked := False;

    case Caption of
      'Red':    FDrawColor := clRed;
      'Green':  FDrawColor := clGreen;
      'Blue':   FDrawColor := clBlue;
      'Yellow': FDrawColor := clYellow;
    end;
  end;
end;

function TSimbaACAForm.GetColors: TColorArray;
var
  i: Integer;
begin
  SetLength(Result, ColorListBox.Count);
  for i := 0 to ColorListBox.Count - 1 do
    Result[i] := ColorListBox.Colors[i];
end;

procedure TSimbaACAForm.ButtonUpdateImageClick(Sender: TObject);
begin
  //if not FClient.IOManager.TargetValid() then
  //  FClient.IOManager.SetDesktop();

  //FImageBox.SetBackground(FClient.IOManager);
end;

procedure TSimbaACAForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSimbaACAForm.ButtonCTSClick(Sender: TObject);
begin
  CalculateBestColor();
end;

procedure TSimbaACAForm.ColorSelectionChanged(Sender: TObject; User: Boolean);
begin
  if User and (ColorListBox.ItemIndex >= 0) then
  begin
    FImageZoom.SetTempColor(ColorListBox.Selected);

    with ColorListBox.Selected.ToRGB(), ColorListBox.Selected.ToHSL() do
      FZoomInfo.Caption := Format('Color: %d', [ColorListBox.Selected]) + LineEnding +
                           Format('RGB: %d, %d, %d', [R, G, B])         + LineEnding +
                           Format('HSL: %.2f, %.2f, %.2f', [H, S, L])   + LineEnding;
  end;

  CalculateBestColor();
end;

procedure TSimbaACAForm.DoColorSpaceChanged(Sender: TObject);

  procedure SetChannelNames(const C1, C2, C3: Char);
  begin
    LabelMulti1.Caption := 'Best ' + C1 + ' Multiplier';
    LabelMulti2.Caption := 'Best ' + C2 + ' Multiplier';
    LabelMulti3.Caption := 'Best ' + C3 + ' Multiplier';
  end;

begin
  case GetColorSpace() of
    EColorSpace.RGB:    SetChannelNames('R', 'G', 'B');
    EColorSpace.XYZ:    SetChannelNames('X', 'Y', 'Z');
    EColorSpace.LAB:    SetChannelNames('L', 'A', 'B');
    EColorSpace.HSV:    SetChannelNames('H', 'S', 'V');
    EColorSpace.HSL:    SetChannelNames('H', 'S', 'L');
    EColorSpace.LCH:    SetChannelNames('L', 'C', 'H');
    EColorSpace.DeltaE: SetChannelNames('L', 'A', 'B');
  end;

  CalculateBestColor();
end;

procedure TSimbaACAForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
  begin
    ColorListBox.DeleteSelected();
    ColorListBox.OnSelectionChange(Sender, False);

    Key := VK_UNKNOWN;
  end;
end;

procedure TSimbaACAForm.ButtonLoadColorsClick(Sender: TObject);
var
  i: Integer;
begin
  try
    with TOpenDialog.Create(Self) do
    try
      if Execute() then
      begin
        ColorListBox.Items.BeginUpdate();
        ColorListBox.Items.Clear();
        ColorListBox.Items.LoadFromFile(FileName);

        for i := 0 to ColorListBox.Items.Count - 1 do
        try
          ColorListBox.Items.Objects[i] := TObject(PtrInt(StrToInt(ColorListBox.Items[i])));
        except
        end;

        ColorListBox.Items.EndUpdate();
        ColorListBox.OnSelectionChange(Sender, True);
      end;
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaACAForm.CalculateBestColor;
var
  Colors: TColorArray;
  Best: TBestColor;
begin
  Colors := GetColors();

  if (Length(GetColors) > 0) then
  begin
    Best := GetBestColor(GetColorSpace(), Colors);

    BestColorEdit.Text := IntToStr(Best.Color);
    BestToleranceEdit.Text := Format('%.3f', [Best.Tolerance]);
    BestMulti1Edit.Text := Format('%.3f', [Best.Mods[0]]);
    BestMulti2Edit.Text := Format('%.3f', [Best.Mods[1]]);
    BestMulti3Edit.Text := Format('%.3f', [Best.Mods[2]]);
  end else
  begin
    BestColorEdit.Clear();
    BestToleranceEdit.Clear();
    BestMulti1Edit.Clear();
    BestMulti2Edit.Clear();
    BestMulti3Edit.Clear();
  end;

  //  if Assigned(OnCalculateBestColor) then
  //    OnCalculateBestColor(CTS, Col, Tol, Hue, Sat);
  //  if Assigned(OnCalculateBestColorEx) then
  //    OnCalculateBestColorEx(CTS, Col, Tol, Hue, Sat);
end;

procedure TSimbaACAForm.GetColorStuff(out ColorSpace: EColorSpace; out Col: Integer; out Tol: Single; out Mods: TChannelMultipliers);
begin
  ColorSpace := GetColorSpace();
  Col := String(BestColorEdit.Text).ToInteger(0);
  Tol := String(BestToleranceEdit.Text).ToSingle(0);
  Mods[0] := String(BestMulti1Edit.Text).ToSingle(0);
  Mods[1] := String(BestMulti2Edit.Text).ToSingle(0);
  Mods[2] := String(BestMulti3Edit.Text).ToSingle(0);
end;

function TSimbaACAForm.GetColorSpace: EColorSpace;
begin
  if ButtonRGB.Checked then Result := EColorSpace.RGB else
  if ButtonHSL.Checked then Result := EColorSpace.HSL else
  if ButtonHSV.Checked then Result := EColorSpace.HSV else
  if ButtonXYZ.Checked then Result := EColorSpace.XYZ else
  if ButtonLAB.Checked then Result := EColorSpace.LAB else
  if ButtonLCH.Checked then Result := EColorSpace.LCH else
  if ButtonDeltaE.Checked then Result := EColorSpace.DELTAE;
end;

function TSimbaACAForm.GetColorSpaceStr: String;
begin
  Result := 'EColorSpace.' + GetEnumName(TypeInfo(EColorSpace), Ord(GetColorSpace()));
end;

procedure TSimbaACAForm.ButtonMatchColorClick(Sender: TObject);
var
  ColorSpace: EColorSpace;
  Col: Integer;
  Tol: Single;
  Multi: TChannelMultipliers;
  TempBackground: TMufasaBitmap;
begin
  GetColorStuff(ColorSpace, Col, Tol, Multi);

  FDebugTPA := [];

  TempBackground := TMufasaBitmap.Create();
  TempBackground.DrawMatrix(FImageBox.MatchColor(Col, ColorSpace, Multi));

  FImageBox.SetTempBackground(TempBackground);
  FImageBox.Paint();

  TempBackground.Free();
end;

procedure TSimbaACAForm.ButtonFindColorClick(Sender: TObject);
var
  ColorSpace: EColorSpace;
  Col: Integer;
  Tol: Single;
  Multi: TChannelMultipliers;
begin
  GetColorStuff(ColorSpace, Col, Tol, Multi);

  FDebugTPA := FImageBox.FindColor(Col, Tol, ColorSpace, Multi);

  FImageBox.StatusPanel.Text := Format('Found %.0n matches', [Double(Length(FDebugTPA))]);
  FImageBox.Paint();
end;

procedure TSimbaACAForm.ButtonRemoveColorClick(Sender: TObject);
begin
  ColorListBox.DeleteSelected();
  ColorListBox.OnSelectionChange(Sender, False);
end;

procedure TSimbaACAForm.ButtonRemoveAllColorsClick(Sender: TObject);
begin
  ColorListBox.Clear();
  ColorListBox.OnSelectionChange(Sender, False);
end;

procedure TSimbaACAForm.ButtonLoadImageClick(Sender: TObject);
begin
  try
    with TOpenDialog.Create(Self) do
    try
      InitialDir := Application.Location;
      if Execute() then
        FImageBox.SetBackground(FileName);
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaACAForm.ButtonSaveColorsClick(Sender: TObject);
begin
  try
    with TSaveDialog.Create(Self) do
    try
      InitialDir := Application.Location;
      if Execute() then
        ColorListBox.Items.SaveToFile(FileName);
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaACAForm.DoButtonClearImageClick(Sender: TObject);
begin
  FDebugTPA := [];

  FImageBox.ClearTempBackground();
  FImageBox.Paint();
end;

constructor TSimbaACAForm.Create(Window: TWindowHandle);
begin
  inherited Create(Application.MainForm);

  ColorListBox.Options := [];

  FWindow := Window;
  if (FWindow = 0) or (not FWindow.IsValid()) then
    FWindow := GetDesktopWindow();

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := PanelMain;
  FImageBox.Align := alClient;
  FImageBox.OnMouseDown := @ClientImageMouseDown;
  FImageBox.OnMouseMove := @ClientImageMouseMove;
  FImageBox.OnPaintArea := @DoPaintArea;
  FImageBox.SetBackground(FWindow);

  FImageZoom := TSimbaImageBoxZoom.Create(Self);
  FImageZoom.Parent := PanelTop;
  FImageZoom.SetZoom(4, 5);
  FImageZoom.BorderSpacing.Around := 5;

  FZoomInfo := TLabel.Create(Self);
  FZoomInfo.Parent := PanelTop;
  FZoomInfo.BorderSpacing.Right := 10;
  FZoomInfo.AnchorToNeighbour(akLeft, 10, FImageZoom);

  FDrawColor := clRed;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    ColorListBox.ItemHeight := Round(Canvas.TextHeight('123') * 1.3);
  finally
    Free();
  end;
end;

end.
