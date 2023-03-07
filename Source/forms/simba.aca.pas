{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.aca;

{$i simba.inc}

interface

uses
  classes, sysutils, fileutil, dividerbevel, forms, controls,
  graphics, dialogs, extctrls, comctrls, stdctrls, menus, colorbox, lcltype,
  simba.client, simba.mufasatypes, simba.imagebox, simba.imagebox_zoom, simba.imagebox_bitmap;

type
  TACABestColorEvent = procedure(CTS: Integer; Color, Tolerance: Integer; Hue, Sat: Extended) of object;
  TACABestColorEventEx = procedure(CTS: Integer; Color, Tolerance: Integer; Hue, Sat: Extended) is nested;

  TSimbaACAForm = class(TForm)
    ButtonDebugColor: TButton;
    ButtonUpdateImage: TButton;
    ButtonClearImage: TButton;
    ButtonDeleteColors: TButton;
    ButtonDeleteSelectedColor: TButton;
    ColorListBox: TColorListBox;
    Divider1: TDividerBevel;
    Divider2: TDividerBevel;
    EditColor: TEdit;
    EditTolerance: TEdit;
    EditHue: TEdit;
    EditSat: TEdit;
    LabelColor: TLabel;
    LabelTolerance: TLabel;
    LabelHue: TLabel;
    LabelSat: TLabel;
    MainMenu: TMainMenu;
    MenuImage: TMenuItem;
    MenuColors: TMenuItem;
    MenuItemLoadHSLCircleEx: TMenuItem;
    MenuItemLoadHSLCircle: TMenuItem;
    MenuItemUpdateImage: TMenuItem;
    MenuItemLoadColors: TMenuItem;
    MenuItemSaveColors: TMenuItem;
    MenuItemCopyBestColor: TMenuItem;
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
    ButtonCTS0: TRadioButton;
    ButtonCTS1: TRadioButton;
    ButtonCTS2: TRadioButton;
    Separator1: TMenuItem;

    procedure ButtonClearImageClick(Sender: TObject);
    procedure ButtonDeleteColorsClick(Sender: TObject);
    procedure ButtonDeleteSelectedColorClick(Sender: TObject);
    procedure ButtonLoadImageClick(Sender: TObject);
    procedure ButtonSaveColorsClick(Sender: TObject);
    procedure CenterDivider(Sender: TObject);
    procedure ButtonDebugColorClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonLoadColorsClick(Sender: TObject);
    procedure ButtonCTSClick(Sender: TObject);
    procedure CopyBestColorClick(Sender: TObject);
    procedure ColorSelectionChanged(Sender: TObject; User: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ButtonUpdateImageClick(Sender: TObject);
    procedure ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemLoadHSLCircleClick(Sender: TObject);
    procedure MenuItemLoadHSLCircleExClick(Sender: TObject);
    procedure PanelRightResize(Sender: TObject);
  protected
    FOnCalculateBestColor: TACABestColorEvent;
    FOnCalculateBestColorEx: TACABestColorEventEx;

    FManageClient: Boolean;
    FClient: TClient;
    FImageBox: TSimbaImageBox;
    FImageZoom: TSimbaImageBoxZoom;
    FZoomInfo: TLabel;
    FDebugTPA: TPointArray;
    FDrawColor: TColor;

    procedure LoadHSLCircle(Radius: Integer);
    procedure DoPaintArea(Sender: TObject; Bitmap: TSimbaImageBoxBitmap; R: TRect);
    procedure CalculateBestColor;

    function GetColors: TIntegerArray;
  public
    constructor Create(Client: TClient; ManageClient: Boolean); reintroduce;
    constructor Create(Window: TWindowHandle); reintroduce;
    destructor Destroy; override;

    property OnCalculateBestColor: TACABestColorEvent read FOnCalculateBestColor write FOnCalculateBestColor;
    property OnCalculateBestColorEx: TACABestColorEventEx read FOnCalculateBestColorEx write FOnCalculateBestColorEx;
  end;

implementation

{$R *.lfm}

uses
  clipbrd,
  simba.colormath, simba.windowhandle, simba.bitmap;

procedure TSimbaACAForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R, G, B: Byte;
  H, S, L: Extended;
begin
  FImageZoom.MoveTest(FImageBox, X, Y);

  ColorToRGB(FImageBox.Background.Canvas.Pixels[X, Y], R, G, B);
  //ColorToHSL(FImageBox.Background.Canvas.Pixels[X, Y], H, S, L);

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

function TSimbaACAForm.GetColors: TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, ColorListBox.Count);
  for i := 0 to ColorListBox.Count - 1 do
    Result[i] := ColorListBox.Colors[i];
end;

procedure TSimbaACAForm.ButtonUpdateImageClick(Sender: TObject);
begin
  if not FClient.IOManager.TargetValid() then
    FClient.IOManager.SetDesktop();

  FImageBox.SetBackground(FClient.IOManager);
end;

procedure TSimbaACAForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSimbaACAForm.ButtonCTSClick(Sender: TObject);
begin
  CalculateBestColor();
end;

procedure TSimbaACAForm.CopyBestColorClick(Sender: TObject);

  function CopyEdit(Edit: TEdit): Boolean;
  begin
    Result := Edit.Focused and (Edit.SelLength > 0);
    if Result then
      Edit.CopyToClipboard();
  end;

begin
  if CopyEdit(EditColor) or CopyEdit(EditTolerance) or CopyEdit(EditHue) or CopyEdit(EditSat) then
    Exit;

  if ButtonCTS0.Checked then Clipboard.AsText := Format('CTS0(%s, %s)', [EditColor.Text, EditTolerance.Text]);
  if ButtonCTS1.Checked then Clipboard.AsText := Format('CTS1(%s, %s)', [EditColor.Text, EditTolerance.Text]);
  if ButtonCTS2.Checked then Clipboard.AsText := Format('CTS2(%s, %s, %s, %s)', [EditColor.Text, EditTolerance.Text, EditHue.Text, EditSat.Text]);
end;

procedure TSimbaACAForm.ColorSelectionChanged(Sender: TObject; User: Boolean);
var
  R, G, B: Integer;
  H, S, L: Extended;
begin
  if User and (ColorListBox.ItemIndex >= 0) then
  begin
    FImageZoom.SetTempColor(ColorListBox.Selected);

    ColorToRGB(ColorListBox.Selected, R, G, B);
    //ColorToHSL(ColorListBox.Selected, H, S, L);

    FZoomInfo.Caption := Format('Color: %d', [ColorListBox.Selected]) + LineEnding +
                         Format('RGB: %d, %d, %d', [R, G, B])         + LineEnding +
                         Format('HSL: %.2f, %.2f, %.2f', [H, S, L])   + LineEnding;
  end;

  CalculateBestColor();
end;

procedure TSimbaACAForm.ButtonDebugColorClick(Sender: TObject);
var
  CTS: Integer;
  Col, Tol: Integer;
  HueMod, SatMod: Extended;
begin
  if ButtonCTS0.Checked then CTS := 0;
  if ButtonCTS1.Checked then CTS := 1;
  if ButtonCTS2.Checked then CTS := 2;

  Col := StrToIntDef(EditColor.Text, -1);
  Tol := StrToIntDef(EditTolerance.Text, -1);
  HueMod := StrToFloatDef(EditHue.Text, -1);
  SatMod := StrToFloatDef(EditSat.Text, -1);

  FDebugTPA := FImageBox.FindColors(CTS, Col, Tol, HueMod, SatMod);

  FImageBox.Paint();
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
  CTS: Integer;
  Col, Tol: Integer;
  Hue, Sat: Extended;
begin
  if Length(GetColors()) = 0 then
  begin
    EditColor.Text := '';
    EditTolerance.Text := '';
    EditHue.Text := '';
    EditSat.Text := '';
  end else
  begin
    if ButtonCTS0.Checked then CTS := 0;
    if ButtonCTS1.Checked then CTS := 1;
    if ButtonCTS2.Checked then CTS := 2;

    case CTS of
      0: BestColor_CTS0(GetColors(), Col, Tol);
      1: BestColor_CTS1(GetColors(), Col, Tol);
      2: BestColor_CTS2(GetColors(), Col, Tol, Hue, Sat);
    end;

    EditColor.Text := Format('%d', [Col]);
    EditTolerance.Text := Format('%d', [Tol]);

    if (CTS = 2) then
    begin
      EditHue.Text := Format('%.2f', [Hue + 0.5e-2]);
      EditSat.Text := Format('%.2f', [Sat + 0.5e-2]);

      Hue := StrToFloat(EditHue.Text);
      Sat := StrToFloat(EditSat.Text);
    end else
    begin
      EditHue.Text := '';
      EditSat.Text := '';
    end;

    if Assigned(OnCalculateBestColor) then
      OnCalculateBestColor(CTS, Col, Tol, Hue, Sat);
    if Assigned(OnCalculateBestColorEx) then
      OnCalculateBestColorEx(CTS, Col, Tol, Hue, Sat);
  end;
end;

procedure TSimbaACAForm.ButtonDeleteColorsClick(Sender: TObject);
begin
  ColorListBox.Clear();
  ColorListBox.OnSelectionChange(Sender, False);
end;

procedure TSimbaACAForm.ButtonClearImageClick(Sender: TObject);
begin
  FDebugTPA := [];

  FImageBox.Paint();
end;

procedure TSimbaACAForm.ButtonDeleteSelectedColorClick(Sender: TObject);
begin
  ColorListBox.DeleteSelected();
  ColorListBox.OnSelectionChange(Sender, False);
end;

procedure TSimbaACAForm.ButtonLoadImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    InitialDir := Application.Location;

    if Execute() then
    try
      FImageBox.SetBackground(FileName);
    except
    end;
  finally
    Free();
  end;
end;

procedure TSimbaACAForm.ButtonSaveColorsClick(Sender: TObject);
begin
  try
    with TSaveDialog.Create(Self) do
    try
      if Execute() then
        ColorListBox.Items.SaveToFile(FileName);
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaACAForm.CenterDivider(Sender: TObject);
var
  Divider: TDividerBevel absolute Sender;
begin
  Divider.LeftIndent := (Divider.Width div 2) - (Divider.Canvas.TextWidth(Divider.Caption) div 2) - Divider.CaptionSpacing;
end;

constructor TSimbaACAForm.Create(Client: TClient; ManageClient: Boolean);
begin
  inherited Create(Application.MainForm);

  ColorListBox.Options := [];

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := PanelMain;
  FImageBox.Align := alClient;
  FImageBox.OnMouseDown := @ClientImageMouseDown;
  FImageBox.OnMouseMove := @ClientImageMouseMove;
  FImageBox.OnPaintArea := @DoPaintArea;

  FImageZoom := TSimbaImageBoxZoom.Create(Self);
  FImageZoom.Parent := PanelTop;
  FImageZoom.SetZoom(4, 5);
  FImageZoom.BorderSpacing.Around := 5;

  FZoomInfo := TLabel.Create(Self);
  FZoomInfo.Parent := PanelTop;
  FZoomInfo.BorderSpacing.Right := 10;
  FZoomInfo.AnchorToNeighbour(akLeft, 10, FImageZoom);

  FDrawColor := clRed;
  FManageClient := ManageClient;
  FClient := Client;

  FImageBox.SetBackground(FClient.IOManager);
end;

constructor TSimbaACAForm.Create(Window: TWindowHandle);
var
  Client: TClient;
begin
  Client := TClient.Create();
  if (Window > 0) and Window.IsValid() then
    Client.IOManager.SetTarget(Window);

  Create(Client, True);
end;

destructor TSimbaACAForm.Destroy;
begin
  if (FClient <> nil) and FManageClient then
    FreeAndNil(FClient);

  inherited Destroy();
end;

end.
