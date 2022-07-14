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
  simba.client, simba.imagebox, simba.mufasatypes, simba.imageboxzoom;

type
  TACABestColorEvent = procedure(CTS: Int32; Color, Tolerance: Int32; Hue, Sat: Extended) of object;
  TACABestColorEventEx = procedure(CTS: Int32; Color, Tolerance: Int32; Hue, Sat: Extended) is nested;

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
    procedure PanelRightResize(Sender: TObject);
  protected
    FOnCalculateBestColor: TACABestColorEvent;
    FOnCalculateBestColorEx: TACABestColorEventEx;

    FManageClient: Boolean;
    FClient: TClient;
    FImageBox: TSimbaImageBox;
    FImageZoom: TSimbaImageBoxZoom;
    FZoomInfo: TLabel;

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
  simba.colormath, simba.helpers_windowhandle;

procedure TSimbaACAForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R, G, B: Byte;
  H, S, L: Extended;
begin
  FImageZoom.MoveTest(FImageBox, X, Y);

  ColorToRGB(FImageBox.Background.Pixels[X, Y], R, G, B);
  ColorToHSL(FImageBox.Background.Pixels[X, Y], H, S, L);

  FZoomInfo.Caption := Format('Color: %d', [FImageBox.Background.Pixels[X, Y]]) + LineEnding +
                       Format('RGB: %d, %d, %d', [R, G, B])                     + LineEnding +
                       Format('HSL: %.2f, %.2f, %.2f', [H, S, L])               + LineEnding;
end;

procedure TSimbaACAForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixel: Int32;
begin
  if (Button = mbLeft) then
  begin
    Pixel := FImageBox.Background.Pixels[X, Y];

    if ColorListBox.Items.IndexOf(Pixel.ToString()) = -1 then
      ColorListBox.ItemIndex := ColorListBox.Items.AddObject(Pixel.ToString(), TObject(PtrUInt(Pixel)));
  end;
end;

procedure TSimbaACAForm.PanelRightResize(Sender: TObject);
begin
  PanelRight.Constraints.MinWidth := PanelRight.Width;
end;

procedure TSimbaACAForm.ChangeDrawColor(Sender: TObject);
var
  i: Int32;
begin
  with Sender as TMenuItem do
  begin
    for i := 0 to Parent.Count - 1 do
      if (Parent[i] <> Sender) then
        Parent[i].Checked := False;

    case Caption of
      'Red': FImageBox.Canvas.Pen.Color := clRed;
      'Green': FImageBox.Canvas.Pen.Color := clGreen;
      'Blue': FImageBox.Canvas.Pen.Color := clBlue;
      'Yellow': FImageBox.Canvas.Pen.Color := clYellow;
    end;
  end;
end;

function TSimbaACAForm.GetColors: TIntegerArray;
var
  i: Int32;
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
begin
  if ButtonCTS0.Checked then Clipboard.AsText := Format('CTS0(%s, %s)', [EditColor.Text, EditTolerance.Text]);
  if ButtonCTS1.Checked then Clipboard.AsText := Format('CTS1(%s, %s)', [EditColor.Text, EditTolerance.Text]);
  if ButtonCTS2.Checked then Clipboard.AsText := Format('CTS2(%s, %s, %s, %s)', [EditColor.Text, EditTolerance.Text, EditHue.Text, EditSat.Text]);
end;

procedure TSimbaACAForm.ColorSelectionChanged(Sender: TObject; User: Boolean);
var
  R, G, B: Int32;
  H, S, L: Extended;
begin
  if User and (ColorListBox.ItemIndex >= 0) then
  begin
    FImageZoom.SetTempColor(ColorListBox.Selected);

    ColorToRGB(ColorListBox.Selected, R, G, B);
    ColorToHSL(ColorListBox.Selected, H, S, L);

    FZoomInfo.Caption := Format('Color: %d', [ColorListBox.Selected]) + LineEnding +
                         Format('RGB: %d, %d, %d', [R, G, B])         + LineEnding +
                         Format('HSL: %.2f, %.2f, %.2f', [H, S, L])   + LineEnding;
  end;

  CalculateBestColor();
end;

procedure TSimbaACAForm.ButtonDebugColorClick(Sender: TObject);
var
  CTS: Int32;
  Col, Tol: Int32;
  HueMod, SatMod: Extended;
begin
  if ButtonCTS0.Checked then CTS := 0;
  if ButtonCTS1.Checked then CTS := 1;
  if ButtonCTS2.Checked then CTS := 2;

  Col := StrToIntDef(EditColor.Text, -1);
  Tol := StrToIntDef(EditTolerance.Text, -1);
  HueMod := StrToFloatDef(EditHue.Text, -1);
  SatMod := StrToFloatDef(EditSat.Text, -1);

  FImageBox.Clear();
  FImageBox.DebugColor(CTS, Col, Tol, HueMod, SatMod);
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
  i: Int32;
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
  CTS: Int32;
  Col, Tol: Int32;
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
  FImageBox.Clear();
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
  FImageBox.Canvas.Pen.Color := clRed;

  FImageZoom := TSimbaImageBoxZoom.Create(Self);
  FImageZoom.Parent := PanelTop;
  FImageZoom.SetZoom(4, 5);
  FImageZoom.BorderSpacing.Around := 5;

  FZoomInfo := TLabel.Create(Self);
  FZoomInfo.Parent := PanelTop;
  FZoomInfo.BorderSpacing.Right := 10;
  FZoomInfo.AnchorToNeighbour(akLeft, 10, FImageZoom);

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
