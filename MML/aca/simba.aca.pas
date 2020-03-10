unit simba.aca;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fileutil, dividerbevel, lresources, forms, controls,
  graphics, dialogs, extctrls, comctrls, stdctrls, menus, colorbox, lcltype,
  simba.client, simba.imagebox, simba.mufasatypes;

type
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
    ImageZoom: TImage;
    LabelColor: TLabel;
    LabelTolerance: TLabel;
    LabelHue: TLabel;
    LabelSat: TLabel;
    ColorLabel: TLabel;
    MainMenu: TMainMenu;
    MenuImage: TMenuItem;
    MenuColors: TMenuItem;
    MenuItemUpdateImage: TMenuItem;
    MenuItemLoadColors: TMenuItem;
    MenuItemSaveColors: TMenuItem;
    MenuItemCopyBestColor: TMenuItem;
    ColorLabelPanel: TPanel;
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
    PanelZoom: TPanel;
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
    procedure MouseZoomPaint(Sender: TObject);
    procedure ButtonCTSClick(Sender: TObject);
    procedure CopyBestColorClick(Sender: TObject);
    procedure ColorSelectionChanged(Sender: TObject; User: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ButtonUpdateImageClick(Sender: TObject);
    procedure ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    FClient: TClient;
    FImageBox: TSimbaImageBox;

    procedure FontChanged(Sender: TObject); override;

    procedure CalculateBestColor;

    function GetColors: TIntegerArray;
  public
    OnCalculateBestColor: procedure(CTS: Int32; Color, Tolerance: Int32; Hue, Sat: Extended) of object;

    constructor Create(TargetWindow: THandle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  math, clipbrd, simba.bitmap,
  simba.aca_math, simba.colormath;

procedure TSimbaACAForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R, G, B: Byte;
  H, S, L: Extended;
  localX, localY, globalX, globalY: Int32;
begin
  ImageZoom.Picture.Bitmap.BeginUpdate(True);
  ImageZoom.Tag := PtrInt(True);

  try
    for localX := 0 to 5 do
      for localY := 0 to 5 do
      begin
        globalX := X + localX - 2;
        globalY := Y + localY - 2;

        ImageZoom.Picture.Bitmap.Canvas.Pixels[localX, localY] := FImageBox.Background.Canvas.Pixels[globalX, globalY];
      end;
  finally
    ImageZoom.Picture.Bitmap.EndUpdate(False);
  end;

  ColorToRGB(FImageBox.Background.Canvas.Pixels[X, Y], R, G, B);
  ColorToHSL(FImageBox.Background.Canvas.Pixels[X, Y], H, S, L);

  ColorLabel.Caption := Format('Color: %d', [FImageBox.Background.Canvas.Pixels[X, Y]]) + LineEnding +
                        Format('RGB: %d, %d, %d', [R, G, B])                            + LineEnding +
                        Format('HSL: %.2f, %.2f, %.2f', [H, S, L])                      + LineEnding;
end;

procedure TSimbaACAForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixel: Int32;
begin
  if (Button = mbLeft) then
  begin
    Pixel := FImageBox.Background.Canvas.Pixels[X, Y];

    if ColorListBox.Items.IndexOf(Pixel.ToString()) = -1 then
      ColorListBox.ItemIndex := ColorListBox.Items.AddObject(Pixel.ToString(), TObject(PtrUInt(Pixel)));
  end;
end;

procedure TSimbaACAForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (not (csLoading in ComponentState)) then
  begin
    with Canvas.TextExtent(' HSL: 100.00, 100.00, 100.00 ') do
    begin
      ColorListBox.ItemHeight := CY + 10;
      ColorListBox.ColorRectWidth := ColorListBox.ItemHeight - (ColorListBox.ColorRectOffset * 2);

      ColorLabelPanel.Width := CX;
      ColorLabelPanel.Height := CY * 3;
    end;

    if ColorLabelPanel.Height > PanelZoom.Height then
    begin
      PanelZoom.Width := (ColorLabelPanel.Height-5) + (5 - (ColorLabelPanel.Height-5) mod 5);
      PanelZoom.Height := (ColorLabelPanel.Height-5) + (5 - (ColorLabelPanel.Height-5) mod 5);
      PanelZoom.Height := PanelZoom.Height + 2;
      PanelZoom.Width := PanelZoom.Height + 2;
    end;
  end;
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
      'Red': FImageBox.Overlay.Canvas.Pen.Color := clRed;
      'Green': FImageBox.Overlay.Canvas.Pen.Color := clGreen;
      'Blue': FImageBox.Overlay.Canvas.Pen.Color := clBlue;
      'Yellow': FImageBox.Overlay.Canvas.Pen.Color := clYellow;
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
var
  W, H: Int32;
begin
  if not FClient.IOManager.TargetValid then
    FClient.IOManager.SetDesktop();

  FClient.IOManager.GetDimensions(W, H);
  FClient.MBitmaps[0].CopyClientToBitmap(FClient.IOManager, True, 0, 0, W-1, H-1);

  FImageBox.Background.LoadFromMufasaBitmap(FClient.MBitmaps[0]);
  FImageBox.BackgroundChanged();
end;

procedure TSimbaACAForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSimbaACAForm.MouseZoomPaint(Sender: TObject);
begin
  with Sender as TGraphicControl do
    if Boolean(Tag) then
    begin
      Canvas.Pen.Color := FImageBox.Overlay.Canvas.Pen.Color;
      Canvas.Frame(
        (Width div 2) - Floor(Width / 5 / 2),
        (Height div 2) - Floor(Width / 5 / 2),
        (Width div 2) + Ceil(Width / 5 / 2),
        (Height div 2) + Ceil(Width / 5 / 2)
      );
    end;
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
    ImageZoom.Tag := PtrInt(False);
    ImageZoom.Canvas.Brush.Color := ColorListBox.Selected;
    ImageZoom.Canvas.Clear();

    ColorToRGB(ColorListBox.Selected, R, G, B);
    ColorToHSL(ColorListBox.Selected, H, S, L);

    ColorLabel.Caption := Format('Color: %d', [ColorListBox.Selected]) + LineEnding +
                          Format('RGB: %d, %d, %d', [R, G, B])         + LineEnding +
                          Format('HSL: %.2f, %.2f, %.2f', [H, S, L])   + LineEnding;
  end;

  CalculateBestColor();
end;

procedure TSimbaACAForm.ButtonDebugColorClick(Sender: TObject);
var
  CTS: Int32;
  Col, Tol: Int32;
  Hue, Sat: Extended;
  Matches: Int32;
begin
  if ButtonCTS0.Checked then CTS := 0;
  if ButtonCTS1.Checked then CTS := 1;
  if ButtonCTS2.Checked then CTS := 2;

  Col := StrToIntDef(EditColor.Text, -1);
  Tol := StrToIntDef(EditTolerance.Text, -1);
  Hue := StrToFloatDef(EditHue.Text, -1);
  Sat := StrToFloatDef(EditSat.Text, -1);

  case CTS of
    0: Matches := FImageBox.Overlay.DebugColorCTS0(Col, Tol);
    1: Matches := FImageBox.Overlay.DebugColorCTS1(Col, Tol);
    2: Matches := FImageBox.Overlay.DebugColorCTS2(Col, Tol, Hue, Sat);
  end;

  FImageBox.Repaint();
  FImageBox.StatusPanel.Text := Format('Found %.0n matches', [Double(Matches)]);
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
          ColorListBox.Items.Objects[i] := TObject(PtrInt(ColorListBox.Items[i].ToInteger()));
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

    if (OnCalculateBestColor <> nil) then
      OnCalculateBestColor(CTS, Col, Tol, Hue, Sat);
  end;
end;

procedure TSimbaACAForm.ButtonDeleteColorsClick(Sender: TObject);
begin
  ColorListBox.Clear();
  ColorListBox.OnSelectionChange(Sender, False);
end;

procedure TSimbaACAForm.ButtonClearImageClick(Sender: TObject);
begin
  FImageBox.Overlay.Clear();
  FImageBox.Repaint();
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
      FImageBox.Background.LoadFromFile(FileName);
      FImageBox.BackgroundChanged();
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

constructor TSimbaACAForm.Create(TargetWindow: THandle);
begin
  inherited Create(Application.MainForm);

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := PanelMain;
  FImageBox.Align := alClient;
  FImageBox.OnMouseDown := @ClientImageMouseDown;
  FImageBox.OnMouseMove := @ClientImageMouseMove;
  FImageBox.Overlay.Canvas.Pen.Color := clRed;

  FClient := TClient.Create();
  FClient.IOManager.SetTarget(TargetWindow);
  FClient.MBitmaps.CreateBMP(0, 0);

  ImageZoom.Picture.Bitmap.Width := 5;
  ImageZoom.Picture.Bitmap.Height := 5;

  ColorListBox.Options := [];

  ButtonUpdateImage.Click();
end;

destructor TSimbaACAForm.Destroy;
begin
  FClient.MBitmaps[0].Free();
  FClient.Free();

  inherited Destroy();
end;

initialization
  {$I simba.aca.lrs}

end.

