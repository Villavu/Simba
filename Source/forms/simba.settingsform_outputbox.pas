{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_outputbox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ColorBox, StdCtrls, Spin;

type
  TSimbaOutputBoxFrame = class(TFrame)
    AntiAliasingCheckbox: TCheckBox;
    ButtonReset: TButton;
    ColorBoxBackground: TColorBox;
    ColorBoxFontColor: TColorBox;
    FontNameComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FontSizeSpinEdit: TSpinEdit;
    Label5: TLabel;
    procedure AntiAliasingCheckboxChange(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ColorBoxBackgroundChange(Sender: TObject);
    procedure ColorBoxFontColorChange(Sender: TObject);
    procedure FontNameComboBoxChange(Sender: TObject);
    procedure FontSizeSpinEditChange(Sender: TObject);
  protected
    procedure SizeComponents;
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Load;
    procedure Save;
  end;

implementation

{$R *.lfm}

uses
  graphics,
  simba.settings, simba.fonthelpers;

type
  TRGB = record R, G, B: Integer; end;
  TRGBArray = array of TRGB;

const
  ColorBoxPresets: TRGBArray = (
    (R: 255; G: 255; B: 255),
    (R: 235; G: 235; B: 235),
    (R: 215; G: 215; B: 215),
    (R: 195; G: 195; B: 195),
    (R: 175; G: 175; B: 175),
    (R: 158; G: 158; B: 158),
    (R: 138; G: 138; B: 138),
    (R: 118; G: 118; B: 118),
    (R: 108; G: 108; B: 108),
    (R: 98;  G: 98;  B: 98),
    (R: 78;  G: 78;  B: 78),
    (R: 58;  G: 58;  B: 58),
    (R: 38;  G: 38;  B: 38),
    (R: 63;  G: 63;  B: 70),
    (R: 59;  G: 74;  B: 87),
    (R: 69;  G: 84;  B: 94),
    (R: 79;  G: 94;  B: 107),
    (R: 89;  G: 104; B: 117),
    (R: 153; G: 180; B: 209),
    (R: 191; G: 205; B: 219)
  );

procedure TSimbaOutputBoxFrame.FontSizeSpinEditChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontSize.Value := TSpinEdit(Sender).Value;
end;

procedure TSimbaOutputBoxFrame.SizeComponents;
begin
  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    if (Canvas.Font.Size = 0) then
      Canvas.Font.Size := GetDefaultFontSize() + 1
    else
      Canvas.Font.Size := Canvas.Font.Size + 1;

    ColorBoxBackground.ItemHeight := Canvas.TextHeight('Fj');
    ColorBoxFontColor.ItemHeight := Canvas.TextHeight('Fj');
  finally
    Free();
  end;
end;

procedure TSimbaOutputBoxFrame.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  SizeComponents();
end;

procedure TSimbaOutputBoxFrame.FontNameComboBoxChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontName.Value := TComboBox(Sender).Text;
end;

procedure TSimbaOutputBoxFrame.ColorBoxFontColorChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontColor.Value := TColorBox(Sender).Selected;
end;

procedure TSimbaOutputBoxFrame.ColorBoxBackgroundChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.Color.Value := TColorBox(Sender).Selected;
end;

procedure TSimbaOutputBoxFrame.AntiAliasingCheckboxChange(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontAntiAliased.Value := TCheckBox(Sender).Checked;
end;

procedure TSimbaOutputBoxFrame.ButtonResetClick(Sender: TObject);
begin
  SimbaSettings.OutputBox.FontSize.SetDefault();
  SimbaSettings.OutputBox.FontName.SetDefault();
  SimbaSettings.OutputBox.FontColor.SetDefault();
  SimbaSettings.OutputBox.Color.SetDefault();
  SimbaSettings.OutputBox.FontAntiAliased.SetDefault();

  Load();
end;

constructor TSimbaOutputBoxFrame.Create(TheOwner: TComponent);
var
  Preset: TRGB;
begin
  inherited Create(TheOwner);

  FontNameComboBox.Items.AddStrings(GetFixedFonts());

  ColorBoxBackground.ItemHeight := Scale96ToScreen(20);
  for Preset in ColorBoxPresets do
    with Preset do
      ColorBoxBackground.Items.AddObject('Color (%d, %d, %d)', [R, G, B], TObject(PtrUInt(RGBToColor(R, G, B))));

  ColorBoxFontColor.ItemHeight := Scale96ToScreen(20);
  ColorBoxFontColor.Items.Assign(ColorBoxBackground.Items);
end;

procedure TSimbaOutputBoxFrame.Load;
begin
  AntiAliasingCheckbox.Checked := SimbaSettings.OutputBox.FontAntiAliased.Value;
  FontSizeSpinEdit.Value := SimbaSettings.OutputBox.FontSize.Value;
  FontNameComboBox.ItemIndex := FontNameComboBox.Items.IndexOf(SimbaSettings.OutputBox.FontName.Value);

  ColorBoxBackground.Selected := SimbaSettings.OutputBox.Color.Value;
  ColorBoxFontColor.Selected := SimbaSettings.OutputBox.FontColor.Value;
end;

procedure TSimbaOutputBoxFrame.Save;
begin
  { nothing - it's adjusted on the go }
end;

end.

