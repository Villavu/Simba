{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.frame_settings_general;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ComCtrls, ExtCtrls, Spin, DividerBevel,
  simba.base, simba.ide_theme;

type
  TSimbaGeneralFrame = class(TFrame)
    ImageSizeLabel: TLabel;
    ImageSizeTrackBar: TTrackBar;
    Label1: TLabel;
    ToolbarSpacingSpinEdit: TSpinEdit;
    ToolbarPositionComboBox: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    PlaceholderLabel: TLabel;
    ScrollBarSizeLabel: TLabel;
    ScrollBarArrowSizeLabel: TLabel;
    ToolbarSizeCaption: TLabel;
    FontSizeLabel: TLabel;
    ToolbarSizeCaption1: TLabel;
    ToolbarSizeTrackBar: TTrackBar;
    FontSizeTrackBar: TTrackBar;
    ScrollBarSizeTrackBar: TTrackBar;
    ScrollBarArrowSizeTrackBar: TTrackBar;

    procedure ImageSizeTrackBarChange(Sender: TObject);
    procedure ToolbarPositionComboBoxChange(Sender: TObject);
    procedure FontSizeTrackBarChange(Sender: TObject);
    procedure ToolbarSizeTrackBarChange(Sender: TObject);
    procedure DoScrollBarArrowTrackBarChange(Sender: TObject);
    procedure DoScrollBarTrackBarChange(Sender: TObject);
    procedure ToolbarSpacingSpinEditChange(Sender: TObject);
  protected
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Load;
    procedure Save;
  end;

implementation

uses
  ATScrollBar, simba.settings;

{$R *.lfm}

procedure TSimbaGeneralFrame.FontSizeTrackBarChange(Sender: TObject);
begin
  SimbaSettings.General.CustomFontSize.Value := FontSizeTrackBar.Position;

  FontSizeLabel.Caption := IfThen(
    SimbaSettings.General.CustomFontSize.IsDefault(),
    'Font Size: Default',
    'Font Size: ' + IntToStr(FontSizeTrackBar.Position)
  );
end;

procedure TSimbaGeneralFrame.ToolbarPositionComboBoxChange(Sender: TObject);
begin
  case ToolbarPositionComboBox.ItemIndex of
    0: SimbaSettings.General.ToolbarPosition.Value := 'Top';
    1: SimbaSettings.General.ToolbarPosition.Value := 'Left';
    2: SimbaSettings.General.ToolbarPosition.Value := 'Right';
  end;
end;

procedure TSimbaGeneralFrame.ImageSizeTrackBarChange(Sender: TObject);
begin
  ImageSizeLabel.Caption := IfThen(
    ImageSizeTrackBar.Position = ImageSizeTrackBar.Min,
    'Image Size: Default',
    'Image Size: ' + IntToStr(ImageSizeTrackBar.Position)
  );

  if ImageSizeTrackBar.Position = ImageSizeTrackBar.Min then
    SimbaSettings.General.CustomImageSize.SetDefault()
  else
    SimbaSettings.General.CustomImageSize.Value := ImageSizeTrackBar.Position;
end;

procedure TSimbaGeneralFrame.DoScrollBarArrowTrackBarChange(Sender: TObject);
begin
  SimbaSettings.General.ScrollBarArrowSize.Value := ScrollBarArrowSizeTrackBar.Position;

  ScrollBarArrowSizeLabel.Caption := IfThen(
    SimbaSettings.General.ScrollBarArrowSize.IsDefault(),
    'Arrow Size: Default',
    'Arrow Size: ' + IntToStr(ScrollBarArrowSizeTrackBar.Position)
  );
end;

procedure TSimbaGeneralFrame.ToolbarSizeTrackBarChange(Sender: TObject);
begin
  SimbaSettings.General.ToolbarSize.Value := ToolbarSizeTrackBar.Position;

  ToolbarSizeCaption.Caption := IfThen(
    SimbaSettings.General.ToolbarSize.IsDefault(),
    'Size: Default',
    'Size: ' + IntToStr(ToolbarSizeTrackBar.Position)
  );
end;

procedure TSimbaGeneralFrame.DoScrollBarTrackBarChange(Sender: TObject);
begin
  SimbaSettings.General.ScrollBarSize.Value := ScrollBarSizeTrackBar.Position;

  ScrollBarSizeLabel.Caption := IfThen(
    SimbaSettings.General.ScrollBarSize.IsDefault(),
    'Size: Default',
    'Size: ' + IntToStr(ScrollBarSizeTrackBar.Position)
  );
end;

procedure TSimbaGeneralFrame.ToolbarSpacingSpinEditChange(Sender: TObject);
begin
  SimbaSettings.General.ToolBarSpacing.Value := ToolbarSpacingSpinEdit.Value;
end;

procedure TSimbaGeneralFrame.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  PlaceholderLabel.Font := Self.Font;
  PlaceholderLabel.Font.Color := clForm;
end;

constructor TSimbaGeneralFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PlaceholderLabel.Font.Color := clForm;
end;

procedure TSimbaGeneralFrame.Load;
begin
  case String(SimbaSettings.General.ToolbarPosition.Value) of
    'Top':   ToolbarPositionComboBox.ItemIndex := 0;
    'Left':  ToolbarPositionComboBox.ItemIndex := 1;
    'Right': ToolbarPositionComboBox.ItemIndex := 2;
  end;

  ToolbarSizeTrackBar.Position := SimbaSettings.General.ToolbarSize.Value;
  ToolbarSpacingSpinEdit.Value := SimbaSettings.General.ToolBarSpacing.Value;

  FontSizeTrackBar.Position := SimbaSettings.General.CustomFontSize.Value;
  ScrollBarSizeTrackBar.Position := SimbaSettings.General.ScrollBarSize.Value;
  ScrollBarArrowSizeTrackBar.Position := SimbaSettings.General.ScrollBarArrowSize.Value;
  ImageSizeTrackBar.Position := SimbaSettings.General.CustomImageSize.Value;
end;

procedure TSimbaGeneralFrame.Save;
begin
  { nothing }
end;

end.

