{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_simba_general;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Spin,
  DividerBevel;

type
  TSimbaGeneralFrame = class(TFrame)
    CheckGroup1: TCheckGroup;
    OutputFontAntiAliased: TCheckBox;
    OutputFontName: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OutputDivider: TDividerBevel;
    OutputFontSize: TSpinEdit;
    ToolbarPositionRadioGroup: TRadioGroup;
    ToolbarSizeCaption: TLabel;
    FontSizeLabel: TLabel;
    ToolbarSizeTrackBar: TTrackBar;
    FontSizeTrackBar: TTrackBar;

    procedure FontSizeTrackBarChange(Sender: TObject);
    procedure ToolbarSizeTrackBarChange(Sender: TObject);
  private
    function GetToolbarPosition: String;
    procedure SetToolbarPosition(AValue: String);
  public
    property ToolbarPosition: String read GetToolbarPosition write SetToolbarPosition;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

procedure TSimbaGeneralFrame.FontSizeTrackBarChange(Sender: TObject);
begin
  if (FontSizeTrackBar.Position = FontSizeTrackBar.Min) then
    FontSizeLabel.Caption := 'Font Size (Default)'
  else
    FontSizeLabel.Caption := 'Font Size (' + IntToStr(FontSizeTrackBar.Position) + ')';
end;

procedure TSimbaGeneralFrame.ToolbarSizeTrackBarChange(Sender: TObject);
begin
  if (ToolbarSizeTrackBar.Position = ToolbarSizeTrackBar.Min) then
    ToolbarSizeCaption.Caption := 'Toolbar Size (Default)'
  else
    ToolbarSizeCaption.Caption := 'Toolbar Size (' + IntToStr(ToolbarSizeTrackBar.Position) + ')';
end;

function TSimbaGeneralFrame.GetToolbarPosition: String;
begin
  case ToolbarPositionRadioGroup.ItemIndex of
    0: Result := 'Top';
    1: Result := 'Left';
    2: Result := 'Right';
  end;
end;

procedure TSimbaGeneralFrame.SetToolbarPosition(AValue: String);
begin
  case AValue of
    'Top':   ToolbarPositionRadioGroup.ItemIndex := 0;
    'Left':  ToolbarPositionRadioGroup.ItemIndex := 1;
    'Right': ToolbarPositionRadioGroup.ItemIndex := 2;
    else
      ToolbarPositionRadioGroup.ItemIndex := 0;
  end;
end;

constructor TSimbaGeneralFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CheckGroup1.CheckEnabled[1] := {$IFDEF WINDOWS}True{$ELSE}False{$ENDIF};
  CheckGroup1.CheckEnabled[2] := {$IFDEF DARWIN}True{$ELSE}False{$ENDIF};
end;

end.

