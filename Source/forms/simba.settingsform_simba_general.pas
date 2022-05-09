{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_simba_general;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Spin, CheckLst,
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
    ToolbarSizeCaption: TLabel;
    FontSizeLabel: TLabel;
    ToolbarSizeTrackBar: TTrackBar;
    FontSizeTrackBar: TTrackBar;

    procedure FontSizeTrackBarChange(Sender: TObject);
    procedure ToolbarSizeTrackBarChange(Sender: TObject);
  public
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

constructor TSimbaGeneralFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CheckGroup1.CheckEnabled[1] := {$IFDEF WINDOWS}True{$ELSE}False{$ENDIF};
  CheckGroup1.CheckEnabled[2] := {$IFDEF DARWIN}True{$ELSE}False{$ENDIF};
end;

end.

