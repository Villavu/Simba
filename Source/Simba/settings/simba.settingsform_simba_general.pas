unit simba.settingsform_simba_general;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  DividerBevel;

type
  TSimbaGeneralFrame = class(TFrame)
    OpenSSLoadedValueCaption: TLabel;
    OpenSSLStartupCheckbox: TCheckBox;
    Divider: TDividerBevel;
    ToolbarSizeCaption: TLabel;
    FontSizeLabel: TLabel;
    OpenSSLLoadedCaption: TLabel;
    ToolbarSizeTrackBar: TTrackBar;
    FontSizeTrackBar: TTrackBar;

    procedure FontSizeTrackBarChange(Sender: TObject);
    procedure ToolbarSizeTrackBarChange(Sender: TObject);
  public
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

end.

