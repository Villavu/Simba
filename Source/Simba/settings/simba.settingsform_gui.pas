unit simba.settingsform_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls;

type
  TGUIFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    ToolbarSizeTrackBar: TTrackBar;
    FontSizeTrackBar: TTrackBar;

    procedure FontSizeTrackBarChange(Sender: TObject);
    procedure ToolbarSizeTrackBarChange(Sender: TObject);
  public
  end;

implementation

procedure TGUIFrame.FontSizeTrackBarChange(Sender: TObject);
begin
  if FontSizeTrackBar.Position = 0 then
    Label2.Caption := 'Font Size (Default)'
  else
    Label2.Caption := 'Font Size (' + IntToStr(FontSizeTrackBar.Position) + ')';
end;

procedure TGUIFrame.ToolbarSizeTrackBarChange(Sender: TObject);
begin
  if ToolbarSizeTrackBar.Position = 0 then
    Label1.Caption := 'Toolbar Size (Default)'
  else
    Label1.Caption := 'Toolbar Size (' + IntToStr(ToolbarSizeTrackBar.Position) + ')';
end;

initialization
  {$I simba.settingsform_gui.lrs}

end.

