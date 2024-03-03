{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_editor_default;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, EditBtn;

type
  TEditorDefaultFrame = class(TFrame)
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    LabelFileName: TLabel;
    Memo1: TMemo;
    Notebook1: TNotebook;
    PageLoadFile: TPage;
    PageSetScript: TPage;
    RadioGroup1: TRadioGroup;

    procedure RadioGroup1SelectionChanged(Sender: TObject);
  public
    procedure Load;
    procedure Save;
  end;

implementation

uses
  simba.settings;

procedure TEditorDefaultFrame.RadioGroup1SelectionChanged(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: PageLoadFile.Show();
    1: PageSetScript.Show();
  end;
end;

procedure TEditorDefaultFrame.Load;
begin
  Memo1.Lines.Text := SimbaSettings.Editor.DefaultScript.Value;
  FileNameEdit1.FileName := SimbaSettings.Editor.DefaultScriptFile.Value;
  RadioGroup1.ItemIndex := SimbaSettings.Editor.DefaultScriptType.Value;
end;

procedure TEditorDefaultFrame.Save;
begin
  SimbaSettings.Editor.DefaultScript.Value := Memo1.Lines.Text;
  SimbaSettings.Editor.DefaultScriptFile.Value := FileNameEdit1.FileName;
  SimbaSettings.Editor.DefaultScriptType.Value := RadioGroup1.ItemIndex;
end;

{$R *.lfm}

end.

