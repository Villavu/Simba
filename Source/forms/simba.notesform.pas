{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.notesform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TSimbaNotesForm = class(TForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  SimbaNotesForm: TSimbaNotesForm;

implementation

{$R *.lfm}

uses
  AnchorDocking,
  simba.settings;

procedure TSimbaNotesForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.GUI.Notes.Value := Memo.Lines.Text;
end;

procedure TSimbaNotesForm.FormCreate(Sender: TObject);
begin
  Memo.Lines.Text := SimbaSettings.GUI.Notes.Value;
end;

end.

