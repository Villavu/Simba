unit simba.notesform;

{$mode objfpc}{$H+}

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

