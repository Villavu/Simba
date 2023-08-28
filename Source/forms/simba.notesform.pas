{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.notesform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls,
  simba.component_synedit, simba.settings;

type
  TSimbaNotesForm = class(TForm)
  published
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    SynEdit: TSimbaSynEdit;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaNotesForm: TSimbaNotesForm;

implementation

{$R *.lfm}

procedure TSimbaNotesForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.General.Notes.Value := SynEdit.Text;
end;

procedure TSimbaNotesForm.FormCreate(Sender: TObject);
begin
  SynEdit.Text := SimbaSettings.General.Notes.Value;
end;

constructor TSimbaNotesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SynEdit := TSimbaMemo.Create(Self, True);
  SynEdit.Parent := Self;
  SynEdit.Align := alClient;
end;

end.

