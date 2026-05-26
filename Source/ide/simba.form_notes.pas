{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_notes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls,
  simba.base,
  simba.ide_events,
  simba.component_synedit,
  simba.settings;

type
  TSimbaNotesForm = class(TForm)
  published
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  public
    SynEdit: TSimbaSynEdit;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaNotesForm: TSimbaNotesForm;

implementation

uses
  AnchorDocking, Menus,
  simba.ide_dockinghelpers;

procedure TSimbaNotesForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.General.Notes.Value := SynEdit.Text;
end;

procedure TSimbaNotesForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewNotes(Item: TMenuItem);
  begin
    DockMaster.Show(Self);
  end;

begin
  case Event of
    ESimbaEvent.ACTION_VIEW_NOTES: DoViewNotes(TMenuItem(Data));
  end;
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

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_VIEW_NOTES]);
end;

{$R *.lfm}

end.

