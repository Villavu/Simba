unit simba.notesform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TSimbaNotesForm = class(TForm)
    Memo: TMemo;
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaNotesForm: TSimbaNotesForm;

implementation

uses
  AnchorDocking,
  simba.settings;

procedure TSimbaNotesForm.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  if DockMaster.GetAnchorSite(Self) <> nil then
    DockMaster.GetAnchorSite(Self).Visible := Value;
end;

constructor TSimbaNotesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Memo.Lines.Text := SimbaSettings.GUI.Notes.Value;
end;

destructor TSimbaNotesForm.Destroy;
begin
  SimbaSettings.GUI.Notes.Value := Memo.Lines.Text;

  inherited Destroy();
end;

initialization
  {$I simba.notesform.lrs}

end.

