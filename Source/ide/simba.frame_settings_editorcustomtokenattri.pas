{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.frame_settings_editorcustomtokenattri;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, Buttons;

type
  TEditorCustomTokenAttriFrame = class(TFrame)
    ButtonRemove: TButton;
    ButtonAdd: TButton;
    ComboAttributes: TComboBox;
    EditToken: TEdit;
    LabelToken: TLabel;
    LabelAttribute: TLabel;
    ListBox: TListBox;
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ComboAttributesChange(Sender: TObject);
    procedure EditTokenChange(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FChanging: Boolean;

    procedure Modify(Index: Integer; NewToken, NewAttri: String);
    // the list box does not own its item objects
    procedure ClearItems;
  public
    destructor Destroy; override;

    procedure Load;
    procedure Save;
  end;

implementation

uses
  SynHighlighterPas,
  simba.editor_highlighter,
  simba.settings,
  simba.vartype_string;

type
  TListObject = class(TObject)
  public
    Token: String;
    Attri: String;
  end;

procedure TEditorCustomTokenAttriFrame.ButtonAddClick(Sender: TObject);
begin
  ListBox.Items.AddObject('Token: "" Attribute:', TListObject.Create());
end;

procedure TEditorCustomTokenAttriFrame.ButtonRemoveClick(Sender: TObject);
begin
  if (ListBox.ItemIndex > -1) then
  begin
    FChanging := True;

    ListBox.Items.Objects[ListBox.ItemIndex].Free();
    ListBox.Items.Delete(ListBox.ItemIndex);
    ListBox.ItemIndex := -1;

    EditToken.Enabled := False;
    EditToken.Text := '';
    ComboAttributes.Enabled := False;
    ComboAttributes.Text := '';

    FChanging := False;
  end;
end;

procedure TEditorCustomTokenAttriFrame.ComboAttributesChange(Sender: TObject);
begin
  if FChanging then
    Exit;
  Modify(ListBox.ItemIndex, EditToken.Text, ComboAttributes.Text);
end;

procedure TEditorCustomTokenAttriFrame.EditTokenChange(Sender: TObject);
begin
  if FChanging then
    Exit;
  Modify(ListBox.ItemIndex, EditToken.Text, ComboAttributes.Text);
end;

procedure TEditorCustomTokenAttriFrame.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  FChanging := True;
  if (ListBox.ItemIndex > -1) then
  begin
    EditToken.Enabled := True;
    EditToken.Text := TListObject(ListBox.Items.Objects[ListBox.ItemIndex]).Token;
    ComboAttributes.Enabled := True;
    ComboAttributes.ItemIndex := ComboAttributes.Items.IndexOf(TListObject(ListBox.Items.Objects[ListBox.ItemIndex]).Attri);
    if (ComboAttributes.ItemIndex = -1) then
      ComboAttributes.ItemIndex := ComboAttributes.Items.IndexOf('Reserved word');
  end else
  begin
    EditToken.Enabled := False;
    EditToken.Text := '';
    ComboAttributes.Enabled := False;
    ComboAttributes.Text := '';
  end;
  FChanging := False;
end;

procedure TEditorCustomTokenAttriFrame.Modify(Index: Integer; NewToken, NewAttri: String);
begin
  if (Index >= 0) and (Index < ListBox.Count) then
  begin
    TListObject(ListBox.Items.Objects[Index]).Token := NewToken;
    TListObject(ListBox.Items.Objects[Index]).Attri := NewAttri;

    ListBox.Items[Index] := 'Token "' + NewToken + '" Attribute: ' + NewAttri;
  end;
end;

procedure TEditorCustomTokenAttriFrame.ClearItems;
var
  I: Integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
    ListBox.Items.Objects[I].Free();
  ListBox.Items.Clear();
end;

destructor TEditorCustomTokenAttriFrame.Destroy;
begin
  ClearItems();

  inherited Destroy();
end;

procedure TEditorCustomTokenAttriFrame.Load;
var
  Val: String;
  Obj: TListObject;
begin
  if (ComboAttributes.Items.Count = 0) then
  begin
    with TSimbaEditorHighlighter.Create(nil) do
    try
      ComboAttributes.Items.AddStrings(AttributeNames);
    finally
      Free();
    end;
  end;

  ClearItems();
  EditToken.Enabled := False;
  EditToken.Text := '';
  ComboAttributes.Enabled := False;
  ComboAttributes.Text := '';

  for Val in String(SimbaSettings.Editor.CustomTokenAttris.Value).Split(',') do
  begin
    if (Val.Before('=') = '') or (Val.After('=') = '') then
      Continue;

    Obj := TListObject.Create();
    Obj.Token := Val.Before('=');
    Obj.Attri := Val.After('=');

    ListBox.Items.AddObject('Token "' + Obj.Token + '" Attribute: ' + Obj.Attri, Obj);
  end;
end;

procedure TEditorCustomTokenAttriFrame.Save;
var
  I: Integer;
  Value: String;
begin
  Value := '';
  for I := 0 to ListBox.Count - 1 do
    if (TListObject(ListBox.Items.Objects[I]).Token <> '') and (TListObject(ListBox.Items.Objects[I]).Attri <> '') then
      Value := Value + TListObject(ListBox.Items.Objects[I]).Token + '=' + TListObject(ListBox.Items.Objects[I]).Attri + ',';
  SimbaSettings.Editor.CustomTokenAttris.Value := Value;
end;

{$R *.lfm}

end.

