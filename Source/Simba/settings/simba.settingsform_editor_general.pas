unit simba.settingsform_editor_general;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Spin, DividerBevel;

type
  TEditorGeneralFrame = class(TFrame)
    IgnoreCodeToolsDirectiveCheckbox: TCheckBox;
    OpenDefaultScriptButton: TButton;
    CaretPastEOLCheckBox: TCheckBox;
    DividerVisibleCheckbox: TCheckBox;
    DividerBevel4: TDividerBevel;
    VisibleRightMarginCheckbox: TCheckBox;
    Label2: TLabel;
    ShowParameterHintsCheckbox: TCheckBox;
    OpenAutoCompletionCheckbox: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DefaultScriptEdit: TEdit;
    Label1: TLabel;
    RightMarginEdit: TSpinEdit;
    procedure OpenDefaultScriptButtonClick(Sender: TObject);
  private

  public

  end;

implementation

uses
  dialogs,
  simba.files;

procedure TEditorGeneralFrame.OpenDefaultScriptButtonClick(Sender: TObject);
var
  Directory: String;
begin
  if SelectDirectory('Default Script', GetScriptPath(), Directory) then
    DefaultScriptEdit.Text := Directory;
end;

initialization
  {$I simba.settingsform_editor_general.lrs}

end.

