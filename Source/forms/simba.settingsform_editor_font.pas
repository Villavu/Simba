{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_editor_font;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Spin,
  simba.editor;

type
  TEditorFontFrame = class(TFrame)
    FontAntiAliasedCheckbox: TCheckBox;
    FontsNameComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    EditorPanel: TPanel;
    FontSizeSpinEdit: TSpinEdit;
    procedure FontAntiAliasedCheckboxChange(Sender: TObject);
    procedure FontsNameComboBoxChange(Sender: TObject);
    procedure FontSizeSpinEditChange(Sender: TObject);
  protected
    FEditor: TSimbaEditor;
  public
    property Editor: TSimbaEditor read FEditor;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

procedure TEditorFontFrame.FontAntiAliasedCheckboxChange(Sender: TObject);
begin
  if FontAntiAliasedCheckbox.Checked then
    FEditor.Font.Quality := fqCleartypeNatural
  else
    FEditor.Font.Quality := fqNonAntialiased;
end;

procedure TEditorFontFrame.FontsNameComboBoxChange(Sender: TObject);
begin
  FEditor.Font.Name := FontsNameComboBox.Text;
end;

procedure TEditorFontFrame.FontSizeSpinEditChange(Sender: TObject);
begin
  FEditor.Font.Size := FontSizeSpinEdit.Value;
end;

constructor TEditorFontFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditor := TSimbaEditor.Create(Self);
  with FEditor do
  begin
    Parent := EditorPanel;
    Align := alClient;
    BorderStyle := bsNone;
    ReadOnly := True;
    Text := 'program Font;                                           ' + LineEnding +
            '{  brace }                                              ' + LineEnding +
            '(* round *)                                             ' + LineEnding +
            '// slash                                                ' + LineEnding +
            '                                                        ' + LineEnding +
            '{$I SRL/osr.simba}                                      ' + LineEnding +
            '                                                        ' + LineEnding +
            'procedure Test(var i: Int32);                           ' + LineEnding +
            'var                                                     ' + LineEnding +
            '  s: String;                                            ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  i := 1000 * (5 + 7);                                  ' + LineEnding +
            '  s := ' + #39 + 'The number is :' + #39 + ' + ToStr(i);' + LineEnding +
            '                                                        ' + LineEnding +
            '  case Random(5) of                                     ' + LineEnding +
            '    1: ;                                                ' + LineEnding +
            '    2..4: ;                                             ' + LineEnding +
            '  end;                                                  ' + LineEnding +
            'end;                                                    ' + LineEnding +
            '                                                        ' + LineEnding +
            'function TPoint.Test: Boolean; overload;                ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  Result := True;                                       ' + LineEnding +
            'end;                                                    ';
  end;
end;

end.

