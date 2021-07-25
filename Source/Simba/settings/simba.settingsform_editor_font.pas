unit simba.settingsform_editor_font;

{$mode objfpc}{$H+}
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
    Text := 'program Highlight;                                      ' + LineEnding +
            '{comment}                                               ' + LineEnding +
            '{$I SRL/osr.simba}                                      ' + LineEnding +
            '                                                        ' + LineEnding +
            '// this function does stuff                             ' + LineEnding +
            'procedure Test(var i: Int32);                           ' + LineEnding +
            'var                                                     ' + LineEnding +
            '  x: Int32;                                             ' + LineEnding +
            '  s: String;                                            ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  x := 1000 * (5 + 7);                                  ' + LineEnding +
            '  s := ' + #39 + 'The number is :' + #39 + ' +ToStr(x); ' + LineEnding +
            '                                                        ' + LineEnding +
            '  Inc(x);                                               ' + LineEnding +
            '  {$RANGECHECKS ON}                                     ' + LineEnding +
            '  case x of                                             ' + LineEnding +
            '    1: ;                                                ' + LineEnding +
            '    2: ;                                                ' + LineEnding +
            '    3: ;                                                ' + LineEnding +
            '  end;                                                  ' + LineEnding +
            'end;                                                    ' + LineEnding +
            '                                                        ' + LineEnding +
            '(* object method! *)                                    ' + LineEnding +
            'function TPoint.Test: Boolean; overload;                ' + LineEnding +
            'begin                                                   ' + LineEnding +
            'end;                                                    ';
  end;
end;

end.

