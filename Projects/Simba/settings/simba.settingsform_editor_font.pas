unit simba.settingsform_editor_font;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  Spin,
  simba.editor;

type
  TEditorFontFrame = class(TFrame)
    FontAntiAliasedCheckbox: TCheckBox;
    FontsComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    EditorPanel: TPanel;
    FontHeightEdit: TSpinEdit;
    procedure FontAntiAliasedCheckboxChange(Sender: TObject);
    procedure FontsComboBoxChange(Sender: TObject);
    procedure FontHeightEditChange(Sender: TObject);
  protected
    FEditor: TSimbaEditor;
  public
    property Editor: TSimbaEditor read FEditor;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

procedure TEditorFontFrame.FontAntiAliasedCheckboxChange(Sender: TObject);
begin
  FEditor.AntiAliasing := FontAntiAliasedCheckbox.Checked;
end;

procedure TEditorFontFrame.FontsComboBoxChange(Sender: TObject);
begin
  FEditor.Font.Name := FontsComboBox.Text;
end;

procedure TEditorFontFrame.FontHeightEditChange(Sender: TObject);
begin
  FEditor.Font.Height := FontHeightEdit.Value;
end;

constructor TEditorFontFrame.Create(AOwner: TComponent);
var
  i: Int32;
begin
  inherited Create(AOwner);

  FontsComboBox.Items.BeginUpdate();

  with TBitmap.Create() do
  try
    for i := 0 to Screen.Fonts.Count - 1 do
    begin
      Canvas.Font.Name := Screen.Fonts[i];
      if Canvas.TextWidth('i') = Canvas.TextWidth('M') then
        FontsComboBox.Items.Add(Screen.Fonts[i]);
    end;
  finally
    Free();
  end;

  FontsComboBox.Items.EndUpdate();

  FEditor := TSimbaEditor.Create(Self);
  with FEditor do
  begin
    Parent := EditorPanel;
    Align := alClient;
    BorderStyle := bsNone;
    ReadOnly := True;
    Text := 'program Highlight;                                      '+ LineEnding +
            '{comment}                                               '+ LineEnding +
            '{$I SRL/osr.simba}                                      '+ LineEnding +
            '                                                        '+ LineEnding +
            '// this function does stuff                             '+ LineEnding +
            'procedure Test(var i: Int32);                           '+ LineEnding +
            'var                                                     '+ LineEnding +
            '  x: Int32;                                             '+ LineEnding +
            '  s: String;                                            '+ LineEnding +
            'begin                                                   '+ LineEnding +
            '  x := 1000 * (5 + 7);                                  '+ LineEnding +
            '  s := ' + #39 + 'The number is :' + #39 + ' +ToStr(x); '+ LineEnding +
            '                                                        '+ LineEnding +
            '  Inc(x);                                               '+ LineEnding +
            '  {$R+}                                                 '+ LineEnding +
            '  case x of                                             '+ LineEnding +
            '    1: ;                                                '+ LineEnding +
            '    2: ;                                                '+ LineEnding +
            '    3: ;                                                '+ LineEnding +
            '  end;                                                  '+ LineEnding +
            'end;                                                    '+ LineEnding +
            '                                                        '+ LineEnding +
            'function TPoint.Test: Boolean; overload;                '+ LineEnding +
            'begin                                                   '+ LineEnding +
            '  Result := inherited();                                '+ LineEnding +
            'end;                                                    ';
  end;
end;

initialization
  {$I simba.settingsform_editor_font.lrs}

end.

