unit simba.settingsform_editor_font;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Spin,
  simba.editor;

type
  TEditorFontFrame = class(TFrame)
    FontAntiAliasedCheckbox: TCheckBox;
    FontsComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    EditorPanel: TPanel;
    FontSizeEdit: TSpinEdit;
    procedure FontAntiAliasedCheckboxChange(Sender: TObject);
    procedure FontsComboBoxChange(Sender: TObject);
    procedure FontSizeEditChange(Sender: TObject);
  protected
    FEditor: TSimbaEditor;
    FFonts: TStringList;

    procedure PopulateFonts;
    procedure ApplyFonts(Sender: TObject);
  public
    property Editor: TSimbaEditor read FEditor;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  LCLIntf, LCLType;

function EnumFontsFixedPitchNoDups(var LogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx; FontType: LongInt; Data: LParam): LongInt; stdcall;
var
  S: String;
begin
  S := LogFont.elfLogFont.lfFaceName;
  if ((LogFont.elfLogFont.lfPitchAndFamily and FIXED_PITCH) = FIXED_PITCH) then
    TStringList(PtrUInt(Data)).Add(S);

  Result := 1;
end;

procedure TEditorFontFrame.FontAntiAliasedCheckboxChange(Sender: TObject);
begin
  FEditor.AntiAliasing := FontAntiAliasedCheckbox.Checked;
end;

procedure TEditorFontFrame.FontsComboBoxChange(Sender: TObject);
begin
  FEditor.Font.Name := FontsComboBox.Text;
end;

procedure TEditorFontFrame.FontSizeEditChange(Sender: TObject);
begin
  FEditor.Font.Size := FontSizeEdit.Value;
end;

procedure TEditorFontFrame.PopulateFonts;
var
  DC: HDC;
  LogFont: TLogFont;
begin
  LogFont.lfCharSet := DEFAULT_CHARSET;
  LogFont.lfFaceName := '';
  LogFont.lfPitchAndFamily := {$IFDEF LINUX}FIXED_PITCH{$ELSE}0{$ENDIF};

  DC := GetDC(0);

  try
    EnumFontFamiliesEx(DC, @LogFont, @EnumFontsFixedPitchNoDups, PtrUInt(FFonts), 0);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TEditorFontFrame.ApplyFonts(Sender: TObject);
begin
  FontsComboBox.Items.Assign(FFonts);
  FontsComboBox.Text := GetFontData(FEditor.Font.Handle).Name;
end;

constructor TEditorFontFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFonts := TStringList.Create();
  FFonts.Sorted := True;
  FFonts.Duplicates := dupIgnore;

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
            '(* this is an object method *)                          '+ LineEnding +
            'function TPoint.Test: Boolean; overload;                '+ LineEnding +
            'begin                                                   '+ LineEnding +
            'end;                                                    ';
  end;

  TThread.ExecuteInThread(@PopulateFonts, @ApplyFonts);
end;

destructor TEditorFontFrame.Destroy;
begin
  FFonts.Free();

  inherited Destroy();
end;

initialization
  {$I simba.settingsform_editor_font.lrs}

end.

