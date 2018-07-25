unit colorscheme;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, SynEdit, SynEditTypes, ColorBox, Buttons,
  newsimbasettings;

type
  EEditorAttr = (
    ecGutter, ecGutterLineNumberPart, ecGutterChangesPart, ecGutterCodeFoldPart,
    ecGutterMarksPart,  ecGutterSeparatorPart,

    ecEdtior, ecRightEdge, ecLineHighlight, ecFoldedCode, ecFoldedCodeLine, ecBracketMatch,

    ecAttrAsm, ecAttrCase, ecAttrComment, ecAttrPreProc, ecAttrPreProc2, ecAttrIdent,
    ecAttrNumber, ecAttrReserved, ecAttrSpacing, ecAttrString, ecAttrOperator,

    ecCaretMark
  );

  TFrameStyle = record
    Color: TColor;
    Style: TSynLineStyle;
    Edges: TSynFrameEdges;
  end;

  TStyleAndColor = record
    Foreground: TColor;
    Background: TColor;
    Style: TFontStyles;
    Frame: TFrameStyle;
  end;

  TColorScheme = array [EEditorAttr] of TStyleAndColor;

  TSimbaColors = class(TForm)
  public
    FileName: String;
    Edited: Boolean;
    SimbaSettings: TSimbaSettings;

    List: TListBox;
    Splitter: TSplitter;
    Editor: TSynEdit;
    TopPanel, BtmPanel: TPanel;

    LabelFG, LabelBG: TLabel;
    SelectFG, SelectBG: TColorBox;
    CustomFGbtn, CustomBGbtn: TSpeedButton;
    CustomFG, CustomBG: TColorDialog;

    CheckBold, CheckItalic, CheckUnderline: TCheckBox;

    ApplyBtn, SaveBtn, LoadBtn: TButton;

    Scheme, DefaultScheme: TColorScheme;

    constructor Create(TheOwner: TComponent); override;
    procedure LoadDefaults();
    procedure UpdateEditor(SynEdit: TSynEdit);
    function LoadScheme(AFileName: String): TColorScheme;

    procedure OnSelectionChange(Sender: TObject; User: Boolean);
    procedure OnSelectBG(Sender: TObject);
    procedure OnSelectFG(Sender: TObject);

    procedure OnBoldChange(Sender: TObject);
    procedure OnItalicChange(Sender: TObject);
    procedure OnUnderlineChange(Sender: TObject);

    procedure OnApply(Sender: TObject);
    procedure OnSave(Sender: TObject);
    procedure OnLoad(Sender: TObject);
  end;

var
  SimbaColors: TSimbaColors;

implementation

uses
  SynHighlighterLape, LResources, SimbaUnit, IniFiles, FileUtil;

resourcestring
  ExampleText =
    'program Highlight;                              '+ LineEnding +
    '{comment}                                       '+ LineEnding +
    '{$I SRL/osr.simba}                              '+ LineEnding +
    '                                                '+ LineEnding +
    '// this function does stuff                     '+ LineEnding +
    'procedure Test();                               '+ LineEnding +
    'var                                             '+ LineEnding +
    '  x: Int32;                                     '+ LineEnding +
    '  s: String;                                    '+ LineEnding +
    'begin                                           '+ LineEnding +
    '  x := 1000 * (5 + 7);                          '+ LineEnding +
    '  s := '+#39+'The number is :'+#39+' +ToStr(x); '+ LineEnding +
    '  asm                                           '+ LineEnding +
    '    mov AX 1234h;                               '+ LineEnding +
    '    mov Number, AX;                             '+ LineEnding +
    '  end;                                          '+ LineEnding +
    '                                                '+ LineEnding +
    '  Inc(x);                                       '+ LineEnding +
    '  {$R+}                                         '+ LineEnding +
    '  case x of                                     '+ LineEnding +
    '    1: ;                                        '+ LineEnding +
    '    2: ;                                        '+ LineEnding +
    '    3: ;                                        '+ LineEnding +
    '  end;                                          '+ LineEnding +
    'end;                                            ';



// -----------------------------------------------------------------------------

procedure TSimbaColors.LoadDefaults();
var i: Int32;
begin
  DefaultScheme[ecGutter].Background := Editor.Gutter.Color;

  DefaultScheme[ecGutterLineNumberPart].Foreground := Editor.Gutter.LineNumberPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterLineNumberPart].Background := Editor.Gutter.LineNumberPart.MarkupInfo.Background;
  DefaultScheme[ecGutterLineNumberPart].Style      := Editor.Gutter.LineNumberPart.MarkupInfo.Style;

  DefaultScheme[ecGutterChangesPart].Foreground := Editor.Gutter.ChangesPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterChangesPart].Background := Editor.Gutter.ChangesPart.MarkupInfo.Background;
  DefaultScheme[ecGutterChangesPart].Style      := Editor.Gutter.ChangesPart.MarkupInfo.Style;

  DefaultScheme[ecGutterCodeFoldPart].Foreground := Editor.Gutter.CodeFoldPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterCodeFoldPart].Background := Editor.Gutter.CodeFoldPart.MarkupInfo.Background;
  DefaultScheme[ecGutterCodeFoldPart].Style      := Editor.Gutter.CodeFoldPart.MarkupInfo.Style;

  DefaultScheme[ecGutterMarksPart].Foreground := Editor.Gutter.MarksPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterMarksPart].Background := Editor.Gutter.MarksPart.MarkupInfo.Background;
  DefaultScheme[ecGutterMarksPart].Style      := Editor.Gutter.MarksPart.MarkupInfo.Style;

  DefaultScheme[ecGutterMarksPart].Foreground := Editor.Gutter.SeparatorPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterMarksPart].Background := Editor.Gutter.SeparatorPart.MarkupInfo.Background;
  DefaultScheme[ecGutterMarksPart].Style      := Editor.Gutter.SeparatorPart.MarkupInfo.Style;

  // general editor
  DefaultScheme[ecEdtior].Background    := Editor.Color;
  DefaultScheme[ecRightEdge].Foreground := Editor.RightEdgeColor;

  DefaultScheme[ecLineHighlight].Foreground := Editor.LineHighlightColor.Foreground;
  DefaultScheme[ecLineHighlight].Background := Editor.LineHighlightColor.Background;
  DefaultScheme[ecLineHighlight].Style      := Editor.LineHighlightColor.Style;

  DefaultScheme[ecFoldedCode].Foreground := Editor.FoldedCodeColor.Foreground;
  DefaultScheme[ecFoldedCode].Background := Editor.FoldedCodeColor.Background;
  DefaultScheme[ecFoldedCode].Style      := Editor.FoldedCodeColor.Style;

  DefaultScheme[ecFoldedCodeLine].Foreground := Editor.FoldedCodeLineColor.Foreground;
  DefaultScheme[ecFoldedCodeLine].Background := Editor.FoldedCodeLineColor.Background;
  DefaultScheme[ecFoldedCodeLine].Style      := Editor.FoldedCodeLineColor.Style;

  DefaultScheme[ecBracketMatch].Foreground := Editor.BracketMatchColor.Foreground;
  DefaultScheme[ecBracketMatch].Background := Editor.BracketMatchColor.Background;
  DefaultScheme[ecBracketMatch].Style      := Editor.BracketMatchColor.Style;

  // attributes
  for i:=0 to 10 do
  begin
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground  := Editor.Highlighter.Attribute[i].Foreground;
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background  := Editor.Highlighter.Attribute[i].Background;
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style       := Editor.Highlighter.Attribute[i].Style;
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges := Editor.Highlighter.Attribute[i].FrameEdges;
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color := Editor.Highlighter.Attribute[i].FrameColor;
    DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style := Editor.Highlighter.Attribute[i].FrameStyle;
  end;

  // word match highlight
  //DefaultScheme[ecCaretMark].DefaultForeground := MarkCaret.Foreground;
  //DefaultScheme[ecCaretMark].DefaultBackground := MarkCaret.Background;
  //DefaultScheme[ecCaretMark].DefaultStyle      := MarkCaret.Style;

  Scheme := DefaultScheme;
end;

procedure TSimbaColors.UpdateEditor(SynEdit: TSynEdit);
var i: Int32;
begin
  // gutter
  SynEdit.Gutter.Color := Scheme[ecGutter].Background;

  SynEdit.Gutter.LineNumberPart.MarkupInfo.Foreground := Scheme[ecGutterLineNumberPart].Foreground;
  SynEdit.Gutter.LineNumberPart.MarkupInfo.Background := Scheme[ecGutterLineNumberPart].Background;
  SynEdit.Gutter.LineNumberPart.MarkupInfo.Style      := Scheme[ecGutterLineNumberPart].Style;

  SynEdit.Gutter.ChangesPart.MarkupInfo.Foreground := Scheme[ecGutterChangesPart].Foreground;
  SynEdit.Gutter.ChangesPart.MarkupInfo.Background := Scheme[ecGutterChangesPart].Background;
  SynEdit.Gutter.ChangesPart.MarkupInfo.Style      := Scheme[ecGutterChangesPart].Style;

  SynEdit.Gutter.CodeFoldPart.MarkupInfo.Foreground := Scheme[ecGutterCodeFoldPart].Foreground;
  SynEdit.Gutter.CodeFoldPart.MarkupInfo.Background := Scheme[ecGutterCodeFoldPart].Background;
  SynEdit.Gutter.CodeFoldPart.MarkupInfo.Style      := Scheme[ecGutterCodeFoldPart].Style;

  SynEdit.Gutter.MarksPart.MarkupInfo.Foreground := Scheme[ecGutterMarksPart].Foreground;
  SynEdit.Gutter.MarksPart.MarkupInfo.Background := Scheme[ecGutterMarksPart].Background;
  SynEdit.Gutter.MarksPart.MarkupInfo.Style      := Scheme[ecGutterMarksPart].Style;

  SynEdit.Gutter.SeparatorPart.MarkupInfo.Foreground := Scheme[ecGutterMarksPart].Foreground;
  SynEdit.Gutter.SeparatorPart.MarkupInfo.Background := Scheme[ecGutterMarksPart].Background;
  SynEdit.Gutter.SeparatorPart.MarkupInfo.Style      := Scheme[ecGutterMarksPart].Style;

  // general editor
  SynEdit.Color := Scheme[ecEdtior].Background;
  SynEdit.RightEdgeColor := Scheme[ecRightEdge].Foreground;

  SynEdit.LineHighlightColor.Foreground := Scheme[ecLineHighlight].Foreground;
  SynEdit.LineHighlightColor.Background := Scheme[ecLineHighlight].Background;
  SynEdit.LineHighlightColor.Style      := Scheme[ecLineHighlight].Style;

  SynEdit.FoldedCodeColor.Foreground := Scheme[ecFoldedCode].Foreground;
  SynEdit.FoldedCodeColor.Background := Scheme[ecFoldedCode].Background;
  SynEdit.FoldedCodeColor.Style      := Scheme[ecFoldedCode].Style;

  SynEdit.FoldedCodeLineColor.Foreground := Scheme[ecFoldedCodeLine].Foreground;
  SynEdit.FoldedCodeLineColor.Background := Scheme[ecFoldedCodeLine].Background;
  SynEdit.FoldedCodeLineColor.Style      := Scheme[ecFoldedCodeLine].Style;

  SynEdit.BracketMatchColor.Foreground := Scheme[ecBracketMatch].Foreground;
  SynEdit.BracketMatchColor.Background := Scheme[ecBracketMatch].Background;
  SynEdit.BracketMatchColor.Style      := Scheme[ecBracketMatch].Style;

  // attributes
  for i:=0 to 10 do
  begin
    SynEdit.Highlighter.Attribute[i].Foreground := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground;
    SynEdit.Highlighter.Attribute[i].Background := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background;
    SynEdit.Highlighter.Attribute[i].Style      := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style;
    SynEdit.Highlighter.Attribute[i].FrameEdges := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges;
    SynEdit.Highlighter.Attribute[i].FrameColor := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color;
    SynEdit.Highlighter.Attribute[i].FrameStyle := Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style;
  end;

  // word match highlight
  //MarkCaret.Foreground := Scheme[ecCaretMark].Foreground;
  //MarkCaret.Background := Scheme[ecCaretMark].Background;
  //MarkCaret.Style      := Scheme[ecCaretMark].Style;
  //MarkCaret.FrameEdges := Scheme[ecCaretMark].Frame.Edges;
  //MarkCaret.FrameColor := Scheme[ecCaretMark].Frame.Color;
  //MarkCaret.FrameStyle := Scheme[ecCaretMark].Frame.Style;

  Self.Edited := True;
end;

function TSimbaColors.LoadScheme(AFileName: String): TColorScheme;
var
  INI: TINIFile;
  i: Int32;
  x: String;
begin
  INI := TINIFile.Create(AFileName);

  Result := DefaultScheme;
  try
    UInt32(Result[ecGutter].Background) := INI.ReadInt64('Gutter', 'ecGutter.Background', UInt32(DefaultScheme[ecGutter].Background));

    UInt32(Result[ecGutterLineNumberPart].Foreground) := INI.ReadInt64('Gutter', 'ecGutterLineNumberPart.Foreground', UInt32(DefaultScheme[ecGutterLineNumberPart].Foreground));
    UInt32(Result[ecGutterLineNumberPart].Background) := INI.ReadInt64('Gutter', 'ecGutterLineNumberPart.Background', UInt32(DefaultScheme[ecGutterLineNumberPart].Background));
    UInt32(Result[ecGutterLineNumberPart].Style)      := INI.ReadInt64('Gutter', 'ecGutterLineNumberPart.Style',      UInt32(DefaultScheme[ecGutterLineNumberPart].Style));

    UInt32(Result[ecGutterChangesPart].Foreground) := INI.ReadInt64('Gutter', 'ecGutterChangesPart.Foreground', UInt32(DefaultScheme[ecGutterChangesPart].Foreground));
    UInt32(Result[ecGutterChangesPart].Background) := INI.ReadInt64('Gutter', 'ecGutterChangesPart.Background', UInt32(DefaultScheme[ecGutterChangesPart].Background));
    UInt32(Result[ecGutterChangesPart].Style)      := INI.ReadInt64('Gutter', 'ecGutterChangesPart.Style',      UInt32(DefaultScheme[ecGutterChangesPart].Style));

    UInt32(Result[ecGutterCodeFoldPart].Foreground) := INI.ReadInt64('Gutter', 'ecGutterCodeFoldPart.Foreground', UInt32(DefaultScheme[ecGutterCodeFoldPart].Foreground));
    UInt32(Result[ecGutterCodeFoldPart].Background) := INI.ReadInt64('Gutter', 'ecGutterCodeFoldPart.Background', UInt32(DefaultScheme[ecGutterCodeFoldPart].Background));
    UInt32(Result[ecGutterCodeFoldPart].Style)      := INI.ReadInt64('Gutter', 'ecGutterCodeFoldPart.Style',      UInt32(DefaultScheme[ecGutterCodeFoldPart].Style));

    UInt32(Result[ecGutterMarksPart].Foreground) := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Foreground', UInt32(DefaultScheme[ecGutterMarksPart].Foreground));
    UInt32(Result[ecGutterMarksPart].Background) := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Background', UInt32(DefaultScheme[ecGutterMarksPart].Background));
    UInt32(Result[ecGutterMarksPart].Style)      := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Style',      UInt32(DefaultScheme[ecGutterMarksPart].Style));

    UInt32(Result[ecGutterMarksPart].Foreground) := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Foreground', UInt32(DefaultScheme[ecGutterMarksPart].Foreground));
    UInt32(Result[ecGutterMarksPart].Background) := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Background', UInt32(DefaultScheme[ecGutterMarksPart].Background));
    UInt32(Result[ecGutterMarksPart].Style)      := INI.ReadInt64('Gutter', 'ecGutterMarksPart.Style',      UInt32(DefaultScheme[ecGutterMarksPart].Style));

    // general editor
    UInt32(Result[ecEdtior].Background) := INI.ReadInt64('Editor', 'ecEdtior.Background',    UInt32(DefaultScheme[ecEdtior].Background));

    UInt32(Result[ecRightEdge].Foreground) := INI.ReadInt64('Editor', 'ecRightEdge.Foreground', UInt32(DefaultScheme[ecRightEdge].Foreground));

    UInt32(Result[ecLineHighlight].Foreground) := INI.ReadInt64('Editor', 'ecLineHighlight.Foreground', UInt32(DefaultScheme[ecLineHighlight].Foreground));
    UInt32(Result[ecLineHighlight].Background) := INI.ReadInt64('Editor', 'ecLineHighlight.Background', UInt32(DefaultScheme[ecLineHighlight].Background));
    UInt32(Result[ecLineHighlight].Style)      := INI.ReadInt64('Editor', 'ecLineHighlight.Style',      UInt32(DefaultScheme[ecLineHighlight].Style));

    UInt32(Result[ecFoldedCode].Foreground) := INI.ReadInt64('Editor', 'ecFoldedCode.Foreground', UInt32(DefaultScheme[ecFoldedCode].Foreground));
    UInt32(Result[ecFoldedCode].Background) := INI.ReadInt64('Editor', 'ecFoldedCode.Background', UInt32(DefaultScheme[ecFoldedCode].Background));
    UInt32(Result[ecFoldedCode].Style)      := INI.ReadInt64('Editor', 'ecFoldedCode.Style',      UInt32(DefaultScheme[ecFoldedCode].Style));

    UInt32(Result[ecFoldedCodeLine].Foreground) := INI.ReadInt64('Editor', 'ecFoldedCodeLine.Foreground', UInt32(DefaultScheme[ecFoldedCodeLine].Foreground));
    UInt32(Result[ecFoldedCodeLine].Background) := INI.ReadInt64('Editor', 'ecFoldedCodeLine.Background', UInt32(DefaultScheme[ecFoldedCodeLine].Background));
    UInt32(Result[ecFoldedCodeLine].Style)      := INI.ReadInt64('Editor', 'ecFoldedCodeLine.Style',      UInt32(DefaultScheme[ecFoldedCodeLine].Style));

    UInt32(Result[ecBracketMatch].Foreground) := INI.ReadInt64('Editor', 'ecBracketMatch.Foreground', UInt32(DefaultScheme[ecBracketMatch].Foreground));
    UInt32(Result[ecBracketMatch].Background) := INI.ReadInt64('Editor', 'ecBracketMatch.Background', UInt32(DefaultScheme[ecBracketMatch].Background));
    UInt32(Result[ecBracketMatch].Style)      := INI.ReadInt64('Editor', 'ecBracketMatch.Style',      UInt32(DefaultScheme[ecBracketMatch].Style));

    for i:=0 to 10 do
    begin
      WriteStr(x, EEditorAttr(Ord(ecAttrAsm)+i));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground)  := INI.ReadInt64('Highlighter', x+'.Foreground', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Background)  := INI.ReadInt64('Highlighter', x+'.Background', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Style)       := INI.ReadInt64('Highlighter', x+'.Style',      UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges) := INI.ReadInt64('Highlighter', x+'.FrameEdges', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color) := INI.ReadInt64('Highlighter', x+'.FrameColor', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style) := INI.ReadInt64('Highlighter', x+'.FrameStyle', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style));
    end;

  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    INI.Free;
  end;
end;


// -----------------------------------------------------------------------------
// callbacks

procedure TSimbaColors.OnSelectionChange(Sender: TObject; User: boolean);
var
  currSelection,i: Int32;
begin
  currSelection := List.ItemIndex;
  if (SelectFG = nil) or (SelectBG = nil) then
    Exit;

  with SelectFG do
  begin
    i := Items.IndexOfObject(TObject(PtrInt(Scheme[EEditorAttr(currSelection)].Foreground)));
    if Scheme[EEditorAttr(currSelection)].Foreground = DefaultScheme[EEditorAttr(currSelection)].Foreground then
    begin
      ItemIndex := Items.Count-1;
      Items.Objects[ItemIndex] := TObject(PtrInt(DefaultScheme[EEditorAttr(currSelection)].Foreground));
    end else if i = -1 then
      ItemIndex := 0
    else
      ItemIndex := i;
  end;

  with SelectBG do
  begin
    i := Items.IndexOfObject(TObject(PtrInt(Scheme[EEditorAttr(currSelection)].Background)));
    if Scheme[EEditorAttr(currSelection)].Background = DefaultScheme[EEditorAttr(currSelection)].Background then
    begin
      ItemIndex := Items.Count-1;
      Items.Objects[ItemIndex] := TObject(PtrInt(DefaultScheme[EEditorAttr(currSelection)].Background));
    end else if i = -1 then
      ItemIndex := 0
    else
      ItemIndex := i;
  end;

  CheckBold.Checked      := fsBold      in Scheme[EEditorAttr(currSelection)].Style;
  CheckItalic.Checked    := fsItalic    in Scheme[EEditorAttr(currSelection)].Style;
  CheckUnderline.Checked := fsUnderline in Scheme[EEditorAttr(currSelection)].Style;

  SelectFG.Repaint();
  SelectBG.Repaint();
end;

procedure TSimbaColors.OnSelectFG(Sender: TObject);
begin
  if SelectFG.ItemIndex = SelectFG.Items.Count-1 then
    Scheme[EEditorAttr(List.ItemIndex)].Foreground := DefaultScheme[EEditorAttr(List.ItemIndex)].Foreground
  else
    Scheme[EEditorAttr(List.ItemIndex)].Foreground := SelectFG.Selected;
  UpdateEditor(Self.Editor);
end;

procedure TSimbaColors.OnSelectBG(Sender: TObject);
begin
  if SelectBG.ItemIndex = SelectBG.Items.Count-1 then
    Scheme[EEditorAttr(List.ItemIndex)].Background := DefaultScheme[EEditorAttr(List.ItemIndex)].Background
  else
    Scheme[EEditorAttr(List.ItemIndex)].Background := SelectBG.Selected;
  UpdateEditor(Self.Editor);
end;

procedure TSimbaColors.OnBoldChange(Sender: TObject);
begin
  with Scheme[EEditorAttr(List.ItemIndex)] do
    if CheckBold.Checked then Style := Style + [fsBold]
    else                      Style := Style - [fsBold];
  UpdateEditor(Self.Editor);
end;

procedure TSimbaColors.OnItalicChange(Sender: TObject);
begin
  with Scheme[EEditorAttr(List.ItemIndex)] do
    if CheckItalic.Checked then Style := Style + [fsItalic]
    else                        Style := Style - [fsItalic];
  UpdateEditor(Self.Editor);
end;

procedure TSimbaColors.OnUnderlineChange(Sender: TObject);
begin
  with Scheme[EEditorAttr(List.ItemIndex)] do
    if CheckUnderline.Checked then Style := Style + [fsUnderline]
    else                           Style := Style - [fsUnderline];
  UpdateEditor(Self.Editor);
end;

procedure TSimbaColors.OnApply(Sender: TObject);
var i : Int32;
begin
  for i := 0 to SimbaForm.Tabs.Count - 1 do
    UpdateEditor(TMufasaTab(SimbaForm.Tabs[i]).ScriptFrame.SynEdit);

  SimbaSettings.SourceEditor.HighlighterPath.Value := Self.FileName;
end;

procedure TSimbaColors.OnSave(Sender: TObject);
var
  INI: TINIFile;
  UI: TSaveDialog;
  i: Int32;
  x: String;
begin
  UI := TSaveDialog.Create(Self);
  UI.FileName:= ExtractFileName(Self.FileName);
  UI.DefaultExt := 'ini';
  UI.Filter := 'Text file|*.ini';
  UI.InitialDir := GetCurrentDir;
  UI.Execute();

  INI := TINIFile.Create(UI.FileName);

  try
    INI.WriteInt64('Gutter', 'ecGutter.Background', UInt32(Scheme[ecGutter].Background));

    INI.WriteInt64('Gutter', 'ecGutterLineNumberPart.Foreground', UInt32(Scheme[ecGutterLineNumberPart].Foreground));
    INI.WriteInt64('Gutter', 'ecGutterLineNumberPart.Background', UInt32(Scheme[ecGutterLineNumberPart].Background));
    INI.WriteInt64('Gutter', 'ecGutterLineNumberPart.Style',      UInt32(Scheme[ecGutterLineNumberPart].Style));

    INI.WriteInt64('Gutter', 'ecGutterChangesPart.Foreground', UInt32(Scheme[ecGutterChangesPart].Foreground));
    INI.WriteInt64('Gutter', 'ecGutterChangesPart.Background', UInt32(Scheme[ecGutterChangesPart].Background));
    INI.WriteInt64('Gutter', 'ecGutterChangesPart.Style',      UInt32(Scheme[ecGutterChangesPart].Style));

    INI.WriteInt64('Gutter', 'ecGutterCodeFoldPart.Foreground', UInt32(Scheme[ecGutterCodeFoldPart].Foreground));
    INI.WriteInt64('Gutter', 'ecGutterCodeFoldPart.Background', UInt32(Scheme[ecGutterCodeFoldPart].Background));
    INI.WriteInt64('Gutter', 'ecGutterCodeFoldPart.Style',      UInt32(Scheme[ecGutterCodeFoldPart].Style));

    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Foreground', UInt32(Scheme[ecGutterMarksPart].Foreground));
    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Background', UInt32(Scheme[ecGutterMarksPart].Background));
    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Style',      UInt32(Scheme[ecGutterMarksPart].Style));

    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Foreground', UInt32(Scheme[ecGutterMarksPart].Foreground));
    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Background', UInt32(Scheme[ecGutterMarksPart].Background));
    INI.WriteInt64('Gutter', 'ecGutterMarksPart.Style',      UInt32(Scheme[ecGutterMarksPart].Style));

    // general editor
    INI.WriteInt64('Editor', 'ecEdtior.Background',    UInt32(Scheme[ecEdtior].Background));
    INI.WriteInt64('Editor', 'ecRightEdge.Foreground', UInt32(Scheme[ecRightEdge].Foreground));

    INI.WriteInt64('Editor', 'ecLineHighlight.Foreground', UInt32(Scheme[ecLineHighlight].Foreground));
    INI.WriteInt64('Editor', 'ecLineHighlight.Background', UInt32(Scheme[ecLineHighlight].Background));
    INI.WriteInt64('Editor', 'ecLineHighlight.Style',      UInt32(Scheme[ecLineHighlight].Style));

    INI.WriteInt64('Editor', 'ecFoldedCode.Foreground', UInt32(Scheme[ecFoldedCode].Foreground));
    INI.WriteInt64('Editor', 'ecFoldedCode.Background', UInt32(Scheme[ecFoldedCode].Background));
    INI.WriteInt64('Editor', 'ecFoldedCode.Style',      UInt32(Scheme[ecFoldedCode].Style));

    INI.WriteInt64('Editor', 'ecFoldedCodeLine.Foreground', UInt32(Scheme[ecFoldedCodeLine].Foreground));
    INI.WriteInt64('Editor', 'ecFoldedCodeLine.Background', UInt32(Scheme[ecFoldedCodeLine].Background));
    INI.WriteInt64('Editor', 'ecFoldedCodeLine.Style',      UInt32(Scheme[ecFoldedCodeLine].Style));

    INI.WriteInt64('Editor', 'ecBracketMatch.Foreground', UInt32(Scheme[ecBracketMatch].Foreground));
    INI.WriteInt64('Editor', 'ecBracketMatch.Background', UInt32(Scheme[ecBracketMatch].Background));
    INI.WriteInt64('Editor', 'ecBracketMatch.Style',      UInt32(Scheme[ecBracketMatch].Style));

    for i:=0 to 10 do
    begin
      WriteStr(x, EEditorAttr(Ord(ecAttrAsm)+i));
      INI.WriteInt64('Highlighter', x+'.Foreground', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground));
      INI.WriteInt64('Highlighter', x+'.Background', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background));
      INI.WriteInt64('Highlighter', x+'.Style',      UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style));
      INI.WriteInt64('Highlighter', x+'.FrameEdges', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges));
      INI.WriteInt64('Highlighter', x+'.FrameColor', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color));
      INI.WriteInt64('Highlighter', x+'.FrameStyle', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style));
    end;

  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    INI.Free;

    Self.FileName := UI.FileName;
    Self.Edited   := False;
  end;
end;

procedure TSimbaColors.OnLoad(Sender: TObject);
var
  UI: TOpenDialog;
begin
  UI := TOpenDialog.Create(Self);
  UI.DefaultExt := 'ini';
  UI.Filter := 'Text file|*.ini';
  UI.InitialDir := GetCurrentDir;
  UI.Execute();

  Self.Scheme   := Self.LoadScheme(UI.FileName);
  Self.FileName := UI.FileName;
  Self.Edited   := False;

  UpdateEditor(Self.Editor);
end;

constructor TSimbaColors.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Simba colors';
  SimbaSettings := newsimbasettings.SimbaSettings;
  Width  := 800;
  Height := 600;
  Position := poMainFormCenter;
  BorderStyle := bsSingle;
  Edited := True;
  FileName := '';

// Top portion
  TopPanel := TPanel.Create(Self);
  with TopPanel do
  begin
    Parent := Self;
    Width  := Self.Width;
    Height := Round(Self.Height * 0.75);
    Align  := alTop;
  end;

  Editor := TSynEdit.Create(TopPanel);
  with Editor do
  begin
    Parent := TopPanel;
    Align := alRight;
    Width := Round(Self.Width * 0.8);
    Font.Assign(SimbaSettings.SourceEditor.Font.Value);
    Highlighter := TSynFreePascalSyn.Create(Editor);
    Editor.Text := ExampleText;

    Highlighter.CommentAttribute;
    with TSynPasSyn(Highlighter) do
    begin
      CommentAttri.Foreground := clBlue;
      CommentAttri.Style := [fsBold];
      IdentifierAttri.Foreground := clDefault;
      NumberAttri.Foreground := clNavy;
      StringAttri.Foreground := clBlue;
      SymbolAttri.Foreground := clRed;
      DirectiveAttri.Foreground := clRed;
      DirectiveAttri.Style := [fsBold];
      NestedComments := False;
      StringKeywordMode := spsmNone;
    end;
  end;

  Splitter := TSplitter.Create(TopPanel);
  with Splitter do
  begin
    Parent := TopPanel;
    Align := alLeft;
  end;

  List := TListBox.Create(TopPanel);
  List.OnSelectionChange := @Self.OnSelectionChange;
  with List do
  begin
    Parent := TopPanel;
    Align := alLeft;
    Width := Round(Self.Width * 0.19);

    Items.Add('Gutter');
    Items.Add('   Linenumber');
    Items.Add('   Changes');
    Items.Add('   CodeFold');
    Items.Add('   Marks');
    Items.Add('   Separator');

    Items.Add('Editor');
    Items.Add('   Right Edge');
    Items.Add('   LineHighlight');
    Items.Add('   Folded Code');
    Items.Add('   Folded Code Line');
    Items.Add('   Backet Match');

    Items.Add('Assembler');
    Items.Add('Case label');
    Items.Add('Comment');
    Items.Add('Pre processor');
    Items.Add('IDE directive');
    Items.Add('Identifiers');
    Items.Add('Numbers');
    Items.Add('Reserved word');
    Items.Add('Spaces');
    Items.Add('Strings');
    Items.Add('Operator');

    List.ItemIndex := 0;
  end;

// The bottom portion:
  BtmPanel := TPanel.Create(Self);
  with BtmPanel do
  begin
    Parent := Self;
    Width  := Self.Width;
    align  := alBottom;
    Height := Round(Self.Height * 0.25);
    BevelColor:=clLtGray;
    BevelWidth:=4;
  end;

// Colorlists
  LabelFG := TLabel.Create(BtmPanel);
  LabelBG := TLabel.Create(BtmPanel);

  with LabelFG do
  begin
    Parent  := BtmPanel;
    Caption := 'Foreground';
    Left := 10;
    Top  := 12;
  end;

  with LabelBG do
  begin
    Parent  := BtmPanel;
    Caption := 'Background';
    Left := 10;
    Top  := 40;
  end;

  SelectFG := TColorBox.Create(BtmPanel);
  SelectBG := TColorBox.Create(BtmPanel);

  with SelectFG do
  begin
    Parent   := BtmPanel;
    OnChange := @Self.OnSelectFG;
    Style    := [cbStandardColors,cbExtendedColors,cbSystemColors,cbCustomColor,cbPrettyNames,cbCustomColors];
    Left  := 100;
    Top   := 8;
    Width := 150;
    Items.Objects[Items.Add('Default')]   := TObject(PtrInt(-1));
  end;

  with SelectBG do
  begin
    Parent   := BtmPanel;
    OnChange := @Self.OnSelectBG;
    Style    := [cbStandardColors,cbExtendedColors,cbSystemColors,cbCustomColor,cbPrettyNames,cbCustomColors];
    Left  := 100;
    Top   := 36;
    Width := 150;
    Items.Objects[Items.Add('Default')] := TObject(PtrInt(-1));
  end;

// The checkboxes
  CheckBold      := TCheckBox.Create(BtmPanel);
  CheckItalic    := TCheckBox.Create(BtmPanel);
  CheckUnderline := TCheckBox.Create(BtmPanel);

  with CheckBold do
  begin
    Parent := BtmPanel;
    Left   := 300;
    Top    := 8;
    Caption:= 'Bold';
    OnChange := @Self.OnBoldChange;
  end;

  with CheckItalic do
  begin
    Parent := BtmPanel;
    Left   := 300;
    Top    := 36;
    Caption:= 'Italic';
    OnChange := @Self.OnItalicChange;
  end;

  with CheckUnderline do
  begin
    Parent := BtmPanel;
    Left   := 300;
    Top    := 64;
    Caption:= 'Underline';
    OnChange := @Self.OnUnderlineChange;
  end;


// save, load, apply
  ApplyBtn := TButton.Create(BtmPanel);
  with ApplyBtn do
  begin
    Parent := BtmPanel;
    Left   := 10;
    Top    := 115;
    Caption:= 'Apply';
    OnClick := @Self.OnApply;
  end;

  LoadBtn := TButton.Create(BtmPanel);
  with LoadBtn do
  begin
    Parent := BtmPanel;
    Left   := Self.Width - 170;
    Top    := 115;
    Caption:= 'Load';
    OnClick := @Self.OnLoad;
  end;

  SaveBtn := TButton.Create(BtmPanel);
  with SaveBtn do
  begin
    Parent := BtmPanel;
    Left   := Self.Width - 90;
    Top    := 115;
    Caption:= 'Save';
    OnClick := @Self.OnSave;
  end;

  LoadDefaults();
  Self.Scheme := LoadScheme(SimbaSettings.SourceEditor.HighlighterPath.Value);
  Self.UpdateEditor(Self.Editor);
end;

{$R *.lfm}

end.

