unit colorscheme;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, SynEdit, SynEditTypes, ColorBox, Buttons;

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

    List: TListBox;
    Editor: TSynEdit;
    TopPanel, BtmPanel: TPanel;

    LabelFG, LabelBG: TLabel;
    SelectFG, SelectBG: TColorBox;
    CustomFGbtn, CustomBGbtn: TSpeedButton;
    CustomFG, CustomBG: TColorDialog;

    CheckBold, CheckItalic, CheckUnderline, CheckStrike: TCheckBox;

    ApplyBtn, ApplyAndSaveBtn, SaveBtn, LoadBtn: TButton;

    Scheme, DefaultScheme: TColorScheme;

    constructor Create(TheOwner: TComponent); override;
    procedure LoadDefaults();
    procedure UpdateEditor(SynEdit: TSynEdit);
    function LoadScheme(AFileName: String): TColorScheme;
    procedure Save(AName: String);

    procedure OnSelectionChange(Sender: TObject; User: Boolean);
    procedure OnSelectBG(Sender: TObject);
    procedure OnSelectFG(Sender: TObject);

    procedure OnBoldChange(Sender: TObject);
    procedure OnItalicChange(Sender: TObject);
    procedure OnUnderlineChange(Sender: TObject);
    procedure OnStrikeChange(Sender: TObject);

    procedure OnApply(Sender: TObject);
    procedure OnApplyAndSave(Sender: TObject);
    procedure OnSave(Sender: TObject);
    procedure OnLoad(Sender: TObject);
  end;

var
  SimbaColorsForm: TSimbaColors;

implementation

uses
  SynHighlighterLape, LResources, SimbaUnit, JSONConf, FileUtil,
  simba.settings;

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

  DefaultScheme[ecGutterSeparatorPart].Foreground := Editor.Gutter.SeparatorPart.MarkupInfo.Foreground;
  DefaultScheme[ecGutterSeparatorPart].Background := Editor.Gutter.SeparatorPart.MarkupInfo.Background;
  DefaultScheme[ecGutterSeparatorPart].Style      := Editor.Gutter.SeparatorPart.MarkupInfo.Style;

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

  SynEdit.Gutter.SeparatorPart.MarkupInfo.Foreground := Scheme[ecGutterSeparatorPart].Foreground;
  SynEdit.Gutter.SeparatorPart.MarkupInfo.Background := Scheme[ecGutterSeparatorPart].Background;
  SynEdit.Gutter.SeparatorPart.MarkupInfo.Style      := Scheme[ecGutterSeparatorPart].Style;

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
  Config: TJSONConfig;
  i: Int32;
  x: String;
begin
  Config := TJSONConfig.Create(nil);
  Config.Formatted := True;

  try
    Config.FileName := AFileName;
  except
    DeleteFile(AFileName);
  end;

  Result := DefaultScheme;

  try
    UInt32(Result[ecGutter].Background) := Config.GetValue('Gutter/ecGutter.Background', UInt32(DefaultScheme[ecGutter].Background));

    UInt32(Result[ecGutterLineNumberPart].Foreground) := Config.GetValue('Gutter/ecGutterLineNumberPart.Foreground', UInt32(DefaultScheme[ecGutterLineNumberPart].Foreground));
    UInt32(Result[ecGutterLineNumberPart].Background) := Config.GetValue('Gutter/ecGutterLineNumberPart.Background', UInt32(DefaultScheme[ecGutterLineNumberPart].Background));
    UInt32(Result[ecGutterLineNumberPart].Style)      := Config.GetValue('Gutter/ecGutterLineNumberPart.Style',      UInt32(DefaultScheme[ecGutterLineNumberPart].Style));

    UInt32(Result[ecGutterChangesPart].Foreground) := Config.GetValue('Gutter/ecGutterChangesPart.Foreground', UInt32(DefaultScheme[ecGutterChangesPart].Foreground));
    UInt32(Result[ecGutterChangesPart].Background) := Config.GetValue('Gutter/ecGutterChangesPart.Background', UInt32(DefaultScheme[ecGutterChangesPart].Background));
    UInt32(Result[ecGutterChangesPart].Style)      := Config.GetValue('Gutter/ecGutterChangesPart.Style',      UInt32(DefaultScheme[ecGutterChangesPart].Style));

    UInt32(Result[ecGutterCodeFoldPart].Foreground) := Config.GetValue('Gutter/ecGutterCodeFoldPart.Foreground', UInt32(DefaultScheme[ecGutterCodeFoldPart].Foreground));
    UInt32(Result[ecGutterCodeFoldPart].Background) := Config.GetValue('Gutter/ecGutterCodeFoldPart.Background', UInt32(DefaultScheme[ecGutterCodeFoldPart].Background));
    UInt32(Result[ecGutterCodeFoldPart].Style)      := Config.GetValue('Gutter/ecGutterCodeFoldPart.Style',      UInt32(DefaultScheme[ecGutterCodeFoldPart].Style));

    UInt32(Result[ecGutterMarksPart].Foreground) := Config.GetValue('Gutter/ecGutterMarksPart.Foreground', UInt32(DefaultScheme[ecGutterMarksPart].Foreground));
    UInt32(Result[ecGutterMarksPart].Background) := Config.GetValue('Gutter/ecGutterMarksPart.Background', UInt32(DefaultScheme[ecGutterMarksPart].Background));
    UInt32(Result[ecGutterMarksPart].Style)      := Config.GetValue('Gutter/ecGutterMarksPart.Style',      UInt32(DefaultScheme[ecGutterMarksPart].Style));

    UInt32(Result[ecGutterSeparatorPart].Foreground) := Config.GetValue('Gutter/ecGutterSeparatorPart.Foreground', UInt32(DefaultScheme[ecGutterSeparatorPart].Foreground));
    UInt32(Result[ecGutterSeparatorPart].Background) := Config.GetValue('Gutter/ecGutterSeparatorPart.Background', UInt32(DefaultScheme[ecGutterSeparatorPart].Background));
    UInt32(Result[ecGutterSeparatorPart].Style)      := Config.GetValue('Gutter/ecGutterSeparatorPart.Style',      UInt32(DefaultScheme[ecGutterSeparatorPart].Style));

    // general editor
    UInt32(Result[ecEdtior].Background) := Config.GetValue('Editor/ecEdtior.Background', UInt32(DefaultScheme[ecEdtior].Background));

    UInt32(Result[ecRightEdge].Foreground) := Config.GetValue('Editor/ecRightEdge.Foreground', UInt32(DefaultScheme[ecRightEdge].Foreground));

    UInt32(Result[ecLineHighlight].Foreground) := Config.GetValue('Editor/ecLineHighlight.Foreground', UInt32(DefaultScheme[ecLineHighlight].Foreground));
    UInt32(Result[ecLineHighlight].Background) := Config.GetValue('Editor/ecLineHighlight.Background', UInt32(DefaultScheme[ecLineHighlight].Background));
    UInt32(Result[ecLineHighlight].Style)      := Config.GetValue('Editor/ecLineHighlight.Style',      UInt32(DefaultScheme[ecLineHighlight].Style));

    UInt32(Result[ecFoldedCode].Foreground) := Config.GetValue('Editor/ecFoldedCode.Foreground', UInt32(DefaultScheme[ecFoldedCode].Foreground));
    UInt32(Result[ecFoldedCode].Background) := Config.GetValue('Editor/ecFoldedCode.Background', UInt32(DefaultScheme[ecFoldedCode].Background));
    UInt32(Result[ecFoldedCode].Style)      := Config.GetValue('Editor/ecFoldedCode.Style',      UInt32(DefaultScheme[ecFoldedCode].Style));

    UInt32(Result[ecFoldedCodeLine].Foreground) := Config.GetValue('Editor/ecFoldedCodeLine.Foreground', UInt32(DefaultScheme[ecFoldedCodeLine].Foreground));
    UInt32(Result[ecFoldedCodeLine].Background) := Config.GetValue('Editor/ecFoldedCodeLine.Background', UInt32(DefaultScheme[ecFoldedCodeLine].Background));
    UInt32(Result[ecFoldedCodeLine].Style)      := Config.GetValue('Editor/ecFoldedCodeLine.Style',      UInt32(DefaultScheme[ecFoldedCodeLine].Style));

    UInt32(Result[ecBracketMatch].Foreground) := Config.GetValue('Editor/ecBracketMatch.Foreground', UInt32(DefaultScheme[ecBracketMatch].Foreground));
    UInt32(Result[ecBracketMatch].Background) := Config.GetValue('Editor/ecBracketMatch.Background', UInt32(DefaultScheme[ecBracketMatch].Background));
    UInt32(Result[ecBracketMatch].Style)      := Config.GetValue('Editor/ecBracketMatch.Style',      UInt32(DefaultScheme[ecBracketMatch].Style));

    for i:=0 to 10 do
    begin
      WriteStr(x, EEditorAttr(Ord(ecAttrAsm)+i));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground)  := Config.GetValue('Highlighter/' + x + '.Foreground', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Background)  := Config.GetValue('Highlighter/' + x + '.Background', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Style)       := Config.GetValue('Highlighter/' + x + '.Style',      UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges) := Config.GetValue('Highlighter/' + x + '.FrameEdges', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color) := Config.GetValue('Highlighter/' + x + '.FrameColor', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color));
      UInt32(Result[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style) := Config.GetValue('Highlighter/' + x + '.FrameStyle', UInt32(DefaultScheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style));
    end;

  finally
    Config.Flush();
    Config.Free();
  end;
end;


procedure TSimbaColors.Save(AName: String);
var
  Config: TJSONConfig;
  i: Int32;
  x: String;
begin
  Config := TJSONConfig.Create(nil);
  Config.Formatted := True;

  try
    Config.FileName := AName;
  except
    DeleteFile(AName);
  end;

  try
    Config.SetValue('Gutter/ecGutter.Background', UInt32(Scheme[ecGutter].Background));

    Config.SetValue('Gutter/ecGutterLineNumberPart.Foreground', UInt32(Scheme[ecGutterLineNumberPart].Foreground));
    Config.SetValue('Gutter/ecGutterLineNumberPart.Background', UInt32(Scheme[ecGutterLineNumberPart].Background));
    Config.SetValue('Gutter/ecGutterLineNumberPart.Style',      UInt32(Scheme[ecGutterLineNumberPart].Style));

    Config.SetValue('Gutter/ecGutterChangesPart.Foreground', UInt32(Scheme[ecGutterChangesPart].Foreground));
    Config.SetValue('Gutter/ecGutterChangesPart.Background', UInt32(Scheme[ecGutterChangesPart].Background));
    Config.SetValue('Gutter/ecGutterChangesPart.Style',      UInt32(Scheme[ecGutterChangesPart].Style));

    Config.SetValue('Gutter/ecGutterCodeFoldPart.Foreground', UInt32(Scheme[ecGutterCodeFoldPart].Foreground));
    Config.SetValue('Gutter/ecGutterCodeFoldPart.Background', UInt32(Scheme[ecGutterCodeFoldPart].Background));
    Config.SetValue('Gutter/ecGutterCodeFoldPart.Style',      UInt32(Scheme[ecGutterCodeFoldPart].Style));

    Config.SetValue('Gutter/ecGutterMarksPart.Foreground', UInt32(Scheme[ecGutterMarksPart].Foreground));
    Config.SetValue('Gutter/ecGutterMarksPart.Background', UInt32(Scheme[ecGutterMarksPart].Background));
    Config.SetValue('Gutter/ecGutterMarksPart.Style',      UInt32(Scheme[ecGutterMarksPart].Style));

    Config.SetValue('Gutter/ecGutterSeparatorPart.Foreground', UInt32(Scheme[ecGutterSeparatorPart].Foreground));
    Config.SetValue('Gutter/ecGutterSeparatorPart.Background', UInt32(Scheme[ecGutterSeparatorPart].Background));
    Config.SetValue('Gutter/ecGutterSeparatorPart.Style',      UInt32(Scheme[ecGutterSeparatorPart].Style));

    // general editor
    Config.SetValue('Editor/ecEdtior.Background',    UInt32(Scheme[ecEdtior].Background));
    Config.SetValue('Editor/ecRightEdge.Foreground', UInt32(Scheme[ecRightEdge].Foreground));

    Config.SetValue('Editor/ecLineHighlight.Foreground', UInt32(Scheme[ecLineHighlight].Foreground));
    Config.SetValue('Editor/ecLineHighlight.Background', UInt32(Scheme[ecLineHighlight].Background));
    Config.SetValue('Editor/ecLineHighlight.Style',      UInt32(Scheme[ecLineHighlight].Style));

    Config.SetValue('Editor/ecFoldedCode.Foreground', UInt32(Scheme[ecFoldedCode].Foreground));
    Config.SetValue('Editor/ecFoldedCode.Background', UInt32(Scheme[ecFoldedCode].Background));
    Config.SetValue('Editor/ecFoldedCode.Style',      UInt32(Scheme[ecFoldedCode].Style));

    Config.SetValue('Editor/ecFoldedCodeLine.Foreground', UInt32(Scheme[ecFoldedCodeLine].Foreground));
    Config.SetValue('Editor/ecFoldedCodeLine.Background', UInt32(Scheme[ecFoldedCodeLine].Background));
    Config.SetValue('Editor/ecFoldedCodeLine.Style',      UInt32(Scheme[ecFoldedCodeLine].Style));

    Config.SetValue('Editor/ecBracketMatch.Foreground', UInt32(Scheme[ecBracketMatch].Foreground));
    Config.SetValue('Editor/ecBracketMatch.Background', UInt32(Scheme[ecBracketMatch].Background));
    Config.SetValue('Editor/ecBracketMatch.Style',      UInt32(Scheme[ecBracketMatch].Style));

    for i:=0 to 10 do
    begin
      WriteStr(x, EEditorAttr(Ord(ecAttrAsm)+i));

      Config.SetValue('Highlighter/' + x + '.Foreground', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Foreground));
      Config.SetValue('Highlighter/' + x + '.Background', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Background));
      Config.SetValue('Highlighter/' + x + '.Style',      UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Style));
      Config.SetValue('Highlighter/' + x + '.FrameEdges', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Edges));
      Config.SetValue('Highlighter/' + x + '.FrameColor', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Color));
      Config.SetValue('Highlighter/' + x + '.FrameStyle', UInt32(Scheme[EEditorAttr(Ord(ecAttrAsm)+i)].Frame.Style));
    end;

  finally
    Config.Flush();
    Config.Free();

    Self.FileName := AName;
    Self.Edited   := False;
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
  CheckStrike.Checked    := fsStrikeOut in Scheme[EEditorAttr(currSelection)].Style;

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

procedure TSimbaColors.OnStrikeChange(Sender: TObject);
begin
  with Scheme[EEditorAttr(List.ItemIndex)] do
    if CheckStrike.Checked then Style := Style + [fsStrikeOut]
    else                        Style := Style - [fsStrikeOut];
  UpdateEditor(Self.Editor);
end;


procedure TSimbaColors.OnApply(Sender: TObject);
var i : Int32;
begin
  for i := 0 to SimbaForm.Tabs.Count - 1 do
    UpdateEditor(TMufasaTab(SimbaForm.Tabs[i]).ScriptFrame.SynEdit);

  if (not Self.Edited) then
    SimbaSettings.Editor.HighlighterPath.Value := Self.FileName;
end;

procedure TSimbaColors.OnApplyAndSave(Sender: TObject);
var i : Int32;
begin
  for i := 0 to SimbaForm.Tabs.Count - 1 do
    UpdateEditor(TMufasaTab(SimbaForm.Tabs[i]).ScriptFrame.SynEdit);

  if Self.FileName <> '' then
    Self.Save(Self.FileName)
  else
    Self.OnSave(Sender);

  SimbaSettings.Editor.HighlighterPath.Value := Self.FileName
end;

procedure TSimbaColors.OnSave(Sender: TObject);
var
  UI: TSaveDialog;
begin
  UI := TSaveDialog.Create(Self);

  if ExtractFileName(Self.FileName) = '' then
    UI.FileName := 'highlighter'
  else
    UI.FileName:= ExtractFileName(Self.FileName);

  UI.DefaultExt := 'json';
  UI.Filter := 'JSON file|*.json';
  UI.InitialDir := DataPath;
  if UI.Execute() then
    Self.Save(UI.FileName);
end;

procedure TSimbaColors.OnLoad(Sender: TObject);
var
  UI: TOpenDialog;
begin
  UI := TOpenDialog.Create(Self);
  UI.DefaultExt := 'json';
  UI.Filter := 'JSON file|*.json';
  UI.InitialDir := DataPath;

  if UI.Execute() then
  begin
    Self.Scheme   := Self.LoadScheme(UI.FileName);
    Self.FileName := UI.FileName;
    Self.Edited   := False;

    UpdateEditor(Self.Editor);
  end;
end;

constructor TSimbaColors.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Editor Colors';
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
    BevelOuter := bvNone;
    BevelInner := bvNone;
  end;

  Editor := TSynEdit.Create(TopPanel);
  with Editor do
  begin
    Parent := TopPanel;
    Align := alRight;
    Width := Round(Self.Width * 0.8);
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
    BevelColor := clLtGray;
    BevelWidth := 2;
    BevelOuter := bvNone;
    BevelInner := bvNone;
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
    Left  := 110;
    Top   := 8;
    Width := 150;
    Items.Objects[Items.Add('Default')]   := TObject(PtrInt(-1));
  end;

  with SelectBG do
  begin
    Parent   := BtmPanel;
    OnChange := @Self.OnSelectBG;
    Style    := [cbStandardColors,cbExtendedColors,cbSystemColors,cbCustomColor,cbPrettyNames,cbCustomColors];
    Left  := 110;
    Top   := 36;
    Width := 150;
    Items.Objects[Items.Add('Default')] := TObject(PtrInt(-1));
  end;

// The checkboxes
  CheckBold      := TCheckBox.Create(BtmPanel);
  CheckItalic    := TCheckBox.Create(BtmPanel);
  CheckUnderline := TCheckBox.Create(BtmPanel);
  CheckStrike    := TCheckBox.Create(BtmPanel);

  with CheckBold do
  begin
    Parent := BtmPanel;
    Left   := 10;
    Top    := 72;
    Caption:= 'Bold';
    OnChange := @Self.OnBoldChange;
  end;

  with CheckItalic do
  begin
    Parent := BtmPanel;
    Left   := 65;
    Top    := 72;
    Caption:= 'Italic';
    OnChange := @Self.OnItalicChange;
  end;

  with CheckUnderline do
  begin
    Parent := BtmPanel;
    Left   := 120;
    Top    := 72;
    Caption:= 'Underline';
    OnChange := @Self.OnUnderlineChange;
  end;

  with CheckStrike do
  begin
    Parent := BtmPanel;
    Left   := 200;
    Top    := 72;
    Caption:= 'Strike';
    OnChange := @Self.OnStrikeChange;
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

  ApplyAndSaveBtn := TButton.Create(BtmPanel);
  with ApplyAndSaveBtn do
  begin
    Parent := BtmPanel;
    Left   := 90;
    Top    := 115;
    Caption:= 'Apply and Save';
    Width  := Width + 20;
    OnClick := @Self.OnApplyAndSave;
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
  Self.Scheme := LoadScheme(SimbaSettings.Editor.HighlighterPath.Value);
  Self.UpdateEditor(Self.Editor);
end;

{$R *.lfm}

end.

