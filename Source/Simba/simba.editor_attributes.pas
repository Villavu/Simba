unit simba.editor_attributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter,
  simba.editor;

type
  TSimbaEditor_CustomColorAttribute = class(TSynHighlighterAttributes)
  protected
    FEditor: TSimbaEditor;
  public
    constructor Create(Editor: TSimbaEditor); reintroduce;

    procedure Changed;
  end;

  TSimbaEditor_BackgroundColorAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_IndentColorAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_CaretColorAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_GutterColorAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_RightEdgeColorAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteBackgroundAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteAlternatingAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteBorderAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteIdentifierAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteFilterAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteSelectedAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_DividerAttribute = class(TSimbaEditor_CustomColorAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_SimbaAttributesList = class(TSimbaEditor_AttributeList)
  public
    procedure Add(Name: String; Attribute: TSynHighlighterAttributes); overload;

    constructor Create(Editor: TSimbaEditor); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  graphics, syneditmarkupwordgroup, syneditmarkuphighall;

constructor TSimbaEditor_CustomColorAttribute.Create(Editor: TSimbaEditor);
begin
  inherited Create();

  FEditor := Editor;
end;

procedure TSimbaEditor_CustomColorAttribute.Changed;
begin
  inherited Changed();
end;

procedure TSimbaEditor_RightEdgeColorAttribute.DoChange;
begin
  if (FEditor <> nil) then
  begin
    FEditor.RightEdgeColor := Foreground;
    FEditor.Invalidate();
  end;
end;

procedure TSimbaEditor_RightEdgeColorAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

procedure TSimbaEditor_GutterColorAttribute.DoChange;
begin
  if (FEditor <> nil) and (FEditor.Gutter <> nil) then
  begin
    FEditor.Gutter.Color := Foreground;
    FEditor.Invalidate();
  end;
end;

procedure TSimbaEditor_GutterColorAttribute.Init;
begin
  inherited Init();

  Foreground := clBtnFace;
end;

procedure TSimbaEditor_IndentColorAttribute.DoChange;
begin
  if (FEditor <> nil) then
  begin
    FEditor.IndentColor := Foreground;
    FEditor.Invalidate();
  end;
end;

procedure TSimbaEditor_IndentColorAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

procedure TSimbaEditor_BackgroundColorAttribute.DoChange;
begin
  if (FEditor <> nil) then
  begin
    FEditor.Color := Foreground;
    FEditor.Invalidate();
  end;
end;

procedure TSimbaEditor_BackgroundColorAttribute.Init;
begin
  inherited Init();

  Foreground := clWhite;
end;

procedure TSimbaEditor_CaretColorAttribute.DoChange;
begin
  if (FEditor <> nil) then
  begin
    FEditor.CaretColor := Foreground;
    FEditor.Invalidate;
  end;
end;

procedure TSimbaEditor_CaretColorAttribute.Init;
begin
  inherited Init();

  Foreground := clDefault;
end;

procedure TSimbaEditor_AutoCompleteBackgroundAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.BackgroundColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteBackgroundAttribute.Init;
begin
  inherited Init();

  Foreground := clWhite;
end;

procedure TSimbaEditor_AutoCompleteAlternatingAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.AlternatingColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteAlternatingAttribute.Init;
begin
  inherited Init();

  Foreground := $F0F0F0;
end;

procedure TSimbaEditor_AutoCompleteBorderAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.TheForm.DrawBorderColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteBorderAttribute.Init;
begin
  inherited Init();

  Foreground := clBlack;
end;

procedure TSimbaEditor_AutoCompleteIdentifierAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.IdentifierColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteIdentifierAttribute.Init;
begin
  inherited Init();

  Foreground := clBlack;
end;

procedure TSimbaEditor_AutoCompleteFilterAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.FilterColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteFilterAttribute.Init;
begin
  inherited Init();

  Foreground := clMaroon;
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.AutoComplete.SelectedColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.Init;
begin
  inherited Init();

  Foreground := $BE9270;
end;

procedure TSimbaEditor_DividerAttribute.DoChange;
begin
  if (FEditor <> nil) then
    FEditor.DividerColor := Foreground;
end;

procedure TSimbaEditor_DividerAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

procedure TSimbaEditor_SimbaAttributesList.Add(Name: String; Attribute: TSynHighlighterAttributes);
begin
  Attribute.StoredName := Name;

  inherited Add(Attribute);
end;

constructor TSimbaEditor_SimbaAttributesList.Create(Editor: TSimbaEditor);
var
  i: Int32;
begin
  inherited Create(False);

  for i := 0 to Editor.Highlighter.AttrCount - 1 do
    Add('Highlighter.' + Editor.Highlighter.Attribute[i].StoredName, Editor.Highlighter.Attribute[i]);

  with Editor.MarkupByClass[TSynEditMarkupWordGroup] do
    Add('Editor.Word Group', MarkupInfo);

  with Editor.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret do
    Add('Editor.Highlight All Caret', MarkupInfo);

  Add('Editor.Line Highlight', Editor.LineHighlightColor);
  Add('Editor.Folded Code', Editor.FoldedCodeColor);
  Add('Editor.Folded Code Line', Editor.FoldedCodeLineColor);
  Add('Editor.Bracket Match', Editor.BracketMatchColor);
  Add('Editor.Selected', Editor.SelectedColor);
  Add('Editor.Highlight All', Editor.HighlightAllColor);
  Add('Editor.Mouse Link', Editor.MouseLinkColor);
  Add('Editor.Background', TSimbaEditor_BackgroundColorAttribute.Create(Editor));
  Add('Editor.Indent Line', TSimbaEditor_IndentColorAttribute.Create(Editor));
  Add('Editor.Caret', TSimbaEditor_CaretColorAttribute.Create(Editor));
  Add('Editor.Right Edge', TSimbaEditor_RightEdgeColorAttribute.Create(Editor));
  Add('Editor.Divider', TSimbaEditor_DividerAttribute.Create(Editor));

  Add('AutoComplete.Background', TSimbaEditor_AutoCompleteBackgroundAttribute.Create(Editor));
  Add('AutoComplete.Border', TSimbaEditor_AutoCompleteBorderAttribute.Create(Editor));
  Add('AutoComplete.Identifier', TSimbaEditor_AutoCompleteIdentifierAttribute.Create(Editor));
  Add('AutoComplete.Filter', TSimbaEditor_AutoCompleteFilterAttribute.Create(Editor));
  Add('AutoComplete.Alternating', TSimbaEditor_AutoCompleteAlternatingAttribute.Create(Editor));
  Add('AutoComplete.Selected', TSimbaEditor_AutoCompleteSelectedAttribute.Create(Editor));

  Add('Gutter.Background', TSimbaEditor_GutterColorAttribute.Create(Editor));
  Add('Gutter.Seperator', Editor.Gutter.SeparatorPart().MarkupInfo);
  Add('Gutter.Changes', Editor.Gutter.ChangesPart().MarkupInfo);
  Add('Gutter.Marks', Editor.Gutter.MarksPart().MarkupInfo);
  Add('Gutter.Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfo);
  Add('Gutter.Line Number', Editor.Gutter.LineNumberPart().MarkupInfo);
end;

destructor TSimbaEditor_SimbaAttributesList.Destroy;
var
  i: Int32;
begin
  for i := 0 to Count - 1 do
    if Self[i] is TSimbaEditor_CustomColorAttribute then
      Self[i].Free();

  inherited Destroy();
end;

end.

