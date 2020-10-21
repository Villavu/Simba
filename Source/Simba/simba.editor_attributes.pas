unit simba.editor_attributes;

// Store all attributes in a nicely named list.
// This includes highlighter attributes. Basically any TSynHighlighterAttributes.

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, synedithighlighter, synedit, generics.collections;

type
  TSimbaEditor_Attribute = class(TSynHighlighterAttributes)
  public
    Editor: TSynEdit;
  end;

  TSimbaEditor_BackgroundColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_IndentColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_CaretColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_GutterColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_RightEdgeColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteBackgroundAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteAlternatingAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteBorderAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteIdentifierAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteFilterAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteSelectedAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_DividerAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AttributesList = class(specialize TList<TSynHighlighterAttributes>)
  public
    constructor Create(Editor: TSynEdit); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  graphics, syneditmarkupwordgroup, syneditmarkuphighall,
  simba.editor;

procedure TSimbaEditor_RightEdgeColorAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    TSimbaEditor(Editor).RightEdgeColor := Foreground;
    TSimbaEditor(Editor).Invalidate();
  end;
end;

procedure TSimbaEditor_RightEdgeColorAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

procedure TSimbaEditor_GutterColorAttribute.DoChange;
begin
  if (Editor <> nil) and (TSimbaEditor(Editor).Gutter <> nil) then
  begin
    TSimbaEditor(Editor).Gutter.Color := Foreground;
    TSimbaEditor(Editor).Invalidate();
  end;
end;

procedure TSimbaEditor_GutterColorAttribute.Init;
begin
  inherited Init();

  Foreground := clBtnFace;
end;

procedure TSimbaEditor_IndentColorAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    TSimbaEditor(Editor).IndentColor := Foreground;
    TSimbaEditor(Editor).Invalidate();
  end;
end;

procedure TSimbaEditor_IndentColorAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

procedure TSimbaEditor_BackgroundColorAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    TSimbaEditor(Editor).Color := Foreground;
    TSimbaEditor(Editor).Invalidate();
  end;
end;

procedure TSimbaEditor_BackgroundColorAttribute.Init;
begin
  inherited Init();

  Foreground := clWhite;
end;

procedure TSimbaEditor_CaretColorAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    TSimbaEditor(Editor).CaretColor := Foreground;
    TSimbaEditor(Editor).Invalidate;
  end;
end;

procedure TSimbaEditor_CaretColorAttribute.Init;
begin
  inherited Init();

  Foreground := clDefault;
end;

procedure TSimbaEditor_AutoCompleteBackgroundAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.BackgroundColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteBackgroundAttribute.Init;
begin
  inherited Init();

  Foreground := clWhite;
end;

procedure TSimbaEditor_AutoCompleteAlternatingAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.AlternatingColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteAlternatingAttribute.Init;
begin
  inherited Init();

  Foreground := $F0F0F0;
end;

procedure TSimbaEditor_AutoCompleteBorderAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.TheForm.DrawBorderColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteBorderAttribute.Init;
begin
  inherited Init();

  Foreground := clBlack;
end;

procedure TSimbaEditor_AutoCompleteIdentifierAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.IdentifierColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteIdentifierAttribute.Init;
begin
  inherited Init();

  Foreground := clBlack;
end;

procedure TSimbaEditor_AutoCompleteFilterAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.FilterColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteFilterAttribute.Init;
begin
  inherited Init();

  Foreground := clMaroon;
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.SelectedColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.Init;
begin
  inherited Init();

  Foreground := $BE9270;
end;

procedure TSimbaEditor_DividerAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).DividerColor := Foreground;
end;

procedure TSimbaEditor_DividerAttribute.Init;
begin
  inherited Init();

  Foreground := clGray;
end;

// Create the list from the editor
constructor TSimbaEditor_AttributesList.Create(Editor: TSynEdit);

  procedure Add(Name: String; Attribute: TSynHighlighterAttributes);
  begin
    Attribute.StoredName := Name;
    if Attribute is TSimbaEditor_Attribute then
      TSimbaEditor_Attribute(Attribute).Editor := Editor;

    Self.Add(Attribute);
  end;

var
  I: Int32;
begin
  inherited Create();

  for I := 0 to Editor.Highlighter.AttrCount - 1 do
    Add('Highlighter.' + Editor.Highlighter.Attribute[I].StoredName, Editor.Highlighter.Attribute[I]);

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
  Add('Editor.Background', TSimbaEditor_BackgroundColorAttribute.Create());
  Add('Editor.Indent Line', TSimbaEditor_IndentColorAttribute.Create());
  Add('Editor.Caret', TSimbaEditor_CaretColorAttribute.Create());
  Add('Editor.Right Edge', TSimbaEditor_RightEdgeColorAttribute.Create());
  Add('Editor.Divider', TSimbaEditor_DividerAttribute.Create());

  Add('AutoComplete.Background', TSimbaEditor_AutoCompleteBackgroundAttribute.Create());
  Add('AutoComplete.Border', TSimbaEditor_AutoCompleteBorderAttribute.Create());
  Add('AutoComplete.Identifier', TSimbaEditor_AutoCompleteIdentifierAttribute.Create());
  Add('AutoComplete.Filter', TSimbaEditor_AutoCompleteFilterAttribute.Create());
  Add('AutoComplete.Alternating', TSimbaEditor_AutoCompleteAlternatingAttribute.Create());
  Add('AutoComplete.Selected', TSimbaEditor_AutoCompleteSelectedAttribute.Create());

  Add('Gutter.Background', TSimbaEditor_GutterColorAttribute.Create());
  Add('Gutter.Seperator', Editor.Gutter.SeparatorPart().MarkupInfo);
  Add('Gutter.Changes', Editor.Gutter.ChangesPart().MarkupInfo);
  Add('Gutter.Marks', Editor.Gutter.MarksPart().MarkupInfo);
  Add('Gutter.Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfo);
  Add('Gutter.Line Number', Editor.Gutter.LineNumberPart().MarkupInfo);
end;

destructor TSimbaEditor_AttributesList.Destroy;
var
  I: Int32;
begin
  for I := 0 to Count - 1 do
    if Self[I] is TSimbaEditor_Attribute then
      Self[I].Free();

  inherited Destroy();
end;

end.

