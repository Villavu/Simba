{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_attributes;

// Store all attributes in a nicely named list.
// This includes highlighter attributes. Basically any TSynHighlighterAttributes.

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  SynEdit, SynEditHighlighter;

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

  TSimbaEditor_GutterColorModifiedAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_GutterColorSavedAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_RightEdgeColorAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_DividerAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AttributeArray = array of TSynHighlighterAttributes;
  TSimbaEditor_Attributes = class
  protected
    FAttributes: TSimbaEditor_AttributeArray;
  public
    constructor Create(Editor: TSynEdit);
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; ManageStream: Boolean);
    procedure LoadFromFile(FileName: String);

    procedure SaveToFile(FileName: String);

    property Attributes: TSimbaEditor_AttributeArray read FAttributes;
  end;

implementation

uses
  Graphics, IniFiles,
  SynGutterBase, SynEditMarkupHighAll, SynEditMarkupWordGroup, SynEditMarkupFoldColoring, SynEditPointClasses, SynHighlighterPas_Simba,
  simba.editor, simba.theme;

type
  TSynAttributeProtectedAccess = class(TSynHighlighterAttributes);
  TSynEditProtectedAccess = class(TSynEdit);

procedure TSimbaEditor_GutterColorModifiedAttribute.DoChange;
begin
  if (Editor = nil) then
    Exit;

  with TSimbaEditor(Editor) do
  begin
    ColorModified := Foreground;
    Invalidate();
  end;
end;

procedure TSimbaEditor_GutterColorModifiedAttribute.Init;
begin
  inherited;

  Foreground := RGBToColor(254, 221, 0);
end;

procedure TSimbaEditor_GutterColorSavedAttribute.DoChange;
begin
  if (Editor = nil) then
    Exit;

  with TSimbaEditor(Editor) do
  begin
    ColorSaved := Foreground;
    Invalidate();
  end;
end;

procedure TSimbaEditor_GutterColorSavedAttribute.Init;
begin
  inherited;

  Foreground := RGBToColor(0, 128, 0);
end;

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
  inherited;

  Foreground := clGray;
end;

procedure TSimbaEditor_GutterColorAttribute.DoChange;
var
  I: Integer;
begin
  if (Editor <> nil) then
    with Editor do
    begin
      Gutter.Color := Foreground;
      for I := 0 to Gutter.Parts.Count - 1 do
        if (Gutter.Parts[I] is TSynGutterPartBase) then
          TSynGutterPartBase(Gutter.Parts[I]).MarkupInfo.Background := Foreground;

      RightGutter.Color := Foreground;
      for I := 0 to RightGutter.Parts.Count - 1 do
        if (RightGutter.Parts[I] is TSynGutterPartBase) then
          TSynGutterPartBase(RightGutter.Parts[I]).MarkupInfo.Background := Foreground;

      Editor.Invalidate();
    end;
end;

procedure TSimbaEditor_GutterColorAttribute.Init;
begin
  inherited;

  Foreground := SimbaTheme.ColorBackground;
end;

procedure TSimbaEditor_IndentColorAttribute.DoChange;
var
  MarkupFoldColors: TSynEditMarkupFoldColors;
begin
  if (Editor = nil) then
    Exit;

  with Editor do
  begin
    // Create markup
    if (MarkupByClass[TSynEditMarkupFoldColors] = nil) then
    begin
      MarkupFoldColors := TSynEditMarkupFoldColors.Create(Editor);
      MarkupFoldColors.ColorCount := 1;
      with MarkupFoldColors.Color[0] do
      begin
        Foreground := clNone;
        Background := clNone;
      end;

      MarkupManager.AddMarkUp(MarkupFoldColors);
    end;

    with MarkupByClass[TSynEditMarkupFoldColors] as TSynEditMarkupFoldColors do
      LineColor[0].Color := Foreground;

    Invalidate();
  end;
end;

procedure TSimbaEditor_IndentColorAttribute.Init;
begin
  inherited;

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
  inherited;

  Foreground := SimbaTheme.ColorBackground;
end;

procedure TSimbaEditor_CaretColorAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    with TSynEditProtectedAccess(Editor) do
      if (Foreground = Editor.Color) then
        TSynEditScreenCaretPainterInternal(FScreenCaret.Painter).Color := Foreground
      else
        TSynEditScreenCaretPainterInternal(FScreenCaret.Painter).Color := (Foreground and $FF000000) or (not (Foreground xor Editor.Color) and $00FFFFFF);

    Editor.Invalidate();
  end;
end;

procedure TSimbaEditor_CaretColorAttribute.Init;
begin
  inherited;

  Foreground := SimbaTheme.ColorBackground;
end;

procedure TSimbaEditor_DividerAttribute.DoChange;
var
  I: Integer;
begin
  if (Editor = nil) or (Editor.Highlighter = nil) then
    Exit;

  with Editor do
  begin
    for I := 0 to Highlighter.DividerDrawConfigCount - 1 do
    begin
      Highlighter.DividerDrawConfig[I].TopColor := Foreground;
      Highlighter.DividerDrawConfig[I].NestColor := Foreground;
    end;

    Invalidate();
  end;
end;

procedure TSimbaEditor_DividerAttribute.Init;
begin
  inherited;

  Foreground := clNone;
end;

constructor TSimbaEditor_Attributes.Create(Editor: TSynEdit);

  procedure Add(AName: String; Attribute: TSynHighlighterAttributes);
  begin
    if Attribute is TSimbaEditor_Attribute then
      TSimbaEditor_Attribute(Attribute).Editor := Editor;

    with TSynAttributeProtectedAccess(Attribute) do
    begin
      StoredName := AName;
      Changed();
    end;

    FAttributes += [Attribute];
  end;

var
  I: Integer;
begin
  inherited Create();

  if (Editor.Highlighter is TSynFreePascalSyn) then
    with TSynFreePascalSyn(Editor.Highlighter) do
    begin
      KeywordAttribute.Foreground := RGBToColor(199, 125, 187);
      StringAttribute.Foreground := RGBToColor(232, 191, 106);
      SymbolAttri.Foreground := RGBToColor(190, 20, 20);
      CommentAttri.Foreground := RGBToColor(95, 130, 107);
      CommentAttri.Style := [fsBold];
      IdentifierAttri.Foreground := RGBToColor(242, 242, 242);
      NumberAttri.Foreground := RGBToColor(87, 170, 247);
      DirectiveAttri.Foreground := RGBToColor(13, 161, 149);
      DirectiveAttri.Style := [fsBold];

      NestedComments := True;
      TypeHelpers := True;
      StringKeywordMode := spsmNone;
      StringMultilineMode := [spmsmDoubleQuote];
    end;

  Editor.BracketMatchColor.FrameColor := RGBToColor(190, 20, 20);

  Editor.SelectedColor.Background := SimbaTheme.ColorActive;
  Editor.SelectedColor.BackAlpha := 220;

  Editor.LineHighlightColor.Background := $FFFFFF;
  Editor.LineHighlightColor.BackAlpha := 15;

  for I := 0 to Editor.Highlighter.AttrCount - 1 do
    Add('Highlighter.' + Editor.Highlighter.Attribute[I].StoredName, Editor.Highlighter.Attribute[I]);

  with Editor.MarkupByClass[TSynEditMarkupHighlightAll] as TSynEditMarkupHighlightAll do
  begin
    MarkupInfo.Background := RGBToColor(128, 0, 128);
    MarkupInfo.BackAlpha := 220;

    Add('Editor.Find Matches', MarkupInfo);
  end;

  with Editor.MarkupByClass[TSynEditMarkupWordGroup] as TSynEditMarkupWordGroup do
  begin
    MarkupInfo.FrameColor := RGBToColor(199, 125, 187);

    Add('Editor.Word Group', MarkupInfo);
  end;

  Add('Editor.Line Highlight', Editor.LineHighlightColor);
  Add('Editor.Folded Code', Editor.FoldedCodeColor);
  Add('Editor.Folded Code Line', Editor.FoldedCodeLineColor);
  Add('Editor.Bracket Match', Editor.BracketMatchColor);
  Add('Editor.Selected', Editor.SelectedColor);
  Add('Editor.Mouse Link', Editor.MouseLinkColor);
  Add('Editor.Background', TSimbaEditor_BackgroundColorAttribute.Create());
  Add('Editor.Indent Line', TSimbaEditor_IndentColorAttribute.Create());
  Add('Editor.Caret', TSimbaEditor_CaretColorAttribute.Create());
  Add('Editor.Right Edge', TSimbaEditor_RightEdgeColorAttribute.Create());
  Add('Editor.Divider', TSimbaEditor_DividerAttribute.Create());

  Add('Gutter.Background', TSimbaEditor_GutterColorAttribute.Create());
  Add('Gutter.Saved Changes', TSimbaEditor_GutterColorSavedAttribute.Create());
  Add('Gutter.Modified Changes', TSimbaEditor_GutterColorModifiedAttribute.Create());
  Add('Gutter.Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfo);
  Add('Gutter.Line Number', Editor.Gutter.LineNumberPart().MarkupInfo);
end;

destructor TSimbaEditor_Attributes.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FAttributes) do
    if (FAttributes[I] is TSimbaEditor_Attribute) then
      TSimbaEditor_Attribute(FAttributes[I]).Free();

  FAttributes := nil;

  inherited Destroy();
end;

procedure TSimbaEditor_Attributes.LoadFromStream(Stream: TStream; ManageStream: Boolean);
var
  INI: TINIFile;
  I: Integer;
begin
  INI := TINIFile.Create(Stream);
  for I := 0 to High(FAttributes) do
    with TSynAttributeProtectedAccess(FAttributes[I]) do
    begin
      Background := INI.ReadInteger(StoredName, 'Background', Background);
      Foreground := INI.ReadInteger(StoredName, 'Foreground', Foreground);

      IntegerStyle     := INI.ReadInteger(StoredName, 'Style', IntegerStyle);
      IntegerStyleMask := INI.ReadInteger(StoredName, 'StyleMask', IntegerStyleMask);

      FrameColor := INI.ReadInteger(StoredName, 'Frame', FrameColor);

      Changed();
    end;

  INI.Free();
  if ManageStream then
    Stream.Free();
end;

procedure TSimbaEditor_Attributes.LoadFromFile(FileName: String);
begin
  if FileExists(FileName) then
    LoadFromStream(TFileStream.Create(FileName, fmOpenRead), True);
end;

procedure TSimbaEditor_Attributes.SaveToFile(FileName: String);
var
  INI: TINIFile;
  I: Integer;
begin
  try
    INI := TIniFile.Create(FileName);
    INI.CacheUpdates := True;

    for I := 0 to High(FAttributes) do
      with TSynHighlighterAttributes(FAttributes[I]) do
      begin
        INI.WriteInteger(StoredName, 'Background', Background);
        INI.WriteInteger(StoredName, 'Foreground', Foreground);

        INI.WriteInteger(StoredName, 'Style', IntegerStyle);
        INI.WriteInteger(StoredName, 'StyleMask', IntegerStyleMask);

        INI.WriteInteger(StoredName, 'Frame', FrameColor);
      end;

    INI.UpdateFile();
    INI.Free();
  except
  end;
end;

end.

