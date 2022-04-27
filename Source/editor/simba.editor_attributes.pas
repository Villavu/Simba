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
  classes, sysutils,
  synedit, synedithighlighter;

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

  TSimbaEditor_AutoCompleteSelectedAttribute = class(TSimbaEditor_Attribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TSimbaEditor_AutoCompleteTextAttribute = class(TSimbaEditor_Attribute)
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
  graphics, syneditmarkupwordgroup, syneditmarkuphighall, SynEditPointClasses, SynEditMarkupFoldColoring, inifiles,
  simba.editor;

type
  TSynAttributeProtectedAccess = class(TSynHighlighterAttributes);
  TSynEditProtectedAccess = class(TSynEdit);

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
  if (Editor = nil) then
    Exit;

  with TSynEditProtectedAccess(Editor) do
  begin
    if (Foreground = clDefault) or (Foreground = clNone) then
    begin
      FScreenCaretPainterClass := TSynEditScreenCaretPainterSystem;
      if FScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterSystem then
        FScreenCaret.ChangePainter(TSynEditScreenCaretPainterSystem);
    end else
    begin
      FScreenCaretPainterClass := TSynEditScreenCaretPainterInternal;
      if FScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal then
        FScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);

      TSynEditScreenCaretPainterInternal(FScreenCaret.Painter).Color := Foreground;
    end;

    Editor.Invalidate();
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
  begin
    TSimbaEditor(Editor).AutoComplete.TheForm.BackgroundColor := Foreground;
    TSimbaEditor(Editor).AutoComplete.TheForm.DrawBorderColor := Foreground;
  end;
end;

procedure TSimbaEditor_AutoCompleteBackgroundAttribute.Init;
begin
  inherited Init();

  Foreground := RGBToColor(240, 240, 240);
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.DoChange;
begin
  if (Editor <> nil) then
    TSimbaEditor(Editor).AutoComplete.SelectedColor := Foreground;
end;

procedure TSimbaEditor_AutoCompleteSelectedAttribute.Init;
begin
  inherited Init();

  Foreground := RGBToColor(159, 180, 208);
end;

procedure TSimbaEditor_AutoCompleteTextAttribute.Init;
begin
  inherited Init();

  Foreground := clBlack;
end;

procedure TSimbaEditor_AutoCompleteTextAttribute.DoChange;
begin
  if (Editor <> nil) then
  begin
    TSimbaEditor(Editor).AutoComplete.TheForm.TextColor := Foreground;
    TSimbaEditor(Editor).AutoComplete.TheForm.TextSelectedColor := Foreground;
  end;
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
  inherited Init();

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
  Add('AutoComplete.Selected', TSimbaEditor_AutoCompleteSelectedAttribute.Create());
  Add('AutoComplete.Text', TSimbaEditor_AutoCompleteTextAttribute.Create());

  Add('Gutter.Background', TSimbaEditor_GutterColorAttribute.Create());
  Add('Gutter.Seperator', Editor.Gutter.SeparatorPart().MarkupInfo);
  Add('Gutter.Changes', Editor.Gutter.ChangesPart().MarkupInfo);
  Add('Gutter.Marks', Editor.Gutter.MarksPart().MarkupInfo);
  Add('Gutter.Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfo);
  Add('Gutter.Line Number', Editor.Gutter.LineNumberPart().MarkupInfo);
end;

destructor TSimbaEditor_Attributes.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FAttributes) do
    if FAttributes[I] is TSimbaEditor_Attribute then
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

