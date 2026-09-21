{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Handles all SynEdit styling for our needs.
  Each "part" is a different attribute.
  Will also assign a highlighter to the editor.
}
unit simba.component_syneditstyler;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  SynEdit,
  SynEditHighlighter;

type
  TSimbaSynEditCustomAttribute = class(TSynHighlighterAttributes)
  private
    FEditor: TSynEdit;
  public
    constructor Create(AEditor: TSynEdit); reintroduce;
    property Editor: TSynEdit read FEditor;
  end;

  TSimbaSynEditAttributeArray = array of TSynHighlighterAttributes;
  TSimbaSynEditStyler = class(TComponent)
  private
    FAttributes: TSimbaSynEditAttributeArray;
  public
    constructor Create(Editor: TSynEdit); reintroduce;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; ManageStream: Boolean);
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
    procedure Changed;

    property Attributes: TSimbaSynEditAttributeArray read FAttributes;
  end;

implementation

uses
  Graphics,
  IniFiles,
  SynGutterBase,
  SynHighlighterPas,
  SynEditMiscClasses,
  SynEditMarkupHighAll,
  SynEditMarkupWordGroup,
  SynEditMarkupFoldColoring,
  SynEditPointClasses,
  simba.component_theme;

type
  TSynAttributeProtectedAccess = class(TSynHighlighterAttributes);
  TSynEditProtectedAccess = class(TSynEdit);

  TBackgroundColorAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TIndentColorAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TCaretColorAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TGutterColorAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TGutterColorModifiedAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TGutterColorSavedAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TRightEdgeColorAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

  TDividerAttribute = class(TSimbaSynEditCustomAttribute)
  protected
    procedure DoChange; override;
    procedure Init; override;
  end;

procedure TGutterColorModifiedAttribute.DoChange;
begin
  if (FEditor = nil) or (FEditor.Gutter.ChangesPart() = nil) then
    Exit;

  FEditor.Gutter.ChangesPart.ModifiedColor := Foreground;
  FEditor.Invalidate();
end;

procedure TGutterColorModifiedAttribute.Init;
begin
  inherited;

  Foreground := RGBToColor(254, 221, 0);
end;

procedure TGutterColorSavedAttribute.DoChange;
begin
  if (FEditor = nil) or (FEditor.Gutter.ChangesPart() = nil) then
    Exit;

  FEditor.Gutter.ChangesPart.SavedColor := Foreground;
  FEditor.Invalidate();
end;

procedure TGutterColorSavedAttribute.Init;
begin
  inherited;

  Foreground := RGBToColor(0, 128, 0);
end;

procedure TRightEdgeColorAttribute.DoChange;
begin
  if (FEditor = nil) then
    Exit;

  FEditor.RightEdgeColor := Foreground;
  FEditor.Invalidate();
end;

procedure TRightEdgeColorAttribute.Init;
begin
  inherited;

  Foreground := clGray;
end;

procedure TGutterColorAttribute.DoChange;
var
  I: Integer;
begin
  if (FEditor = nil) then
    Exit;

  with FEditor do
  begin
    Gutter.Color := Foreground;
    for I := 0 to Gutter.Parts.Count - 1 do
      if (Gutter.Parts[I] is TSynGutterPartBase) then
        TSynGutterPartBase(Gutter.Parts[I]).MarkupInfo.Background := Foreground;

    RightGutter.Color := Foreground;
    for I := 0 to RightGutter.Parts.Count - 1 do
      if (RightGutter.Parts[I] is TSynGutterPartBase) then
        TSynGutterPartBase(RightGutter.Parts[I]).MarkupInfo.Background := Foreground;

    FEditor.Invalidate();
  end;
end;

procedure TGutterColorAttribute.Init;
begin
  inherited;

  Foreground := SimbaComponentTheme.ColorBackground;
end;

procedure TIndentColorAttribute.DoChange;
var
  MarkupFoldColors: TSynEditMarkupFoldColors;
begin
  if (FEditor = nil) then
    Exit;

  with FEditor do
  begin
    // Create markup
    if (MarkupByClass[TSynEditMarkupFoldColors] = nil) then
    begin
      MarkupFoldColors := TSynEditMarkupFoldColors.Create(FEditor);
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

procedure TIndentColorAttribute.Init;
begin
  inherited;

  Foreground := clGray;
end;

procedure TBackgroundColorAttribute.DoChange;
begin
  if (FEditor = nil) then
    Exit;

  FEditor.Color := Foreground;
  FEditor.Invalidate();
end;

procedure TBackgroundColorAttribute.Init;
begin
  inherited;

  Foreground := SimbaComponentTheme.ColorBackground;
end;

procedure TCaretColorAttribute.DoChange;
begin
  if (FEditor = nil) then
    Exit;

  with TSynEditProtectedAccess(FEditor) do
  begin
    if (Foreground = Color) then
      TSynEditScreenCaretPainterInternal(FScreenCaret.Painter).Color := Foreground
    else
      TSynEditScreenCaretPainterInternal(FScreenCaret.Painter).Color := (Foreground and $FF000000) or (not (Foreground xor Color) and $00FFFFFF);

    Invalidate();
  end;
end;

procedure TCaretColorAttribute.Init;
begin
  inherited;

  Foreground := SimbaComponentTheme.ColorBackground;
end;

procedure TDividerAttribute.DoChange;
var
  I: Integer;
begin
  if (FEditor = nil) or (FEditor.Highlighter = nil) then
    Exit;

  with FEditor do
  begin
    for I := 0 to Highlighter.DividerDrawConfigCount - 1 do
    begin
      Highlighter.DividerDrawConfig[I].TopColor := Foreground;
      Highlighter.DividerDrawConfig[I].NestColor := Foreground;
    end;

    Invalidate();
  end;
end;

procedure TDividerAttribute.Init;
begin
  inherited;

  Foreground := clNone;
end;

constructor TSimbaSynEditCustomAttribute.Create(AEditor: TSynEdit);
begin
  inherited Create();

  FEditor := AEditor;
end;

constructor TSimbaSynEditStyler.Create(Editor: TSynEdit);

  procedure Add(AName: String; Attribute: TSynHighlighterAttributes);
  begin
    // dont add things we have no use for
    if (Attribute.StoredName = 'PasDoc-Keyword') or
       (Attribute.StoredName = 'PasDoc-Symbol')  or
       (Attribute.StoredName = 'PasDoc-Unknown') or
       (Attribute.StoredName = 'Assembler') then
      Exit;

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
  inherited Create(Editor);

  Editor.BracketMatchColor.FrameColor := RGBToColor(190, 20, 20);
  Editor.SelectedColor.Background := SimbaComponentTheme.ColorActive;
  Editor.SelectedColor.BackAlpha := 220;
  Editor.LineHighlightColor.Background := $FFFFFF;
  Editor.LineHighlightColor.BackAlpha := 15;
  Editor.MouseLinkColor.Foreground := clNone;
  Editor.MouseLinkColor.Style := [fsUnderline];

  if (Editor.Highlighter <> nil) then
    for I := 0 to Editor.Highlighter.AttrCount - 1 do
      Add('Highlighter.' + Editor.Highlighter.Attribute[I].StoredName, Editor.Highlighter.Attribute[I]);

  with Editor.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret do
  begin
    MarkupInfo.Background := $FFFFFF;
    MarkupInfo.BackAlpha := 50;

    Add('Editor.Caret Selection', MarkupInfo);
  end;

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
  Add('Editor.Background', TBackgroundColorAttribute.Create(Editor));
  Add('Editor.Indent Line', TIndentColorAttribute.Create(Editor));
  Add('Editor.Caret', TCaretColorAttribute.Create(Editor));
  Add('Editor.Right Edge', TRightEdgeColorAttribute.Create(Editor));
  Add('Editor.Divider', TDividerAttribute.Create(Editor));
  Add('Gutter.Background', TGutterColorAttribute.Create(Editor));
  Add('Gutter.Saved Changes', TGutterColorSavedAttribute.Create(Editor));
  Add('Gutter.Modified Changes', TGutterColorModifiedAttribute.Create(Editor));
  Add('Gutter.Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfo);
  Add('Gutter.Current Code Fold', Editor.Gutter.CodeFoldPart().MarkupInfoCurrentFold);
  Add('Gutter.Line Number', Editor.Gutter.LineNumberPart().MarkupInfo);
end;

destructor TSimbaSynEditStyler.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FAttributes) do
    if (FAttributes[I] is TSimbaSynEditCustomAttribute) then
      TSimbaSynEditCustomAttribute(FAttributes[I]).Free();
  FAttributes := [];

  inherited Destroy();
end;

procedure TSimbaSynEditStyler.LoadFromStream(Stream: TStream; ManageStream: Boolean);
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
      IntegerStyle := INI.ReadInteger(StoredName, 'Style', IntegerStyle);
      IntegerStyleMask := INI.ReadInteger(StoredName, 'StyleMask', IntegerStyleMask);
      FrameColor := INI.ReadInteger(StoredName, 'Frame', FrameColor);

      if (FAttributes[I] is TSynSelectedColor) then
      begin
        if INI.ValueExists(StoredName, 'BackAlpha') then
          TSynSelectedColor(FAttributes[I]).BackAlpha := INI.ReadInteger(StoredName, 'BackAlpha', 0);
        if INI.ValueExists(StoredName, 'ForeAlpha') then
          TSynSelectedColor(FAttributes[I]).ForeAlpha := INI.ReadInteger(StoredName, 'ForeAlpha', 0);
      end;

      Changed();
    end;

  INI.Free();
  if ManageStream then
    Stream.Free();
end;

procedure TSimbaSynEditStyler.LoadFromFile(FileName: String);
begin
  if FileExists(FileName) then
    LoadFromStream(TFileStream.Create(FileName, fmOpenRead), True);
end;

procedure TSimbaSynEditStyler.SaveToFile(FileName: String);
var
  INI: TINIFile;
  I: Integer;
begin
  try
    INI := TIniFile.Create(FileName);
    try
      INI.CacheUpdates := True;

      for I := 0 to High(FAttributes) do
        with TSynHighlighterAttributes(FAttributes[I]) do
        begin
          INI.WriteInteger(StoredName, 'Background', Background);
          INI.WriteInteger(StoredName, 'Foreground', Foreground);
          INI.WriteInteger(StoredName, 'Style', IntegerStyle);
          INI.WriteInteger(StoredName, 'StyleMask', IntegerStyleMask);
          INI.WriteInteger(StoredName, 'Frame', FrameColor);

          if (FAttributes[I] is TSynSelectedColor) then
          begin
            INI.WriteInteger(StoredName, 'BackAlpha', TSynSelectedColor(FAttributes[I]).BackAlpha);
            INI.WriteInteger(StoredName, 'ForeAlpha', TSynSelectedColor(FAttributes[I]).ForeAlpha);
          end;
        end;

      INI.UpdateFile();
    finally
      INI.Free();
    end;
  except
  end;
end;

procedure TSimbaSynEditStyler.Changed;
var
  I: Integer;
begin
  for I := 0 to High(FAttributes) do
    TSynAttributeProtectedAccess(FAttributes[I]).Changed();
end;

end.

