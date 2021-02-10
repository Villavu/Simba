unit simba.settingsform_editor_colors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics,
  Dialogs, ColorBox, SynEditHighlighter,
  simba.editor;

type
  TEditorColorsFrame = class(TFrame)
    ResetDarkButton: TButton;
    FrameColorBox: TColorListBox;
    Label4: TLabel;
    ResetButton: TButton;
    SaveAsButton: TButton;
    BoldCheckBox: TCheckBox;
    LoadButton: TButton;
    ItalicCheckBox: TCheckBox;
    UnderlineCheckBox: TCheckBox;
    StrikeCheckBox: TCheckBox;
    BackgroundColorBox: TColorListBox;
    ForegoundColorBox: TColorListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TreeView: TTreeView;

    procedure ResetDarkButtonClick(Sender: TObject);
    procedure FrameColorBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ResetButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ForegoundColorBoxSelectionChange(Sender: TObject; User: boolean);
    procedure BackgroundColorBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FontStyleChangeHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewSelectionChanged(Sender: TObject);
  protected
    FEditor: TSimbaEditor;

    procedure Reset;

    procedure AddCustomColor(CustomColor: TColor; ColorListBox: TColorListBox);
    function SelectedAttr: TSynHighlighterAttributes;
  public
    property Editor: TSimbaEditor read FEditor;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  inifiles,
  simba.settings, simba.editor_attributes, simba.files;

const
  DARK_DEFAULT =
    '[Highlighter.Assembler]                     ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Case label]                    ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Comment]                       ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=6189429                          ' + LineEnding +
    'Style=1                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Directive]                     ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=2070525                          ' + LineEnding +
    'Style=1                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.IDE Directive]                 ' + LineEnding +
    'Background=3292478                          ' + LineEnding +
    'Foreground=2070525                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Identifier]                    ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=15923448                         ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Method Type]                   ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=1562982                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Number]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=7658470                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Reserved word]                 ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=15718758                         ' + LineEnding +
    'Style=1                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Space]                         ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.String]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=7658470                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Highlighter.Symbol]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=5506787                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Word Group]                         ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=2                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=16744878                              ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Highlight All Caret]                ' + LineEnding +
    'Background=8421504                          ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=8421504                               ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Line Highlight]                     ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Folded Code]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=1562982                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=59900                                 ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Folded Code Line]                   ' + LineEnding +
    'Background=4081737                          ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Bracket Match]                      ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=3                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=255                                   ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Selected]                           ' + LineEnding +
    'Background=7697781                          ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Highlight All]                      ' + LineEnding +
    'Background=-2147483635                      ' + LineEnding +
    'Foreground=-2147483634                      ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Mouse Link]                         ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=16711680                         ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Background]                         ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=3815994                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Indent Line]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Caret]                              ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=15718758                         ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Right Edge]                         ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=6189429                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Editor.Divider]                            ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=6189429                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Background]                   ' + LineEnding +
    'Background=7697781                          ' + LineEnding +
    'Foreground=3815994                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Border]                       ' + LineEnding +
    'Background=0                                ' + LineEnding +
    'Foreground=6579300                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Identifier]                   ' + LineEnding +
    'Background=0                                ' + LineEnding +
    'Foreground=11162978                         ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Filter]                       ' + LineEnding +
    'Background=0                                ' + LineEnding +
    'Foreground=5263440                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Alternating]                  ' + LineEnding +
    'Background=7697781                          ' + LineEnding +
    'Foreground=3815994                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[AutoComplete.Selected]                     ' + LineEnding +
    'Background=0                                ' + LineEnding +
    'Foreground=4934475                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Background]                         ' + LineEnding +
    'Background=0                                ' + LineEnding +
    'Foreground=3815994                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=0                                     ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Seperator]                          ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=536870911                        ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Changes]                            ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=32768                            ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=59900                                 ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Marks]                              ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=2070525                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Code Fold]                          ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=6189429                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ' + LineEnding +
    '                                            ' + LineEnding +
    '[Gutter.Line Number]                        ' + LineEnding +
    'Background=536870911                        ' + LineEnding +
    'Foreground=6189429                          ' + LineEnding +
    'Style=0                                     ' + LineEnding +
    'StyleMask=0                                 ' + LineEnding +
    'Frame=536870911                             ';

procedure TEditorColorsFrame.TreeViewSelectionChanged(Sender: TObject);
begin
  if SelectedAttr <> nil then
  begin
    ForegoundColorBox.ItemIndex := ForegoundColorBox.Items.IndexOfObject(TObject(PtrUInt(SelectedAttr.Foreground)));
    if ForegoundColorBox.ItemIndex = -1 then
    begin
      ForegoundColorBox.Items.Objects[0] := TObject(PtrUInt(SelectedAttr.Foreground));
      ForegoundColorBox.ItemIndex := 0;
    end;

    BackgroundColorBox.ItemIndex := BackgroundColorBox.Items.IndexOfObject(TObject(PtrUInt(SelectedAttr.Background)));
    if BackgroundColorBox.ItemIndex = -1 then
    begin
      BackgroundColorBox.Items.Objects[0] := TObject(PtrUInt(SelectedAttr.Background));
      BackgroundColorBox.ItemIndex := 0;
    end;

    FrameColorBox.ItemIndex := FrameColorBox.Items.IndexOfObject(TObject(PtrUInt(SelectedAttr.FrameColor)));
    if FrameColorBox.ItemIndex = -1 then
    begin
      FrameColorBox.Items.Objects[0] := TObject(PtrUInt(SelectedAttr.FrameColor));
      FrameColorBox.ItemIndex := 0;
    end;

    Panel3.Visible := SelectedAttr is TSimbaEditor_Attribute;
    if Panel3.Visible then
      Panel3.BringToFront()
    else
      Panel3.SendToBack();

    if SelectedAttr is TSimbaEditor_Attribute then
      Label1.Caption := 'Color:'
    else
      Label1.Caption := 'Foreground:';

    BoldCheckBox.Checked := fsBold in SelectedAttr.Style;
    ItalicCheckBox.Checked := fsItalic in SelectedAttr.Style;
    UnderlineCheckBox.Checked := fsUnderline in SelectedAttr.Style;
    StrikeCheckBox.Checked := fsStrikeOut in SelectedAttr.Style;
  end;
end;

procedure TEditorColorsFrame.Reset();

  procedure AddAttribute(Attri: TSynHighlighterAttributes);
  var
    Node: TTreeNode;
    Name: TStringArray;
  begin
    Name := Attri.StoredName.Split(['.']);

    if Length(Name) = 2 then
    begin
      Node := TreeView.Items.FindNodeWithText(Name[0]);
      if Node = nil then
        Node := TreeView.Items.Add(nil, Name[0]);

      Node := TreeView.Items.AddChild(Node, Name[1]);
      Node.Data := Attri;
    end;
  end;

var
  I: Int32;
begin
  if (FEditor <> nil) then
    FEditor.Free();

  FEditor := TSimbaEditor.Create(Self);
  with FEditor do
  begin
    Parent := Panel2;
    Align := alClient;
    ReadOnly := True;
    Text := 'program Highlight;                                      '+ LineEnding +
            '{comment}                                               '+ LineEnding +
            '{$I SRL/osr.simba}                                      '+ LineEnding +
            '                                                        '+ LineEnding +
            '// this function does stuff                             '+ LineEnding +
            'procedure Test(var I: Int32);                           '+ LineEnding +
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

  TreeView.Items.Clear();

  for I := 0 to FEditor.Attributes.Count - 1 do
    AddAttribute(FEditor.Attributes[I]);

  TreeView.AlphaSort();
end;

procedure TEditorColorsFrame.AddCustomColor(CustomColor: TColor; ColorListBox: TColorListBox);
var
  I: Int32;
begin
  for I := 1 to ColorListBox.Count - 1 do
  begin
    if (CustomColor = ColorListBox.Colors[I]) then
    begin
      ColorListBox.ItemIndex := I;

      Exit;
    end;
  end;

  ColorListBox.Items.AddObject('Custom Color (%d, %d, %d)', [Red(UInt32(CustomColor)), Green(UInt32(CustomColor)), Blue(UInt32(CustomColor))], TObject(PtrUInt(CustomColor)));
end;

function TEditorColorsFrame.SelectedAttr: TSynHighlighterAttributes;
begin
  Result := nil;
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    Result := TSynHighlighterAttributes(TreeView.Selected.Data);
end;

procedure TEditorColorsFrame.SaveAsButtonClick(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  try
    InitialDir := GetDataPath();
    Title := 'Save Editor Colors';
    Filter := 'INI Files (*.ini)|*.ini';
    DefaultExt := 'ini';

    if Execute then
    begin
      if ExtractFileExt(FileName) <> '.ini' then
        FileName := FileName + '.ini';
      FEditor.SaveColors(FileName);

      SimbaSettings.Editor.ColorsPath.Value := FileName;
    end;
  finally
    Free();
  end;
end;

procedure TEditorColorsFrame.LoadButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    InitialDir := GetDataPath();
    Title := 'Load Editor Colors';
    Filter := 'INI Files (*.ini)|*.ini';

    if Execute then
      FEditor.LoadColors(FileName);
  finally
    Free();
  end;
end;

procedure TEditorColorsFrame.FontStyleChangeHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if SelectedAttr <> nil then
  begin
    SelectedAttr.BeginUpdate();
    SelectedAttr.Style := [];

    if BoldCheckBox.Checked then
      SelectedAttr.Style := SelectedAttr.Style + [fsBold];
    if ItalicCheckBox.Checked then
      SelectedAttr.Style := SelectedAttr.Style + [fsItalic];
    if UnderlineCheckBox.Checked then
      SelectedAttr.Style := SelectedAttr.Style + [fsUnderline];
    if StrikeCheckBox.Checked then
      SelectedAttr.Style := SelectedAttr.Style + [fsStrikeOut];

    SelectedAttr.EndUpdate();
  end;
end;

procedure TEditorColorsFrame.ForegoundColorBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (SelectedAttr <> nil) and (ForegoundColorBox.ItemIndex > -1) then
    SelectedAttr.Foreground := ForegoundColorBox.Selected;

  if ForegoundColorBox.ItemIndex = 0 then
    AddCustomColor(ForegoundColorBox.Selected, ForegoundColorBox);
end;

procedure TEditorColorsFrame.BackgroundColorBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (SelectedAttr <> nil) and (BackgroundColorBox.ItemIndex > -1) then
    SelectedAttr.Background := BackgroundColorBox.Selected;

  if BackgroundColorBox.ItemIndex = 0 then
    AddCustomColor(BackgroundColorBox.Selected, BackgroundColorBox);
end;

procedure TEditorColorsFrame.FrameColorBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (SelectedAttr <> nil) and (FrameColorBox.ItemIndex > -1) then
    SelectedAttr.FrameColor := FrameColorBox.Selected;

  if FrameColorBox.ItemIndex = 0 then
    AddCustomColor(FrameColorBox.Selected, FrameColorBox);
end;

procedure TEditorColorsFrame.ResetDarkButtonClick(Sender: TObject);
var
  INI: TIniFile;
  I: Int32;
begin
  INI := TIniFile.Create(TStringStream.Create(DARK_DEFAULT));

  for I := 0 to FEditor.Attributes.Count - 1 do
    FEditor.Attributes[I].Load(INI);

  INI.Stream.Free();
  INI.Free();
end;

procedure TEditorColorsFrame.ResetButtonClick(Sender: TObject);
begin
  Reset();
end;

constructor TEditorColorsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Reset();

  BackgroundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  ForegoundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  FrameColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));

  BackgroundColorBox.Height := Scale96ToScreen(BackgroundColorBox.Height);
  ForegoundColorBox.Height := Scale96ToScreen(ForegoundColorBox.Height);
  FrameColorBox.Height := Scale96ToScreen(FrameColorBox.Height);
end;

initialization
  {$I simba.settingsform_editor_colors.lrs}

end.

