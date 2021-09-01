unit simba.settingsform_editor_colors;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics,
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
    EditorPanel: TPanel;
    Panel3: TPanel;
    TreeView: TTreeView;

    procedure ResetDarkButtonClick(Sender: TObject);
    procedure FrameColorBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ResetButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ForegoundColorBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure BackgroundColorBoxSelectionChange(Sender: TObject; User: Boolean);
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

{$R *.lfm}

uses
  simba.settings, simba.editor_attributes, simba.files;

{$i simba.editorcolorpresets.inc}

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

procedure TEditorColorsFrame.Reset;

  procedure AddAttribute(Attri: TSynHighlighterAttributes);
  var
    Node: TTreeNode;
    Name: TStringArray;
  begin
    Name := Attri.StoredName.Split(['.']);

    if (Length(Name) = 2) then
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
  FEditor := TSimbaEditor.Create(Self, FEditor = nil);

  with FEditor do
  begin
    Parent := EditorPanel;
    Align := alClient;
    ReadOnly := True;
    Text := 'program Highlight;                                      ' + LineEnding +
            '{  brace }                                              ' + LineEnding +
            '(* round *)                                             ' + LineEnding +
            '// slash                                                ' + LineEnding +
            '                                                        ' + LineEnding +
            '{$I SRL/osr.simba}                                      ' + LineEnding +
            '                                                        ' + LineEnding +
            'procedure Test(var i: Int32);                           ' + LineEnding +
            'var                                                     ' + LineEnding +
            '  s: String;                                            ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  i := 1000 * (5 + 7);                                  ' + LineEnding +
            '  s := ' + #39 + 'The number is :' + #39 + ' + ToStr(i);' + LineEnding +
            '                                                        ' + LineEnding +
            '  case Random(5) of                                     ' + LineEnding +
            '    1: ;                                                ' + LineEnding +
            '    2..4: ;                                             ' + LineEnding +
            '  end;                                                  ' + LineEnding +
            'end;                                                    ' + LineEnding +
            '                                                        ' + LineEnding +
            'function TPoint.Test: Boolean; overload;                ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  Result := True;                                       ' + LineEnding +
            'end;                                                    ';
  end;

  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  for I := 0 to High(FEditor.Attributes.Attributes) do
    AddAttribute(FEditor.Attributes.Attributes[I]);

  TreeView.AlphaSort();
  TreeView.EndUpdate();
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
    Filter := 'Colors Files (*.ini)|*.ini';
    FileName := 'colors';
    Options := [ofOverwritePrompt, ofNoReadOnlyReturn];

    if Execute then
    begin
      FileName := ChangeFileExt(FileName, '.ini');

      FEditor.Attributes.SaveToFile(FileName);

      SimbaSettings.Editor.CustomColors.Value := FileName;
      SimbaSettings.Changed(SimbaSettings.Editor.CustomColors);
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
      FEditor.Attributes.LoadFromFile(FileName);
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

procedure TEditorColorsFrame.ForegoundColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (ForegoundColorBox.ItemIndex > -1) then
    SelectedAttr.Foreground := ForegoundColorBox.Selected;

  if ForegoundColorBox.ItemIndex = 0 then
    AddCustomColor(ForegoundColorBox.Selected, ForegoundColorBox);
end;

procedure TEditorColorsFrame.BackgroundColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (BackgroundColorBox.ItemIndex > -1) then
    SelectedAttr.Background := BackgroundColorBox.Selected;

  if BackgroundColorBox.ItemIndex = 0 then
    AddCustomColor(BackgroundColorBox.Selected, BackgroundColorBox);
end;

procedure TEditorColorsFrame.FrameColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (FrameColorBox.ItemIndex > -1) then
    SelectedAttr.FrameColor := FrameColorBox.Selected;

  if FrameColorBox.ItemIndex = 0 then
    AddCustomColor(FrameColorBox.Selected, FrameColorBox);
end;

procedure TEditorColorsFrame.ResetDarkButtonClick(Sender: TObject);
begin
  FEditor.Attributes.LoadFromStream(TStringStream.Create(EDITOR_PRESET_DARK), True);
end;

procedure TEditorColorsFrame.ResetButtonClick(Sender: TObject);
begin
  Reset();
end;

constructor TEditorColorsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BackgroundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  ForegoundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  FrameColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));

  BackgroundColorBox.Height := Scale96ToScreen(BackgroundColorBox.Height);
  ForegoundColorBox.Height := Scale96ToScreen(ForegoundColorBox.Height);
  FrameColorBox.Height := Scale96ToScreen(FrameColorBox.Height);

  Reset();
end;

end.

