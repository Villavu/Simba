{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_editor_colors;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics,
  Dialogs, ColorBox, CheckLst, Spin, SynEditHighlighter, DividerBevel,
  simba.editor;

type
  TEditorColorsFrame = class(TFrame)
    ButtonResetAttribute: TButton;
    ButtonLoadFromURL: TButton;
    CheckListBox1: TCheckListBox;
    DividerBevel1: TDividerBevel;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AlphaLabel: TLabel;
    LabelBackground: TLabel;
    Panel2: TPanel;
    FrameColorBox: TColorListBox;
    LabelFrame: TLabel;
    Panel3: TPanel;
    AlphaPanel: TPanel;
    SaveAsButton: TButton;
    LoadButton: TButton;
    BackgroundColorBox: TColorListBox;
    ForegoundColorBox: TColorListBox;
    LabelForeground: TLabel;
    Panel1: TPanel;
    ResetButton: TButton;
    ForegroundAlphaEdit: TSpinEdit;
    BackgroundAlphaEdit: TSpinEdit;
    TreeView: TTreeView;

    procedure BackgroundAlphaEditChange(Sender: TObject);
    procedure ButtonLoadFromURLClick(Sender: TObject);
    procedure ButtonResetAttributeClick(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure ForegroundAlphaEditChange(Sender: TObject);
    procedure FrameColorBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure FrameResize(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure DoSaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ForegoundColorBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure BackgroundColorBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure DoResetButtonClick(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewSelectionChanged(Sender: TObject);
  protected
    FEditor: TSimbaEditor;

    procedure CreateHandle; override;

    procedure SelectColor(CustomColor: TColor; ColorListBox: TColorListBox);
    function SelectedAttr: TSynHighlighterAttributes;
  public
    property Editor: TSimbaEditor read FEditor;

    procedure Load;
    procedure Save;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  SynEditMiscClasses,
  simba.settings, simba.editor_attributes, simba.env, simba.mufasatypes,
  simba.httpclient;

type
  TNodeData = record
    Attri: TSynHighlighterAttributes;
    DefaultAttri: TSynHighlighterAttributes;
  end;

procedure TEditorColorsFrame.TreeViewSelectionChanged(Sender: TObject);
begin
  if SelectedAttr <> nil then
  begin
    SelectColor(SelectedAttr.Foreground, ForegoundColorBox);
    SelectColor(SelectedAttr.Background, BackgroundColorBox);
    SelectColor(SelectedAttr.FrameColor, FrameColorBox);

    if SelectedAttr is TSynSelectedColor then
    begin
      BackgroundAlphaEdit.Value := TSynSelectedColor(SelectedAttr).BackAlpha;
      ForegroundAlphaEdit.Value := TSynSelectedColor(SelectedAttr).ForeAlpha;

      AlphaLabel.Show();
      AlphaPanel.Show();
    end else
    begin
      AlphaLabel.Hide();
      AlphaPanel.Hide();
    end;

    if SelectedAttr is TSimbaEditor_Attribute then
    begin
      LabelForeground.Caption := 'Color';

      BackgroundColorBox.Hide();
      LabelBackground.Hide();

      FrameColorBox.Hide();
      LabelFrame.Hide();
    end else
    begin
      LabelForeground.Caption := 'Foreground';

      BackgroundColorBox.Show();
      LabelBackground.Show();

      FrameColorBox.Show();
      LabelFrame.Show();
    end;

    CheckListBox1.Checked[0] := fsBold in SelectedAttr.Style;
    CheckListBox1.Checked[1] := fsItalic in SelectedAttr.Style;
    CheckListBox1.Checked[2] := fsUnderline in SelectedAttr.Style;
  end;
end;

procedure TEditorColorsFrame.CreateHandle;
begin
  inherited CreateHandle;

  CheckListBox1.Height := CheckListBox1.ItemRect(2).Bottom + 5;
end;

procedure TEditorColorsFrame.Load;
begin
  FEditor.Attributes.LoadFromFile(SimbaSettings.Editor.CustomColors.Value);

  TreeView.Selected := TreeView.Items.FindNodeWithText('Background');
end;

procedure TEditorColorsFrame.SelectColor(CustomColor: TColor; ColorListBox: TColorListBox);
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 1 to ColorListBox.Items.Count - 1 do
    if (ColorListBox.Items.Objects[I] = TObject(PtrUInt(CustomColor))) then
    begin
      Found := True;
      Break;
    end;

  if (not Found) then
    ColorListBox.Items.AddObject('Custom (%d, %d, %d)', [Red(UInt32(CustomColor)), Green(UInt32(CustomColor)), Blue(UInt32(CustomColor))], TObject(PtrUInt(CustomColor)));

  ColorListBox.Selected := CustomColor;
  ColorListBox.MakeCurrentVisible();
end;

function TEditorColorsFrame.SelectedAttr: TSynHighlighterAttributes;
begin
  Result := nil;
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    Result := TNodeData(TreeView.Selected.Data^).Attri;
end;

procedure TEditorColorsFrame.Save;
var
  FileName: String;
begin
  FileName := GetDataPath() + 'colors_editor.ini';

  FEditor.Attributes.SaveToFile(FileName);

  SimbaSettings.Editor.CustomColors.Value := FileName;
  SimbaSettings.Changed(SimbaSettings.Editor.CustomColors);
end;

procedure TEditorColorsFrame.DoSaveButtonClick(Sender: TObject);
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

procedure TEditorColorsFrame.ForegoundColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (ForegoundColorBox.ItemIndex > -1) then
    SelectedAttr.Foreground := ForegoundColorBox.Selected;
  SelectColor(SelectedAttr.Foreground, ForegoundColorBox);

  FEditor.Invalidate();
end;

procedure TEditorColorsFrame.BackgroundColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (BackgroundColorBox.ItemIndex > -1) then
    SelectedAttr.Background := BackgroundColorBox.Selected;
  SelectColor(SelectedAttr.Background, BackgroundColorBox);

  FEditor.Invalidate();
end;

procedure TEditorColorsFrame.DoResetButtonClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Items.GetFirstNode();
  while Assigned(Node) do
  begin
    if Assigned(Node.Data) then
      with TNodeData(Node.Data^) do
        Attri.Assign(DefaultAttri);

    Node := Node.GetNext();
  end;

  FEditor.Invalidate();
end;

procedure TEditorColorsFrame.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    FreeMem(Node.Data);
end;

procedure TEditorColorsFrame.FrameColorBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if (SelectedAttr <> nil) and (FrameColorBox.ItemIndex > -1) then
    SelectedAttr.FrameColor := FrameColorBox.Selected;
  SelectColor(SelectedAttr.FrameColor, FrameColorBox);

  FEditor.Invalidate();
end;

procedure TEditorColorsFrame.FrameResize(Sender: TObject);
begin
  Panel3.Width := Width - (TreeView.Width + 20);
end;

procedure TEditorColorsFrame.ButtonResetAttributeClick(Sender: TObject);
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
  begin
    with TNodeData(TreeView.Selected.Data^) do
      Attri.Assign(DefaultAttri);
    FEditor.Invalidate();
    TreeViewSelectionChanged(nil);
  end;
end;

procedure TEditorColorsFrame.CheckListBox1ClickCheck(Sender: TObject);
begin
  if SelectedAttr <> nil then
  begin
    SelectedAttr.BeginUpdate();
    SelectedAttr.Style := [];

    if CheckListBox1.Checked[0] then
      SelectedAttr.Style := SelectedAttr.Style + [fsBold];
    if CheckListBox1.Checked[1] then
      SelectedAttr.Style := SelectedAttr.Style + [fsItalic];
    if CheckListBox1.Checked[2] then
      SelectedAttr.Style := SelectedAttr.Style + [fsUnderline];

    SelectedAttr.EndUpdate();
  end;
end;

procedure TEditorColorsFrame.ForegroundAlphaEditChange(Sender: TObject);
begin
  if SelectedAttr is TSynSelectedColor then
    TSynSelectedColor(SelectedAttr).ForeAlpha := TSpinEdit(Sender).Value;
end;

procedure TEditorColorsFrame.ButtonLoadFromURLClick(Sender: TObject);
var
  Value, Contents: String;
begin
  if InputQuery('Simba - Editor Colors', 'Enter URL', Value) then
  try
    Contents := TSimbaHTTPClient.SimpleGet(Value, [EHTTPStatus.OK]);
    if (Contents <> '') then
      FEditor.Attributes.LoadFromStream(TStringStream.Create(Contents), True);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TEditorColorsFrame.BackgroundAlphaEditChange(Sender: TObject);
begin
  if SelectedAttr is TSynSelectedColor then
    TSynSelectedColor(SelectedAttr).BackAlpha := TSpinEdit(Sender).Value;
end;

procedure TEditorColorsFrame.Panel2Resize(Sender: TObject);
begin
  ForegoundColorBox.Width  := (TPanel(Sender).Width - 25) div 3;
  BackgroundColorBox.Width := (TPanel(Sender).Width - 25) div 3;
  FrameColorBox.Width      := (TPanel(Sender).Width - 25) div 3;
  CheckListBox1.Width      := (TPanel(Sender).Width - 25) div 3;
end;

constructor TEditorColorsFrame.Create(AOwner: TComponent);

  procedure AddAttribute(Attri: TSynHighlighterAttributes; DefaultAttr: TSynHighlighterAttributes);
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
      Node.Data := GetMem(SizeOf(TNodeData));

      TNodeData(Node.Data^).Attri := Attri;
      TNodeData(Node.Data^).DefaultAttri := DefaultAttr;
    end;
  end;

var
  DefaultEditor: TSimbaEditor;
  I: Integer;
begin
  inherited Create(AOwner);

  BackgroundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  ForegoundColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));
  FrameColorBox.Items.AddObject('Transparent', TObject(PtrUInt(clNone)));

  BackgroundColorBox.Height := Scale96ToScreen(BackgroundColorBox.Height);
  ForegoundColorBox.Height := Scale96ToScreen(ForegoundColorBox.Height);
  FrameColorBox.Height := Scale96ToScreen(FrameColorBox.Height);

  FEditor := TSimbaEditor.Create(Self);

  with FEditor do
  begin
    Parent := Panel3;
    Align := alClient;
    ReadOnly := True;
    Text := 'program Highlight;                                      ' + LineEnding +
            '{  brace }                                              ' + LineEnding +
            '(* round *)                                             ' + LineEnding +
            '// slash                                                ' + LineEnding +
            '                                                        ' + LineEnding +
            '{$I SRL/osr.simba}                                      ' + LineEnding +
            '                                                        ' + LineEnding +
            'procedure Test(var i: Integer);                         ' + LineEnding +
            'var                                                     ' + LineEnding +
            '  s: String;                                            ' + LineEnding +
            'begin                                                   ' + LineEnding +
            '  i := 1000 * (5 + 7);                                  ' + LineEnding +
            '  s := ' + #39 + 'The number is :' + #39 + ' + ToStr(i);' + LineEnding +
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

    UseSimbaColors := True;
  end;

  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  DefaultEditor := TSimbaEditor.Create(Self);
  for I := 0 to High(FEditor.Attributes.Attributes) do
    AddAttribute(FEditor.Attributes.Attributes[I], DefaultEditor.Attributes.Attributes[I]);

  TreeView.AlphaSort();
  TreeView.EndUpdate();
end;

end.

