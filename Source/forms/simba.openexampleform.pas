{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openexampleform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, comctrls, extctrls,
  buttonpanel, synedit, synhighlighterpas,
  simba.settings;

type
  TSimbaOpenExampleForm = class(TForm)
    ButtonPanel: TButtonPanel;
    EditorPanel: TPanel;
    Splitter: TSplitter;
    Editor: TSynEdit;
    Highlighter: TSynFreePascalSyn;
    TreeView: TTreeView;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
  public
    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure LoadExamples;
  end;

var
  SimbaOpenExampleForm: TSimbaOpenExampleForm;

implementation

{$R *.lfm}

uses
  lazloggerbase, lazfileutils,
  simba.main, simba.package, simba.fonthelpers, simba.scripttabsform;

procedure TSimbaOpenExampleForm.LoadExamples;
var
  I: Integer;
  ParentNode, Node: TTreeNode;
  Files: TStringList;
begin
  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  Files := GetPackageFiles('example');
  for I := 0 to Files.Count - 1 do
  begin
    ParentNode := TreeView.Items.FindTopLvlNode(Files.Names[I]);
    if (ParentNode = nil) then
    begin
      ParentNode := TreeView.Items.Add(nil, Files.Names[I]);
      ParentNode.ImageIndex := IMAGE_PACKAGE;
      ParentNode.SelectedIndex := IMAGE_PACKAGE;
    end;

    Node := TreeView.Items.AddChild(ParentNode, ExtractFileNameOnly(Files.ValueFromIndex[I]));
    Node.ImageIndex := IMAGE_SIMBA;
    Node.SelectedIndex := IMAGE_SIMBA;
    Node.Data := GetMem(SizeOf(ShortString));

    PShortString(Node.Data)^ := Files.ValueFromIndex[I];
  end;
  Files.Free();

  TreeView.Width := TreeView.Canvas.TextWidth('SomePackageName - Example');
  TreeView.EndUpdate();

  Editor.Visible := False;
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  LoadExamples();
end;

procedure TSimbaOpenExampleForm.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
    FreeMem(Node.Data);
end;

procedure TSimbaOpenExampleForm.ButtonOkClick(Sender: TObject);
var
  Node: TTreeNode;
  FileName: String;
begin
  Node := TreeView.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  FileName := PShortString(Node.Data)^;
  if FileExists(FileName) then
  begin
    SimbaScriptTabsForm.AddTab().Load(FileName, '', Node.Text);
    if (Sender = TreeView) then
      Close();
  end;
end;

procedure TSimbaOpenExampleForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.Editor.FontSize) then
    Editor.Font.Size := Setting.Value;

  if (Setting = SimbaSettings.Editor.FontName) then
  begin
    if SimbaFontHelpers.IsFontFixed(Setting.Value) then
      Editor.Font.Name := Setting.Value;
  end;

  if SimbaSettings.Editor.AntiAliased.Value then
    Editor.Font.Quality := fqCleartypeNatural
  else
    Editor.Font.Quality := fqNonAntialiased;
end;

procedure TSimbaOpenExampleForm.FormCreate(Sender: TObject);
begin
  Width := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);

  with Highlighter as TSynFreePascalSyn do
  begin
    CommentAttri.Foreground := clBlue;
    CommentAttri.Style := [fsBold];
    IdentifierAttri.Foreground := clDefault;
    NumberAttri.Foreground := clNavy;
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    DirectiveAttri.Foreground := clRed;
    DirectiveAttri.Style := [fsBold];
    NestedComments := True;
    StringKeywordMode := spsmNone;
    TypeHelpers := True;
    ExtendedKeywordsMode := True;
  end;

  SimbaSettingChanged(SimbaSettings.Editor.FontSize);
  SimbaSettingChanged(SimbaSettings.Editor.FontName);
  SimbaSettingChanged(SimbaSettings.Editor.AntiAliased);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
end;

procedure TSimbaOpenExampleForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);
end;

procedure TSimbaOpenExampleForm.TreeViewSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  FileName: String;
begin
  Node := TreeView.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  FileName := PShortString(Node.Data)^;
  if FileExists(FileName) then
  begin
    Editor.Lines.LoadFromFile(FileName);
    Editor.Visible := True;
  end;
end;

end.

