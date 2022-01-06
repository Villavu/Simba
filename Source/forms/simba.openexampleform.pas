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
    procedure TreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
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
  lazfileutils, lazloggerbase,
  simba.fonthelpers, simba.main, simba.package, simba.scripttabsform;

type
  TCustomNode = class(TTreeNode)
  public
    FileName: String;
  end;

procedure TSimbaOpenExampleForm.LoadExamples;
var
  Packages: TSimbaPackageList;
  Examples: TStringArray;
  I, J: Integer;
  ParentNode: TTreeNode;
begin
  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  Packages := LoadPackages();
  try
    for I := 0 to Packages.Count - 1 do
    begin
      Examples := Packages[I].InstalledExamples;
      if (Length(Examples) = 0) then
        Continue;

      ParentNode := TreeView.Items.Add(nil, Packages[I].InstalledName);
      ParentNode.ImageIndex := IMAGE_PACKAGE;
      ParentNode.SelectedIndex := IMAGE_PACKAGE;

      for J := 0 to High(Examples) do
      begin
        with TreeView.Items.AddChild(ParentNode, ExtractFileNameOnly(Examples[J])) as TCustomNode do
        begin
          SelectedIndex := IMAGE_SIMBA;
          ImageIndex := IMAGE_SIMBA;

          FileName := Examples[J];
        end;
      end;
    end;
  finally
    Packages.Free();
  end;

  TreeView.Width := TreeView.Canvas.TextWidth('SomePackageName - Example');
  TreeView.EndUpdate();

  Editor.Visible := False;
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  LoadExamples();
end;

procedure TSimbaOpenExampleForm.ButtonOkClick(Sender: TObject);
var
  Node: TCustomNode;
begin
  if (TreeView.Selected is TCustomNode) then
  begin
    Node := TreeView.Selected as TCustomNode;

    if FileExists(Node.FileName) then
    begin
      SimbaScriptTabsForm.AddTab().Load(Node.FileName, '', Node.Text);
      if (Sender = TreeView) then
        Close();
    end;
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

procedure TSimbaOpenExampleForm.TreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TCustomNode;
end;

procedure TSimbaOpenExampleForm.TreeViewSelectionChanged(Sender: TObject);
begin
  if (TreeView.Selected is TCustomNode) then
    with TreeView.Selected as TCustomNode do
    begin
      if (FileName <> '') and FileExists(FileName) then
      try
        Editor.Lines.LoadFromFile(FileName);
        Editor.Visible := True;
      except
        on E: Exception do
          DebugLn('[TSimbaOpenExampleForm.TreeViewSelectionChanged]: Exception "%s"', [E.Message]);
      end;
    end;
end;

end.

