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

    procedure LoadTemplates;
  end;

var
  SimbaOpenExampleForm: TSimbaOpenExampleForm;

implementation

{$R *.lfm}

uses
  lazfileutils, lazloggerbase,
  simba.files, simba.fonthelpers, simba.main, simba.package, simba.scripttabsform;

type
  TFileTreeNode = class(TTreeNode)
  public
    FileName: String;
  end;

procedure TSimbaOpenExampleForm.LoadTemplates;

  procedure AddFilesInDirectory(Name, Directory: String);
  var
    Files: TStringArray;
    Node: TTreeNode;
    I: Integer;
  begin
    Files := GetFiles(Directory, 'simba');

    if (Length(Files) > 0) then
    begin
      Node := TreeView.Items.Add(nil, Name);
      Node.ImageIndex := IMAGE_PACKAGE;
      Node.SelectedIndex := IMAGE_PACKAGE;

      for I := 0 to High(Files) do
        with TreeView.Items.AddChild(Node, ExtractFileNameOnly(Files[I])) as TFileTreeNode do
        begin
          SelectedIndex := IMAGE_SIMBA;
          ImageIndex := IMAGE_SIMBA;

          FileName := ConcatPaths([Directory, Files[I]]);
        end;
    end;
  end;

var
  Packages: TSimbaPackageList;
  I: Integer;
  CaseMatch: TFilenameCaseMatch;
begin
  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  Packages := LoadPackages();
  try
    for I := 0 to Packages.Count - 1 do
      if DirectoryExists(Packages[I].InstalledVersion.Path) then
      begin
        AddFilesInDirectory(Packages[I].Name + ' - Examples', ExpandFileNameCase(Packages[I].InstalledVersion.Path + '/Examples', CaseMatch));
        AddFilesInDirectory(Packages[I].Name + ' - Tools', ExpandFileNameCase(Packages[I].InstalledVersion.Path + '/Tools', CaseMatch));
      end;
  finally
    Packages.Free();
  end;

  TreeView.Width := TreeView.Canvas.TextWidth('SomeSection - Examples');
  TreeView.EndUpdate();

  Editor.Visible := False;
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  LoadTemplates();
end;

procedure TSimbaOpenExampleForm.ButtonOkClick(Sender: TObject);
var
  Node: TFileTreeNode;
begin
  if (TreeView.Selected is TFileTreeNode) then
  begin
    Node := TreeView.Selected as TFileTreeNode;

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
  Width := 800;
  Height := 600;

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
  NodeClass := TFileTreeNode;
end;

procedure TSimbaOpenExampleForm.TreeViewSelectionChanged(Sender: TObject);
begin
  if (TreeView.Selected is TFileTreeNode) then
    with TreeView.Selected as TFileTreeNode do
    begin
      if (FileName <> '') and FileExists(FileName) then
      try
        Editor.Lines.LoadFromFile(FileName);
        Editor.Visible := True;
      except
        on E: Exception do
          DebugLn('Error opening template: ', E.Message);
      end;
    end;
end;

end.

