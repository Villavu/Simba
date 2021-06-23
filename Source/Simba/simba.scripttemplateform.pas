unit simba.scripttemplateform;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, ExtCtrls, SynEdit, SynHighlighterPas;

type
  TSimbaScriptTemplateForm = class(TForm)
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
    procedure SimbaSettingChanged_FontName(Value: String);
    procedure SimbaSettingChanged_FontSize(Value: Int64);

    procedure LoadTemplates;
  end;

var
  SimbaScriptTemplateForm: TSimbaScriptTemplateForm;

implementation

uses
  FileUtil, LazFileUtils, Math, LazLoggerBase,
  simba.main, simba.package_form, simba.package, simba.settings, simba.scripttabsform;

function GetTemplates(Directory: String): TStringArray;
var
  Files: TStringList;
begin
  Result := Default(TStringArray);

  if DirectoryExists(Directory) then
  begin
    Files := FindAllFiles(Directory, '*.simba');

    try
      Result := Files.ToStringArray();
    finally
      Files.Free();
    end;
  end;
end;

type
  TScriptTemplateTreeNode = class(TTreeNode)
  protected
    FFileName: String;

    procedure SetFileName(Value: String);
  public
    property FileName: String read FFileName write SetFileName;

    constructor Create(AOwner: TTreeNodes); override;
  end;

procedure TScriptTemplateTreeNode.SetFileName(Value: String);
begin
  FFileName := Value;

  if (FFileName <> '') then
  begin
    ImageIndex    := IMAGE_SIMBA;
    SelectedIndex := IMAGE_SIMBA;
  end;
end;

constructor TScriptTemplateTreeNode.Create(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);

  ImageIndex    := IMAGE_PACKAGE;
  SelectedIndex := IMAGE_PACKAGE;
end;

procedure TSimbaScriptTemplateForm.LoadTemplates;
var
  Packages: TSimbaPackageList;
  I, MaxWidth: Integer;
  Node: TTreeNode;
  Template: String;
begin
  TreeView.BeginUpdate();
  TreeView.Items.Clear();

  Packages := TSimbaPackageList.Create();
  try
    Packages.Load();

    for I := 0 to Packages.Count - 1 do
      with Packages[I].InstalledData do
      begin
        if (Name = '') then
          Continue;

        Node := TreeView.Items.Add(nil, Name);
        for Template in GetTemplates(Templates) do
          TScriptTemplateTreeNode(TreeView.Items.AddChild(Node, ExtractFileNameOnly(Template))).FileName := Template;
      end;
  finally
    Packages.Free();
  end;

  MaxWidth := 0;
  for I := 0 to TreeView.Items.Count - 1 do
    MaxWidth := Max(MaxWidth, TreeView.Items[I].DisplayTextRight + 15);

  TreeView.Width := MaxWidth;
  TreeView.EndUpdate();
end;

procedure TSimbaScriptTemplateForm.SimbaSettingChanged_FontName(Value: String);
begin
  if (Value <> '') then
    Editor.Font.Name := Value;
end;

procedure TSimbaScriptTemplateForm.SimbaSettingChanged_FontSize(Value: Int64);
begin
  Editor.Font.Size := Value;
end;

procedure TSimbaScriptTemplateForm.FormShow(Sender: TObject);
begin
  Editor.Visible := False;

  LoadTemplates();
end;

procedure TSimbaScriptTemplateForm.ButtonOkClick(Sender: TObject);
var
  Node: TScriptTemplateTreeNode;
begin
  Node := TreeView.Selected as TScriptTemplateTreeNode;
  if (Node <> nil) and (Node.FileName <> '') then
    with SimbaScriptTabsForm.AddTab() do
    begin
      ScriptName := 'Untitled';

      if Editor.Load(Node.FileName) and (Editor.OnChange <> nil) then
      begin
        Editor.FileName := '';
        Editor.OnChange(Editor);
      end;
    end;
end;

procedure TSimbaScriptTemplateForm.FormCreate(Sender: TObject);
begin
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
    NestedComments := False;
    StringKeywordMode := spsmNone;
  end;

  if (SimbaSettings <> nil) then
  begin
    SimbaSettings.Editor.FontName.AddOnChangeHandler(@SimbaSettingChanged_FontName).Changed();
    SimbaSettings.Editor.FontSize.AddOnChangeHandler(@SimbaSettingChanged_FontSize).Changed();
  end;

  Editor.Font.Quality := fqDefault;
end;

procedure TSimbaScriptTemplateForm.FormDestroy(Sender: TObject);
begin
  if (SimbaSettings <> nil) then
  begin
    SimbaSettings.Editor.FontName.RemoveOnChangeHandler(@SimbaSettingChanged_FontName);
    SimbaSettings.Editor.FontSize.RemoveOnChangeHandler(@SimbaSettingChanged_FontSize);
  end;
end;

procedure TSimbaScriptTemplateForm.TreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TScriptTemplateTreeNode;
end;

procedure TSimbaScriptTemplateForm.TreeViewSelectionChanged(Sender: TObject);
var
  Node: TScriptTemplateTreeNode;
begin
  Node := TreeView.Selected as TScriptTemplateTreeNode;

  Editor.Visible := (Node <> nil) and (Node.FileName <> '');
  if Editor.Visible then
  try
    Editor.Lines.LoadFromFile(Node.FileName);
  except
    on E: Exception do
      DebugLn('Error opening template: ', E.Message);
  end;
end;

initialization
  {$I simba.scripttemplateform.lrs}

end.

