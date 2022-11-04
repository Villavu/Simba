{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openexampleform;

{$i simba.inc}
{$R ../../Examples/examples.res}

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

    procedure LoadPackageExamples;
  end;

var
  SimbaOpenExampleForm: TSimbaOpenExampleForm;

implementation

{$R *.lfm}

uses
  lazloggerbase, lazfileutils, lcltype,
  simba.main, simba.package, simba.fonthelpers, simba.scripttabsform;

procedure TSimbaOpenExampleForm.LoadPackageExamples;
var
  I: Integer;
  ParentNode, Node: TTreeNode;
  Files: TStringList;
begin
  TreeView.BeginUpdate();
  for I := 1 to TreeView.Items.TopLvlCount - 1 do
    TreeView.Items.TopLvlItems[I].Free();

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
    Node.Data := NewStr(Files.ValueFromIndex[I]);
  end;
  Files.Free();

  TreeView.Width := TreeView.Canvas.TextWidth('SomePackageName - Example');
  TreeView.EndUpdate();

  Editor.Visible := False;
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  LoadPackageExamples();
end;

procedure TSimbaOpenExampleForm.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
    DisposeStr(PString(Node.Data));
end;

procedure TSimbaOpenExampleForm.ButtonOkClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  SimbaScriptTabsForm.AddTab().Editor.Text := Editor.Text;
  if (Sender = TreeView) then
    Close();
end;

procedure TSimbaOpenExampleForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.Editor.FontSize) then
    Editor.Font.Size := Setting.Value;

  if (Setting = SimbaSettings.Editor.FontName) then
  begin
    if IsFontFixed(Setting.Value) then
      Editor.Font.Name := Setting.Value;
  end;

  if SimbaSettings.Editor.AntiAliased.Value then
    Editor.Font.Quality := fqCleartypeNatural
  else
    Editor.Font.Quality := fqNonAntialiased;
end;

function ExtractExamples(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name, Value: String;
begin
  Result := True;

  Name := ResourceName;
  if Name.EndsWith('.SIMBA') then
  begin
    with TResourceStream.Create(HINSTANCE, ResourceName, ResourceType) do
    try
      SetLength(Value, Size);
      Read(Value[1], Size);
    finally
      Free();
    end;

    TStringList(Param).Values[Name] := Value;
  end;
end;

procedure TSimbaOpenExampleForm.FormCreate(Sender: TObject);
var
  List: TStringList;
  SimbaNode, Node: TTreeNode;
  I: Integer;
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

  List := TStringList.Create();
  List.LineBreak := #0;
  EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractExamples, PtrInt(List));

  SimbaNode := TreeView.Items.Add(nil, 'Simba');
  SimbaNode.ImageIndex := IMAGE_PACKAGE;
  SimbaNode.SelectedIndex := IMAGE_PACKAGE;
  for I := 0 to List.Count - 1 do
  begin
    Node := TreeView.Items.AddChild(SimbaNode, ExtractFileNameWithoutExt(List.Names[I]).ToLower());
    Node.ImageIndex := IMAGE_SIMBA;
    Node.SelectedIndex := IMAGE_SIMBA;
    Node.Data := NewStr(List.ValueFromIndex[I]);
  end;

  List.Free();
end;

procedure TSimbaOpenExampleForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);
end;

procedure TSimbaOpenExampleForm.TreeViewSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  try
    if (Node.Parent.Text = 'Simba') then
      Editor.Lines.Text := PString(Node.Data)^
    else
      Editor.Lines.LoadFromFile(PString(Node.Data)^);
  except
  end;

  Editor.Visible := True;
end;

end.

