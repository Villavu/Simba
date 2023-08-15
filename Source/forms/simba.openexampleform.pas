{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openexampleform;

// "Project options > Resources" to add examples

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls, ExtCtrls,
  simba.mufasatypes, simba.editor,
  simba.component_treeview, simba.component_buttonpanel;

type
  TSimbaOpenExampleForm = class(TForm)
    TreeView: TSimbaTreeView;
    Editor: TSimbaEditor;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    ButtonPanel: TSimbaButtonPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SplitterPaint(Sender: TObject);
  protected
    procedure DoButtonOkClick(Sender: TObject);
    procedure DoSplitterEnterExit(Sender: TObject);
    procedure DoTreeViewSelectionChanged(Sender: TObject);

    procedure AddPackageExamples;
  end;

var
  SimbaOpenExampleForm: TSimbaOpenExampleForm;

implementation

{$R *.lfm}

uses
  LCLType, AnchorDocking,
  simba.main, simba.package, simba.scripttabsform, simba.files, simba.theme;

function ReadResourceString(ResourceName: String): String;
begin
  Result := '';

  with TResourceStream.Create(HINSTANCE, ResourceName, RT_RCDATA) do
  try
    SetLength(Result, Size);

    Read(Result[1], Size);
  finally
    Free();
  end;
end;

type
  TExampleNode = class(TTreeNode)
  public
    FileName: String;
    Script: String;
  end;

procedure TSimbaOpenExampleForm.AddPackageExamples;

  procedure AddNode(const AParentNode: TTreeNode; const AFileName: String);
  begin
    with TExampleNode(TreeView.AddNode(AParentNode, TSimbaPath.PathExtractNameWithoutExt(AFileName), IMG_SIMBA)) do
      FileName := AFileName;
  end;

var
  I: Integer;
  ParentNode: TTreeNode;
  FileName: String;
  Files: TStringArray;
  Packages: TSimbaPackageArray;
  Str: String;
begin
  TreeView.BeginUpdate();
  for I := 1 to TreeView.Items.TopLvlCount - 1 do // I := 1 to skip Simba node
    TreeView.Items.TopLvlItems[I].Free();

  Packages := GetInstalledPackages();
  for I := 0 to High(Packages) do
  begin
    Files := Packages[I].GetExamples();
    if (Length(Files) = 0) then
      Continue;

    Str := Packages[I].URL;
    while (Str.Count('/') > 1) do
      Str := Str.After('/');

    ParentNode := TreeView.AddNode(Str, IMG_PACKAGE);
    for FileName in Files do
      AddNode(ParentNode, FileName);
  end;
  FreePackages(Packages);

  TreeView.EndUpdate();
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  Splitter.Width := DockMaster.SplitterWidth;

  AddPackageExamples();

  TreeView.ClearSelection();
  TreeView.FullExpand();
end;

procedure TSimbaOpenExampleForm.SplitterPaint(Sender: TObject);
begin
  with TSplitter(Sender) do
  begin
    Canvas.Brush.Color := SimbaTheme.ColorFrame;
    Canvas.FillRect(ClientRect);

    if MouseInClient then
    begin
      Canvas.Brush.Color := SimbaTheme.ColorActive;
      Canvas.FillRect(3, 3, Width-3, Height-3);
    end;
  end;
end;

procedure TSimbaOpenExampleForm.DoButtonOkClick(Sender: TObject);
begin
  if Editor.Visible then
  begin
    SimbaScriptTabsForm.AddTab().Editor.Text := Editor.Text;
    if (Sender is TTreeView) then
      Close();
  end;
end;

procedure TSimbaOpenExampleForm.DoSplitterEnterExit(Sender: TObject);
begin
  Splitter.Invalidate();
end;

procedure TSimbaOpenExampleForm.FormCreate(Sender: TObject);
var
  SimbaNode: TTreeNode;

  procedure AddNode(const Name, ResourceName: String);
  begin
    with TExampleNode(TreeView.AddNode(SimbaNode, Name, IMG_SIMBA)) do
      Script := ReadResourceString(ResourceName);
  end;

begin
  Width := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);

  TreeView := TSimbaTreeView.Create(Self, TExampleNode);
  TreeView.Parent := LeftPanel;
  TreeView.Align := alClient;
  TreeView.OnSelectionChange := @DoTreeViewSelectionChanged;
  TreeView.OnDoubleClick := @DoButtonOkClick;

  Editor := TSimbaEditor.Create(Self);
  Editor.Parent := RightPanel;
  Editor.Align := alClient;
  Editor.Visible := False;

  Splitter.OnEnter := @DoSplitterEnterExit;
  Splitter.OnExit := @DoSplitterEnterExit;

  RightPanel.Color := SimbaTheme.ColorBackground;
  RightPanel.Font.Color := SimbaTheme.ColorFont;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.ButtonOk.OnClick := @DoButtonOkClick;

  SimbaNode := TExampleNode(TreeView.AddNode('Simba', IMG_PACKAGE));

  AddNode('Array',     'EXAMPLE_ARRAY'    );
  AddNode('Bitmap',    'EXAMPLE_BITMAP'   );
  AddNode('Function',  'EXAMPLE_FUNCTION' );
  AddNode('Loop',      'EXAMPLE_LOOP'     );
  AddNode('Procedure', 'EXAMPLE_PROCEDURE');
  AddNode('Timing',    'EXAMPLE_TIMING'   );
end;

procedure TSimbaOpenExampleForm.DoTreeViewSelectionChanged(Sender: TObject);
var
  Node: TExampleNode;
begin
  Node := TExampleNode(TreeView.Selected);

  if (Node is TExampleNode) then
  begin
    if (Node.Script <> '') then
    begin
      Editor.Lines.Text := Node.Script;
      Editor.Visible := True;
    end else
    if (Node.FileName <> '') and TSimbaFile.FileExists(Node.FileName) then
    begin
      Editor.Lines.LoadFromFile(Node.FileName);
      Editor.Visible := True;
    end else
      Editor.Visible := False;
  end else
    Editor.Visible := False;
end;

end.

