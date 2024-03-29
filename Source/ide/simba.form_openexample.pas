{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_openexample;

// "Project options > Resources" to add examples

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls, ExtCtrls,
  simba.base, simba.ide_editor,
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

    procedure UpdateTreeSize;

    procedure AddSimbaExamples;
  public
    procedure ClearPackageExamples;
    procedure AddPackageExamples(PackageName: String; Files: TStringArray);
  end;

var
  SimbaOpenExampleForm: TSimbaOpenExampleForm;

implementation

{$R *.lfm}

uses
  LCLType, AnchorDocking,
  simba.form_main, simba.form_tabs, simba.files, simba.ide_theme;

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

procedure TSimbaOpenExampleForm.AddSimbaExamples;
var
  SimbaNode: TTreeNode;

  procedure AddNode(const Name, ResourceName: String);
  begin
    with TExampleNode(TreeView.AddNode(SimbaNode, Name, IMG_SIMBA)) do
      Script := ReadResourceString(ResourceName);
  end;

begin
  TreeView.BeginUpdate();

  SimbaNode := TExampleNode(TreeView.AddNode('Simba', IMG_PACKAGE));

  AddNode('Array',                'EXAMPLE_ARRAY'               );
  AddNode('Image',                'EXAMPLE_IMAGE'               );
  AddNode('Function',             'EXAMPLE_FUNCTION'            );
  AddNode('Loop',                 'EXAMPLE_LOOP'                );
  AddNode('Stopwatch',            'EXAMPLE_STOPWATCH'           );
  AddNode('Point Cluster',        'EXAMPLE_CLUSTER_POINTS'      );
  AddNode('Mouse Teleport Event', 'EXAMPLE_MOUSE_TELEPORT_EVENT');
  AddNode('Static Method',        'EXAMPLE_STATIC_METHOD'       );
  AddNode('JSON',                 'EXAMPLE_JSON'                );
  AddNode('Form',                 'EXAMPLE_FORM'                );
  AddNode('Image Box',            'EXAMPLE_IMAGEBOX'            );
  AddNode('IRC',                  'EXAMPLE_IRC'                 );
  AddNode('Draw Text',            'EXAMPLE_DRAWTEXT'            );
  AddNode('RandomLeft',           'EXAMPLE_RANDOMLEFT'          );

  TreeView.EndUpdate();
end;

procedure TSimbaOpenExampleForm.ClearPackageExamples;
var
  I: Integer;
begin
  for I := TreeView.Items.TopLvlCount - 1 downto 1 do
    TreeView.Items.TopLvlItems[I].Free();
end;

procedure TSimbaOpenExampleForm.AddPackageExamples(PackageName: String; Files: TStringArray);
var
  I: Integer;
  Node: TTreeNode;
begin
  TreeView.BeginUpdate();
  if Assigned(TreeView.Items.FindTopLvlNode(PackageName)) then
    TreeView.Items.FindTopLvlNode(PackageName).Free();

  Node := TreeView.AddNode(PackageName, IMG_PACKAGE);
  for I := 0 to High(Files) do
    with TExampleNode(TreeView.AddNode(Node, TSimbaPath.PathExtractNameWithoutExt(Files[I]), IMG_SIMBA)) do
      FileName := Files[I];

  TreeView.EndUpdate();

  UpdateTreeSize();
end;

procedure TSimbaOpenExampleForm.FormShow(Sender: TObject);
begin
  Splitter.Width := DockMaster.SplitterWidth;

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
    SimbaTabsForm.AddTab().Editor.Text := Editor.Text;
    if (Sender is TTreeView) then
      Close();
  end;
end;

procedure TSimbaOpenExampleForm.DoSplitterEnterExit(Sender: TObject);
begin
  Splitter.Invalidate();
end;

procedure TSimbaOpenExampleForm.FormCreate(Sender: TObject);
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

  AddSimbaExamples();
  UpdateTreeSize();
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

procedure TSimbaOpenExampleForm.UpdateTreeSize;
var
  Node: TTreeNode;
  MaxWidth: Integer;
begin
  MaxWidth := 0;

  Node := TreeView.Items.GetFirstNode();
  while Assigned(Node) do
  begin
    MaxWidth := Max(MaxWidth, Node.DisplayTextRight);
    Node := Node.GetNext();
  end;

  LeftPanel.Width := MaxWidth + TreeView.ScrollbarVert.Width + 1;
end;

end.

