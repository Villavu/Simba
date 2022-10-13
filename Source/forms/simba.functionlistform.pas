{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, comctrls, extctrls, treefilteredit,
  simba.codeparser, simba.codeinsight, simba.hintwindow, simba.simplelock;

type
  TSimbaFunctionList = class;
  TSimbaFunctionListForm = class(TForm);

  TSimbaFunctionListStaticSection = class(TObject)
  public
    procedure Add(FunctionList: TSimbaFunctionList); virtual; abstract;
  end;

  TSimbaFunctionList = class(TCustomControl)
  protected
    FRefCount: TSimpleLock;
    FTreeView: TTreeView;
    FFilter: TTreeFilterEdit;
    FHint: TSimbaHintWindow;

    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;

    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoTreeViewDoubleClick(Sender: TObject);
    procedure DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoAfterFilter(Sender: TObject);
  public
  class var
    StaticSections: TList;

    class constructor Create;
    class destructor Destroy;
  public
    IncludesHash: String;
    ExpandedState: TTreeNodeExpandedState;

    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure IncRef;
    procedure DecRef;

    function AddNode(ParentNode: TTreeNode; Node: TTreeNode): TTreeNode;
    procedure AddDecls(ParentNode: TTreeNode; Decls: TDeclarationList);
    procedure AddInternalDecls(ParentNode: TTreeNode; Decls: TDeclarationList);
    procedure AddInclude(Include: TCodeParser);

    property TreeView: TTreeView read FTreeView;
    property ScriptNode: TTreeNode read FScriptNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property IncludesNode: TTreeNode read FIncludesNode;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  lazfileutils, LCLIntf,
  simba.main, simba.functionlist_nodes;

procedure TSimbaFunctionList.DoTreeViewSelectionChanged(Sender: TObject);
var
  Str: String;
begin
  if (FTreeView.Selected is TFunctionListNode) then
  begin
    Str := TFunctionListNode(FTreeView.Selected).Hint;
    if (Str <> '') then
      SimbaForm.StatusPanelFileName.Caption := TFunctionListNode(FTreeView.Selected).Hint;
  end;
end;

procedure TSimbaFunctionList.DoTreeViewDoubleClick(Sender: TObject);
begin
  if (FTreeView.Selected is TFunctionListNode) then
    TFunctionListNode(FTreeView.Selected).Open();
end;

procedure TSimbaFunctionList.DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Str: String;
begin
  Node := FTreeView.GetNodeAt(X, Y);

  if (Node is TFunctionListNode) then
  begin
    Str := TFunctionListNode(Node).Hint;

    if (Length(Str) > 125) then
      FHint.Show(Node, Copy(Str, 1, 125) + ' ...')
    else
    if (Length(Str) > 0) then
      FHint.Show(Node, Str);
  end;
end;

procedure TSimbaFunctionList.DoAfterFilter(Sender: TObject);
var
  I: Integer;
begin
  Assert(Sender is TTreeFilterEdit);
  if (TTreeFilterEdit(Sender).Filter <> '') then
    Exit;

  FTreeView.FullCollapse();
  for I := 0 to TreeView.Items.TopLvlCount - 1 do
    if (FTreeView.Items.TopLvlItems[I] <> FIncludesNode) and (FTreeView.Items.TopLvlItems[I] <> FPluginsNode) then
      FTreeView.Items.TopLvlItems[I].Expanded := True;
end;

class constructor TSimbaFunctionList.Create;
begin
  StaticSections := TList.Create();
end;

class destructor TSimbaFunctionList.Destroy;
var
  I: Integer;
begin
  if (StaticSections <> nil) then
  begin
    for I := 0 to StaticSections.Count - 1 do
      TSimbaFunctionListStaticSection(StaticSections[I]).Free();

    FreeAndNil(StaticSections);
  end;
end;

function TSimbaFunctionList.AddNode(ParentNode: TTreeNode; Node: TTreeNode): TTreeNode;
begin
  Result := FTreeView.Items.AddNode(Node, ParentNode, Node.Text, nil, naAddChild);
end;

procedure TSimbaFunctionList.AddDecls(ParentNode: TTreeNode; Decls: TDeclarationList);
var
  I: Integer;
begin
  for I := 0 to Decls.Count - 1 do
    if (Decls[I].Name <> '') then
      AddNode(ParentNode, TFunctionList_DeclNode.Create(Self, Decls.Items[I]));
end;

procedure TSimbaFunctionList.AddInternalDecls(ParentNode: TTreeNode; Decls: TDeclarationList);
var
  I: Integer;
begin
  for I := 0 to Decls.Count - 1 do
    if (Decls[I].Name <> '') then
      AddNode(ParentNode, TFunctionList_InternalDeclNode.Create(Self, Decls.Items[I]));
end;

procedure TSimbaFunctionList.AddInclude(Include: TCodeParser);
var
  I: Integer;
  CurrentFile: String;
  CurrentNode: TTreeNode;
  Decl: TDeclaration;
begin
  CurrentFile := '';
  CurrentNode := nil;

  for I := 0 to Include.Items.Count - 1 do
  begin
    Decl := Include.Items[I];
    if (Decl.Name = '') then
      Continue;

    if (CurrentFile <> Decl.Lexer.FileName) then
    begin
      CurrentFile := Decl.Lexer.FileName;

      if Decl.Lexer.IsLibrary then
        CurrentNode := AddNode(FPluginsNode, TFunctionList_InternalFileNode.Create(Self, CurrentFile))
      else
        CurrentNode := AddNode(FIncludesNode, TFunctionList_FileNode.Create(Self, CurrentFile));
    end;

    if Decl.Lexer.IsLibrary then
      AddNode(CurrentNode, TFunctionList_InternalDeclNode.Create(Self, Decl))
    else
      AddNode(CurrentNode, TFunctionList_DeclNode.Create(Self, Decl));
  end;
end;

constructor TSimbaFunctionList.Create;
var
  I: Integer;
begin
  inherited Create(nil);

  FTreeView := TTreeView.Create(Self);
  FTreeView.BorderSpacing.Left := 3;
  FTreeView.BorderSpacing.Right := 3;
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.Images := SimbaForm.Images;
  FTreeView.OnDblClick := @DoTreeViewDoubleClick;
  FTreeView.OnSelectionChanged := @DoTreeViewSelectionChanged;
  FTreeView.OnMouseMove := @DoTreeViewMouseMove;
  FTreeView.Options := FTreeView.Options + [tvoRightClickSelect, tvoReadOnly, tvoAutoItemHeight] - [tvoToolTips, tvoThemedDraw];

  ExpandedState := TTreeNodeExpandedState.Create(FTreeView);

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.BorderSpacing.Around := 3;
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.FilteredTreeview := FTreeView;
  FFilter.OnAfterFilter := @DoAfterFilter;
  FFilter.Spacing := 2;
  FFilter.TextHint := '(search)';
  FFilter.Flat := True;
  FFilter.ExpandAllInitially := True;

  FHint := TSimbaHintWindow.Create(FTreeView);

  FScriptNode := FTreeView.Items.Add(nil, 'Script');
  FScriptNode.ImageIndex := IMAGE_DIRECTORY;
  FScriptNode.SelectedIndex := IMAGE_DIRECTORY;

  FPluginsNode := FTreeView.Items.Add(nil, 'Plugins');
  FPluginsNode.ImageIndex := IMAGE_DIRECTORY;
  FPluginsNode.SelectedIndex := IMAGE_DIRECTORY;

  FIncludesNode := FTreeView.Items.Add(nil, 'Includes');
  FIncludesNode.ImageIndex := IMAGE_DIRECTORY;
  FIncludesNode.SelectedIndex := IMAGE_DIRECTORY;

  for I := 0 to StaticSections.Count - 1 do
    TSimbaFunctionListStaticSection(StaticSections[I]).Add(Self);

  IncRef();
end;

destructor TSimbaFunctionList.Destroy;
begin
  if (ExpandedState <> nil) then
    FreeAndNil(ExpandedState);

  inherited Destroy();
end;

procedure TSimbaFunctionList.IncRef;
begin
  FRefCount.IncLock();
end;

procedure TSimbaFunctionList.DecRef;
begin
  FRefCount.DecLock();
  if (not FRefCount.IsLocked()) then
    Free();
end;

end.

