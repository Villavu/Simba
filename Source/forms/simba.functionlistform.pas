{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, TreeFilterEdit,
  simba.ide_codetools_parser, simba.ide_codetools_insight, simba.simplelock, simba.component_treeview;

type
  TSimbaFunctionList = class;
  TSimbaFunctionListForm = class(TForm);

  TSimbaFunctionListStaticSection = class(TObject)
  protected
    FLoaded: TSimpleLock;

    function GetLoaded: Boolean;
    procedure SetLoaded(Value: Boolean);
  public
    property Loaded: Boolean read GetLoaded write SetLoaded;

    function Added(FunctionList: TSimbaFunctionList): Boolean; virtual; abstract;
    procedure Add(FunctionList: TSimbaFunctionList); virtual; abstract;
  end;

  TSimbaFunctionList = class(TCustomControl)
  protected
    FRefCount: TSimpleLock;
    FTreeView: TSimbaTreeView;
    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;

    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoTreeViewDoubleClick(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);
    function DoGetNodeHint(Node: TTreeNode): String;
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

    property TreeView: TSimbaTreeView read FTreeView;
    property ScriptNode: TTreeNode read FScriptNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property IncludesNode: TTreeNode read FIncludesNode;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  simba.main, simba.functionlist_nodes, simba.ide_mainstatusbar;

procedure TSimbaFunctionListStaticSection.SetLoaded(Value: Boolean);
begin
  if Value then
    FLoaded.IncLock()
  else
    FLoaded.DecLock();
end;

function TSimbaFunctionListStaticSection.GetLoaded: Boolean;
begin
  Result := FLoaded.IsLocked();
end;

procedure TSimbaFunctionList.DoTreeViewSelectionChanged(Sender: TObject);
var
  Node: TFunctionListNode;
begin
  Node := TFunctionListNode(FTreeView.Selected);
  if (Node is TFunctionListNode) and Node.HasHint() then
    SimbaMainStatusBar.SetMainPanelText(Node.Hint);
end;

procedure TSimbaFunctionList.DoTreeViewDoubleClick(Sender: TObject);
var
  Node: TFunctionListNode;
begin
  Node := TFunctionListNode(FTreeView.Selected);
  if (Node is TFunctionListNode) then
    TFunctionListNode(FTreeView.Selected).Open();
end;

procedure TSimbaFunctionList.DoAfterFilter(Sender: TObject);
begin
  if (Sender is TTreeFilterEdit) and (TTreeFilterEdit(Sender).Filter = '') then
  begin
    FTreeView.FullCollapse();
    FTreeView.ExpandTopLevelNode('Script');
    FTreeView.ExpandTopLevelNode('Simba');
  end;
end;

function TSimbaFunctionList.DoGetNodeHint(Node: TTreeNode): String;
begin
  if (Node is TFunctionListNode) then
    with TFunctionListNode(Node) do
    begin
      if (Length(Hint) > 125) then
        Result := Copy(Hint, 1, 125) + ' ...'
      else
        Result := Hint;
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
    if (Decls[I].Name <> '') and (not Decls[I].isOverrideMethod) then
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
begin
  inherited Create(nil);

  FTreeView := TSimbaTreeView.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.Images := SimbaForm.Images;
  FTreeView.OnDoubleClick := @DoTreeViewDoubleClick;
  FTreeView.OnSelectionChange := @DoTreeViewSelectionChanged;
  FTreeView.TabStop := False;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;

  //ExpandedState := TTreeNodeExpandedState.Create(FTreeView);


  FScriptNode := FTreeView.AddNode('Script');
  FScriptNode.ImageIndex := IMAGE_DIRECTORY;
  FScriptNode.SelectedIndex := IMAGE_DIRECTORY;

  FPluginsNode := FTreeView.AddNode('Plugins');
  FPluginsNode.ImageIndex := IMAGE_DIRECTORY;
  FPluginsNode.SelectedIndex := IMAGE_DIRECTORY;

  FIncludesNode := FTreeView.AddNode('Includes');
  FIncludesNode.ImageIndex := IMAGE_DIRECTORY;
  FIncludesNode.SelectedIndex := IMAGE_DIRECTORY;

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

