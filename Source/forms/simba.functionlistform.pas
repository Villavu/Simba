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
  simba.codeparser, simba.hintwindow;

type
  TSimbaFunctionList = class;
  TSimbaFunctionListForm = class(TForm);

  TSimbaFunctionListNode = class(TTreeNode)
  public
    Hint: String;

    procedure Open; virtual; abstract;
  end;

  TSimbaFunctionListFileNode = class(TSimbaFunctionListNode)
  public
    FileName: String;

    procedure Open; override;
    constructor Create(FunctionList: TSimbaFunctionList; AFileName: String); reintroduce;
  end;

  TSimbaFunctionListDeclNode = class(TSimbaFunctionListNode)
  public
    FileName: String;
    StartPos, EndPos: Integer;
    Line: Integer;
    Internal: Boolean;

    procedure Open; override;
    constructor Create(FunctionList: TSimbaFunctionList; ADeclaration: TDeclaration); reintroduce;
  end;

  TSimbaFunctionListStaticSection = class(TObject)
  public
    procedure Add(FunctionList: TSimbaFunctionList); virtual; abstract;
  end;

  TSimbaFunctionList = class(TCustomControl)
  protected
  class var
    FStaticSections: TList;
  protected
    FTreeView: TTreeView;
    FFilter: TTreeFilterEdit;
    FHint: TSimbaHintWindow;

    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;

    FAddingInternal: Integer;

    function Add(ParentNode: TTreeNode; Node: TTreeNode; NodeText: String): TTreeNode;
    function AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;

    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoTreeViewDoubleClick(Sender: TObject);
    procedure DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoAfterFilter(Sender: TObject);
  public
    class constructor Create;
    class destructor Destroy;
    class procedure AddSection(Section: TSimbaFunctionListStaticSection);

    function IsAddingInternal: Boolean;
    procedure BeginAddingInternal;
    procedure EndAdddingInternal;

    function AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
    procedure AddMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
    procedure AddType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
    procedure AddVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
    procedure AddConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
    procedure AddDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);
    procedure AddInclude(Include: TCodeParser);

    property TreeView: TTreeView read FTreeView;
    property ScriptNode: TTreeNode read FScriptNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property IncludesNode: TTreeNode read FIncludesNode;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  lazfileutils,
  simba.main, simba.scripttabsform;

procedure TSimbaFunctionListFileNode.Open;
begin
  if FileExists(FileName) then
    SimbaScriptTabsForm.Open(FileName);
end;

constructor TSimbaFunctionListFileNode.Create(FunctionList: TSimbaFunctionList; AFileName: String);
begin
  inherited Create(FunctionList.TreeView.Items);

  FileName := AFileName;
  if not FunctionList.IsAddingInternal then
    Hint := AFileName;
end;

procedure TSimbaFunctionListDeclNode.Open;
begin
  if Internal then
    SimbaScriptTabsForm.OpenInternalDeclaration(Hint, FileName)
  else
    SimbaScriptTabsForm.OpenDeclaration(Hint, StartPos, EndPos, Line, FileName);
end;

constructor TSimbaFunctionListDeclNode.Create(FunctionList: TSimbaFunctionList; ADeclaration: TDeclaration);
begin
  inherited Create(FunctionList.TreeView.Items);

  Internal := FunctionList.IsAddingInternal;

  StartPos := ADeclaration.StartPos;
  EndPos   := ADeclaration.EndPos;
  Line     := ADeclaration.Line;
  FileName := ADeclaration.Lexer.FileName;

  if (ADeclaration.ClassType = TciProcedureDeclaration) then
    Hint := TciProcedureDeclaration(ADeclaration).Header
  else
  if (ADeclaration.ClassType = TciTypeDeclaration) then
    Hint := 'type ' + ADeclaration.ShortText
  else
  if (ADeclaration.ClassType = TciConstantDeclaration) then
    Hint := 'const ' + ADeclaration.ShortText
  else
  if (ADeclaration.ClassType = TciVarDeclaration) then
    Hint := 'var ' + ADeclaration.ShortText;
end;

function TSimbaFunctionList.AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
begin
  Result := Add(
    ParentNode,
    TSimbaFunctionListFileNode.Create(Self, FileName),
    ExtractFileNameOnly(FileName)
   );

  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

procedure TSimbaFunctionList.DoTreeViewSelectionChanged(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionListNode) then
    SimbaForm.StatusPanelFileName.Caption := TSimbaFunctionListNode(FTreeView.Selected).Hint;
end;

function TSimbaFunctionList.Add(ParentNode: TTreeNode; Node: TTreeNode; NodeText: String): TTreeNode;
begin
  Result := FTreeView.Items.AddNode(Node, ParentNode, NodeText, nil, naAddChild);
end;

function TSimbaFunctionList.AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;
begin
  Result := Add(
    ParentNode,
    TSimbaFunctionListDeclNode.Create(Self, Declaration),
    Declaration.Name
  );
end;

procedure TSimbaFunctionList.DoTreeViewDoubleClick(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionListNode) then
    TSimbaFunctionListNode(FTreeView.Selected).Open();
end;

procedure TSimbaFunctionList.DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Str: String;
begin
  Node := FTreeView.GetNodeAt(X, Y);

  if (Node is TSimbaFunctionListNode) then
  begin
    Str := TSimbaFunctionListNode(Node).Hint;
    if (Length(Str) > 150) then
      Str := Copy(Str, 1, 150) + ' ...';

    if (Str <> '') then
      FHint.Show(Node, Str);
  end else
    FHint.Hide();
end;

procedure TSimbaFunctionList.AddMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = '') then
    Exit;

  with AddDeclaration(ParentNode, Declaration) do
  begin
    if Declaration.IsMethodOfType then
      Text := Declaration.ObjectName + '.' + Declaration.Name;

    if Declaration.IsFunction then
    begin
      ImageIndex := IMAGE_FUNCTION;
      SelectedIndex := IMAGE_FUNCTION;
    end else
    begin
      ImageIndex := IMAGE_PROCEDURE;
      SelectedIndex := IMAGE_PROCEDURE;
    end;
  end;
end;

procedure TSimbaFunctionList.AddType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = '') then
    Exit;

  with AddDeclaration(ParentNode, Declaration) do
  begin
    ImageIndex := IMAGE_TYPE;
    SelectedIndex := IMAGE_TYPE;
  end;
end;

procedure TSimbaFunctionList.AddVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
begin
  with AddDeclaration(ParentNode, Declaration) do
  begin
    ImageIndex := IMAGE_VARIABLE;
    SelectedIndex := IMAGE_VARIABLE;
  end;
end;

procedure TSimbaFunctionList.AddConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
begin
  with AddDeclaration(ParentNode, Declaration) do
  begin
    Text := Declaration.Name;

    ImageIndex := IMAGE_CONSTANT;
    SelectedIndex := IMAGE_CONSTANT;
  end;
end;

procedure TSimbaFunctionList.AddDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);
var
  i: Integer;
begin
  if Clear then
    ParentNode.DeleteChildren();

  for i := 0 to Declarations.Count - 1 do
    if (Declarations[i] is TciProcedureDeclaration) then
      AddMethod(Declarations[i] as TciProcedureDeclaration, ParentNode)
    else
    if (Declarations[i] is TciTypeDeclaration) then
      AddType(Declarations[i] as TciTypeDeclaration, ParentNode)
    else
    if (Declarations[i] is TciConstantDeclaration) then
      AddConst(Declarations[i] as TciConstantDeclaration, ParentNode)
    else
    if (Declarations[i] is TciVarDeclaration) then
      AddVar(Declarations[i] as TciVarDeclaration, ParentNode);

  if Sort then
    ParentNode.AlphaSort();

  if Expand then
    ParentNode.Expanded := True;
end;

procedure TSimbaFunctionList.AddInclude(Include: TCodeParser);
var
  I: Integer;
  CurrentFile: String;
  CurrentNode: TTreeNode;
  Declaration: TDeclaration;
begin
  CurrentFile := '';
  CurrentNode := nil;

  for I := 0 to Include.Items.Count - 1 do
  begin
    Declaration := Include.Items[I];

    if (CurrentFile <> Declaration.Lexer.FileName) then
    begin
      CurrentFile := Declaration.Lexer.FileName;

      if Declaration.Lexer.IsLibrary then
        CurrentNode := AddFile(FPluginsNode, CurrentFile)
      else
        CurrentNode := AddFile(FIncludesNode, CurrentFile);
    end;

    if (CurrentNode <> nil) then
    begin
      if (Declaration is TciProcedureDeclaration) then
        AddMethod(Declaration as TciProcedureDeclaration, CurrentNode)
      else
      if (Declaration is TciTypeDeclaration) then
        AddType(Declaration as TciTypeDeclaration, CurrentNode)
      else
      if (Declaration is TciConstantDeclaration) then
        AddConst(Declaration as TciConstantDeclaration, CurrentNode)
      else
      if (Declaration is TciVarDeclaration) then
        AddVar(Declaration as TciVarDeclaration, CurrentNode);
    end;
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
  FStaticSections := TList.Create();
end;

class destructor TSimbaFunctionList.Destroy;
var
  I: Integer;
begin
  if (FStaticSections <> nil) then
  begin
    for I := 0 to FStaticSections.Count - 1 do
      TSimbaFunctionListStaticSection(FStaticSections[I]).Free();

    FreeAndNil(FStaticSections);
  end;
end;

class procedure TSimbaFunctionList.AddSection(Section: TSimbaFunctionListStaticSection);
begin
  FStaticSections.Add(Section);
end;

function TSimbaFunctionList.IsAddingInternal: Boolean;
begin
  Result := FAddingInternal <> 0;
end;

procedure TSimbaFunctionList.BeginAddingInternal;
begin
  Inc(FAddingInternal);
end;

procedure TSimbaFunctionList.EndAdddingInternal;
begin
  Dec(FAddingInternal);
end;

constructor TSimbaFunctionList.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  FTreeView := TTreeView.Create(Self);
  FTreeView.BorderSpacing.Left := 3;
  FTreeView.BorderSpacing.Right := 3;
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.Images := SimbaForm.Images;
  FTreeView.ReadOnly := True;
  FTreeView.ToolTips := False;
  FTreeView.OnDblClick := @DoTreeViewDoubleClick;
  FTreeView.OnSelectionChanged := @DoTreeViewSelectionChanged;
  FTreeView.OnMouseMove := @DoTreeViewMouseMove;
  FTreeView.Options := FTreeView.Options;

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
  FScriptNode.Expanded := True;

  FPluginsNode := FTreeView.Items.Add(nil, 'Plugins');
  FPluginsNode.ImageIndex := IMAGE_DIRECTORY;
  FPluginsNode.SelectedIndex := IMAGE_DIRECTORY;

  FIncludesNode := FTreeView.Items.Add(nil, 'Includes');
  FIncludesNode.ImageIndex := IMAGE_DIRECTORY;
  FIncludesNode.SelectedIndex := IMAGE_DIRECTORY;

  for I := 0 to FStaticSections.Count - 1 do
    TSimbaFunctionListStaticSection(FStaticSections[I]).Add(Self);
end;

end.

