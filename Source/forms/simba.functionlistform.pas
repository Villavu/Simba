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
  protected
    FFileName: String;
    FDeclarationClass: TDeclarationClass;
    FStartPos, FEndPos: Integer;
    FLine: Integer;
    FHeader: String;
    FFunctionList: TSimbaFunctionList;
  public
    constructor Create(FunctionList: TSimbaFunctionList; Declaration: TDeclaration); reintroduce; overload;
    constructor Create(FunctionList: TSimbaFunctionList; FileName: String); reintroduce; overload;

    procedure Open;

    property FunctionList: TSimbaFunctionList read FFunctionList;
    property Hint: String read FHeader;
    property DeclarationClass: TDeclarationClass read FDeclarationClass;
  end;

  TSimbaFunctionList = class(TCustomControl)
  protected
    FTreeView: TTreeView;
    FFilter: TTreeFilterEdit;
    FHint: TSimbaHintWindow;

    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;
    FSimbaNode: TTreeNode;

    function CustomSort(A, B: TTreeNode): Integer;

    function AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;
    function AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;

    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoTreeViewDoubleClick(Sender: TObject);
    procedure DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoTreeViewMouseLeave(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);
  public
    procedure AddMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
    procedure AddType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
    procedure AddVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
    procedure AddConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
    procedure AddDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);
    procedure AddInclude(Include: TCodeParser);
    procedure AddSimbaSection(Include: TCodeParser);

    property TreeView: TTreeView read FTreeView;

    property SimbaNode: TTreeNode read FSimbaNode;
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

procedure TSimbaFunctionListNode.Open;
begin
  if (FDeclarationClass <> nil) then
  begin
    if HasAsParent(FFunctionList.SimbaNode) or HasAsParent(FFunctionList.PluginsNode) then
      SimbaScriptTabsForm.OpenInternalDeclaration(FHeader, FFileName)
    else
      SimbaScriptTabsForm.OpenDeclaration(FHeader, FStartPos, FEndPos, FLine, FFileName);
  end else
  if HasAsParent(FFunctionList.ScriptNode) or HasAsParent(FFunctionList.IncludesNode) then
    SimbaScriptTabsForm.Open(FFileName);
end;

constructor TSimbaFunctionListNode.Create(FunctionList: TSimbaFunctionList; Declaration: TDeclaration);
begin
  inherited Create(FunctionList.TreeView.Items);

  FFunctionList := FunctionList;
  FDeclarationClass := TDeclarationClass(Declaration.ClassType);
  FStartPos := Declaration.StartPos;
  FEndPos := Declaration.EndPos;
  FLine := Declaration.Line;
  FFileName := Declaration.Lexer.FileName;

  if (FDeclarationClass = TciProcedureDeclaration) then
    FHeader := TciProcedureDeclaration(Declaration).Header
  else
  if (FDeclarationClass = TciTypeDeclaration) then
    FHeader := 'type ' + Declaration.ShortText
  else
  if (FDeclarationClass = TciConstantDeclaration) then
    FHeader := 'const ' + Declaration.ShortText
  else
  if (FDeclarationClass = TciVarDeclaration) then
    FHeader := 'var ' + Declaration.ShortText;
end;

constructor TSimbaFunctionListNode.Create(FunctionList: TSimbaFunctionList; FileName: String);
begin
  inherited Create(FunctionList.TreeView.Items);

  FFunctionList := FunctionList;
  FFileName := FileName;
end;

function TSimbaFunctionList.AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
begin
  Assert(ParentNode <> nil);

  Result := FTreeView.Items.AddNode(TSimbaFunctionListNode.Create(Self, FileName), ParentNode, ExtractFileNameOnly(FileName), nil, naAddChild);
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

procedure TSimbaFunctionList.DoTreeViewSelectionChanged(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionListNode) then
    SimbaForm.StatusPanelFileName.Caption := ' ' + TSimbaFunctionListNode(FTreeView.Selected).Hint;
end;

function TSimbaFunctionList.CustomSort(A, B: TTreeNode): Integer;
var
  Weights: array of TDeclarationClass;
  I: Integer;
  LeftNode: TSimbaFunctionListNode absolute A;
  RightNode: TSimbaFunctionListNode absolute B;
begin
  Result := 0;

  Weights := [TciTypeDeclaration, TciConstantDeclaration, TciVarDeclaration, TciProcedureDeclaration];
  for I := 0 to High(Weights) do
  begin
    if (Weights[I] = LeftNode.DeclarationClass) then
      Inc(Result, I+1);
    if (Weights[I] = RightNode.DeclarationClass) then
      Dec(Result, I+1);
  end;

  if (LeftNode.DeclarationClass = RightNode.DeclarationClass) then
    Inc(Result, CompareText(LeftNode.Text, RightNode.Text));
end;

function TSimbaFunctionList.AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;
begin
  Result := FTreeView.Items.AddNode(TSimbaFunctionListNode.Create(Self, Declaration), ParentNode, Declaration.Name, Pointer(nil), naAddChild);
end;

procedure TSimbaFunctionList.DoTreeViewDoubleClick(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionListNode) then
    TSimbaFunctionListNode(FTreeView.Selected).Open();
end;

procedure TSimbaFunctionList.DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  R: TRect;
  Header: String;
begin
  Node := FTreeView.GetNodeAt(X, Y);

  if (Node is TSimbaFunctionListNode) then
  begin
    Header := TSimbaFunctionListNode(Node).Hint;
    if (Header = '') then
      Exit;

    if (Length(Header) > 180) then
      Header := Copy(Header, 1, 180) + ' ...';

    R := Node.DisplayRect(True);
    R.TopLeft := FTreeView.ClientToScreen(R.TopLeft);
    R.BottomRight := FTreeView.ClientToScreen(R.BottomRight);

    FHint.ActivateHint(R, Header);
  end else
    FHint.Hide();
end;

procedure TSimbaFunctionList.DoTreeViewMouseLeave(Sender: TObject);
begin
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
  i: Int32;
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
  I: Int32;
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

procedure TSimbaFunctionList.AddSimbaSection(Include: TCodeParser);
var
  Node: TTreeNode;
begin
  Node := AddFile(FSimbaNode, Include.Lexer.FileName);
  Node.ImageIndex := IMAGE_FILE;
  Node.SelectedIndex := IMAGE_FILE;

  AddDeclarations(Include.Items, Node, False, False, False);

  Node.CustomSort(@CustomSort);
end;

procedure TSimbaFunctionList.DoAfterFilter(Sender: TObject);
begin
  if (FFilter.Filter <> '') then
    FTreeView.FullExpand()
  else
  begin
    FTreeView.FullCollapse();

    FSimbaNode.Expanded := True;
    FScriptNode.Expanded := True;
  end;
end;

constructor TSimbaFunctionList.Create(AOwner: TComponent);
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
  FTreeView.OnMouseLeave := @DoTreeViewMouseLeave;
  FTreeView.Options := FTreeView.Options + [tvoNoDoubleClickExpand];

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.BorderSpacing.Around := 3;
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.FilteredTreeview := FTreeView;
  FFilter.OnAfterFilter := @DoAfterFilter;
  FFilter.ButtonWidth := 0;
  FFilter.Spacing := 0;
  FFilter.TextHint := '(search)';

  FHint := TSimbaHintWindow.Create(Self);

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

  FSimbaNode := FTreeView.Items.Add(nil, 'Simba');
  FSimbaNode.ImageIndex := IMAGE_DIRECTORY;
  FSimbaNode.SelectedIndex := IMAGE_DIRECTORY;
  FSimbaNode.Expanded := True;
end;

end.

