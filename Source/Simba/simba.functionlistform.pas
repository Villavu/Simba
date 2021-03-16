unit simba.functionlistform;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, lresources, forms, controls, stdctrls, comctrls, treefilteredit, sha1,
  simba.codeinsight, simba.codeparser, simba.hintwindow;

type
  TSimbaFunctionList_Node = class(TTreeNode)
  public
    procedure Open; virtual; abstract;
  end;

  TSimbaFunctionList_DeclarationNode = class(TSimbaFunctionList_Node)
  public
    StartPos, EndPos: Int32;
    Line: Int32;
    Header: String;

    procedure Open; override;

    constructor Create(AOwner: TTreeNodes; ADeclaration: TDeclaration); reintroduce;
  end;

  TSimbaFunctionList_FileNode = class(TSimbaFunctionList_Node)
  public
    FileName: String;

    procedure Open; override;

    constructor Create(AOwner: TTreeNodes; AFileName: String); reintroduce;
  end;

  TSimbaFunctionList_InternalFileNode = class(TSimbaFunctionList_FileNode)
  public
    procedure Open; override;
  end;

  TSimbaFunctionList_Hint = class(TSimbaHintWindow)
  protected
    procedure DoHide; override;
  public
    Node: TTreeNode;
  end;

  TSimbaFunctionList = class(TCustomControl)
  protected
    FTreeView: TTreeView;
    FFilter: TTreeFilterEdit;
    FHint: TSimbaFunctionList_Hint;

    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;
    FSimbaNode: TTreeNode;

    FUpdating: Boolean;

    function AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;
    function AddInternalFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
    function AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;

    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoTreeViewDoubleClick(Sender: TObject);
    procedure DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoTreeViewMouseLeave(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);

    function GetExpandedState: TTreeNodeExpandedState;
    procedure SetExpandedState(Value: TTreeNodeExpandedState);

    procedure SetVisible(Value: Boolean); override;
  public
    IncludesHash: TSHA1Digest; // Current hash of the includes displayed
    ChangeStamp: Int64;        // Current change stamp of the script displayed

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
    procedure AddType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
    procedure AddVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
    procedure AddConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
    procedure AddDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);
    procedure AddInclude(Include: TCodeParser);
    procedure AddSimbaSection(Include: TCodeParser);

    property SimbaNode: TTreeNode read FSimbaNode;
    property ScriptNode: TTreeNode read FScriptNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property IncludesNode: TTreeNode read FIncludesNode;

    property ExpandedState: TTreeNodeExpandedState read GetExpandedState write SetExpandedState;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSimbaFunctionListForm = class(TForm)
  protected
    FFunctionList: TSimbaFunctionList;
    FParser: TCodeInsight;
    FUpdating: Boolean;
    FUpdateThread: TThread;
    FUpdateThreadFinished: Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DoUpdateThread;
    procedure DoUpdateThreadTerminated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

uses
  graphics, lazfileutils,
  simba.main, simba.scripttabsform, simba.scripttab, simba.debugform;

procedure TSimbaFunctionList_DeclarationNode.Open;
var
  FileName: String;
  Internal: Boolean;
begin
  FileName := '';
  if (Parent is TSimbaFunctionList_FileNode) then
    FileName := TSimbaFunctionList_FileNode(Parent).FileName;

  Internal := Parent is TSimbaFunctionList_InternalFileNode;

  SimbaScriptTabsForm.OpenDeclaration(Header, StartPos, EndPos, Line, FileName, Internal);
end;

constructor TSimbaFunctionList_DeclarationNode.Create(AOwner: TTreeNodes; ADeclaration: TDeclaration);
begin
  inherited Create(AOwner);

  StartPos := ADeclaration.StartPos;
  EndPos := ADeclaration.EndPos;
  Line := ADeclaration.Line;

  if ADeclaration.ClassType = TciProcedureDeclaration then
    Header := TciProcedureDeclaration(ADeclaration).Header
  else
  if ADeclaration.ClassType = TciTypeDeclaration then
    Header := 'type ' + ADeclaration.ShortText
  else
  if ADeclaration.ClassType = TciConstantDeclaration then
    Header := 'const ' + ADeclaration.ShortText
  else
  if ADeclaration.ClassType = TciVarDeclaration then
    Header := 'var ' + ADeclaration.ShortText;
end;

procedure TSimbaFunctionList_FileNode.Open;
begin
  SimbaScriptTabsForm.Open(FileName);
end;

constructor TSimbaFunctionList_FileNode.Create(AOwner: TTreeNodes; AFileName: String);
begin
  inherited Create(AOwner);

  FileName := AFileName;
end;

procedure TSimbaFunctionList_InternalFileNode.Open;
begin
  { nothing }
end;

procedure TSimbaFunctionList_Hint.DoHide;
begin
  inherited DoHide();

  Node := nil;
end;


procedure TSimbaFunctionListForm.BeginUpdate;
var
  I: Int32;
  Tab: TSimbaScriptTab;
begin
  Assert(GetCurrentThreadId() = MainThreadID);

  FUpdating := False;
  if (SimbaScriptTabsForm = nil) then
    Exit;

  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    Tab := SimbaScriptTabsForm.Tabs[I];
    if (Tab = nil) or (not Tab.Visible) then
      Continue;

    FFunctionList := Tab.FunctionList;
    if (FFunctionList = nil) or (FFunctionList.ChangeStamp = Tab.Editor.ChangeStamp) then
      Continue;

    FParser := Tab.GetParser();
    if (FParser = nil) then
      Continue;

    FFunctionList.ChangeStamp := Tab.Editor.ChangeStamp;
    FFunctionList.BeginUpdate();

    FUpdating := True;

    Exit;
  end;
end;

procedure TSimbaFunctionListForm.EndUpdate;
begin
  Assert(GetCurrentThreadId() = MainThreadID);

  FFunctionList.EndUpdate();
end;

procedure TSimbaFunctionListForm.DoUpdateThread;
var
  I: Int32;
  ExpandedState: TTreeNodeExpandedState;
begin
  try
    while not FUpdateThread.CheckTerminated do
    begin
      TThread.Synchronize(TThread.CurrentThread, @BeginUpdate);

      if FUpdating then
      try
        //WriteLn('Update script');

        ExpandedState := FFunctionList.ExpandedState;

        FParser.Run();
        FFunctionList.AddDeclarations(FParser.Items, FFunctionList.ScriptNode, True, False, True);

        if (not SHA1Match(FFunctionList.IncludesHash, FParser.IncludesHash)) then
        begin
          // WriteLn('Update includes');

          FFunctionList.IncludesNode.DeleteChildren();
          FFunctionList.PluginsNode.DeleteChildren();

          for I := 0 to High(FParser.Includes) do
            FFunctionList.AddInclude(FParser.Includes[I]);

          FFunctionList.IncludesHash := FParser.IncludesHash;
        end;

        if (FFunctionList.SimbaNode.Count = 0) then
        begin
          for I := 0 to High(FParser.FunctionListSections) do
            FFunctionList.AddSimbaSection(FParser.FunctionListSections[I]);

          FFunctionList.SimbaNode.AlphaSort();
          FFunctionList.SimbaNode.Expanded := True;
        end;

        FFunctionList.ExpandedState := ExpandedState;
      finally
        TThread.Synchronize(TThread.CurrentThread, @EndUpdate);

        if (FParser <> nil) then
        begin
          FParser.Free();
          FParser := nil;
        end;
      end;

      Sleep(500);
    end;
  except
    on E: Exception do
      SimbaDebugForm.Add('Function list updater crashed: ' + E.Message);
  end;
end;

procedure TSimbaFunctionListForm.DoUpdateThreadTerminated(Sender: TObject);
begin
  FUpdateThreadFinished := True;
end;

constructor TSimbaFunctionListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUpdateThread := TThread.ExecuteInThread(@DoUpdateThread, @DoUpdateThreadTerminated);
end;

destructor TSimbaFunctionListForm.Destroy;
begin
  FUpdateThread.Terminate();

  if not FUpdateThreadFinished then
  begin
    WriteLn('Waiting for function list thread to finish...');
    while not FUpdateThreadFinished do
      CheckSynchronize(50); // TThread.OnTerminate is synchronized
  end;

  inherited Destroy();
end;

function TSimbaFunctionList.GetExpandedState: TTreeNodeExpandedState;
begin
  Result := TTreeNodeExpandedState.Create(FTreeView);
end;

procedure TSimbaFunctionList.SetExpandedState(Value: TTreeNodeExpandedState);
begin
  Value.Apply(FTreeView);
  Value.Free();
  Value := nil;
end;

procedure TSimbaFunctionList.SetVisible(Value: Boolean);
begin
  FHint.Hide();

  inherited SetVisible(Value);
end;

function TSimbaFunctionList.AddFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
begin
  Assert(ParentNode <> nil);

  Result := FTreeView.Items.AddNode(TSimbaFunctionList_FileNode.Create(FTreeView.Items, FileName), ParentNode, ExtractFileNameOnly(FileName), nil, naAddChild);
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

function TSimbaFunctionList.AddInternalFile(ParentNode: TTreeNode; FileName: String): TTreeNode;
begin
  Assert(ParentNode <> nil);

  Result := FTreeView.Items.AddNode(TSimbaFunctionList_InternalFileNode.Create(FTreeView.Items, FileName), ParentNode, ExtractFileNameOnly(FileName), nil, naAddChild);
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

procedure TSimbaFunctionList.DoTreeViewSelectionChanged(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionList_DeclarationNode) then
    SimbaScriptTabsForm.StatusPanelFileName.Caption := TSimbaFunctionList_DeclarationNode(FTreeView.Selected).Header;
end;

function TSimbaFunctionList.AddDeclaration(ParentNode: TTreeNode; Declaration: TDeclaration): TTreeNode;
begin
  Result := FTreeView.Items.AddNode(TSimbaFunctionList_DeclarationNode.Create(FTreeView.Items, Declaration), ParentNode, Declaration.Name, Pointer(nil), naAddChild);
end;

procedure TSimbaFunctionList.DoTreeViewDoubleClick(Sender: TObject);
begin
  if (FTreeView.Selected is TSimbaFunctionList_Node) then
  begin
    FTreeView.Selected.Expanded := not FTreeView.Selected.Expanded;

    TSimbaFunctionList_Node(FTreeView.Selected).Open();
  end;
end;

procedure TSimbaFunctionList.DoTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  R: TRect;
  Header: String;
begin
  Node := FTreeView.GetNodeAt(X, Y);
  if (Node = FHint.Node) then
    Exit;

  if (Node is TSimbaFunctionList_DeclarationNode) and (Node <> FHint.Node) then
  begin
    Header := TSimbaFunctionList_DeclarationNode(Node).Header;
    if Length(Header) > 3500 then
    begin
      FHint.Hide();

      Exit;
    end;

    R := Node.DisplayRect(True);
    R.TopLeft := FTreeView.ClientToScreen(R.TopLeft);
    R.BottomRight := FTreeView.ClientToScreen(R.BottomRight);

    FHint.Node := Node;
    FHint.ActivateHint(R, TSimbaFunctionList_DeclarationNode(Node).Header);
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
        CurrentNode := AddInternalFile(FPluginsNode, CurrentFile)
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
  Node := AddInternalFile(FSimbaNode, Include.Lexer.FileName);
  Node.ImageIndex := IMAGE_FILE;
  Node.SelectedIndex := IMAGE_FILE;

  AddDeclarations(Include.Items, Node, False, True, False);
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

procedure TSimbaFunctionList.BeginUpdate;
begin
  Assert(GetCurrentThreadID() = MainThreadID);

  FUpdating := True;
  FTreeView.BeginUpdate();
end;

procedure TSimbaFunctionList.EndUpdate;
begin
  Assert(GetCurrentThreadID() = MainThreadID);

  FUpdating := False;
  FTreeView.EndUpdate();
end;

destructor TSimbaFunctionList.Destroy;
begin
  while FUpdating do
    Application.ProcessMessages();

  inherited Destroy();
end;

constructor TSimbaFunctionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTreeView := TTreeView.Create(Self);
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

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.FilteredTreeview := FTreeView;
  FFilter.OnAfterFilter := @DoAfterFilter;
  FFilter.ButtonWidth := 0;
  FFilter.Spacing := 0;
  FFilter.TextHint := '(search)';

  FHint := TSimbaFunctionList_Hint.Create(Self);

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

initialization
  {$I simba.functionlistform.lrs}

end.

