{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_insight, simba.component_treeview, simba.dictionary;

type
  TSimbaFunctionListForm = class(TForm)
  protected
  type
    TFunctionListState = record
      Nodes: TTreeNodeExpandedState;
      Filter: String;
      ScrolledTop: Integer;
      ScrolledLeft: Integer;
    end;
    TFunctionListStateDict = specialize TDictionary<Integer, TFunctionListState>;
  protected
    FLastTabID: Integer;
    FSavedStates: TFunctionListStateDict;
    FTreeView: TSimbaTreeView;
    FScriptNode: TTreeNode;
    FSimbaNode: TTreeNode;

    FUpdateThread: TThread;
    FNeedUpdate: Boolean;
    FForceUpdate: Boolean;

    FCodeinsight: TCodeInsight;

    FIsIdle: Boolean;

    procedure ResetState;
    procedure SaveState(TabID: Integer);
    procedure RestoreState(TabID: Integer);
    procedure DeleteState(TabID: Integer);

    procedure DoIdleBegin(Sender: TObject);
    procedure DoIdleEnd(Sender: TObject);

    procedure Fill;
    procedure DoUpdateThread;

    function DoGetNodeHint(const Node: TTreeNode): String;
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoNodeDoubleClick(Sender: TObject);
    procedure DoCodetoolsSetup(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);
    procedure DoEditorModified(Sender: TObject);
    procedure DoTabBeforeChange(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoTabClosed(Sender: TObject);

    procedure AddPluginsNode(Plugins: TCodeParserList; Hash: String);
    procedure AddIncludesNode(Includes: TCodeParserList; Hash: String);
    procedure PurgeNodes;

    function AddDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
    function AddSimbaDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
    function AddPluginDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;

    procedure AddSimbaNodes;

    // Sort so order is: Types, Constants, Variables, Methods
    function CompareDecl(A, B: TTreeNode): Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  StrUtils,
  simba.main, simba.ide_mainstatusbar, simba.ide_events, simba.threading,
  simba.scripttabsform, simba.scripttab, simba.ide_showdeclaration;

function GetImage(const Decl: TDeclaration): Integer;
begin
  if (Decl is TDeclaration_Method) and Decl.isFunction       then Result := IMAGE_FUNCTION  else
  if (Decl is TDeclaration_Method) and Decl.isProcedure      then Result := IMAGE_PROCEDURE else
  if (Decl is TDeclaration_Method) and Decl.isOperatorMethod then Result := IMAGE_OPERATOR  else
  if (Decl is TDeclaration_Type)                             then Result := IMAGE_TYPE      else
  if (Decl is TDeclaration_Const)                            then Result := IMAGE_CONSTANT  else
  if (Decl is TDeclaration_Var)                              then Result := IMAGE_VARIABLE  else Result := -1;
end;

function GetText(const Decl: TDeclaration): String;
begin
  if (Decl is TDeclaration_Method) and Decl.isObjectMethod then
    Result := TDeclaration_Method(Decl).ObjectName + '.' + TDeclaration_Method(Decl).Name
  else
    Result := Decl.Name;
end;

function GetHint(const Decl: TDeclaration): String;
begin
  if (Decl is TDeclaration_Method) then Result := TDeclaration_Method(Decl).HeaderString                                                                else
  if (Decl is TDeclaration_Type)   then Result := 'type '  + Decl.Name + ' = ' + Decl.TextNoCommentsSingleLine                                          else
  if (Decl is TDeclaration_Const)  then Result := 'const ' + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString else
  if (Decl is TDeclaration_Var)    then Result := 'var '   + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString else Result := '';
end;

function GetURL(const Section: String): String;
begin
  Result := '';

  case Section of
    'TPoint':        Result := 'https://villavu.github.io/Simba/TPoint.html';
    'TPointArray':   Result := 'https://villavu.github.io/Simba/TPointArray.html';
    'TBox':          Result := 'https://villavu.github.io/Simba/TBox.html';
    'TBoxArray':     Result := 'https://villavu.github.io/Simba/TBoxArray.html';
    'TQuad':         Result := 'https://villavu.github.io/Simba/TQuad.html';
    'Random':        Result := 'https://villavu.github.io/Simba/Random.html';
    'T2DPointArray': Result := 'https://villavu.github.io/Simba/T2DPointArray.html';
    'Debug Image':   Result := 'https://villavu.github.io/Simba/Debug Image.html';
    'Script':        Result := 'https://villavu.github.io/Simba/Script.html';
    'Variant':       Result := 'https://villavu.github.io/Simba/Variant.html';
    'TWindowHandle': Result := 'https://villavu.github.io/Simba/TWindowHandle.html';
    'TMufasaBitmap': Result := 'https://villavu.github.io/Simba/TMufasaBitmap.html';
    'Finder':        Result := 'https://villavu.github.io/Simba/Finder.html';
    'Input':         Result := 'https://villavu.github.io/Simba/Input.html';
    'Target':        Result := 'https://villavu.github.io/Simba/Target.html';
    'Internet':      Result := 'https://villavu.github.io/Simba/Internet.html';
  end;
end;

type
  ENodeType = (ntUnknown, ntDecl, ntSimbaDecl, ntPluginDecl, ntIncludes, ntPlugins, ntIncludeFile, ntPluginFile);

type
  TSimbaFunctionListNode = class(TTreeNode)
  public
    NodeType: ENodeType;
    Hint: String;
    Line: Integer;
    StartPos: Integer;
    EndPos: Integer;
    FileName: String;
    LastUsed: Integer;
  end;

procedure TSimbaFunctionListForm.ResetState;
begin
  FTreeView.Filter := '';
  FTreeView.FullCollapse();
  FTreeView.ScrolledTop := 0;
  FTreeView.ScrolledLeft := 0;

  FSimbaNode.Expanded := True;
  FScriptNode.Expanded := True;
end;

procedure TSimbaFunctionListForm.SaveState(TabID: Integer);
var
  State: TFunctionListState;
begin
  State := FSavedStates.GetDef(TabID, Default(TFunctionListState));
  State.ScrolledTop := FTreeView.ScrolledTop;
  State.ScrolledLeft := FTreeView.ScrolledLeft;
  State.Filter := FTreeView.Filter;
  if (State.Nodes = nil) then
    State.Nodes := TTreeNodeExpandedState.Create(TTreeNode(nil));
  State.Nodes.CreateChildNodes(FTreeView.Items.GetFirstNode());

  FSavedStates.AddOrModify(TabID, State);
end;

procedure TSimbaFunctionListForm.RestoreState(TabID: Integer);
var
  SavedState: TFunctionListState;
begin
  if FSavedStates.Get(TabID, SavedState) then
  begin
    FTreeView.FullCollapse();
    FTreeView.Filter := SavedState.Filter;
    if Assigned(SavedState.Nodes) then
      SavedState.Nodes.Apply(FTreeView.Items.GetFirstNode());
    FTreeView.ScrolledTop := SavedState.ScrolledTop;
    FTreeView.ScrolledLeft := SavedState.ScrolledLeft;
  end else
    ResetState();
end;

procedure TSimbaFunctionListForm.DeleteState(TabID: Integer);
var
  SavedState: TFunctionListState;
begin
  if FSavedStates.Get(TabID, SavedState) then
  begin
    if Assigned(SavedState.Nodes) then
      SavedState.Nodes.Free();
    FSavedStates.Remove(TabID);
  end;
end;

procedure TSimbaFunctionListForm.Fill;
var
  CanUpdate: Boolean;
  Script, ScriptFileName: String;
  ExpandScriptNode: Boolean;
  TabID: Integer;

  procedure BeginUpdate;
  begin
    CanUpdate := (FNeedUpdate or FForceUpdate) and Assigned(SimbaScriptTabsForm) and Assigned(SimbaScriptTabsForm.CurrentEditor);

    if CanUpdate then
    begin
      Script := SimbaScriptTabsForm.CurrentEditor.Text;
      ScriptFileName := SimbaScriptTabsForm.CurrentEditor.FileName;
      TabID := SimbaScriptTabsForm.CurrentTab.UID;
      ExpandScriptNode := (FScriptNode.Count = 0) or FScriptNode.Expanded;

      FTreeView.BeginUpdate();
    end;

    FNeedUpdate := False;
    FForceUpdate := False;
  end;

  procedure EndUpdate;
  var
    I: Integer;
  begin
    FScriptNode.DeleteChildren();
    for I := 0 to FCodeinsight.ScriptParser.Items.Count - 1 do
      AddDecl(FScriptNode, FCodeinsight.ScriptParser.Items[I]);

    AddIncludesNode(FCodeInsight.Includes, FCodeinsight.IncludesHash);
    AddPluginsNode(FCodeInsight.Plugins, FCodeinsight.PluginsHash);
    PurgeNodes();

    FScriptNode.Expanded := ExpandScriptNode;

    if (TabID <> FLastTabID) then
      RestoreState(TabID);

    FTreeView.Loading := False;
    FTreeView.EndUpdate();

    FLastTabID := TabID;
  end;

begin
  ExecuteOnMainThread(@BeginUpdate);

  if CanUpdate then
  begin
    FCodeInsight.SetScript(Script, ScriptFileName);
    FCodeInsight.Run();

    PurgeNodes();

    ExecuteOnMainThread(@EndUpdate);
  end;
end;

procedure TSimbaFunctionListForm.DoUpdateThread;
begin
  try
    while (not (csDestroying in ComponentState)) do
    begin
      if (FIsIdle and FNeedUpdate) or FForceUpdate then
        Fill();

      Sleep(350);
    end;
  except
    on E: Exception do
      DebugLn('FunctionList update thread exception: ' + E.Message);
  end;
end;

procedure TSimbaFunctionListForm.DoNodeDoubleClick(Sender: TObject);
var
  Node: TSimbaFunctionListNode;
begin
  Node := TSimbaFunctionListNode(FTreeView.Selected);

  if (Node is TSimbaFunctionListNode) then
    case Node.NodeType of
      ntSimbaDecl:   ShowSimbaDeclaration(Node.Hint, Node.FileName);
      ntDecl:        ShowDeclaration(Node.StartPos, Node.EndPos, Node.Line, Node.FileName);
      ntPluginDecl:  ShowPluginDeclaration(Node.Hint, Node.FileName);
      ntIncludeFile: SimbaScriptTabsForm.Open(Node.FileName);
    end;
end;

procedure TSimbaFunctionListForm.DoCodetoolsSetup(Sender: TObject);
begin
  ExecuteOnMainThread(@AddSimbaNodes);

  FUpdateThread := Threaded(@DoUpdateThread);
end;

procedure TSimbaFunctionListForm.DoAfterFilter(Sender: TObject);
begin
  if (FTreeView.Filter = '') then
    ResetState();
end;

procedure TSimbaFunctionListForm.DoEditorModified(Sender: TObject);
begin
  FNeedUpdate := True;
end;

procedure TSimbaFunctionListForm.DoTabBeforeChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    SaveState(TSimbaScriptTab(Sender).UID);
end;

procedure TSimbaFunctionListForm.DoTabChange(Sender: TObject);
begin
  FTreeView.Loading := True;

  FForceUpdate := True;
end;

procedure TSimbaFunctionListForm.DoTabClosed(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    DeleteState(TSimbaScriptTab(Sender).UID);
end;

procedure TSimbaFunctionListForm.AddPluginsNode(Plugins: TCodeParserList; Hash: String);
var
  PluginsNode: TTreeNode = nil;

  procedure Find(const ANode: TTreeNode);
  var
    Node: TSimbaFunctionListNode absolute ANode;
  begin
    if (Node.NodeType <> ntPlugins) then
      Exit;

    if (PluginsNode = nil) and (Hash <> '') and (Node.FileName = Hash) then
    begin
      PluginsNode := Node;

      Node.LastUsed := 0;
      Node.Visible := True;
    end else
    begin
      Node.LastUsed := Node.LastUsed + 1;
      Node.Visible := False;
    end;
  end;

var
  I: Integer;
  Node, ParentNode: TTreeNode;
  Decl: TDeclaration;
begin
  FTreeView.ForEachTopLevel(@Find); // Find previously added node

  if (PluginsNode = nil) and (Plugins.Count > 0) then // No? Build it
  begin
    PluginsNode := FTreeView.AddNode('Plugins', IMAGE_DIRECTORY);
    with TSimbaFunctionListNode(PluginsNode) do
    begin
      NodeType := ntPlugins;
      FileName := Hash;

      Node := FTreeView.Items.FindTopLvlNode('Includes');
      if Assigned(Node) then
        Index := Node.Index + 1
      else
        Index := 1;
    end;

    for I := 0 to Plugins.Count - 1 do
    begin
      ParentNode := FTreeView.AddNode(PluginsNode, ChangeFileExt(ExtractFileName(Plugins[I].FileName), ''), IMAGE_FILE);
      with TSimbaFunctionListNode(ParentNode) do
      begin
        NodeType := ntPluginFile;
        Hint := Plugins[I].FileName;
      end;

      for Decl in Plugins[I].Items.ToArray do
        if (Decl.Name <> '') then
          AddPluginDecl(ParentNode, Decl);
    end;
  end;
end;

procedure TSimbaFunctionListForm.AddIncludesNode(Includes: TCodeParserList; Hash: String);
var
  IncludesNode: TTreeNode = nil;

  procedure Find(const ANode: TTreeNode);
  var
    Node: TSimbaFunctionListNode absolute ANode;
  begin
    if (Node.NodeType <> ntIncludes) then
      Exit;

    if (IncludesNode = nil) and (Hash <> '') and (Node.FileName = Hash) then
    begin
      IncludesNode := Node;

      Node.LastUsed := 0;
      Node.Visible := True;
    end else
    begin
      Node.LastUsed := Node.LastUsed + 1;
      Node.Visible := False;
    end;
  end;

var
  I: Integer;
  CurrentFile: String;
  CurrentNode: TTreeNode;
  Decl: TDeclaration;
begin
  FTreeView.ForEachTopLevel(@Find); // Find previously added node

  if (IncludesNode = nil) and (Includes.Count > 0) then // No? Build it
  begin
    IncludesNode := FTreeView.AddNode('Includes', IMAGE_DIRECTORY);
    with TSimbaFunctionListNode(IncludesNode) do
    begin
      NodeType := ntIncludes;
      FileName := Hash;
      Index := 1;
    end;

    CurrentFile := '';
    CurrentNode := nil;

    for I := 0 to Includes.Count - 1 do
    begin
      for Decl in Includes[I].Items.ToArray do
      begin
        if (CurrentFile <> Decl.Lexer.FileName) then
        begin
          CurrentFile := Decl.Lexer.FileName;
          CurrentNode := FTreeView.AddNode(IncludesNode, ChangeFileExt(ExtractFileName(CurrentFile), ''), IMAGE_FILE);

          with TSimbaFunctionListNode(CurrentNode) do
          begin
            Hint := CurrentFile;
            FileName := CurrentFile;
            NodeType := ntIncludeFile;
          end;
        end;

        if (Decl.Name <> '') then
          AddDecl(CurrentNode, Decl);
      end;
    end;
  end;
end;

procedure TSimbaFunctionListForm.PurgeNodes;
var
  NodesToPurge: array of TTreeNode;

  procedure Find(const ANode: TTreeNode);
  var
    Node: TSimbaFunctionListNode absolute ANode;
  begin
    if (Node.NodeType in [ntIncludes, ntPlugins]) and (Node.LastUsed > 50) then
      NodesToPurge := NodesToPurge + [ANode];
  end;

var
  I: Integer;
begin
  NodesToPurge := [];

  if (FTreeView.TopLevelCount > 8) then
  begin
    FTreeView.ForEachTopLevel(@Find);
    for I := 0 to High(NodesToPurge) do
      NodesToPurge[I].Free();
  end;
end;

function TSimbaFunctionListForm.DoGetNodeHint(const Node: TTreeNode): String;
begin
  if (Node is TSimbaFunctionListNode) then
    Result := TSimbaFunctionListNode(Node).Hint
  else
    Result := '';
end;

procedure TSimbaFunctionListForm.DoSelectionChanged(Sender: TObject);
var
  Node: TSimbaFunctionListNode;
begin
  Node := TSimbaFunctionListNode(FTreeView.Selected);
  if (Node is TSimbaFunctionListNode) then
    SimbaMainStatusBar.SetMainPanelText(Node.Hint);
end;

function TSimbaFunctionListForm.AddDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
begin
  Result := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Result) do
  begin
    NodeType := ntDecl;

    FileName := Decl.Lexer.FileName;
    StartPos := Decl.StartPos;
    EndPos   := Decl.EndPos;
    Line     := Decl.Line;

    Text := GetText(Decl);
    Hint := GetHint(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);
  end;
end;

function TSimbaFunctionListForm.AddSimbaDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
begin
  Result := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Result) do
  begin
    NodeType := ntSimbaDecl;

    FileName := Decl.Lexer.FileName;

    Text := GetText(Decl);
    Hint := GetHint(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);
  end;
end;

function TSimbaFunctionListForm.AddPluginDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
begin
  Result := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Result) do
  begin
    NodeType := ntPluginDecl;

    FileName := Decl.Lexer.FileName;

    Text := GetText(Decl);
    Hint := GetHint(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);
  end;
end;

function TSimbaFunctionListForm.CompareDecl(A, B: TTreeNode): Integer;
begin
  Result := NaturalCompareText(A.Text, B.Text);

  case A.ImageIndex of
    IMAGE_TYPE:      Dec(Result, 2000);
    IMAGE_CONSTANT:  Dec(Result, 1500);
    IMAGE_VARIABLE:  Dec(Result, 1000);
    IMAGE_PROCEDURE: Dec(Result, 500);
    IMAGE_FUNCTION:  Dec(Result, 500);
  end;

  case B.ImageIndex of
    IMAGE_TYPE:      Inc(Result, 2000);
    IMAGE_CONSTANT:  Inc(Result, 1500);
    IMAGE_VARIABLE:  Inc(Result, 1000);
    IMAGE_PROCEDURE: Inc(Result, 500);
    IMAGE_FUNCTION:  Inc(Result, 500);
  end;
end;

procedure TSimbaFunctionListForm.AddSimbaNodes;
var
  I: Integer;
  Decl: TDeclaration;
  Parser: TCodeParser;
  ParentNode: TTreeNode;
begin
  FTreeView.BeginUpdate();

  for I := 0 to TCodeinsight.BaseIncludes.Count - 1 do
  begin
    Parser := TCodeinsight.BaseIncludes[I];
    if (Parser = nil) or (Parser.Items.Count = 0) or (Parser.FileName.StartsWith('!')) then
      Continue;

    ParentNode := FTreeView.AddNode(FSimbaNode, Parser.FileName, IMAGE_FILE);
    with TSimbaFunctionListNode(ParentNode) do
    begin
      FileName := GetURL(Parser.FileName);
      if (FileName <> '') then
        Hint := Text + ' (double click to open online documentation)';
    end;

    for Decl in Parser.Items.ToArray do
      if (Decl.Name <> '') and (not Decl.isOverrideMethod) then
        AddSimbaDecl(ParentNode, Decl);

    ParentNode.CustomSort(@CompareDecl);
  end;

  FSimbaNode.AlphaSort();
  FSimbaNode.Expanded := True; // This needs to be on main thread it seems?

  FTreeView.Loading := False;
  FTreeView.EndUpdate();
end;

procedure TSimbaFunctionListForm.DoIdleBegin(Sender: TObject);
begin
  FIsIdle := True;
end;

procedure TSimbaFunctionListForm.DoIdleEnd(Sender: TObject);
begin
  FIsIdle := False;
end;

constructor TSimbaFunctionListForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FTreeView := TSimbaTreeView.Create(Self, TSimbaFunctionListNode);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaForm.Images;
  FTreeView.OnDoubleClick := @DoNodeDoubleClick;
  FTreeView.OnSelectionChange := @DoSelectionChanged;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.Loading := True;

  FScriptNode := FTreeView.AddNode('Script', IMAGE_DIRECTORY);
  FSimbaNode  := FTreeView.AddNode('Simba',  IMAGE_DIRECTORY);

  FCodeinsight := TCodeinsight.Create();
  FCodeinsight.ScriptParser.NoErrorMessages := True;

  FSavedStates := TFunctionListStateDict.Create(@HashInt32);

  SimbaIDEEvents.RegisterOnCodetoolsSetup(@DoCodetoolsSetup);
  SimbaIDEEvents.RegisterMethodOnEditorModified(@DoEditorModified);
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoTabChange);
  SimbaIDEEvents.RegisterOnBeforeTabChange(@DoTabBeforeChange);
  SimbaIDEEvents.RegisterOnTabClosed(@DoTabClosed);

  with TIdleTimer.Create(Self) do
  begin
    AutoEnabled := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent := itaOnUserInput;
    Interval := 500;
    OnTimer := @DoIdleBegin;
    OnStopTimer := @DoIdleEnd;
  end;
end;

destructor TSimbaFunctionListForm.Destroy;
begin
  FUpdateThread.Terminate();
  FUpdateThread.WaitFor();
  FUpdateThread.Free();

  FCodeinsight.Free();

  FSavedStates.Free();

  inherited Destroy();
end;

end.

