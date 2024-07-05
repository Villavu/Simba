{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_functionlist;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Menus, StrUtils,
  simba.base, simba.ide_codetools_base, simba.ide_codetools_parser, simba.ide_codetools_insight,
  simba.component_treeview, simba.container_dict;

type
  ENodeType = (ntUnknown, ntSimbaSection, ntDecl, ntSimbaDecl, ntPluginDecl, ntIncludes, ntPlugins, ntIncludeFile, ntPluginFile);

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

  TSimbaFunctionListForm = class(TForm)
    MenuItemHideAll: TMenuItem;
    MenuItemShowAll: TMenuItem;
    PopupItemShowMouseoverHint: TMenuItem;
    PopupItemShowHide: TMenuItem;
    ContextMenu: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    procedure MenuItemHideAllClick(Sender: TObject);
    procedure MenuItemShowAllClick(Sender: TObject);
    procedure PopupItemShowMouseoverHintClick(Sender: TObject);
    procedure ContextMenuPopup(Sender: TObject);
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
    procedure DoPopupShowHideClick(Sender: TObject);

    procedure AddPluginsNode(Plugins: TCodeParserList; Hash: String);
    procedure AddIncludesNode(Includes: TCodeParserList; Hash: String);
    procedure PurgeNodes;

    procedure AddDecl(ParentNode: TTreeNode; Decl: TDeclaration);
    procedure AddSimbaDecl(ParentNode: TTreeNode; Decl: TDeclaration);
    procedure AddPluginDecl(ParentNode: TTreeNode; Decl: TDeclaration);

    function ShouldSimbaNodeBeHidden(S: String): Boolean;
    procedure SetSimbaNodeShouldBeHidden(S: String; Hidden: Boolean);

    procedure AddSimbaNodes;
    procedure ArrangeSimbaNodes;

    function CompareNodes(A, B: TTreeNode): Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  simba.form_main, simba.ide_events, simba.threading, simba.settings,
  simba.form_tabs, simba.ide_tab, simba.ide_showdeclaration, simba.nativeinterface,
  simba.vartype_string;

function GetImage(const Decl: TDeclaration): Integer;
begin
  Result := DeclarationImage(Decl);
end;

function GetText(const Decl: TDeclaration): String;
begin
  Result := Decl.FullName;
end;

function GetHint(const Decl: TDeclaration): String;
begin
  if (Decl is TDeclaration_Property) then
    Result := PropertyHeader(Decl as TDeclaration_Property)
  else
    Result := Decl.Header;
end;

function GetURL(const Section: String): String;
const
  ROOT = 'https://villavu.github.io/Simba/api/';
begin
  Result := '';

  case Section of
    'Base':           Result := ROOT + 'Base.html';
    'TPoint':         Result := ROOT + 'TPoint.html';
    'TPointArray':    Result := ROOT + 'TPointArray.html';
    'TBox':           Result := ROOT + 'TBox.html';
    'TBoxArray':      Result := ROOT + 'TBoxArray.html';
    'TQuad':          Result := ROOT + 'TQuad.html';
    'Random':         Result := ROOT + 'Random.html';
    'T2DPointArray':  Result := ROOT + 'T2DPointArray.html';
    'Date & Time':    Result := ROOT + 'Date %26 Time';
    'TWindowHandle':  Result := ROOT + 'TWindowHandle.html';
    'Image':          Result := ROOT + 'Image.html';
    'Finder':         Result := ROOT + 'Finder.html';
    'Input':          Result := ROOT + 'Input.html';
    'Target':         Result := ROOT + 'Target.html';
    'Web':            Result := ROOT + 'Web.html';
    'Encoding':       Result := ROOT + 'Encoding.html';
    'JSON':           Result := ROOT + 'JSON.html';
    'Color Math':     Result := ROOT + 'Color Math.html';
    'File':           Result := ROOT + 'File.html';
    'Timing':         Result := ROOT + 'Timing.html';
    'String':         Result := ROOT + 'String.html';
    'Process':        Result := ROOT + 'Process.html';
    'Match Template': Result := ROOT + 'Match Template.html';
    'Math':           Result := ROOT + 'Math.html';
    'Matrix':         Result := ROOT + 'Matrix.html';
    'Misc':           Result := ROOT + 'Misc.html';
    'DTM':            Result := ROOT + 'DTM.html';
    'TCircle':        Result := ROOT + 'TCircle.html';
  end;
end;

procedure TSimbaFunctionListForm.ContextMenuPopup(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
  Val: String;
begin
  if (PopupItemShowHide.Count = 0) then
    for I := 0 to FSimbaNode.Count - 1 do
    begin
      Item := TMenuItem.Create(PopupItemShowHide);
      Item.Caption := FSimbaNode.Items[I].Text;
      Item.AutoCheck := True;
      Item.OnClick := @DoPopupShowHideClick;

      PopupItemShowHide.Add(Item);
    end;

  Val := SimbaSettings.FunctionList.HiddenSimbaSections.Value;
  for I := 0 to PopupItemShowHide.Count - 1 do
    PopupItemShowHide.Items[i].Checked := not (PopupItemShowHide.Items[I].Caption + ',' in Val);

  PopupItemShowMouseoverHint.Checked := SimbaSettings.FunctionList.ShowMouseoverHint.Value;
end;

procedure TSimbaFunctionListForm.PopupItemShowMouseoverHintClick(Sender: TObject);
begin
  SimbaSettings.FunctionList.ShowMouseoverHint.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaFunctionListForm.MenuItemShowAllClick(Sender: TObject);
var
  I: Integer;
begin
  SimbaSettings.FunctionList.HiddenSimbaSections.Value := '';
  for I := 0 to FSimbaNode.Count - 1 do
    FSimbaNode.Items[I].Visible := True;
  ContextMenu.Close();
end;

procedure TSimbaFunctionListForm.MenuItemHideAllClick(Sender: TObject);
var
  I: Integer;
  Val: String;
begin
  Val := '';
  for I := 0 to PopupItemShowHide.Count - 1 do
    Val += PopupItemShowHide.Items[I].Caption + ',';

  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Val;
  for I := 0 to FSimbaNode.Count - 1 do
    FSimbaNode.Items[I].Visible := False;

  ContextMenu.Close();
end;

procedure TSimbaFunctionListForm.ResetState;

  procedure HideAllIncludePluginNodes(const ANode: TTreeNode);
  var
    Node: TSimbaFunctionListNode absolute ANode;
  begin
    if (Node.NodeType in [ntIncludes, ntPlugins]) then
    begin
      Node.Visible := False;
      Node.Enabled := False;
    end;
  end;

begin
  FTreeView.Filter := '';
  FTreeView.FullCollapse();
  FTreeView.ScrolledTop := 0;
  FTreeView.ScrolledLeft := 0;

  FSimbaNode.Expanded := True;
  FScriptNode.Expanded := True;

  FTreeView.ForEachTopLevel(@HideAllIncludePluginNodes);

  FForceUpdate := True;
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
    CanUpdate := (FNeedUpdate or FForceUpdate) and Assigned(SimbaTabsForm) and Assigned(SimbaTabsForm.CurrentEditor);

    if CanUpdate then
    begin
      Script := SimbaTabsForm.CurrentEditor.Text;
      ScriptFileName := SimbaTabsForm.CurrentTab.ScriptFileName;
      TabID := SimbaTabsForm.CurrentTab.UID;
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

    AddIncludesNode(FCodeInsight.IncludeParsers, FCodeinsight.IncludesHash);
    AddPluginsNode(FCodeInsight.PluginParsers, FCodeinsight.PluginsHash);
    PurgeNodes();

    FScriptNode.Expanded := ExpandScriptNode;

    if (TabID <> FLastTabID) then
      RestoreState(TabID);

    FTreeView.Loading := False;
    FTreeView.EndUpdate();

    FLastTabID := TabID;
  end;

begin
  RunInMainThread(@BeginUpdate);

  if {%H-}CanUpdate then
  begin
    FCodeInsight.SetScript(Script, ScriptFileName);
    FCodeInsight.Run();

    PurgeNodes();

    RunInMainThread(@EndUpdate);
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
  begin
    case Node.NodeType of
      ntSimbaDecl:    ShowSimbaDeclaration(Node.Hint, Node.FileName);
      ntDecl:         ShowDeclaration(Node.StartPos, Node.EndPos, Node.Line, Node.FileName);
      ntPluginDecl:   ShowPluginDeclaration(Node.Hint, Node.FileName);
      ntIncludeFile:  SimbaTabsForm.Open(Node.FileName);
      ntSimbaSection: SimbaNativeInterface.OpenURL(Node.FileName);
    end;
  end;
end;

procedure TSimbaFunctionListForm.DoCodetoolsSetup(Sender: TObject);
begin
  RunInMainThread(@AddSimbaNodes);

  FUpdateThread := RunInThread(@DoUpdateThread);
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

procedure TSimbaFunctionListForm.DoPopupShowHideClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  SetSimbaNodeShouldBeHidden(TMenuItem(Sender).Caption, not TMenuItem(Sender).Checked);

  Node := FSimbaNode.FindNode(TMenuItem(Sender).Caption);
  if Assigned(Node) then
    Node.Visible := TMenuItem(Sender).Checked;
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
      Node.Enabled := True;
    end else
    begin
      Node.LastUsed := Node.LastUsed + 1;
      Node.Visible := False;
      Node.Enabled := False;
    end;
  end;

var
  I, J: Integer;
  Node, ParentNode: TTreeNode;
  Decl: TDeclaration;
begin
  FTreeView.ForEachTopLevel(@Find); // Find previously added node

  if (PluginsNode = nil) and (Plugins.Count > 0) then // No? Build it
  begin
    PluginsNode := FTreeView.AddNode('Plugins', IMG_FOLDER);
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
      ParentNode := FTreeView.AddNode(PluginsNode, ChangeFileExt(ExtractFileName(Plugins[I].Lexer.FileName), ''), IMG_FILE);
      with TSimbaFunctionListNode(ParentNode) do
      begin
        NodeType := ntPluginFile;
        Hint := Plugins[I].Lexer.FileName;
      end;

      for J := 0 to Plugins[I].Items.Count - 1 do
      begin
        Decl := Plugins[I].Items[J];
        if (Decl.Name <> '') then
          AddPluginDecl(ParentNode, Decl);
      end;
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
      Node.Enabled := True;
    end else
    begin
      Node.LastUsed := Node.LastUsed + 1;
      Node.Visible := False;
      Node.Enabled := False;
    end;
  end;

var
  I, J: Integer;
  CurrentFile: String;
  CurrentNode: TTreeNode;
  Decl: TDeclaration;
begin
  FTreeView.ForEachTopLevel(@Find); // Find previously added node

  if (IncludesNode = nil) and (Includes.Count > 0) then // No? Build it
  begin
    IncludesNode := FTreeView.AddNode('Includes', IMG_FOLDER);
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
      for J := 0 to Includes[I].Items.Count - 1 do
      begin
        Decl := Includes[I].Items[J];

        // includes are flattened
        // so add a new node when file changes
        if (CurrentFile <> Decl.DocPos.FileName) then
        begin
          CurrentFile := Decl.DocPos.FileName;
          CurrentNode := FTreeView.AddNode(IncludesNode, ChangeFileExt(ExtractFileName(CurrentFile), ''), IMG_FILE);

          with TSimbaFunctionListNode(CurrentNode) do
          begin
            Hint := CurrentFile;
            FileName := CurrentFile;
            NodeType := ntIncludeFile;
          end;
        end;

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
  Result := '';

  if SimbaSettings.FunctionList.ShowMouseoverHint.Value then
  begin
    if (Node is TSimbaFunctionListNode) then
    begin
      Result := TSimbaFunctionListNode(Node).Hint;
      if (Length(Result) > 100) then
        Result := Copy(Result, 1, 100) + ' ...';
    end;
  end;
end;

procedure TSimbaFunctionListForm.DoSelectionChanged(Sender: TObject);
begin
  SimbaIDEEvents.Notify(SimbaIDEEvent.FUNCTIONLIST_SELECTION, FTreeView.Selected);
end;

procedure TSimbaFunctionListForm.AddDecl(ParentNode: TTreeNode; Decl: TDeclaration);
var
  Node: TTreeNode;
  I: Integer;
begin
  if (Decl.Name = '') then
    Exit;

  Node := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Node) do
  begin
    NodeType := ntDecl;

    FileName := Decl.DocPos.FileName;
    StartPos := Decl.StartPos;
    EndPos   := Decl.EndPos;
    Line     := Decl.DocPos.Line;

    Text := GetText(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);

    if (Decl is TDeclaration_TypeRecord) or (Decl is TDeclaration_TypeEnum) then
      for I := 0 to Decl.Items.Count - 1 do
        AddDecl(Node, Decl.Items[I])
    else
      Hint := GetHint(Decl);
  end;
end;

procedure TSimbaFunctionListForm.AddSimbaDecl(ParentNode: TTreeNode; Decl: TDeclaration);
var
  Node: TTreeNode;
  I: Integer;
begin
  // skip empty names or methods that start with _ or overrides
  if (Decl.Name = '') or ((Decl is TDeclaration_Method) and ((Decl.Name[1] = '_') or TDeclaration_Method(Decl).isOverride)) then
    Exit;

  Node := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Node) do
  begin
    NodeType := ntSimbaDecl;

    FileName := Decl.DocPos.FileName;

    Text := GetText(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);

    if (Decl is TDeclaration_TypeRecord) or (Decl is TDeclaration_TypeEnum) then
      for I := 0 to Decl.Items.Count - 1 do
        AddSimbaDecl(Node, Decl.Items[I])
    else
      Hint := GetHint(Decl);
  end;
end;

procedure TSimbaFunctionListForm.AddPluginDecl(ParentNode: TTreeNode; Decl: TDeclaration);
var
  Node: TTreeNode;
  I: Integer;
begin
  if (Decl.Name = '') then
    Exit;

  Node := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Node) do
  begin
    NodeType := ntPluginDecl;

    FileName := Decl.DocPos.FileName;

    Text := GetText(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);

    if (Decl is TDeclaration_TypeRecord) or (Decl is TDeclaration_TypeEnum) then
      for I := 0 to Decl.Items.Count - 1 do
        AddPluginDecl(Node, Decl.Items[I])
    else
      Hint := GetHint(Decl);
  end;
end;

function TSimbaFunctionListForm.ShouldSimbaNodeBeHidden(S: String): Boolean;
var
  Val: String;
begin
  Val := SimbaSettings.FunctionList.HiddenSimbaSections.Value;

  Result := Val.Contains(S + ',');
end;

procedure TSimbaFunctionListForm.SetSimbaNodeShouldBeHidden(S: String; Hidden: Boolean);
var
  Val: String;
begin
  Val := SimbaSettings.FunctionList.HiddenSimbaSections.Value;
  Val := Val.Replace(S + ',', ''); // remove
  if Hidden then
    Val := Val + S + ','; // add

  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Val;
end;

procedure TSimbaFunctionListForm.AddSimbaNodes;
var
  I: Integer;
  Decl: TDeclaration;
  Parser: TCodeParser;
  ParentNode: TTreeNode;
begin
  FTreeView.BeginUpdate();

  for I := 0 to TCodeinsight.BaseParsers.Count - 1 do
  begin
    Parser := TCodeinsight.BaseParsers[I];
    if (Parser = nil) or (Parser.Items.Count = 0) or (Parser.Lexer.FileName.StartsWith('!')) then
      Continue;

    ParentNode := FTreeView.AddNode(FSimbaNode, Parser.Lexer.FileName, IMG_FILE);
    with TSimbaFunctionListNode(ParentNode) do
    begin
      NodeType := ntSimbaSection;
      FileName := GetURL(Parser.Lexer.FileName);
      if (FileName <> '') then
        Hint := Text + ' (double click to open online docs)';
    end;

    for Decl in RemoveDuplicateProperties(Parser.Items.ToArray) do
      AddSimbaDecl(ParentNode, Decl);

    ParentNode.CustomSort(@CompareNodes);
    if ShouldSimbaNodeBeHidden(ParentNode.Text) then
      ParentNode.Visible := False;
  end;

  //FSimbaNode.AlphaSort();
  FSimbaNode.Expanded := True; // This needs to be on main thread it seems?

  ArrangeSimbaNodes();

  FTreeView.Loading := False;
  FTreeView.EndUpdate();
end;

procedure TSimbaFunctionListForm.ArrangeSimbaNodes;
var
  Cur: Integer = 0;

  procedure MoveToTop(const NodeText: String);
  var
    Node: TTreeNode;
  begin
    Node := FSimbaNode.FindNode(NodeText);
    if not Assigned(Node) then
    begin
      DebugLn('ArrangeSimbaNodes: Not found: ' + NodeText);
      Exit;
    end;

    Node.Index := Cur;
    Inc(Cur);
  end;

begin
  MoveToTop('Base');
  MoveToTop('Math');
  MoveToTop('Color Math');
  MoveToTop('File');
  MoveToTop('String');
  MoveToTop('Random');
  MoveToTop('Date & Time');
  MoveToTop('TBox');
  MoveToTop('TBoxArray');
  MoveToTop('TPoint');
  MoveToTop('TPointArray');
  MoveToTop('T2DPointArray');
  MoveToTop('TCircle');
  MoveToTop('TQuad');
  MoveToTop('Target');
  MoveToTop('Image');
  MoveToTop('Debug Image');
  MoveToTop('Match Template');
  MoveToTop('Matrix');
  MoveToTop('DTM');
  MoveToTop('TWindowHandle');
  MoveToTop('Process');
  MoveToTop('JSON');
  MoveToTop('Encoding');
  MoveToTop('Web');
  MoveToTop('ASync');
  MoveToTop('Misc');
end;

function TSimbaFunctionListForm.CompareNodes(A, B: TTreeNode): Integer;
begin
  Result := NaturalCompareText(A.Text, B.Text);

  if ((A.ImageIndex = IMG_FUNC) or (A.ImageIndex = IMG_PROC) or (A.ImageIndex = IMG_PROPERTY)) and
     ((B.ImageIndex = IMG_FUNC) or (B.ImageIndex = IMG_PROC) or (B.ImageIndex = IMG_PROPERTY)) then
  begin
    if ('.' in A.Text) then Dec(Result, 100);
    if ('.' in B.Text) then Inc(Result, 100);
  end;

  case A.ImageIndex of
    IMG_TYPE:     Dec(Result, 500);
    IMG_CONST:    Dec(Result, 400);
    IMG_VAR:      Dec(Result, 300);
    IMG_PROC:     Dec(Result, 200);
    IMG_FUNC:     Dec(Result, 200);
    IMG_PROPERTY: Dec(Result, 200);
  end;

  case B.ImageIndex of
    IMG_TYPE:     Inc(Result, 500);
    IMG_CONST:    Inc(Result, 400);
    IMG_VAR:      Inc(Result, 300);
    IMG_PROC:     Inc(Result, 200);
    IMG_FUNC:     Inc(Result, 200);
    IMG_PROPERTY: Inc(Result, 200);
  end;

  if (A.ImageIndex = IMG_PROPERTY) and (A.Text.Before('.') = B.Text.Before('.')) then
    Inc(Result, 100);
  if (B.ImageIndex = IMG_PROPERTY) and (A.Text.Before('.') = B.Text.Before('.')) then
    Dec(Result, 100);
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
  FTreeView.Images := SimbaMainForm.Images;
  FTreeView.OnDoubleClick := @DoNodeDoubleClick;
  FTreeView.OnSelectionChange := @DoSelectionChanged;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.Loading := True;

  FScriptNode := FTreeView.AddNode('Script', IMG_FOLDER);
  FSimbaNode  := FTreeView.AddNode('Simba',  IMG_FOLDER);

  FCodeinsight := TCodeinsight.Create();
  FCodeinsight.ScriptParser.SkipErrorMessages := True;

  FSavedStates := TFunctionListStateDict.Create();

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.CODETOOLS_SETUP,  @DoCodetoolsSetup);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_MODIFIED,     @DoEditorModified);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE,       @DoTabChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_BEFORECHANGE, @DoTabBeforeChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CLOSED,       @DoTabClosed);

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

