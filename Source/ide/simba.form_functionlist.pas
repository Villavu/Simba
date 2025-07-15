{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_functionlist;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Menus, ImgList, syncobjs,
  simba.base,
  simba.ide_codetools_parser,
  simba.ide_codetools_insight,
  simba.ide_tab,
  simba.component_treeview,
  simba.component_notebook,
  simba.settings;

type
  ENodeType = (ntNone, ntSimbaFile, ntScriptFile, ntPluginFile, ntSimbaDecl, ntScriptDecl, ntPluginDecl);

  TSimbaFunctionListPage = class(TSimbaPage)
  protected
    FTabID: Integer;
    FTreeView: TSimbaTreeView;
    FCodeInsight: TCodeinsight;
    FNeedUpdate: Boolean;

    FScriptNode: TTreeNode;
    FIncludesNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FSimbaNode: TTreeNode;

    FScriptNodeState: TTreeNodeExpandedState;
    FIncludesNodeState: TTreeNodeExpandedState;
    FPluginsNodeState: TTreeNodeExpandedState;

    FMenuHideShowSection: TMenuItem;

    procedure AddDecl(ParentNode: TTreeNode; Decl: TDeclaration; ANodeType: ENodeType);
    procedure AddSimbaNode;

    // returns true if some changes were made
    function AddIncludes(Parsers: TCodeParserList; ParentNode: TTreeNode; DeclType, FileType: ENodeType): Boolean;

    procedure DoHiddenSimbaSectionsChange(Setting: TSimbaSetting);
    procedure DoCustomOrderChange(Setting: TSimbaSetting);
    procedure DoEditorModified(Sender: TObject);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoNodeDoubleClick(Sender: TObject);
    function DoGetNodeHint(Node: TTreeNode): String;

    procedure DoOpenSimbaDocClick(Sender: TObject);
    procedure DoOpenContextMenu(Sender: TObject);
    procedure DoUpdateHiddenSections(Sender: TObject);
    procedure DoMouseOverTooltipClick(Sender: TObject);
    procedure DoShowAllClick(Sender: TObject);
    procedure DoHideAllClick(Sender: TObject);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoAfterFilter(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Fill;
  end;

  TSimbaFunctionListNode = class(TTreeNode)
  public
    NodeType: ENodeType;
    Hint: String;
    Line: Integer;
    StartPos: Integer;
    EndPos: Integer;
    FileName: String;
    Hash: String;
  end;

  TSimbaFunctionListForm = class(TForm)
    Images: TImageList;

    procedure ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
  protected
    FUpdateLock: TCriticalSection;
    FUpdateThread: TThread;
    FIsIdle: Boolean;
    FNotebook: TSimbaNotebook;

    function PageForTab(Tab: TSimbaScriptTab): TSimbaPage;

    procedure DoUpdateThread;
    procedure DoIdleBegin(Sender: TObject);
    procedure DoIdleEnd(Sender: TObject);
    procedure DoCodetoolsSetup(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoTabClosed(Sender: TObject);
    procedure DoTabAdd(Sender: TObject);

    procedure DoDoubleClickSplitter(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{.$DEFINE DEBUG}

uses
  AnchorDocking,
  simba.ide_events,
  simba.ide_utils,
  simba.ide_showdeclaration,
  simba.vartype_string,
  simba.threading,
  simba.form_tabs,
  simba.nativeinterface,
  simba.fs;

const
  IMG_FOLDER = 0;
  IMG_FILE   = 1;
  IMG_FUNC   = 2;
  IMG_TYPE   = 3;
  IMG_VAR    = 4;
  IMG_PROP   = 5;
  IMG_ENUM   = 6;
  IMG_ANCHOR = 7;
  IMG_SIMBA  = 8;

function GetImage(const Decl: TDeclaration): Integer;
begin
  if (Decl is TDeclaration_Property) then
    Result := IMG_PROP
  else if (Decl is TDeclaration_Method) then
    Result := IMG_FUNC
  else if (Decl is TDeclaration_Type) then
    Result := IMG_TYPE
  else if (Decl is TDeclaration_Var) then
    Result := IMG_VAR
  else if (Decl is TDeclaration_EnumElement) then
    Result := IMG_ENUM
  else if (Decl is TDeclaration_Anchor) then
    Result := IMG_ANCHOR
  else
    Result := -1;
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

procedure TSimbaFunctionListPage.AddDecl(ParentNode: TTreeNode; Decl: TDeclaration; ANodeType: ENodeType);
var
  Node: TTreeNode;
  I: Integer;
begin
  if (Decl.Name = '') then
    Exit;
  if (ANodeType = ntSimbaDecl) and ((Decl is TDeclaration_Method) and ((Decl.Name[1] = '_') or TDeclaration_Method(Decl).isOverride)) then
    Exit;

  Node := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Node) do
  begin
    NodeType := ANodeType;

    FileName := Decl.DocPos.FileName;
    StartPos := Decl.StartPos;
    EndPos   := Decl.EndPos;
    Line     := Decl.DocPos.Line;

    Text := GetText(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);

    if (Decl is TDeclaration_TypeRecord) or (Decl is TDeclaration_TypeEnum) then
    begin
      for I := 0 to Decl.Items.Count - 1 do
        AddDecl(Node, Decl.Items[I], ANodeType);
    end else
      Hint := GetHint(Decl);
  end;
end;

procedure TSimbaFunctionListPage.AddSimbaNode;
var
  I: Integer;
  Decl: TDeclaration;
  Parser: TCodeParser;
  ParentNode: TTreeNode;
begin
  if (FSimbaNode <> nil) then
    Exit;
  FSimbaNode := FTreeView.AddNode('Simba', IMG_FOLDER);

  for I := 0 to TCodeinsight.BaseParsers.Count - 1 do
  begin
    Parser := TCodeinsight.BaseParsers[I];
    if (Parser = nil) or (Parser.Items.Count = 0) or (Parser.Lexer.FileName.StartsWith('!')) then
      Continue;

    ParentNode := FTreeView.AddNode(FSimbaNode, Parser.Lexer.FileName, IMG_FILE);
    TSimbaFunctionListNode(ParentNode).NodeType := ntSimbaFile;
    for Decl in RemoveDuplicateProperties(Parser.Items.ToArray) do
      AddDecl(ParentNode, Decl, ntSimbaDecl);
  end;

  FSimbaNode.AlphaSort();
  FSimbaNode.Expanded := True;

  DoHiddenSimbaSectionsChange(SimbaSettings.FunctionList.HiddenSimbaSections);
  DoCustomOrderChange(SimbaSettings.FunctionList.CustomOrder);
end;

function TSimbaFunctionListPage.AddIncludes(Parsers: TCodeParserList; ParentNode: TTreeNode; DeclType, FileType: ENodeType): Boolean;

  function ShortenFileName(FileName: String): String;
  begin
    if TSimbaPath.PathIsInDir(FileName, Application.Location) then
      Result := TSimbaPath.PathExtractRelative(Application.Location, FileName)
    else
      Result := FileName;
  end;

  function NeedRemove(Node: TTreeNode): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Parsers.Count - 1 do
      if (TSimbaFunctionListNode(Node).Hash = Parsers[I].Hash) then
        Exit(False);

    Result := True;
  end;

  function NeedAdding(Parser: TCodeParser): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to ParentNode.Count - 1 do
      if (TSimbaFunctionListNode(ParentNode[I]).Hash = Parser.Hash) then
        Exit(False);

    Result := True;
  end;

  procedure Add(Parser: TCodeParser);
  var
    I: Integer;
    Decl: TDeclaration;
    CurrentFile: String;
    CurrentNode: TTreeNode;
    RootNode: TTreeNode;
  begin
    // include has multiple files so keep track of current file and add new nodes when needed
    if (Parser.LexersCount > 1) then
    begin
      RootNode := FTreeView.AddNode(ParentNode, TSimbaPath.PathExtractNameWithoutExt(Parser.Lexer.FileName), IMG_FOLDER);
      TSimbaFunctionListNode(RootNode).NodeType := FileType;
      TSimbaFunctionListNode(RootNode).Hint := ShortenFileName(Parser.Lexer.FileName);
      TSimbaFunctionListNode(RootNode).FileName := Parser.Lexer.FileName;
      TSimbaFunctionListNode(RootNode).Hash := Parser.Hash;

      CurrentFile := '';
      CurrentNode := nil;
      for I := 0 to Parser.Items.Count - 1 do
      begin
        Decl := Parser.Items[I];

        // includes are flattened
        // so add a new node when file changes
        if (CurrentFile <> Decl.DocPos.FileName) then
        begin
          CurrentFile := Decl.DocPos.FileName;
          CurrentNode := FTreeView.AddNode(RootNode, TSimbaPath.PathExtractNameWithoutExt(CurrentFile), IMG_FILE);

          with TSimbaFunctionListNode(CurrentNode) do
          begin
            Hint := ShortenFileName(CurrentFile);
            FileName := CurrentFile;
            NodeType := ntScriptFile;
          end;
        end;

        AddDecl(CurrentNode, Decl, DeclType);
      end;
    end else
    begin
      RootNode := FTreeView.AddNode(ParentNode, TSimbaPath.PathExtractNameWithoutExt(Parser.Lexer.FileName), IMG_FILE);
      TSimbaFunctionListNode(RootNode).NodeType := FileType;
      TSimbaFunctionListNode(RootNode).FileName := Parser.Lexer.FileName;
      TSimbaFunctionListNode(RootNode).Hint := ShortenFileName(Parser.Lexer.FileName);
      TSimbaFunctionListNode(RootNode).Hash := Parser.Hash;
      for I := 0 to Parser.Items.Count - 1 do
        AddDecl(RootNode, Parser.Items[I], DeclType);
    end;
  end;

var
  I: Integer;
begin
  Result := False;

  // first delete nodes that dont exist or are outdated
  for I := ParentNode.Count - 1 downto 0 do
    if NeedRemove(ParentNode[I]) then
    begin
      {$IFDEF DEBUG}
      WriteLn('Removing include: ', TSimbaFunctionListNode(ParentNode[I]).FileName);
      {$ENDIF}
      ParentNode[I].Free();

      Result := True;
    end;

  // now ones that dont exist
  for I := 0 to Parsers.Count - 1 do
    if NeedAdding(Parsers[I]) then
    begin
      {$IFDEF DEBUG}
      WriteLn('Adding include: ', Parsers[I].Lexer.FileName);
      {$ENDIF}
      Add(Parsers[I]);

      Result := True;
    end;
end;

procedure TSimbaFunctionListPage.DoHiddenSimbaSectionsChange(Setting: TSimbaSetting);
var
  Hidden: String;
  I: Integer;
begin
  Hidden := Setting.Value;
  for I := 0 to FSimbaNode.Count - 1 do
    FSimbaNode[I].Visible := Pos('[' + FSimbaNode[I].Text + ']', Hidden) <= 0;
end;

procedure TSimbaFunctionListPage.DoCustomOrderChange(Setting: TSimbaSetting);
var
  Order: TStringArray;
  I: Integer;
begin
  Order := String(Setting.Value).Split(',');

  // will need to be re done when anything changes, just easier
  if (Length(Order) <> FSimbaNode.Count) then
    Exit;
  for I := 0 to High(Order) do
    if (FSimbaNode.FindNode(Order[I]) = nil) then
      Exit;

  for I := 0 to High(Order) do
    FSimbaNode.FindNode(Order[I]).Index := I;
end;

procedure TSimbaFunctionListPage.DoEditorModified(Sender: TObject);
begin
  if (TSimbaScriptTab(Sender).UID = FTabID) then
    FNeedUpdate := True;
end;

procedure TSimbaFunctionListPage.DoSelectionChanged(Sender: TObject);
begin
  SimbaIDEEvents.Notify(SimbaIDEEvent.FUNCTIONLIST_SELECTION, FTreeView.Selected);
end;

procedure TSimbaFunctionListPage.DoNodeDoubleClick(Sender: TObject);
var
  Node: TSimbaFunctionListNode;
begin
  Node := TSimbaFunctionListNode(FTreeView.Selected);

  if (Node is TSimbaFunctionListNode) then
    case Node.NodeType of
      ntScriptFile: SimbaTabsForm.Open(Node.FileName);
      ntSimbaDecl:  ShowSimbaDeclaration(Node.Hint, Node.FileName);
      ntScriptDecl: ShowDeclaration(Node.StartPos, Node.EndPos, Node.Line, Node.FileName);
      ntPluginDecl: ShowPluginDeclaration(Node.Hint, Node.FileName);
    end;
end;

function TSimbaFunctionListPage.DoGetNodeHint(Node: TTreeNode): String;
begin
  if not SimbaSettings.FunctionList.ShowMouseoverHint.Value then
    Exit;

  if (Node is TSimbaFunctionListNode) then
  begin
    Result := TSimbaFunctionListNode(Node).Hint;
    if (Length(Result) > 100) then
      Result := Copy(Result, 1, 100) + ' ...';
  end;
end;

procedure TSimbaFunctionListPage.DoOpenSimbaDocClick(Sender: TObject);
var
  Node: TSimbaFunctionListNode;
  Section: String;
begin
  Section := '';
  Node := TSimbaFunctionListNode(FTreeView.Selected);
  if (Node is TSimbaFunctionListNode) then
    case TSimbaFunctionListNode(Node).NodeType of
      ntSimbaDecl: Section := IfThen(Node.Parent <> nil, Node.Parent.Text, '');
      ntSimbaFile: Section := Node.Text;
    end;

  if (Section <> '') then
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL + 'api/' + Section)
  else
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
end;

procedure TSimbaFunctionListPage.DoOpenContextMenu(Sender: TObject);
var
  I: Integer;
  NewItem: TMenuItem;
  Hidden: String;
begin
  if (FSimbaNode = nil) then
    Exit;

  Hidden := SimbaSettings.FunctionList.HiddenSimbaSections.Value;

  FMenuHideShowSection.Clear();
  for I := 0 to FSimbaNode.Count - 1 do
  begin
    NewItem := TMenuItem.Create(FMenuHideShowSection);
    NewItem.Caption := FSimbaNode.Items[I].Text;
    NewItem.Checked := Pos('[' + FSimbaNode.Items[I].Text + ']', Hidden) <= 0;
    NewItem.AutoCheck := True;
    NewItem.ShowAlwaysCheckable := True;
    NewItem.OnClick := @DoUpdateHiddenSections;

    FMenuHideShowSection.Add(NewItem);
  end;
end;

procedure TSimbaFunctionListPage.DoUpdateHiddenSections(Sender: TObject);
var
  I: Integer;
  Hidden: String;
begin
  Hidden := '';
  for I := 0 to FMenuHideShowSection.Count - 1 do
    if (not FMenuHideShowSection[I].Checked) then
      Hidden := Hidden + '[' + FMenuHideShowSection[I].Caption + ']';
  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Hidden;
end;

procedure TSimbaFunctionListPage.DoMouseOverTooltipClick(Sender: TObject);
begin
  SimbaSettings.FunctionList.ShowMouseoverHint.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaFunctionListPage.DoShowAllClick(Sender: TObject);
begin
  SimbaSettings.FunctionList.HiddenSimbaSections.Value := '';
end;

procedure TSimbaFunctionListPage.DoHideAllClick(Sender: TObject);
var
  Hidden: String;
  I: Integer;
begin
  Hidden := '';
  for I := 0 to FSimbaNode.Count - 1 do
    Hidden := Hidden + '[' + FSimbaNode.Items[I].Text + ']';

  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Hidden;
end;

procedure TSimbaFunctionListPage.DoDragDrop(Sender, Source: TObject; X, Y: Integer);

  procedure SaveCustomOrder;
  var
    I: Integer;
    Order: String;
  begin
    Order := '';
    for I := 0 to FSimbaNode.Count - 1 do
      Order := Order + FSimbaNode[I].Text + ',';

    SimbaSettings.FunctionList.CustomOrder.Value := Order;
  end;

var
  Node: TSimbaFunctionListNode;
  I: Integer;
begin
  Node := TSimbaFunctionListNode(FTreeView.Selected);
  if (Node is TSimbaFunctionListNode) and (TSimbaFunctionListNode(Node).NodeType = ntSimbaFile) then
  begin
    for I := FSimbaNode.Count - 1 downto 0 do
      if (Y > FSimbaNode[I].DisplayRect(True).Top) then
      begin
        FTreeView.Selected.MoveTo(FSimbaNode[I], naInsert);
        SaveCustomOrder();
        Break;
      end;
  end;
end;

procedure TSimbaFunctionListPage.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: TSimbaFunctionListNode;
begin
  Node := TSimbaFunctionListNode(FTreeView.Selected);
  Accept := (Node is TSimbaFunctionListNode) and (TSimbaFunctionListNode(Node).NodeType = ntSimbaFile);
end;

procedure TSimbaFunctionListPage.DoAfterFilter(Sender: TObject);
begin
  if (FTreeView.Filter = '') then
  begin
    FTreeView.BeginUpdate();
    FTreeView.FullCollapse();

    FScriptNode.Expanded := True;
    FIncludesNode.Expanded := True;
    FPluginsNode.Expanded := True;
    FSimbaNode.Expanded := True;

    FTreeView.EndUpdate();
  end;
end;

constructor TSimbaFunctionListPage.Create(AOwner: TComponent);
var
  ContextMenu: TPopupMenu;

  procedure AddLine;
  begin
    ContextMenu.Items.Add(NewLine());
  end;

  function Add(ACaption: String; AOnClick: TNotifyEvent; Checkable: Boolean = False; Checked: Boolean = False; ImageIndex: Integer = -1): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;
    Result.ImageIndex := ImageIndex;
    if Checkable then
    begin
      Result.AutoCheck := Checkable;
      Result.ShowAlwaysCheckable := Checkable;
      Result.Checked := Checked;
    end;

    ContextMenu.Items.Add(Result);
  end;

begin
  inherited Create(AOwner);

  FCodeInsight := TCodeinsight.Create();

  ContextMenu := TPopupMenu.Create(Self);
  ContextMenu.Images := SimbaFunctionListForm.Images;
  ContextMenu.OnPopup := @DoOpenContextMenu;

  FTreeView := TSimbaTreeView.Create(Self, TSimbaFunctionListNode);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaFunctionListForm.Images;
  FTreeView.OnDoubleClick := @DoNodeDoubleClick;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.OnSelectionChange := @DoSelectionChanged;
  FTreeView.OnDragDrop := @DoDragDrop;
  FTreeView.OnDragOver := @DoDragOver;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.PopupMenu := ContextMenu;

  Add('Open Simba Documentation', @DoOpenSimbaDocClick, False, False, IMG_SIMBA);
  Add('Show Mouse-over tooltip', @DoMouseOverTooltipClick, True, SimbaSettings.FunctionList.ShowMouseoverHint.Value);
  AddLine();
  Add('Show all', @DoShowAllClick);
  Add('Hide all', @DoHideAllClick);

  FMenuHideShowSection := Add('Hide/Show Section', nil);
  FScriptNode   := FTreeView.AddNode('Script', IMG_FOLDER);
  FIncludesNode := FTreeView.AddNode('Includes', IMG_FOLDER);
  FPluginsNode  := FTreeView.AddNode('Plugins', IMG_FOLDER);

  FScriptNodeState := TTreeNodeExpandedState.Create(TTreeNode(nil));
  FIncludesNodeState := TTreeNodeExpandedState.Create(TTreeNode(nil));
  FPluginsNodeState := TTreeNodeExpandedState.Create(TTreeNode(nil));

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_MODIFIED, @DoEditorModified);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE, @DoEditorModified); // force a update on change

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.FunctionList.HiddenSimbaSections, @DoHiddenSimbaSectionsChange);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.FunctionList.CustomOrder, @DoCustomOrderChange);
end;

destructor TSimbaFunctionListPage.Destroy;
begin
  if (FScriptNodeState <> nil) then
    FreeAndNil(FScriptNodeState);
  if (FIncludesNodeState <> nil) then
    FreeAndNil(FIncludesNodeState);
  if (FPluginsNodeState <> nil) then
    FreeAndNil(FPluginsNodeState);
  if (FCodeInsight <> nil) then
    FreeAndNil(FCodeInsight);

  inherited Destroy();
end;

procedure TSimbaFunctionListPage.Fill;
var
  Script, ScriptFileName: String;
  ExpandScriptNode: Boolean;

  procedure BeginUpdate;
  var
    Tab: TSimbaScriptTab;
  begin
    Tab := SimbaTabsForm.FindTab(FTabID);
    if (Tab <> nil) then
    begin
      Script := Tab.Script;
      ScriptFileName := Tab.ScriptFileName;
      ExpandScriptNode := (FScriptNode.Count = 0) or FScriptNode.Expanded;

      FScriptNodeState.CreateChildNodes(FScriptNode);
      FIncludesNodeState.CreateChildNodes(FIncludesNode);
      FPluginsNodeState.CreateChildNodes(FPluginsNode);

      FTreeView.BeginUpdate();
    end;
  end;

  procedure EndUpdate;
  var
    I: Integer;
  begin
    FScriptNode.DeleteChildren();
    for I := 0 to FCodeinsight.ScriptParser.Items.Count - 1 do
      AddDecl(FScriptNode, FCodeinsight.ScriptParser.Items[I], ntScriptDecl);
    FScriptNode.Expanded := ExpandScriptNode;

    AddIncludes(FCodeInsight.IncludeParsers, FIncludesNode, ntScriptDecl, ntScriptFile);
    AddIncludes(FCodeInsight.PluginParsers, FPluginsNode, ntPluginDecl, ntPluginFile);
    if (FSimbaNode = nil) then
      AddSimbaNode();

    FScriptNodeState.Apply(FScriptNode);
    FIncludesNodeState.Apply(FIncludesNode);
    FPluginsNodeState.Apply(FPluginsNode);

    FTreeView.EndUpdate();

    FNeedUpdate := False;
  end;

begin
  if not FNeedUpdate then
    Exit;

  RunInMainThread(@BeginUpdate);

  if FTreeView.Items.IsUpdating then
  try
    {$IFDEF DEBUG}
    WriteLn('Need Update');
    {$ENDIF}
    FCodeInsight.SetScript(Script, ScriptFileName);
    FCodeInsight.Run();
  finally
    RunInMainThread(@EndUpdate);
  end;
end;

function TSimbaFunctionListForm.PageForTab(Tab: TSimbaScriptTab): TSimbaPage;
var
  I: Integer;
begin
  for I := 0 to FNotebook.PageCount - 1 do
    if (TSimbaFunctionListPage(FNotebook.Page[I]).FTabID = Tab.UID) then
      Exit(FNotebook.Page[I]);

  Result := nil;
end;

procedure TSimbaFunctionListForm.ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := ImageWidthForDPI(APPI);
end;

procedure TSimbaFunctionListForm.DoUpdateThread;
begin
  try
    while not TThread.CurrentThread.CheckTerminated do
    begin
      if FIsIdle and FUpdateLock.TryEnter() then
      try
        if (FNotebook.ActivePage <> nil) then
          TSimbaFunctionListPage(FNotebook.ActivePage).Fill();
      finally
        FUpdateLock.Leave();
      end;

      Sleep(350);
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaFunctionListForm.DoUpdateThread]: ' + E.Message);
  end;
end;

procedure TSimbaFunctionListForm.DoCodetoolsSetup(Sender: TObject);
begin
  FUpdateThread := RunInThread(@DoUpdateThread);
end;

procedure TSimbaFunctionListForm.DoTabChange(Sender: TObject);
begin
  FNotebook.ActivePage := PageForTab(TSimbaScriptTab(Sender));
end;

procedure TSimbaFunctionListForm.DoTabClosed(Sender: TObject);
var
  Page: TSimbaPage;
begin
  Page := PageForTab(TSimbaScriptTab(Sender));
  if (Page <> nil) then
    Page.Free();

  {$IFDEF DEBUG}
  WriteLn('DoTabClosed: PageCount=',FNotebook.PageCount);
  {$ENDIF}
end;

procedure TSimbaFunctionListForm.DoTabAdd(Sender: TObject);
begin
  with TSimbaFunctionListPage(FNotebook.AddPage()) do
  begin
    FTabID := TSimbaScriptTab(Sender).UID;
    FNeedUpdate := True;
  end;

  {$IFDEF DEBUG}
  WriteLn('DoTabAdd: PageCount=', FNotebook.PageCount);
  {$ENDIF}
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

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.CODETOOLS_SETUP,  @DoCodetoolsSetup);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE,       @DoTabChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CLOSED,       @DoTabClosed);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_ADD,          @DoTabAdd);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.SPLITTER_DOUBLE_CLICK,  @DoDoubleClickSplitter);

  with TIdleTimer.Create(Self) do
  begin
    AutoEnabled := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent := itaOnUserInput;
    Interval := 350;
    OnTimer := @DoIdleBegin;
    OnStopTimer := @DoIdleEnd;
  end;

  FNotebook := TSimbaNotebook.Create(Self, TSimbaFunctionListPage);
  FNotebook.Parent := Self;
  FNotebook.Align := alClient;

  FUpdateLock := TCriticalSection.Create();
end;

destructor TSimbaFunctionListForm.Destroy;
begin
  FUpdateLock.Enter();
  FUpdateThread.Terminate();
  FUpdateThread.WaitFor();
  FUpdateLock.Leave();

  FreeAndNil(FUpdateLock);
  FreeAndNil(FUpdateThread);

  inherited Destroy();
end;

procedure TSimbaFunctionListForm.DoDoubleClickSplitter(Sender: TObject);
var
  Splitter: TAnchorDockSplitter;
begin
  if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akRight, Splitter) and (Splitter = Sender)) then
    Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() - Width) + TSimbaFunctionListPage(FNotebook.ActivePage).FTreeView.MaxRight)
  else if (GetDockSplitter(DockMaster.GetAnchorSite(Self), akLeft, Splitter) and (Splitter = Sender)) then
    Splitter.SetSplitterPosition((Splitter.GetSplitterPosition() + Width) - TSimbaFunctionListPage(FNotebook.ActivePage).FTreeView.MaxRight);
end;

{$R *.lfm}

end.

