{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlist_page;

{$i simba.inc}
{.$define DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Menus, ImgList,
  simba.base,
  simba.ide_codetools_parser,
  simba.ide_codetools_insight,
  simba.ide_tab,
  simba.ide_events,
  simba.component_treeview,
  simba.component_notebook,
  simba.settings;

type
  TSimbaFunctionListPage = class(TSimbaPage)
  protected
    FTabID: Int64;
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

    procedure AddSimbaNode;
    procedure AddDecl(ParentNode: TTreeNode; Decl: TDeclaration);
    procedure AddIncludes(Parsers: TCodeParserList; ParentNode: TTreeNode);

    procedure DoHiddenSimbaSectionsChange(Setting: TSimbaSetting);
    procedure DoCustomOrderChange(Setting: TSimbaSetting);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoNodeDoubleClick(Sender: TObject);
    function DoGetNodeHint(Node: TTreeNode): String;

    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoAfterFilter(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property NeedUpdate: Boolean read FNeedUpdate write FNeedUpdate;
    property TabID: Int64 read FTabID write FTabID;
    property TreeView: TSimbaTreeView read FTreeView;
    property SimbaNode: TTreeNode read FSimbaNode;
    property IncludesNode: TTreeNode read FIncludesNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property ScriptNode: TTreeNode read FScriptNode;

    procedure Fill;
  end;

  TDeclNode = class(TTreeNode)
  public
    Decl: TDeclaration;
  end;

  TFileNode = class(TTreeNode)
  public
    FileName: String;
  end;

  TSimbaSectionNode = class(TTreeNode);

  TParserNode = class(TTreeNode)
  protected
    FParser: TCodeParser;

    procedure SetParser(Value: TCodeParser);
  public
    destructor Destroy; override;

    property Parser: TCodeParser read FParser write SetParser;
  end;

implementation

uses
  AnchorDocking,
  simba.functionlistpage_contextmenu,
  simba.ide_controller,
  simba.vartype_string,
  simba.fs,
  simba.threading,
  simba.component_images;

procedure TSimbaFunctionListPage.AddDecl(ParentNode: TTreeNode; Decl: TDeclaration);
var
  Node: TDeclNode;
  I: Integer;
begin
  if (Decl.Name = '') then
    Exit;
  if ParentNode.HasAsParent(FSimbaNode) and ((Decl is TDeclaration_Method) and ((Decl.Name[1] = '_') or TDeclaration_Method(Decl).isOverride or TDeclaration_Method(Decl).isOperator)) then
    Exit;

  Node := TDeclNode(FTreeView.AddNodeWithClass(TDeclNode, ParentNode, Decl.Name, -1));
  Node.Decl := Decl;
  Node.Text := Decl.FullName;
  Node.ImageIndex := DeclarationImage(Decl);
  Node.SelectedIndex := Node.ImageIndex;

  if (Decl is TDeclaration_TypeRecord) or (Decl is TDeclaration_TypeEnum) then
    for I := 0 to Decl.Items.Count - 1 do
      AddDecl(Node, Decl.Items[I]);
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
  FSimbaNode := FTreeView.AddNode('Simba', SimbaImages.SECTION);

  for I := 0 to TCodeinsight.BaseParsers.Count - 1 do
  begin
    Parser := TCodeinsight.BaseParsers[I];
    if (Parser = nil) or (Parser.Items.Count = 0) or (Parser.Lexer.FileName.StartsWith('!')) then
      Continue;

    ParentNode := FTreeView.AddNodeWithClass(TSimbaSectionNode, FSimbaNode, Parser.Lexer.FileName, SimbaImages.DOCUMENT);
    for Decl in RemoveDuplicateProperties(Parser.Items.ToArray) do
      AddDecl(ParentNode, Decl);
  end;

  //FSimbaNode.AlphaSort();
  FSimbaNode.Expanded := True;

  DoHiddenSimbaSectionsChange(SimbaSettings.FunctionList.HiddenSimbaSections);
  DoCustomOrderChange(SimbaSettings.FunctionList.CustomOrder);
end;

procedure TSimbaFunctionListPage.AddIncludes(Parsers: TCodeParserList; ParentNode: TTreeNode);

  function ShortenFileName(FileName: String): String;
  begin
    if TSimbaPath.PathIsInDir(FileName, Application.Location) then
      Result := TSimbaPath.PathExtractRelative(Application.Location, FileName)
    else
      Result := FileName;
  end;

  // If node doesn't exist in parsers or is outdated
  function NeedRemove(Node: TTreeNode): Boolean;
  var
    I: Integer;
    NodeHash: String;
  begin
    if (Node is TParserNode) then
    begin
      NodeHash := TParserNode(Node).Parser.Hash;
      for I := 0 to Parsers.Count - 1 do
        if (Parsers[I].Hash = NodeHash) then
          Exit(False);
    end;

    Result := True;
  end;

  // if parser doesnt exist in nodes and is not outdated
  function NeedAdding(Parser: TCodeParser): Boolean;
  var
    I: Integer;
    NodeHash: String;
  begin
    for I := 0 to ParentNode.Count - 1 do
      if (ParentNode[I] is TParserNode) then
      begin
        NodeHash := TParserNode(ParentNode[I]).Parser.Hash;
        if (NodeHash = Parser.Hash) then
          Exit(False);
      end;

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
      RootNode := FTreeView.AddNodeWithClass(
        TParserNode,
        ParentNode,
        TSimbaPath.PathExtractNameWithoutExt(Parser.Lexer.FileName),
        SimbaImages.SECTION
      );
      TParserNode(RootNode).Parser := Parser;

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
          CurrentNode := FTreeView.AddNodeWithClass(
            TFileNode,
            RootNode,
            TSimbaPath.PathExtractNameWithoutExt(CurrentFile),
            SimbaImages.DOCUMENT
          );
          TFileNode(CurrentNode).FileName := CurrentFile;
        end;

        AddDecl(CurrentNode, Decl);
      end;
    end else
    begin
      RootNode := FTreeView.AddNodeWithClass(
        TParserNode,
        ParentNode,
        TSimbaPath.PathExtractNameWithoutExt(Parser.Lexer.FileName),
        SimbaImages.DOCUMENT
      );
      TParserNode(RootNode).Parser := Parser;
      for I := 0 to Parser.Items.Count - 1 do
        AddDecl(RootNode, Parser.Items[I]);
    end;
  end;

var
  I: Integer;
begin
  // first delete nodes that dont exist or are outdated
  for I := ParentNode.Count - 1 downto 0 do
    if NeedRemove(ParentNode[I]) then
    begin
      {$IFDEF DEBUG}
      DebugLn('Removing include: ' + TParserNode(ParentNode[I]).Parser.Lexer.FileName);
      {$ENDIF}
      ParentNode[I].Free();
    end;

  // now ones that dont exist
  for I := 0 to Parsers.Count - 1 do
    if NeedAdding(Parsers[I]) then
    begin
      {$IFDEF DEBUG}
      DebugLn('Adding include: ' + Parsers[I].Lexer.FileName);
      {$ENDIF}
      Add(Parsers[I]);
    end;
end;

procedure TSimbaFunctionListPage.DoHiddenSimbaSectionsChange(Setting: TSimbaSetting);
var
  Hidden: String;
  I: Integer;
begin
  if (FSimbaNode = nil) then
    Exit;

  Hidden := Setting.Value;
  for I := 0 to FSimbaNode.Count - 1 do
    FSimbaNode[I].Visible := Pos('[' + FSimbaNode[I].Text + ']', Hidden) <= 0;
end;

procedure TSimbaFunctionListPage.DoCustomOrderChange(Setting: TSimbaSetting);
var
  Order: TStringArray;
  I: Integer;
begin
  if (FSimbaNode = nil) then
    Exit;

  Order := String(Setting.Value).Split(',');
  for I := 0 to High(Order) do
    if (FSimbaNode.FindNode(Order[I]) <> nil) and (I < FSimbaNode.Count) then
      FSimbaNode.FindNode(Order[I]).Index := I;
end;

procedure TSimbaFunctionListPage.DoSelectionChanged(Sender: TObject);
begin
  if FTreeView.Items.IsUpdating or (not (FTreeView.Selected is TDeclNode)) then
    Exit;
  SimbaEvents.Post(ESimbaEvent.FUNCTIONLIST_SELECTION_CHANGE, TDeclNode(FTreeView.Selected).Decl);
end;

procedure TSimbaFunctionListPage.DoNodeDoubleClick(Sender: TObject);
begin
  if FTreeView.Items.IsUpdating then
    Exit;

  if (FTreeView.Selected is TDeclNode) then
    SimbaController.ShowDecl(TDeclNode(FTreeView.Selected).Decl);
end;

function TSimbaFunctionListPage.DoGetNodeHint(Node: TTreeNode): String;
begin
  if FTreeView.Items.IsUpdating or (not SimbaSettings.FunctionList.ShowMouseoverHint.Value) then
    Exit;

  if (Node is TDeclNode) then
  begin
    if (TDeclNode(Node).Decl is TDeclaration_Property) then
      Result := PropertyHeader(TDeclaration_Property(TDeclNode(Node).Decl))
    else
      Result := TDeclNode(Node).Decl.Header;

    if (Length(Result) > 100) then
      Result := Copy(Result, 1, 100) + ' ...';
  end
  else if (Node is TFileNode) or (Node is TParserNode) then
  begin
    if (Node is TFileNode) then
      Result := TFileNode(Node).FileName
    else if (Node is TParserNode) then
      Result := TParserNode(Node).Parser.Lexer.FileName
    else
      Result := '';

    if TSimbaPath.PathIsInDir(Result, Application.Location) then
      Result := TSimbaPath.PathExtractRelative(Application.Location, Result);
  end;
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
  Node: TSimbaSectionNode;
  I: Integer;
begin
  if FTreeView.Items.IsUpdating then
    Exit;

  if (FSimbaNode = nil) then
    Exit;

  Node := TSimbaSectionNode(FTreeView.Selected);
  if (Node is TSimbaSectionNode) then
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
begin
  if FTreeView.Items.IsUpdating then
    Exit;

  Accept := (FTreeView.Selected is TSimbaSectionNode);
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
    if (FSimbaNode <> nil) then
      FSimbaNode.Expanded := True;

    FTreeView.EndUpdate();
  end;
end;

constructor TSimbaFunctionListPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeInsight := TCodeinsight.Create();

  FTreeView := TSimbaTreeView.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaImages;
  FTreeView.OnDoubleClick := @DoNodeDoubleClick;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.OnSelectionChange := @DoSelectionChanged;
  FTreeView.OnDragDrop := @DoDragDrop;
  FTreeView.OnDragOver := @DoDragOver;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.PopupMenu := TFunctionListPage_ContextMenu.Create(Self);

  FScriptNode   := FTreeView.AddNode('Script', SimbaImages.SECTION);
  FIncludesNode := FTreeView.AddNode('Includes', SimbaImages.SECTION);
  FPluginsNode  := FTreeView.AddNode('Plugins', SimbaImages.SECTION);

  FScriptNodeState   := TTreeNodeExpandedState.Create(TTreeNode(nil));
  FIncludesNodeState := TTreeNodeExpandedState.Create(TTreeNode(nil));
  FPluginsNodeState  := TTreeNodeExpandedState.Create(TTreeNode(nil));

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
    Tab := SimbaController.FindTab(FTabID);

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
      AddDecl(FScriptNode, FCodeinsight.ScriptParser.Items[I]);
    FScriptNode.Expanded := ExpandScriptNode;

    AddIncludes(FCodeInsight.IncludeParsers, FIncludesNode);
    AddIncludes(FCodeInsight.PluginParsers, FPluginsNode);
    if (FSimbaNode = nil) then
      AddSimbaNode();

    FScriptNodeState.Apply(FScriptNode);
    FIncludesNodeState.Apply(FIncludesNode);
    FPluginsNodeState.Apply(FPluginsNode);

    FTreeView.EndUpdate();

    FNeedUpdate := False;
  end;

begin
  if (not FNeedUpdate) then
    Exit;

  RunInMainThread(@BeginUpdate);

  if FTreeView.Items.IsUpdating then
  try
    {$IFDEF DEBUG}
    DebugLn('Need Update');
    {$ENDIF}
    FCodeInsight.SetScript(Script, ScriptFileName);
    FCodeInsight.Run();
  finally
    RunInMainThread(@EndUpdate);
  end;
end;

procedure TParserNode.SetParser(Value: TCodeParser);
begin
  if (FParser = Value) then
    Exit;

  FParser := Value;
  if (FParser <> nil) then
    FParser.IncRef();
end;

destructor TParserNode.Destroy;
begin
  if (FParser <> nil) then
  begin
    FParser.DecRef();
    FParser := nil;
  end;

  inherited Destroy();
end;

end.

