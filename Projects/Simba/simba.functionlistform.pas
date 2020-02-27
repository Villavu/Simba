unit simba.functionlistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  TreeFilterEdit, fptimer, simba.codeinsight, simba.codeparser, simba.hintwindow;

type
  TSimbaFunctionListForm = class;

  TSimbaFunctionList_Hint = class(TSimbaHintWindow)
  protected
    procedure Paint; override; // Default drawing will clip the text
  end;

  TSimbaFunctionList_State = class(TTreeNodeExpandedState)
  public
    Filter: String;
    ScrolledLeft: Int32;
    ScrolledTop: Int32;
  end;

  TSimbaFunctionList_Updater = class(TFPTimer)
  protected
    FFunctionList: TSimbaFunctionListForm;
    FScript: String;
    FFileName: String;
    FStamp: Int64;
    FUpdating: Boolean;
    FCaretPos: Int32;

    procedure CheckUpdate;
    procedure Update(Sender: TObject);
  public
    constructor Create(FunctionList: TSimbaFunctionListForm); reintroduce;
  end;

  TSimbaFunctionList_Filter = class(TTreeFilterEdit)
  public
    procedure Reset;
    procedure Force(AFilter: String);
  end;

  TSimbaFunctionListForm = class(TForm)
    TreeView: TTreeView;

    procedure HandleHint(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseLeave(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  protected
    FParser: TCodeInsight;
    FReplacementParser: TCodeInsight;
    FScriptNode: TTreeNode;
    FPluginsNode: TTreeNode;
    FIncludesNode: TTreeNode;
    FSimbaNode: TTreeNode;
    FUpdater: TSimbaFunctionList_Updater;
    FHint: TSimbaFunctionList_Hint;
    TreeEditFilter: TSimbaFunctionList_Filter;

    procedure AfterFilter(Sender: TObject);

    procedure BeginUpdate;
    procedure EndUpdate;

    function GetState: TSimbaFunctionList_State;
    procedure SetState(Value: TSimbaFunctionList_State);
  public
    function addPluginSection(Section: String): TTreeNode;
    function addIncludeSection(Section: String): TTreeNode;
    function addNode(Declaration: TDeclaration; ParentNode: TTreeNode): TTreeNode;
    function addSimbaSection(Section: String): TTreeNode;

    procedure addMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
    procedure addType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
    procedure addVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
    procedure addConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
    procedure addDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);

    procedure Reset;
    procedure Fill(Script: String; FileName: String; CaretPos: Int32);

    property State: TSimbaFunctionList_State read GetState write SetState;

    property SimbaNode: TTreeNode read FSimbaNode;
    property IncludesNode: TTreeNode read FIncludesNode;
    property PluginsNode: TTreeNode read FPluginsNode;
    property ScriptNode: TTreeNode read FScriptNode;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

uses
  CastaliaPasLex,
  simba.main, simba.scripttabsform, simba.editor, simba.ci_includecache,
  lazfileutils, LCLIntf;

type
  TTreeViewHelper = class helper for TTreeView
  protected
    function GetScrolledLeft: Int32;
    function GetScrolledTop: Int32;

    procedure SetScrolledLeft(Value: Int32);
    procedure SetScrolledTop(Value: Int32);
  public
    property ScrolledLeft: Int32 read GetScrolledLeft write SetScrolledLeft;
    property ScrolledTop: Int32 read GetScrolledTop write SetScrolledTop;
  end;


procedure TSimbaFunctionList_Filter.Reset;
begin
  Force('');
end;

procedure TSimbaFunctionList_Filter.Force(AFilter: String);
begin
  Text := AFilter;
  ApplyFilter(True);
  IdleConnected := False;
end;

procedure TSimbaFunctionList_Updater.CheckUpdate;
var
  Editor: TSimbaEditor;
begin
  Editor := SimbaScriptTabsForm.CurrentEditor;

  if (Editor <> nil) then
  begin
    FUpdating := Editor.ChangeStamp <> FStamp;

    if FUpdating then
    begin
      FStamp := Editor.ChangeStamp;
      FScript := Editor.Text;
      FFileName := Editor.FileName;
      FCaretPos := Editor.SelStart - 1;
    end;
  end;
end;

procedure TSimbaFunctionList_Updater.Update(Sender: TObject);
begin
  TThread.Synchronize(TThread.CurrentThread, @CheckUpdate);
  if FUpdating then
    FFunctionList.Fill(FScript, FFileName, FCaretPos);
end;

constructor TSimbaFunctionList_Updater.Create(FunctionList: TSimbaFunctionListForm);
begin
  inherited Create(FunctionList);

  FFunctionList := FunctionList;

  OnTimer := @Update;
  Interval := 800;
  Enabled := True;
end;

function TTreeViewHelper.GetScrolledLeft: Int32;
begin
  Result := inherited ScrolledLeft;
end;

function TTreeViewHelper.GetScrolledTop: Int32;
begin
  Result := inherited ScrolledTop;
end;

procedure TTreeViewHelper.SetScrolledLeft(Value: Int32);
begin
  inherited ScrolledLeft := Value;
end;

procedure TTreeViewHelper.SetScrolledTop(Value: Int32);
begin
  inherited ScrolledTop := Value;
end;

procedure TSimbaFunctionList_Hint.Paint;
begin
  Canvas.Font.Color := clWindowText;
  Canvas.Pen.Color := clWindowFrame;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);
  Canvas.TextOut(3, 3, Caption);
end;

procedure TSimbaFunctionListForm.TreeViewDblClick(Sender: TObject);
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
  begin
    if TObject(TreeView.Selected.Data) is TmwPasLex then
      SimbaScriptTabsForm.Open(TmwPasLex(TreeView.Selected.Data).FileName)
    else
    if TObject(TreeView.Selected.Data) is TDeclaration then
      SimbaScriptTabsForm.OpenDeclaration(TDeclaration(TreeView.Selected.Data));
  end;
end;

procedure TSimbaFunctionListForm.AfterFilter(Sender: TObject);
begin
  if TreeEditFilter.Filter <> '' then
    TreeView.FullExpand()
  else
  begin
    TreeView.FullCollapse();

    FSimbaNode.Expanded := True;
    FScriptNode.Expanded := True;
  end;
end;

function TSimbaFunctionListForm.GetState: TSimbaFunctionList_State;
begin
  Result := TSimbaFunctionList_State.Create(TreeView);
  Result.Filter := TreeEditFilter.Text;
  Result.ScrolledLeft := TreeView.ScrolledLeft;
  Result.ScrolledTop := TreeView.ScrolledTop;
end;

procedure TSimbaFunctionListForm.SetState(Value: TSimbaFunctionList_State);
begin
  TreeView.BeginUpdate();

  try
    Reset();

    if (Value <> nil) then
    begin
      if (Value.Filter <> '') then
        TreeEditFilter.Force(Value.Filter);

      Value.Apply(TreeView);

      TreeView.ScrolledTop := Value.ScrolledTop;
      TreeView.ScrolledLeft := Value.ScrolledLeft;
    end;
  finally
    TreeView.EndUpdate();
  end;
end;

procedure TSimbaFunctionListForm.HandleHint(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  P: TPoint;
  R: TRect;
  Header: String;
  Declaration: TDeclaration;
begin
  Node := TreeView.GetNodeAt(X, Y);

  if (Node <> nil) and (Node.Data <> nil) and (TObject(Node.Data) is TDeclaration) then
  begin
    Declaration := TDeclaration(Node.Data);

    if Declaration is TciProcedureDeclaration then
      Header := TciProcedureDeclaration(Declaration).Header
    else
    begin
      if Declaration is TciTypeDeclaration then
        Header := 'type ' + Declaration.ShortText
      else
      if Declaration is TciConstantDeclaration then
        Header := 'const ' + Declaration.ShortText
      else
      if Declaration is TciVarDeclaration then
        Header := 'var ' + Declaration.ShortText;
    end;

    with Node.DisplayRect(True) do
      P := TreeView.ClientToScreen(TopLeft);

    R.Left := P.X;
    R.Top := P.Y - 3;
    R.Right := R.Left + Canvas.TextWidth(Header) + 8;
    R.Bottom := R.Top + Canvas.TextHeight(Header) + 8;

    with Screen.MonitorFromPoint(R.TopLeft).BoundsRect do
    begin
      if R.Right > Right then
        R.Right := Right;
    end;

    FHint.ActivateHint(R, Header);
  end else
    FHint.Hide();
end;

procedure TSimbaFunctionListForm.HandleMouseLeave(Sender: TObject);
begin
  FHint.Visible := False;
end;

function TSimbaFunctionListForm.addPluginSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.AddChild(FPluginsNode, ExtractFileNameOnly(Section));
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

function TSimbaFunctionListForm.addIncludeSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.AddChild(FIncludesNode, ExtractFileNameOnly(Section));
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

function TSimbaFunctionListForm.addNode(Declaration: TDeclaration; ParentNode: TTreeNode): TTreeNode;
begin
  Result := TreeView.Items.AddChild(ParentNode, '');
  Result.Data := Declaration;
  Result.Text := Declaration.Name;
end;

procedure TSimbaFunctionListForm.addMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = '') then
    Exit;

  with addNode(Declaration, ParentNode) do
  begin
    if (Declaration.IsMethodOfType) then
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

procedure TSimbaFunctionListForm.addType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = '') then
    Exit;

  with addNode(Declaration, ParentNode) do
  begin
    ImageIndex := IMAGE_TYPE;
    SelectedIndex := IMAGE_TYPE;
  end;
end;

procedure TSimbaFunctionListForm.addVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
begin
  with addNode(Declaration, ParentNode) do
  begin
    ImageIndex := IMAGE_VARIABLE;
    SelectedIndex := IMAGE_VARIABLE;
  end;
end;

procedure TSimbaFunctionListForm.addConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
begin
  with addNode(Declaration, ParentNode) do
  begin
    Text := Declaration.Name;

    ImageIndex := IMAGE_CONSTANT;
    SelectedIndex := IMAGE_CONSTANT;
  end;
end;

procedure TSimbaFunctionListForm.addDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort, Expand: Boolean);
var
  i: Int32;
begin
  if Clear then
    ParentNode.DeleteChildren();

  for i := 0 to Declarations.Count - 1 do
    if (Declarations[i] is TciProcedureDeclaration) then
      addMethod(Declarations[i] as TciProcedureDeclaration, ParentNode)
    else
    if (Declarations[i] is TciTypeDeclaration) then
      addType(Declarations[i] as TciTypeDeclaration, ParentNode)
    else
    if (Declarations[i] is TciConstantDeclaration) then
      addConst(Declarations[i] as TciConstantDeclaration, ParentNode)
    else
    if (Declarations[i] is TciVarDeclaration) then
      addVar(Declarations[i] as TciVarDeclaration, ParentNode);

  if Sort then
    ParentNode.AlphaSort();

  if Expand then
    ParentNode.Expanded := True;
end;

procedure TSimbaFunctionListForm.Reset;
begin
  TreeEditFilter.Reset();

  TreeView.FullCollapse();

  FSimbaNode.Expanded := True;
  FScriptNode.Expanded := True;
end;

procedure TSimbaFunctionListForm.Fill(Script: String; FileName: String; CaretPos: Int32);

  function IncludesChanged: Boolean;
  var
    I: Int32;
  begin
    Result := False;

    if FParser = nil then
      Exit(True);

    if Length(FParser.Includes) <> LEngth(FReplacementParser.Includes) then
      Exit(True);
    for I := 0 to High(FParser.Includes) do
      if not FParser.Includes[i].Equals(FReplacementParser.Includes[i]) then
        Exit(True);
  end;

var
  i: Int32;
  Declaration: TDeclaration;
  currentFile: String;
  currentNode: TTreeNode;
  Include: TCodeInsight_Include;
begin
  TThread.Synchronize(nil, @BeginUpdate);

  FReplacementParser := TCodeInsight.Create();
  FReplacementParser.OnFindInclude := @SimbaForm.CodeTools_OnFindInclude;
  FReplacementParser.OnFindLibrary := @SimbaForm.CodeTools_OnFindLibrary;
  FReplacementParser.OnLoadLibrary := @SimbaForm.CodeTools_OnLoadLibrary;
  FReplacementParser.OnMessage := @SimbaForm.CodeTools_OnMessage;
  FReplacementParser.Lexer.CaretPos := CaretPos;
  FReplacementParser.Run(Script, FileName);

  addDeclarations(FReplacementParser.Items, FScriptNode, True, False, True);

  if IncludesChanged then
  begin
    FPluginsNode.DeleteChildren();
    FIncludesNode.DeleteChildren();

    currentFile := '';
    currentNode := nil;

    for Include in FReplacementParser.Includes do
    begin
      for i := 0 to Include.Items.Count - 1 do
      begin
        Declaration := Include.Items[i];

        if currentFile <> Declaration.Lexer.FileName then
        begin
          currentFile := Declaration.Lexer.FileName;
          if Declaration.Lexer.IsLibrary then
            currentNode := addPluginSection(currentFile)
          else
          begin
            currentNode := addIncludeSection(currentFile);
            currentNode.Data := Declaration.Lexer;
          end;
        end;

        if currentNode = nil then
          Continue;

        if (Declaration is TciProcedureDeclaration) then
          addMethod(Declaration as TciProcedureDeclaration, currentNode)
        else
        if (Declaration is TciTypeDeclaration) then
          addType(Declaration as TciTypeDeclaration, currentNode)
        else
        if (Declaration is TciConstantDeclaration) then
          addConst(Declaration as TciConstantDeclaration, currentNode)
        else
        if (Declaration is TciVarDeclaration) then
          addVar(Declaration as TciVarDeclaration, currentNode);
      end;
    end;
  end;

  TThread.Synchronize(nil, @EndUpdate);
end;

procedure TSimbaFunctionListForm.BeginUpdate;
begin
  TreeView.BeginUpdate();
end;

procedure TSimbaFunctionListForm.EndUpdate;
begin
  if (FReplacementParser <> nil) then
  begin
    if (FParser <> nil) then
      FParser.Free();
    FParser := FReplacementParser;

    FReplacementParser := nil;
  end;

  TreeView.EndUpdate();
end;

constructor TSimbaFunctionListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScriptNode := TreeView.Items.Add(nil, 'Script');
  FScriptNode.ImageIndex := IMAGE_DIRECTORY;
  FScriptNode.SelectedIndex := IMAGE_DIRECTORY;

  FPluginsNode := TreeView.Items.Add(nil, 'Plugins');
  FPluginsNode.ImageIndex := IMAGE_DIRECTORY;
  FPluginsNode.SelectedIndex := IMAGE_DIRECTORY;

  FIncludesNode := TreeView.Items.Add(nil, 'Includes');
  FIncludesNode.ImageIndex := IMAGE_DIRECTORY;
  FIncludesNode.SelectedIndex := IMAGE_DIRECTORY;

  FSimbaNode := TreeView.Items.Add(nil, 'Simba');
  FSimbaNode.ImageIndex := IMAGE_DIRECTORY;
  FSimbaNode.SelectedIndex := IMAGE_DIRECTORY;
  FSimbaNode.Expanded := True;

  FHint := TSimbaFunctionList_Hint.Create(Self);
  FUpdater := TSimbaFunctionList_Updater.Create(Self);

  TreeEditFilter := TSimbaFunctionList_Filter.Create(Self);
  TreeEditFilter.Parent := Self;
  TreeEditFilter.Align := alBottom;
  TreeEditFilter.FilteredTreeview := TreeView;
  TreeEditFilter.OnAfterFilter := @AfterFilter;
  TreeEditFilter.ButtonWidth := 0;
  TreeEditFilter.Spacing := 0;
  TreeEditFilter.TextHint := '(search)';
end;

destructor TSimbaFunctionListForm.Destroy;
begin
  if (FParser <> nil) then
  begin
    FParser.Free();
    FParser := nil;
  end;

  inherited Destroy();
end;

function TSimbaFunctionListForm.addSimbaSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.AddChild(FSimbaNode, Section);
  Result.ImageIndex := IMAGE_FILE;
  Result.SelectedIndex := IMAGE_FILE;
end;

initialization
  {$I simba.functionlistform.lrs}

end.

