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
  protected
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

    procedure CheckUpdate;
    procedure Update(Sender: TObject);
  public
    constructor Create(FunctionList: TSimbaFunctionListForm); reintroduce;
  end;

  TSimbaFunctionListForm = class(TForm)
    TreeFilterEdit: TTreeFilterEdit;
    TreeView: TTreeView;

    procedure HandleHint(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseLeave(Sender: TObject);
    procedure TreeFilterEditAfterFilter(Sender: TObject);
    procedure TreeFilterEditChange(Sender: TObject);
    procedure TreeFilterEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure addIncludes(Includes: TCodeInsightArray);

    procedure Fill(Script: String; FileName: String);

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
  simba.main, simba.scripttabsform, simba.editor,
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
    end;
  end;
end;

procedure TSimbaFunctionList_Updater.Update(Sender: TObject);
begin
  TThread.Synchronize(TThread.CurrentThread, @CheckUpdate);
  if FUpdating then
    FFunctionList.Fill(FScript, FFileName);
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
    SimbaScriptTabsForm.OpenDeclaration(TDeclaration(TreeView.Selected.Data));
end;

function TSimbaFunctionListForm.GetState: TSimbaFunctionList_State;
begin
  Result := TSimbaFunctionList_State.Create(TreeView);
  Result.Filter := TreeFilterEdit.Text;
  Result.ScrolledLeft := TreeView.ScrolledLeft;
  Result.ScrolledTop := TreeView.ScrolledTop;
end;


var
  b: Int32 = 0;

procedure TSimbaFunctionListForm.SetState(Value: TSimbaFunctionList_State);
begin
  TreeView.BeginUpdate();

  try
    TreeFilterEdit.Text := '';

    FSimbaNode.Expanded := True;
    FScriptNode.Expanded := True;

    if (Value <> nil) then
    begin
      if Value.Filter = '' then
      begin
        Value.Apply(TreeView);
      end
      else
      begin
        TreeFilterEdit.Text := Value.Filter;
        TreeView.FullExpand();
      end;

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

  if (Node <> nil) and (Node.Data <> nil) then
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

procedure TSimbaFunctionListForm.TreeFilterEditAfterFilter(Sender: TObject);
begin
  {
  if b = 2 then
  begin
    Writeln('omg');
  end;
  Writeln('After filter');}
  //if (TreeFilterEdit.Filter = '') then
  //  TreeView.FullCollapse();
end;

procedure TSimbaFunctionListForm.TreeFilterEditChange(Sender: TObject);
begin
  if (TreeFilterEdit.Text = '') then
    TreeView.FullCollapse();
end;

procedure TSimbaFunctionListForm.TreeFilterEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
end;

function TSimbaFunctionListForm.addPluginSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.Add(nil, ExtractFileNameOnly(Section));
  Result.ImageIndex := IMAGE_SCRIPT;
  Result.SelectedIndex := IMAGE_SCRIPT;
end;

function TSimbaFunctionListForm.addIncludeSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.Add(nil, ExtractFileNameOnly(Section));
  Result.ImageIndex := IMAGE_SCRIPT;
  Result.SelectedIndex := IMAGE_SCRIPT;
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

procedure TSimbaFunctionListForm.addIncludes(Includes: TCodeInsightArray);
var
  i: Int32;
begin
  for i := 0 to High(Includes) do
  begin
    if Includes[i].IsLibrary then
      addDeclarations(Includes[i].Items, addPluginSection(ExtractFileNameOnly(Includes[i].FileName)), False, False, False)
    else
      addDeclarations(Includes[i].Items, addIncludeSection(Includes[i].FileName), False, False, False);

    addIncludes(Includes[i].Includes);
  end;
end;

procedure TSimbaFunctionListForm.Fill(Script: String; FileName: String);
begin
  TThread.Synchronize(nil, @BeginUpdate);

  FReplacementParser := TCodeInsight.Create();
  FReplacementParser.FileName := FileName;
  FReplacementParser.Run(Script);

  addDeclarations(FReplacementParser.Items, FScriptNode, True, False, True);

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
  Result.ImageIndex := IMAGE_SCRIPT;
  Result.SelectedIndex := IMAGE_SCRIPT;
end;

initialization
  {$I simba.functionlistform.lrs}

end.

