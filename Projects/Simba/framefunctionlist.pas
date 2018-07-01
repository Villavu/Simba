{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Frame Function List form for Simba
}
unit framefunctionlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, LResources, Forms,
  ComCtrls, StdCtrls, Controls, ExtCtrls, Buttons,
  v_ideCodeParser, v_ideCodeInsight;

type
  TFunctionList_Frame = class;

  TFunctionList_Node = class(TTreeNode)
  public
    Header: String;
    DocPos: record
      StartPos, EndPos: Int32;
      FilePath: String;
    end;
  end;

  TFunctionList_Updater = class(TThread)
  protected
    FParser: TCodeInsight;
    FLastUpdate: Int64;
    FLastIncludes: UInt32;
    FFunctionList: TFunctionList_Frame;
    FStream: TMemoryStream;

    function IncludesUpdated: Boolean;

    procedure StartUpdate;
    procedure FinishUpdate;

    procedure Execute; override;
  public
    property LastUpdate: Int64 read FLastUpdate write FLastUpdate;

    constructor Create(FunctionList: TFunctionList_Frame); reintroduce;
    destructor Destroy; override;
  end;

  TFunctionList_Hint = class(THintWindow)
    procedure Paint; override; // Default drawing will clip the text
  end;

  { TFunctionList_Frame }

  TFunctionList_Frame = class(TFrame)
    FunctionListLabel: TLabel;
    Filter: TTreeFilterEdit;
    TreeView: TTreeView;

    procedure AfterFilter(Sender: TObject);

    procedure FrameEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewDoubleClick(Sender: TObject);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
    procedure TreeViewMouseLeave(Sender: TObject);
    procedure HintMouseLeave(Sender: TObject);
  protected
    FUpdater: TFunctionList_Updater;
    FHint: THintWindow;
  public
    ScriptNode: TTreeNode;
    PluginsNode: TTreeNode;
    IncludesNode: TTreeNode;

    procedure Stop;

    function addPluginSection(Section: String): TTreeNode;
    function addIncludeSection(Section: String): TTreeNode;
    function addNode(Declaration: TDeclaration; ParentNode: TTreeNode): TFunctionList_Node;
    function addSimbaSection(Section: String): TTreeNode;

    procedure addMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
    procedure addType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
    procedure addVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
    procedure addConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
    procedure addDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort: Boolean);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end; 

implementation

uses
  SimbaUnit, LazFileUtils, SynEdit, CastaliaSimplePasPar, Types, Graphics;

procedure TFunctionList_Hint.Paint;
begin
  Canvas.Font.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);
  Canvas.TextOut(4, 4, Caption);
end;

function TFunctionList_Updater.IncludesUpdated: Boolean;
begin
  Result := FLastIncludes <> FParser.IncludesHash;
end;

type
  __TSynEdit = class(TSynEdit);

procedure TFunctionList_Updater.StartUpdate;
var
  SynEdit: TSynEdit;

  function GetUpdateTime: Int64;
  begin
    Result := __TSynEdit(SynEdit).GetTextBuffer.TextChangeStamp;
  end;

begin
  if Terminated then
    Exit;

  if (FParser <> nil) then
  begin
    FParser.Free();
    FParser := nil;
  end;

  if (FStream <> nil) then
  begin
    FStream.Free();
    FStream := nil;
  end;

  if (SimbaForm.CurrScript <> nil) and (SimbaForm.CurrScript.SynEdit <> nil) then
  begin
    SynEdit := SimbaForm.CurrScript.SynEdit;

    if (SynEdit.Text <> '') and (GetUpdateTime() <> FLastUpdate) then
    begin
      FLastUpdate := GetUpdateTime();

      FParser := TCodeInsight.Create();
      FParser.FileName := SimbaForm.CurrScript.ScriptFile;
      FParser.OnFindInclude := @SimbaForm.OnCCFindInclude;
      FParser.OnLoadLibrary := @SimbaForm.OnCCLoadLibrary;

      FStream := TMemoryStream.Create();
      FStream.Write(SynEdit.Text[1], Length(SynEdit.Text));
    end;
  end;
end;

procedure TFunctionList_Updater.FinishUpdate;

  procedure addIncludes(Includes: TCodeInsightArray);
  var
    i: Int32;
  begin
    for i := 0 to High(Includes) do
    begin
      if Includes[i].IsLibrary then
        FFunctionList.addDeclarations(Includes[i].Items, FFunctionList.addPluginSection(ExtractFileNameOnly(Includes[i].FileName)), False, False)
      else
        FFunctionList.addDeclarations(Includes[i].Items, FFunctionList.addIncludeSection(Includes[i].FileName), False, False);

      addIncludes(Includes[i].Includes);
    end;
  end;

begin
  if IncludesUpdated() then
  begin
    FFunctionList.IncludesNode.DeleteChildren();
    FFunctionList.PluginsNode.DeleteChildren();

    addIncludes(FParser.Includes);
  end;

  FFunctionList.addDeclarations(FParser.Items, FFunctionList.ScriptNode, True, False);
  FFunctionList.ScriptNode.Expanded := True;

  FLastIncludes := FParser.IncludesHash;
end;

procedure TFunctionList_Updater.Execute;

  procedure Generate(Parser: TCodeInsight; Includes: Boolean);
  var
    i, j: Int32;
    Items: TDeclarationList;
  begin
    Items := Parser.Items;

    for i := 0 to Parser.Items.Count - 1 do
      if (Items[i] is TciProcedureDeclaration) then
        with Items[i] as TciProcedureDeclaration do
        begin
          CleanDeclaration;
          if (Name <> nil) then
            Name.CleanText;
        end
      else
      if (Items[i] is TciTypeDeclaration) then
        with Items[i] as TciTypeDeclaration do
        begin
          CleanText;
          if (Name <> nil) then
            Name.CleanText;
        end
      else
      if (Items[i] is TciVarDeclaration) then
        with Items[i] as TciVarDeclaration do
        begin
          CleanText;
          for j := 0 to High(Names) do
            Names[j].CleanText;
        end
      else
      if (Items[i] is TciConstantDeclaration) then
        with Items[i] as TciConstantDeclaration do
        begin
          CleanText;
          for j := 0 to High(Names) do
            Names[j].CleanText;
        end;

    if Includes then
      for i := 0 to High(Parser.Includes) do
        Generate(Parser.Includes[i], True);
  end;

begin
  try
    while (not Terminated) do
    begin
      Synchronize(@StartUpdate);

      if (FParser <> nil) and (FStream <> nil) then
      begin
        try
          FParser.Run(FStream);

          (* Might as well do everything we possibly can when on another thread?
             So when on Simba's mainthread everything will be generated already.

             edit: On timing this it's minimal (~10ms) but it does no harm. :)
          *)
          Generate(FParser, IncludesUpdated());
        except
          on e: ESyntaxError do ;
          on e: Exception do raise;
        end;

        Synchronize(@FinishUpdate);
      end;

      Sleep(800);
    end;
  except
    on e: Exception do
      WriteLn('Function list updater exception ` ' + e.ClassName + '::' + e.Message + '`');
  end;
end;

constructor TFunctionList_Updater.Create(FunctionList: TFunctionList_Frame);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FFunctionList := FunctionList;
end;

destructor TFunctionList_Updater.Destroy;
begin
  if (FParser <> nil) then
  begin
    FParser.Free();
    FParser := nil;
  end;

  if (FStream <> nil) then
  begin
    FStream.Free();
    FStream := nil;
  end;

  inherited Destroy();
end;

procedure TFunctionList_Frame.AfterFilter(Sender: TObject);
begin
  with Sender as TTreeFilterEdit do
    if (Filter = '') then
      FilteredTreeView.FullCollapse()
    else
      FilteredTreeView.FullExpand();
end;

procedure TFunctionList_Frame.FrameEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  SimbaForm.SplitterFunctionList.Visible := Target is TPanel;

  if (Target is TCustomDockForm) then
  begin
    TCustomDockForm(Target).Caption := 'Function List';
    TCustomDockForm(Target).OnClose := @DockFormOnClose;
  end;
end;

procedure TFunctionList_Frame.LabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.DragKind := dkDock;
  Self.BeginDrag(False, 40);
end;

procedure TFunctionList_Frame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;

  SimbaForm.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionList_Frame.TreeViewClick(Sender: TObject);

  function SingleLine(const S: String): String;
  begin
    Result := S;

    while (Pos(LineEnding, Result) > 0) do
      Result := StringReplace(Result, LineEnding, #32, [rfReplaceAll]);
    while (Pos(#9, Result) > 0) do
      Result := StringReplace(Result, #9, #32, [rfReplaceAll]);
    while (Pos(#32#32, Result) > 0) do
      Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
  end;

begin
  if (TreeView.Selected <> nil) then
  begin
    if TreeView.Selected is TFunctionList_Node then
      SimbaForm.StatusBar.Panels[Panel_General].Text := SingleLine(TFunctionList_Node(TreeView.Selected).Header)
    else
      SimbaForm.StatusBar.Panels[Panel_General].Text := 'Section: ' + TreeView.Selected.Text;
  end;
end;

procedure TFunctionList_Frame.TreeViewDoubleClick(Sender: TObject);
var
  Node: TFunctionList_Node;
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected is TFunctionList_Node) then
  begin
    Node := TreeView.Selected as TFunctionList_Node;

    if (Node.DocPos.FilePath = '') or FileExists(Node.DocPos.FilePath) then
    begin
      SimbaForm.LoadScriptFile(Node.DocPos.FilePath, True, True);

      with SimbaForm.CurrScript.SynEdit do
      begin
        SelStart := Node.DocPos.StartPos;
        SelEnd := Node.DocPos.EndPos;
      end;
    end;
  end;
end;

procedure TFunctionList_Frame.TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);

  function SingleLine(const S: String): String;
  begin
    Result := S;

    while (Pos(LineEnding, Result) > 0) do
      Result := StringReplace(Result, LineEnding, #32, [rfReplaceAll]);
    while (Pos(#9, Result) > 0) do
      Result := StringReplace(Result, #9, #32, [rfReplaceAll]);
    while (Pos(#32#32, Result) > 0) do
      Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
  end;

  function LimitToMonitor(R: TRect): TRect;
  begin
    Result := R;

    R := Screen.MonitorFromPoint(R.TopLeft).BoundsRect;

    if (Result.Left < R.Left) then
      Result.Left := R.Left;
    if (Result.Top < R.Top) then
      Result.Top := R.Top;
    if (Result.Right > R.Right) then
      Result.Right := R.Right;
    if (Result.Bottom > R.Bottom) then
      Result.Bottom := R.Bottom;
  end;

var
  Node: TTreeNode;
  P: TPoint;
  R: TRect;
begin
  Node := TreeView.GetNodeAt(X, Y);

  if (Node <> nil) and (Node is TFunctionList_Node) then
  begin
    with Node as TFunctionList_Node do
    begin
      P := TreeView.ClientToScreen(Point(DisplayRect(True).Left, DisplayRect(True).Top - 3));

      R.Left := P.X;
      R.Top := P.Y;
      R.Right := R.Left + Canvas.TextWidth(SingleLine(Header)) + 8;
      R.Bottom := R.Top + Node.Height + 6;

      FHint.ActivateHint(LimitToMonitor(R), SingleLine(Header));
    end;
  end else
    FHint.Hide();
end;

function TFunctionList_Frame.addIncludeSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.AddNode(TFunctionList_Node.Create(TreeView.Items), IncludesNode, ExtractFileNameOnly(Section), nil, naAddChild);
  Result.ImageIndex := 38;
  Result.SelectedIndex := 38;

  TFunctionList_Node(Result).DocPos.FilePath := Section;
  TFunctionList_Node(Result).Header := Section;
end;

function TFunctionList_Frame.addNode(Declaration: TDeclaration; ParentNode: TTreeNode): TFunctionList_Node;
begin
  Result := TreeView.Items.AddNode(TFunctionList_Node.Create(TreeView.Items), ParentNode, '', nil, naAddChild) as TFunctionList_Node;
end;

procedure TFunctionList_Frame.addMethod(Declaration: TciProcedureDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = nil) then
    Exit;

  with addNode(Declaration, ParentNode) do
  begin
    with DocPos do
    begin
      FilePath := TCodeInsight(Declaration.Parser).FileName;
      StartPos := Declaration.Name.StartPos + 1;
      EndPos := Declaration.Name.EndPos + 1;
    end;

    Header := Declaration.CleanDeclaration;

    if (Declaration.ObjectName <> nil) then
      Text := Declaration.ObjectName.CleanText + '.' + Declaration.Name.CleanText
    else
      Text := Declaration.Name.CleanText;

    case Declaration.ProcType of
      'function':
        begin
          ImageIndex := 34;
          SelectedIndex := 34;
        end;

      'procedure':
        begin
          ImageIndex := 35;
          SelectedIndex := 35;
        end;
    end;
  end;
end;

procedure TFunctionList_Frame.addType(Declaration: TciTypeDeclaration; ParentNode: TTreeNode);
begin
  if (Declaration.Name = nil) then
    Exit;

  with addNode(Declaration, ParentNode) do
  begin
    with DocPos do
    begin
      FilePath := TCodeInsight(Declaration.Parser).FileName;
      StartPos := Declaration.Name.StartPos + 1;
      EndPos := Declaration.Name.EndPos + 1;
    end;

    Header := 'type ' + Declaration.CleanText;
    Text := Declaration.Name.CleanText;

    ImageIndex := 36;
    SelectedIndex := 36;
  end;
end;

procedure TFunctionList_Frame.addVar(Declaration: TciVarDeclaration; ParentNode: TTreeNode);
var
  i: Int32;
begin
  if (Length(Declaration.Names) = 0) then
    Exit;

  for i := 0 to High(Declaration.Names) do
    with addNode(Declaration, ParentNode) do
    begin
      with DocPos do
      begin
        FilePath := TCodeInsight(Declaration.Parser).FileName;
        StartPos := Declaration.Names[i].StartPos + 1;
        EndPos := Declaration.Names[i].EndPos + 1;
      end;

      Header := 'var ' + Declaration.Names[i].CleanText;
      if (Declaration.VarType <> '') then
        Header := Header + ': ' + Declaration.VarType;
      Text := Declaration.Names[i].CleanText;

      ImageIndex := 33;
      SelectedIndex := 33;
    end;
end;

procedure TFunctionList_Frame.addConst(Declaration: TciConstantDeclaration; ParentNode: TTreeNode);
var
  i: Int32;
begin
  if (Length(Declaration.Names) = 0) then
    Exit;

  for i := 0 to High(Declaration.Names) do
    with addNode(Declaration, ParentNode) do
    begin
      with DocPos do
      begin
        FilePath := TCodeInsight(Declaration.Parser).FileName;
        StartPos := Declaration.Names[i].StartPos + 1;
        EndPos := Declaration.Names[i].EndPos + 1;
      end;

      Header := 'const ' + Declaration.Names[i].CleanText;
      if (Declaration.Value <> '') then
        Header := Header + ' = ' + Declaration.Value;

      Text := Declaration.Names[i].CleanText;

      ImageIndex := 37;
      SelectedIndex := 37;
    end;
end;

procedure TFunctionList_Frame.addDeclarations(Declarations: TDeclarationList; ParentNode: TTreeNode; Clear, Sort: Boolean);
var
  i: Int32;
begin
  TreeView.BeginUpdate();

  try
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
  finally
    TreeView.EndUpdate();
  end;
end;

function TFunctionList_Frame.addSimbaSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.Add(nil, Section);
  Result.ImageIndex := 38;
  Result.SelectedIndex := 38;
end;

procedure TFunctionList_Frame.HintMouseLeave(Sender: TObject);
begin
  if FindLCLControl(Mouse.CursorPos) <> TreeView then
    FHint.Hide();
end;

procedure TFunctionList_Frame.Stop;
begin
  FUpdater.Terminate();
end;

procedure TFunctionList_Frame.TreeViewMouseLeave(Sender: TObject);
begin
  if (not PtInRect(TreeView.ClientRect, ScreenToClient(Mouse.CursorPos))) then
    FHint.Hide();
end;

function TFunctionList_Frame.addPluginSection(Section: String): TTreeNode;
begin
  Result := TreeView.Items.AddChild(PluginsNode, Section);
  Result.ImageIndex := 38;
  Result.SelectedIndex := 38;
end;

constructor TFunctionList_Frame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with TreeView.Items do
  begin
    ScriptNode := Add(nil, 'Script');
    ScriptNode.ImageIndex := 41;
    ScriptNode.SelectedIndex := 41;

    PluginsNode := Add(nil, 'Plugins');
    PluginsNode.ImageIndex := 40;
    PluginsNode.SelectedIndex := 40;

    IncludesNode := Add(nil, 'Includes');
    IncludesNode.ImageIndex := 40;
    IncludesNode.SelectedIndex := 40;
  end;

  FHint := TFunctionList_Hint.Create(Self);
  FHint.OnMouseLeave := @HintMouseLeave;

  FUpdater := TFunctionList_Updater.Create(Self);
end;

destructor TFunctionList_Frame.Destroy;
begin
  if (FUpdater <> nil) then
    Stop();

  inherited Destroy();
end;

initialization
  {$R *.lfm}

end.

