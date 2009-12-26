unit framefunctionlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, StdCtrls, Controls;

type

  { TFunctionListFrame }

  TFunctionListFrame = class(TFrame)
    editSearchList: TEdit;
    FunctionList: TTreeView;
    procedure editSearchListChange(Sender: TObject);
    procedure FunctionListDeletion(Sender: TObject; Node: TTreeNode);
    procedure FunctionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FunctionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FFilterTree : TTreeView;
    procedure FilterTreeVis(Vis : boolean);
    function GetFilterTree: TTreeView;
    { private declarations }
  public
    DraggingNode : TTreeNode;
    ScriptNode : TTreeNode;
    InCodeCompletion : boolean;
    CompletionCaret : TPoint;
    StartWordCompletion : TPoint;
    CompletionLine : string;
    CompletionStart : string;
    property FilterTree : TTreeView read GetFilterTree;
    procedure LoadScriptTree( Script : String);
    function Find(Next : boolean) : boolean;
    { public declarations }
  end; 

implementation

uses
  TestUnit, Graphics, simpleanalyzer;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
begin
  Find(false);
end;

procedure TFunctionListFrame.FunctionListDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if node.data <> nil then
    StrDispose(PChar(Node.Data));
end;

procedure TFunctionListFrame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  Form1.MenuItemFunctionList.Checked := False;
end;


procedure TFunctionListFrame.FilterTreeVis(Vis: boolean);
begin
  FunctionList.Visible:= not Vis;
  FilterTree.Visible := Vis;
end;

function TFunctionListFrame.GetFilterTree: TTreeView;
begin
  Result := FFilterTree;
  if Assigned(Result) then
    exit;
  FFilterTree := TTreeView.Create(Self);
  FFilterTree.Parent := Self;
  FFilterTree.Visible := false;
  FFilterTree.SetBounds(FunctionList.Left,FunctionList.Top,FunctionList.Width,FunctionList.Height);
  FFilterTree.Align := alClient;
  FFilterTree.ReadOnly:= True;
  FFilterTree.ScrollBars:= ssAutoBoth;
  FFilterTree.OnMouseDown:= FunctionList.OnMouseDown;
  FFilterTree.OnMouseUp:= FunctionList.OnMouseUp;
  FFilterTree.OnChange:= FunctionList.OnChange;
  FFilterTree.OnExit := FunctionList.OnExit;
  Result := FFilterTree;
  //We do not want to delete the data from the FilterTree
//  FilterTree.OnDeletion:= FunctionList.OnDeletion;
end;

procedure TFunctionListFrame.LoadScriptTree(Script: String);
var
  I : integer;
  Analyzer : TScriptAnalyzer;
  tmpNode : TTreeNode;
begin
  if FilterTree.Visible then
    Writeln('Might get some acces violations now..');
  ScriptNode.DeleteChildren;
  Analyzer := TScriptAnalyzer.create;
  Analyzer.ScriptToAnalyze:= Script;
  Analyzer.analyze;
  for i := 0 to Analyzer.MethodLen - 1 do
  begin
    tmpNode := FunctionList.Items.AddChild(ScriptNode,Analyzer.Methods[i].Name);
    tmpNode.Data:= strnew(PChar(Analyzer.Methods[i].CreateMethodStr));
  end;
  ScriptNode.Expand(true);
end;

function TFunctionListFrame.Find(Next : boolean) : boolean;
var
  Start,Len,i,index,posi: Integer;
  FoundFunction : boolean;
  LastSection : string;
  str : string;
  RootNode : TTreeNode;
  NormalNode : TTreeNode;
  Node : TTreeNode;
  InsertStr : string;
begin
  if(editSearchList.Text = '')then
  begin
    editSearchList.Color := clWhite;
    FunctionList.FullCollapse;
    if InCodeCompletion then
    begin;
      Form1.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
      Form1.CurrScript.SynEdit.LogicalCaretXY:= point(CompletionCaret.x,CompletionCaret.y);
      Form1.CurrScript.SynEdit.SelEnd:= Form1.CurrScript.SynEdit.SelStart;
    end;
    FilterTreeVis(False);
    ScriptNode.Expand(true);
    exit;
  end;

  //We only have to search the next item in our filter tree.. Fu-king easy!
  if next then
  begin;
    if FilterTree.Visible = false then
    begin;
      Writeln('ERROR: You cannot search next, since the Tree isnt generated yet');
      Find(false);
      exit;
    end;
    if FilterTree.Selected <> nil then
      Start := FilterTree.Selected.AbsoluteIndex + 1
    else
      Start := 0;
    Len := FilterTree.Items.Count;
    for i := start to start + len - 1 do
      if FilterTree.Items[i mod len].Level = 1 then
      begin
        FilterTree.Items[i mod len].Selected:= true;
        InsertStr := FilterTree.Items[i mod len].Text;
        Result := true;
        break;
      end;
  end else
  begin
    FilterTree.Items.Clear;
    FoundFunction := False;
    if FunctionList.Selected <> nil then
      Start := FunctionList.Selected.AbsoluteIndex
    else
      Start := 0;
    Len := FunctionList.Items.Count;
    LastSection := '';
    for i := start to start + FunctionList.Items.Count - 1 do
    begin;
      Node := FunctionList.Items[i mod FunctionList.Items.Count];
      if(Node.Level = 1)then
        if(pos(lowercase(editSearchList.Text), lowercase(Node.Text)) > 0)then
        begin
          if not FoundFunction then
          begin
            FoundFunction := True;
            index := i mod FunctionList.Items.Count;
            InsertStr:= node.Text;
          end;
          if LastSection <> Node.Parent.Text then //We enter a new section, add it to the filter tree!
            RootNode := FilterTree.Items.AddChild(nil,Node.Parent.Text);
          FilterTree.Items.AddChild(RootNode,Node.Text).Data := Node.Data;
          LastSection:= RootNode.Text;
  //        break;
        end;
      end;
      Result := FoundFunction;

      if Result then
      begin;
        FilterTreeVis(True);
        FilterTree.FullExpand;
        FilterTree.Items[1].Selected:= True;
        Writeln(FunctionList.Items[Index].Text);
        FunctionList.FullCollapse;
        FunctionList.Items[Index].Selected := true;
        FunctionList.Items[index].ExpandParents;
        editSearchList.Color := clWhite;


      end else
      begin
        FilterTreeVis(false);
        editSearchList.Color := 6711039;
        if InCodeCompletion then
          Form1.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
      end;
  end;

  if result and InCodeCompletion then
    begin;
      str := format(CompletionLine, [InsertStr]);
      with Form1.CurrScript.SynEdit do
      begin;
        Lines[CompletionCaret.y - 1] := str;
        LogicalCaretXY:= StartWordCompletion;
        i := SelStart;
        posi := pos(lowercase(editSearchList.text), lowercase(InsertStr)) + length(editSearchList.text) - 1; //underline the rest of the word
        if Posi = Length(InsertStr) then //Special occasions
        begin;
          if Length(editSearchList.Text) <> Posi then          //We found the last part of the text -> for exmaple when you Search for bitmap, you can find LoadBitmap -> We underline 'Load'
          begin;
            SelStart := i;
            SelEnd := i + pos(lowercase(editSearchList.text), lowercase(InsertStr)) -1;
            Exit;
          end;
          //We searched for the whole text -> for example LoadBitmap, and we found LoadBitmap -> Underline the whole text
          Posi := 0;
        end;
        //Underline the rest of the word
        SelStart := i + posi;
        SelEnd := SelStart + Length(InsertStr) - posi;
      end;
    end;
end;

procedure TFunctionListFrame.FunctionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
begin
  if InCodeCompletion then
  begin;
    Writeln('Not yet implemented');
    exit;
  end;
  if not (Sender is TTreeView) then
    exit;
  N := TTreeView(Sender).GetNodeAt(x, y);
  if(N = nil)then
  begin
    Self.DragKind := dkDock;
    Self.BeginDrag(false, 40);
    exit;
  end;
  Self.DragKind := dkDrag;
  if(Button = mbLeft) and (N.Level > 0)then
    Self.BeginDrag(False, 10);
  DraggingNode := N;
end;

procedure TFunctionListFrame.FunctionListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  F: ^TCustomDockForm;
begin
  if(Self.Parent is TCustomDockForm)then
  begin
    F := @Self.Parent; //can't typecast parent as a TCustomDockForm
    F^.Caption := 'Function List';
    F^.BorderStyle := bsSizeable;
    F^.OnClose := @DockFormOnClose;
    Form1.Splitter1.Hide;
  end;
end;

initialization
  {$I framefunctionlist.lrs}

end.

