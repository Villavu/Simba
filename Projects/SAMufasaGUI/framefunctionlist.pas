unit framefunctionlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, MufasaBase,Forms, ComCtrls, StdCtrls, Controls,
  ExtCtrls, Buttons;

type

  { TFunctionListFrame }

  TFunctionListFrame = class(TFrame)
    editSearchList: TEdit;
    FunctionList: TTreeView;
    FunctionListLabel: TLabel;
    CloseButton: TSpeedButton;
    procedure editSearchListChange(Sender: TObject);
    procedure FrameEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FunctionListDblClick(Sender: TObject);
    procedure FunctionListDeletion(Sender: TObject; Node: TTreeNode);
    procedure FunctionListLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FunctionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseButtonClick(Sender: TObject);
    procedure FunctionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFilterTree : TTreeView;
    procedure FilterTreeVis(Vis : boolean);
    function GetFilterTree: TTreeView;
    { private declarations }
  public
    DraggingNode : TTreeNode;
    ScriptNode : TTreeNode;
    IncludesNode : TTreeNode;
    InCodeCompletion : boolean;
    CompletionCaret : TPoint;
    StartWordCompletion : TPoint;
    CompletionLine : string;
    CompletionStart : string;
    property FilterTree : TTreeView read GetFilterTree;
    procedure LoadScriptTree( Script : String);
    function Find(Next : boolean; backwards : boolean = false) : boolean;
    { public declarations }
  end; 

  TMethodInfo = record
    MethodStr : PChar;
    BeginPos : integer;
//    FileName : PChar;
  end;
  PMethodInfo = ^TMethodInfo;

implementation

uses
  TestUnit, Graphics, stringutil, simpleanalyzer,v_ideCodeInsight,v_ideCodeParser;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
begin
  Find(false);
end;

procedure TFunctionListFrame.FrameEndDock(Sender, Target: TObject; X, Y: Integer
  );
begin
  if Target is TPanel then
  begin
     Form1.SplitterFunctionList.Visible := true;
     CloseButton.Visible:= true;
  end
  else if Target is TCustomDockForm then
  begin
    TCustomDockForm(Target).Caption := 'Functionlist';
    TCustomDockForm(Target).OnClose := @DockFormOnClose;
    Form1.SplitterFunctionList.Visible:= false;
    CloseButton.Visible:= false;
  end;
end;

procedure TFunctionListFrame.FunctionListDblClick(Sender: TObject);
var
   Node : TTreeNode;
begin
  if FilterTree.Visible then
    Node := FilterTree.Selected
  else
    node := FunctionList.Selected;
  if node<> nil then
    if node.Level > 0 then
      if node.Data <> nil then
      begin;
        Form1.CurrScript.SynEdit.InsertTextAtCaret( GetMethodName(PMethodInfo(node.Data)^.MethodStr,true));
        Form1.RefreshTab;
      end;
end;

procedure TFunctionListFrame.FunctionListDeletion(Sender: TObject;
  Node: TTreeNode);
var
  MethodInfo : PMethodInfo;
begin
  if node.data <> nil then
  begin
    MethodInfo := PMethodInfo(Node.data);
    if MethodInfo^.MethodStr <> nil then
      StrDispose(MethodInfo^.MethodStr);
{    if MethodInfo^.FileName <> nil then
      StrDispose(MethodInfo^.filename);}
    Freemem(node.data,sizeof(TMethodInfo));
  end;
end;

procedure TFunctionListFrame.FunctionListLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.DragKind := dkDock;
  Self.BeginDrag(false, 40);
end;

procedure TFunctionListFrame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  Form1.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionListFrame.CloseButtonClick(Sender: TObject);
begin
  self.Hide;
  Form1.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionListFrame.FunctionListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
   MethodInfo : TMethodInfo;
begin
  if InCodeCompletion then
  begin;
    mDebugLn('Not yet implemented');
    exit;
  end;
  if not (Sender is TTreeView) then
    exit;
  N := TTreeView(Sender).GetNodeAt(x, y);
  if(N = nil)then
    exit;
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
  FFilterTree.OnDblClick:= FunctionList.OnDblClick;
  Result := FFilterTree;
  //We do not want to delete the data from the FilterTree
//  FilterTree.OnDeletion:= FunctionList.OnDeletion;
end;

procedure TFunctionListFrame.LoadScriptTree(Script: String);
procedure AddProcsTree(Node : TTreeNode; Procs : TDeclarationList; FileName : string);
var
  i : integer;
  tmpNode : TTreeNode;
begin;
  for i := 0 to Procs.Count - 1 do
    if (Procs[i] is TciProcedureDeclaration) then
      with Procs[i] as TciProcedureDeclaration do
      begin
        tmpNode := FunctionList.Items.AddChild(Node,name);
        tmpNode.Data := GetMem(SizeOf(TMethodInfo));
        with PMethodInfo(tmpNode.Data)^ do
        begin
          MethodStr := strnew(Pchar(CleanDeclaration));
          BeginPos:= StartPos;
        end;
      end;
end;

procedure AddIncludes(Include : TCodeInsight);
var
  parentNode : TTreeNode;
  i : integer;
begin;
  parentNode := FunctionList.Items.AddChild(
               IncludesNode,ExtractFileNameOnly(
               Include.FileName));
  AddProcsTree(parentNode,Include.Items,Include.FileName);
  for i := 0 to high(Include.Includes) do
    AddIncludes(Include.Includes[i])
end;
var
  I : integer;
  Analyzing : TCodeInsight;
  MS : TMemoryStream;
begin
  if script = '' then
    exit;
  if ScriptNode = nil then
    exit;
  if FilterTree.Visible then
    mDebugLn('Might get some acces violations now..');
  ScriptNode.DeleteChildren;
  Analyzing := TCodeInsight.Create();
  Analyzing.OnFindInclude:= @Form1.OnCCFindInclude;
  Analyzing.FileName:= Form1.CurrScript.ScriptFile;
  MS := TMemoryStream.Create;
  MS.Write(Script[1],length(script));
  Analyzing.Run(MS,nil,-1,true);
  AddProcsTree(ScriptNode,Analyzing.Items,Analyzing.FileName); //Add the procedures of the script to the script tree

  //Lame condition.. We must check if nothing new has been included since
  //last generation of the tree.. However, this will do fine for now ;)
  if IncludesNode.Count <> length(Analyzing.Includes) then
  begin;
    IncludesNode.DeleteChildren;
    for i := 0 to high(Analyzing.Includes) do
      AddIncludes(Analyzing.Includes[i]);
  end;
  ScriptNode.Expand(true);
  Analyzing.Free;
end;

function TFunctionListFrame.Find(Next : boolean; backwards : boolean = false) : boolean;
var
  Start,Len,i,index,posi,c: Integer;
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
      mDebugLn('ERROR: You cannot search next, since the Tree isnt generated yet');
      Find(false);
      exit;
    end;
    if FilterTree.Selected <> nil then
    begin;
      if backwards then
        start := FilterTree.Selected.AbsoluteIndex  - 1
      else
        Start := FilterTree.Selected.AbsoluteIndex + 1;
    end
    else
    begin
      if backwards then
        Start := FilterTree.Items.Count - 1
      else
        Start := 0;
    end;
    Len := FilterTree.Items.Count;
    i := start + len; //This is for the backwards compatibily, we do mod anways.. it just makes sure -1 isn't negative.
    c := 0;
    while c < (len ) do
    begin;
      if FilterTree.Items[i mod len].Level = 1 then
      begin
        FilterTree.Items[i mod len].Selected:= true;
        InsertStr := FilterTree.Items[i mod len].Text;
        Result := true;
        break;
      end;
      if backwards then
        dec(i)
      else
        inc(i);
      inc(c);
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
        mDebugLn(FunctionList.Items[Index].Text);
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
  if button = mbRight then
    exit;
  if InCodeCompletion then
  begin;
    mDebugLn('Not yet implemented');
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


initialization
  {$I framefunctionlist.lrs}

end.

