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
    procedure editSearchListKeyPress(Sender: TObject; var Key: char);
    procedure FunctionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FunctionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    DraggingNode : TTreeNode;
    ScriptNode : TTreeNode;
    InCodeCompletion : boolean;
    CompletionCaret : TPoint;
    StartWordCompletion : TPoint;
    CompletionLine : string;
    CompletionStart : string;
    function Find(Next : boolean) : boolean;
    { public declarations }
  end; 

implementation

uses
  TestUnit, Graphics,simpleanalyzer;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
begin
  Find(false);
end;



procedure TFunctionListFrame.editSearchListKeyPress(Sender: TObject;
  var Key: char);
begin
  Writeln('test');
end;

procedure TFunctionListFrame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  Form1.MenuItemFunctionList.Checked := False;
end;

function TFunctionListFrame.Find(Next : boolean) : boolean;
var
  Start,i,index,posi: Integer;
  FoundFunction : boolean;
  str : string;
  Node : TTreeNode;
begin
  if(editSearchList.Text = '')then
  begin
    editSearchList.Color := clWhite;
    editSearchList.Repaint;
    node := FunctionList.Items.GetFirstNode;
    while node <> nil do
    begin;
      node.Expanded:= false;
      node := Node.GetNext;
    end;
    if InCodeCompletion then
    begin;
      Form1.CurrScript.SynEdit.Lines[ CompletionCaret.y - 1] := CompletionStart;
      Form1.CurrScript.SynEdit.LogicalCaretXY:= point(CompletionCaret.x,CompletionCaret.y);
      Form1.CurrScript.SynEdit.SelEnd:= Form1.CurrScript.SynEdit.SelStart;
    end;
    exit;
  end;
  FoundFunction := false;
  if FunctionList.Selected <> nil then
  begin
    if next then
      Start := FunctionList.Selected.AbsoluteIndex + 1
    else
      Start := FunctionList.Selected.AbsoluteIndex;
  end else
    Start := 0;
  for i := start to FunctionList.Items.Count - 1 do
  begin
    Node := FunctionList.Items.Item[i];
    if Node.Level = 1 then
      if(pos(lowercase(editSearchList.Text), lowercase(FunctionList.Items[I].Text)) > 0)then
      begin;
        FoundFunction := true;
        index := i;
        break;
      end;
  end;
  if not FoundFunction then
  begin;
    for i := 0 to start - 1 do
    begin
      Node := FunctionList.Items.Item[i];
      if Node.Level = 1 then
        if(pos(lowercase(editSearchList.Text), lowercase(FunctionList.Items[I].Text)) > 0)then
        begin;
          FoundFunction := true;
          index := i;
          break;
        end;
    end;
  end;
  Result := FoundFunction;

  if Result then
  begin;
    Writeln(FunctionList.Items[Index].Text);
    for i := 0 to FunctionList.Items.Count - 1 do
      FunctionList.Items[i].Expanded:= false;
    FunctionList.Items[Index].Selected := true;
    FunctionList.Items[index].ExpandParents;
    editSearchList.Color := clWhite;

    if InCodeCompletion then
    begin;
      str :=format(CompletionLine,[ FunctionList.items[index].text]);
      with Form1.CurrScript.SynEdit do
      begin;
        Lines[ CompletionCaret.y - 1] := str;
        LogicalCaretXY:= StartWordCompletion;
        i := SelStart;
        posi := pos(lowercase(editSearchList.text), lowercase(FunctionList.items[index].text));
        SelStart := i + length(editSearchList.Text) + posi - 1;
        SelEnd := i + Length(str);
      end;
    end;
  end else
  begin
    editSearchList.Color := 6711039;
    if InCodeCompletion then
      Form1.CurrScript.SynEdit.Lines[ CompletionCaret.y - 1] := CompletionStart;
  end;
  editSearchList.Repaint;
end;

procedure TFunctionListFrame.FunctionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
begin
  N := Self.FunctionList.GetNodeAt(x, y);
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

