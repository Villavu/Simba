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
  TestUnit, Graphics, simpleanalyzer;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
begin
  Find(false);
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
    FunctionList.FullCollapse;
    if InCodeCompletion then
    begin;
      Form1.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
      Form1.CurrScript.SynEdit.LogicalCaretXY:= point(CompletionCaret.x,CompletionCaret.y);
      Form1.CurrScript.SynEdit.SelEnd:= Form1.CurrScript.SynEdit.SelStart;
    end;
    exit;
  end;
  FoundFunction := False;
  if FunctionList.Selected <> nil then
  begin
    Start := FunctionList.Selected.AbsoluteIndex;
    if(next)then
      inc(Start);
  end else
    Start := 0;
  for i := start to start + FunctionList.Items.Count - 1 do
    if(FunctionList.Items[i mod FunctionList.Items.Count].Level = 1)then
      if(pos(lowercase(editSearchList.Text), lowercase(FunctionList.Items[i mod FunctionList.Items.Count].Text)) > 0)then
      begin
        FoundFunction := True;
        index := i mod FunctionList.Items.Count;
        break;
      end;
  Result := FoundFunction;

  if Result then
  begin;
    Writeln(FunctionList.Items[Index].Text);
    FunctionList.FullCollapse;
    FunctionList.Items[Index].Selected := true;
    FunctionList.Items[index].ExpandParents;
    editSearchList.Color := clWhite;

    if InCodeCompletion then
    begin;
      str := format(CompletionLine, [FunctionList.items[index].text]);
      with Form1.CurrScript.SynEdit do
      begin;
        Lines[CompletionCaret.y - 1] := str;
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
      Form1.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
  end;
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

